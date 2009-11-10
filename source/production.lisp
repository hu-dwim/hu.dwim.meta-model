;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(def (constant e) +generic-command-line-options+
  '(("cluster-name"
     :type string
     :initial-value "production"
     :documentation "The cluster name as stored in the database. Configuration of this cluster node is read from the database by looking up the computer's network name.")
    ("pid-file"
     :type string
     :documentation "The PID file is created when the server starts. The file will be deleted when the server stops.")
    ("swank-port"
     :type integer
     :initial-value 4005
     :documentation "The port is used to connect to the running server with SLIME.")
    ("repl"
     :type boolean
     :optional #t
     :documentation "If provided then instead of starting the server only a REPL will be started. This might be useful for mainenance, testing and bug fixing.")
    ("test-mode"
     :type boolean
     :optional #t
     :documentation "If provided then the server starts up in test mode. This allows to login with the same password for any subject.")))

(def (function e) ready-to-quit? (wui-server)
  (not (or (hu.dwim.wui:is-server-running? wui-server)
           #+nil
           (is-persistent-process-scheduler-running?)
           ;; TODO: we should not depend on hu.dwim.model
           #+nil
           (hu.dwim.model:is-cluster-node-running?))))

(def function setup-loggers-for-production (project-system-name)
  (setf *log-directory* (format nil "/var/log/~A/" (string-downcase project-system-name)))
  (unless (ignore-errors
            (truename *log-directory*))
    (error "Log directory does not exist or is not accessible. Tried: ~S" *log-directory*))
  (bind ((standard-logger (find-logger 'standard-logger)))
    (setf (hu.dwim.logger::appenders-of standard-logger)
          (list (make-level-filtering-appender +warn+ (make-thread-safe-file-appender "error.log"))
                (make-thread-safe-file-appender "root.log")))
    (setf (log-level standard-logger) +info+)))

;; TODO: factor this apart into utility functions for better reusability and more finer control in the end application
;; TODO rename to... what? run-dwim-server? it won't return ever, and when C-c'd it'll call exit...
(def (function e) run-production-server (command-line-arguments project-system-name wui-server wui-application)
  (restart-case
      (progn
        (setup-loggers-for-production project-system-name)
        (meta-model.info "~S toplevel init speaking" project-system-name)
        (bind ((project-system (asdf:find-system project-system-name))
               (project-package (find-package (system-package-name project-system))))
          (setf *random-state* (make-random-state t))
          (setf *package* project-package)
          (ensure-utf-8-external-format)
          ;; TODO: factor out the database arguments into rdbms
          (bind (((&key database-host database-port database-name database-user-name database-password cluster-name pid-file swank-port repl test-mode &allow-other-keys) command-line-arguments)
                 (connection-specification `(:host ,database-host :port ,database-port :database ,database-name :user-name ,database-user-name :password ,database-password))
                 (cluster-name (or cluster-name
                                   (if (hu.dwim.wui:running-in-test-mode? wui-application)
                                       "test"
                                       "production"))))
            (when repl
              (incf swank-port))
            (start-swank-server swank-port)
            (meta-model.info "Database connection is: ~S~%" connection-specification)
            (setf (connection-specification-of *model*) connection-specification)
            (awhen test-mode
              (meta-model.info "Enabling test mode")
              (setf (hu.dwim.wui:running-in-test-mode? wui-application) #t)
              (unless (search "-test" database-name)
                (cerror "Continue"
                        "Do you really want to start up in test mode with a database that does not contain \"-test\" in its name? (~S)."
                        database-name)))
            (with-simple-restart (skip-export-model "Skip synchronizing the model in the VM with the database SQL schema")
              (with-model-transaction
                (meta-model.info "Calling EXPORT-MODEL with database ~A, connection-specification ~A" (database-of *model*) (connection-specification-of *model*))
                (export-model)))
            (awhen (load-and-eval-config-file project-system-name)
              (meta-model.info "Loaded config file ~A" it))
            (if repl
                (sb-impl::toplevel-repl nil)
                (with-pid-file pid-file
                  (disable-debugger)
                  (handler-bind ((hu.dwim.rdbms:unconfirmed-schema-change
                                  ;; NOTE: this handler is not bound in the started worker threads but EXPORT-MODEL is explicitly called at startup, so this is not a problem.
                                  (lambda (error)
                                    (best-effort-log-error "Exiting because something was tried to be altered in the RDBMS schema at unattended startup: ~A" error))))
                    (meta-model.info "Starting up cluster ~S" cluster-name)
                    (with-new-compiled-query-cache
                      #+nil
                      (hu.dwim.model:startup-cluster-node cluster-name wui-server))
                    (hu.dwim.wui:startup-server wui-server)
                    (with-save-core-and-die-restart
                      (block running
                        ;; TODO: put the timer stuff in hu.dwim.wui and remove dependency
                        (bind ((timer (hu.dwim.wui::timer-of wui-server)))
                          (flet ((running-signal-handler (signal code scp)
                                   (declare (ignore signal code scp))
                                   (meta-model.info "SIGTERM/SIGINT was received, initiating shutdown")
                                   (format *debug-io* "~%SIGTERM/SIGINT was received, initiating shutdown~%")
                                   (hu.dwim.wui:register-timer-entry timer (local-time:now)
                                                                     (named-lambda quit-now ()
                                                                       (return-from running))
                                                                     :kind :single-shot
                                                                     :name "Quit now timer entry")))
                            (hu.dwim.wui:register-timer-entry timer (* 60 10)
                                                              (named-lambda stdout-ticker ()
                                                                (format *debug-io* "~A: Another heartbeat at request number ~A; seems like all is well...~%"
                                                                        (local-time:now) (awhen wui-server
                                                                                           (hu.dwim.wui:processed-request-count-of it)))
                                                                (finish-output *debug-io*))
                                                              :kind :periodic
                                                              :name "Standard output ticker")
                            (hu.dwim.wui:register-timer-entry timer 60
                                                              (named-lambda session-purge ()
                                                                (with-model-database
                                                                  (hu.dwim.wui:purge-sessions wui-application)))
                                                              :kind :periodic
                                                              :name "Session purge")
                            (hu.dwim.wui:register-timer-entry timer 5
                                                              (named-lambda ready-to-quit-checker ()
                                                                (when (ready-to-quit? wui-server)
                                                                  (return-from running)))
                                                              :kind :periodic
                                                              :name "Quit checker")
                            (hu.dwim.wui:register-timer-entry timer 5
                                                              'flush-caching-appender-caches
                                                              :kind :periodic
                                                              :name "Log flusher")
                            (sb-sys:enable-interrupt sb-unix:sigterm #'running-signal-handler)
                            (sb-sys:enable-interrupt sb-unix:sigint #'running-signal-handler)
                            (meta-model.info "Final signal handlers are installed, everything's started normally. Calling into DRIVE-TIMER now...")
                            (format *debug-io* "~A: Everything's started normally~%" (local-time:now))
                            (hu.dwim.wui::drive-timer timer))))
                      #+nil
                      (hu.dwim.model:shutdown-cluster-node)
                      (hu.dwim.wui:shutdown-server wui-server)
                      (iter (until (ready-to-quit? wui-server))
                            (meta-model.debug "Still not ready to quit, waiting...")
                            (sleep 1))
                      (flush-caching-appender-caches))
                    (meta-model.info "Everything's down, exiting normally")
                    (format *debug-io* "Everything's down, exiting normally~%"))))))
        (quit 0))
    (abort nil
      :report (lambda (stream)
                (format stream "Give up starting the image and quit the VM process with error code 2"))
      (quit 2))))
