;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(def (constant e) +generic-command-line-options+
  '((("verbose" #\Space)
     :type boolean
     :optional #t
     :documentation "Try to provide more information about what's happening.")
    (("cluster-name" #\Space)
     :type string
     :initial-value "production"
     :documentation "The cluster name as stored in the database. Configuration of this cluster node is read from the database by looking up the computer's network name.")
    (("pid-file" #\Space)
     :type string
     :documentation "The PID file is created when the server starts. The file will be deleted when the server stops.")
    (("swank-port" #\Space)
     :type integer
     :initial-value 4005
     :documentation "The port is used to connect to the running server with SLIME.")
    (("disable-debugger" #\Space)
     :type boolean
     :optional #t
     :documentation "Disable the debugger, so that in case of unhandled toplevel errors the process quits. True by default unless in --repl mode.")
    (("repl" #\Space)
     :type boolean
     :optional #t
     :documentation "If provided then instead of starting the server only a REPL will be started. This might be useful for mainenance, testing and bug fixing.")
    (("test-mode" #\Space)
     :type boolean
     :optional #t
     :documentation "If provided then the server starts up in test mode. This allows to login with the same password for any subject.")
    (("export-model" #\Space)
     :type boolean
     :optional #f
     :documentation "When started in REPL mode, skip exporting the model into the RDBMS")))

(def (function e) ready-to-quit? (wui-server)
  (not (or (is-server-running? wui-server)
           #+nil
           (is-persistent-process-scheduler-running?)
           ;; TODO: we should not depend on hu.dwim.model
           #+nil
           (hu.dwim.model:is-cluster-node-running?))))

(def function log-to-console (format-control &rest format-arguments)
  (apply 'format *standard-output* format-control format-arguments)
  (terpri *standard-output*)
  (finish-output *standard-output*))

;; TODO: factor this apart into utility functions for better reusability and more finer control in the end application
(def (function e) run-production-server (command-line-arguments project-system-name wui-server wui-application)
  (log-to-console "Starting up server, PID is ~S" (isys:getpid))
  (setup-logging-for-production (string+ "/var/log/" (string-downcase project-system-name) "/"))
  (meta-model.info "~S toplevel init speaking" project-system-name)
  (bind ((project-system (asdf:find-system project-system-name))
         (project-package (find-package (system-package-name project-system))))
    (setf *random-state* (make-random-state t))
    (meta-model.debug "*random-state* was randomized to ~A" *random-state*)
    (setf *package* project-package)
    (meta-model.debug "*package* was set ~A" *package*)
    (ensure-default-external-format-is-utf-8)
    ;; TODO what about *terminal-io*? maybe: (setf *terminal-io* *standard-output*)
    ;; TODO: factor out the database arguments into rdbms
    (bind (((&key database-host database-port database-name database-user-name database-password
                  cluster-name pid-file swank-port repl test-mode verbose (export-model #t) (disable-debugger #t disable-debugger-provided?)
                  &allow-other-keys) command-line-arguments)
           (connection-specification `(:host ,database-host :port ,database-port :database ,database-name :user-name ,database-user-name :password ,database-password))
           (loggable-connection-specification (remove-from-plist connection-specification :password))
           (cluster-name (or cluster-name
                             (if (running-in-test-mode? wui-application)
                                 "test"
                                 "production"))))
      (start-swank-server swank-port)
      (when (and disable-debugger
                 (or (not repl)
                     disable-debugger-provided?))
        (disable-debugger))
      (when verbose
        (bind ((root-logger (find-logger 'root-logger)))
          (appendf (hu.dwim.logger::appenders-of root-logger)
                   (list (make-thread-safe-stream-appender '*standard-output*)))
          (setf (log-level root-logger) +debug+)
          (meta-model.debug "Set loggers to be verbose as requested by --verbose")))
      (when (and (not export-model)
                 (not repl))
        (cerror "Start in repl mode" "Skipping export-model is only allowed in REPL mode")
        (setf repl #t))
      (meta-model.info "Using database connection: ~S" loggable-connection-specification)
      (unless (and database-host database-port database-name database-user-name database-password)
        (warn "Database connection specification is not fully provided, which will most probably lead to an error: ~S" loggable-connection-specification))
      (setf (connection-specification-of *model*) connection-specification)
      (awhen test-mode
        (meta-model.info "Enabling test mode")
        (setf (running-in-test-mode? wui-application) #t)
        (unless (search "-test" database-name)
          (cerror "Continue"
                  "Do you really want to start up in test mode with a database name that does not contain \"-test\"? (~S)."
                  database-name)))
      (when export-model
        (with-simple-restart (skip-export-model "Skip synchronizing the model in the VM with the database SQL schema")
          (meta-model.info "Calling EXPORT-MODEL with database ~A, connection-specification ~A" (database-of *model*) loggable-connection-specification)
          (with-model-transaction
            (export-model))))
      (awhen (load-and-eval-config-file project-system-name)
        (meta-model.info "Loaded config file ~A" it))
      (if repl
          (sb-impl::toplevel-repl nil)
          (with-layered-error-handlers ((lambda (error)
                                          (print-error-safely (build-error-log-message :error-condition error
                                                                                       :message "Error reached toplevel in the main thread"))
                                          (unless disable-debugger
                                            (invoke-debugger error)))
                                        (lambda (&key &allow-other-keys)
                                          (print-error-safely "Calling QUIT from toplevel error handler")
                                          (quit 3)))
            (with-temporary-directory ()
              (flet ((startup-signal-handler (signal code scp)
                       (declare (ignore signal code scp))
                       (log-to-console "SIGTERM/SIGINT was received while starting up; exiting abnormally")
                       (quit 2)))
                #*((:sbcl
                    (sb-sys:enable-interrupt sb-unix:sigterm #'startup-signal-handler)
                    (sb-sys:enable-interrupt sb-unix:sigint #'startup-signal-handler)
                    (log-to-console "Temporary startup signal handlers are installed"))
                   (t (warn "No support for installing signal handlers on your implementation, stale PID files may remain"))))
              (surround-body-when pid-file
                  (with-pid-file (pid-file)
                    (-body-))
                (handler-bind ((hu.dwim.rdbms:unconfirmed-schema-change
                                ;; NOTE: this handler is not bound in the started worker threads but EXPORT-MODEL is explicitly called at startup, so this is not a problem.
                                (lambda (error)
                                  (print-error-safely "Exiting because something was tried to be altered in the RDBMS schema at unattended startup: ~A" error))))
                  (meta-model.info "Starting up cluster ~S" cluster-name)
                  #+nil
                  (with-new-compiled-query-cache
                    (hu.dwim.model:startup-cluster-node cluster-name wui-server))
                  (startup-server wui-server)
                  ;; TODO: put the timer stuff in hu.dwim.web-server and remove dependency
                  (bind ((timer (hu.dwim.web-server::timer-of wui-server)))
                    (flet ((%register-timer-entry (name time-interval thunk)
                             (register-timer-entry timer thunk :interval time-interval :name name))
                           (console-status-printer ()
                             (log-to-console "~A: Another heartbeat at request number ~A, used memory ~,2F MiB, application session count ~A"
                                             (local-time:now)
                                             (when wui-server
                                               (processed-request-counter-of wui-server))
                                             (/ (sb-kernel::dynamic-usage) 1024 1024)
                                             (when wui-server
                                               (mapcar (compose 'hash-table-count 'session-id->session-of)
                                                       (collect-if (of-type 'application) (brokers-of wui-server))))))
                           (session-purge ()
                             (with-model-database
                               (purge-sessions wui-application)))
                           (quit-request-checker ()
                             (when (ready-to-quit? wui-server)
                               (drive-timer/abort timer))))
                      (%register-timer-entry "Console status printer" (* 60 10) #'console-status-printer)
                      (%register-timer-entry "Session purge" 60 #'session-purge)
                      (%register-timer-entry "Quit request checker" 5 #'quit-request-checker)
                      (%register-timer-entry "Log flusher" 5 'flush-caching-appenders)
                      #+nil
                      (%register-timer-entry "Status logger" 5
                                             ;; TODO log some useful info like the number of web sessions, etc...
                                             )
                      (flet ((running-signal-handler (signal code scp)
                               (declare (ignore signal code scp))
                               (meta-model.info "SIGTERM/SIGINT was received, initiating shutdown")
                               (log-to-console "~%SIGTERM/SIGINT was received, initiating shutdown")
                               (drive-timer/abort timer)))
                        (sb-sys:enable-interrupt sb-unix:sigterm #'running-signal-handler)
                        (sb-sys:enable-interrupt sb-unix:sigint #'running-signal-handler)))
                    (meta-model.info "Final signal handlers are installed, everything's started normally. Calling into DRIVE-TIMER now...")
                    (log-to-console "~A: Everything's started normally" (local-time:now))
                    (hu.dwim.web-server::drive-timer timer))
                  ;; (hu.dwim.model:shutdown-cluster-node)
                  (shutdown-server wui-server)
                  (iter (until (ready-to-quit? wui-server))
                        (meta-model.debug "Still not ready to quit, waiting...")
                        (sleep 1))
                  (flush-caching-appenders))))
            (meta-model.info "Everything's down, exiting normally")
            (log-to-console "Everything's down, exiting normally"))))))
