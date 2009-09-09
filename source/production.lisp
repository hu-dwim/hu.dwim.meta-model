;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;; TODO: factor out postgresql arguments into hu.dwim.rdbms
(def special-variable *command-line-arguments*
    '(("help"
       :type boolean
       :optional #t
       :documentation "Provides this help text")
      ("database-host"
       :type string
       :initial-value "localhost"
       :documentation "The server host name where the database is listening")
      ("database-port"
       :type integer
       :initial-value 5432
       :documentation "The server port where the database is listening")
      ("database-name"
       :type string
       :documentation "The database name that will be connected")
      ("database-user-name"
       :type string
       :documentation "The user name that is used to connect to the database")
      ("database-password"
       :type string
       :documentation "The password that is used to connect to the database")
      ;; TODO: handle this
      ("http-port"
       :type integer
       :initial-value 80
       :documentation "The HTTP server port where it will listening")
      ("cluster-name"
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

(def (function e) setup-logger (project-system-name)
  (setf *log-directory* (format nil "/var/log/~A/" (string-downcase project-system-name)))
  (unless (ignore-errors
            (truename *log-directory*))
    (error "Log directory does not exist: ~S" *log-directory*))
  (bind ((error-appender (make-level-filter-appender +warn+ (hu.dwim.logger:make-thread-safe-file-log-appender "error.log")))
         (dwim-appender (hu.dwim.logger:make-thread-safe-file-log-appender "dwim.log")))
    ;; TODO: put in hu.dwim.logger?
    (flet ((set-appenders (logger-name &rest appender-designators)
             ;; TODO: KLUDGE: remove this level setting
             (setf (log-level (find-logger logger-name)) +info+)
             (setf (appenders-of (find-logger logger-name))
                   (list* error-appender
                          (mapcar (lambda (appender-designator)
                                    (etypecase appender-designator
                                      (string (hu.dwim.logger:make-thread-safe-file-log-appender appender-designator))
                                      (log-appender appender-designator)))
                                  appender-designators)))))
      (set-appenders 'hu.dwim.wui::wui "wui.log")
      (set-appenders 'hu.dwim.rdbms::log "rdbms.log")
      (set-appenders 'hu.dwim.rdbms::sql-log "sql.log")
      (set-appenders 'hu.dwim.meta-model::log dwim-appender)
      #+nil
      (set-appenders 'hu.dwim.meta-model::audit dwim-appender (make-instance 'hu.dwim.model:persistent-log-appender)))))

(def (function e) production-image-toplevel (project-system-name wui-server wui-application)
  (restart-case 
      (progn
        (setup-logger project-system-name)
        (log.info "~S toplevel init speaking" project-system-name)
        (bind ((project-system (asdf:find-system project-system-name))
               (project-package (find-package (system-package-name project-system))))
          (setf *random-state* (make-random-state t))
          (setf *package* project-package)
          (ensure-external-format-is-utf-8)
          (bind (((&key help database-host database-port database-name database-user-name database-password cluster-name pid-file swank-port repl test-mode)
                  (command-line-arguments:process-command-line-options *command-line-arguments* (command-line-arguments)))
                 (connection-specification `(:host ,database-host :port ,database-port :database ,database-name :user-name ,database-user-name :password ,database-password))
                 (cluster-name (or cluster-name
                                   (if (hu.dwim.wui:running-in-test-mode? wui-application)
                                       "test"
                                       "production"))))
            (when help
              (command-line-arguments:show-option-help *command-line-arguments*)
              (quit-production 0))
            (when repl
              (incf swank-port))
            (log.info "Database connection is: ~S~%" connection-specification)
            (setf (connection-specification-of *model*) connection-specification)
            (awhen test-mode
              (log.info "Enabling test mode")
              (setf (hu.dwim.wui:running-in-test-mode? wui-application) #t)
              (unless (search "-test" database-name)
                (cerror "Continue"
                        "Do you really want to start up in test mode with a database that does not contain \"-test\" in its name? (~S)."
                        database-name)))
            (if repl
                (progn
                  (with-simple-restart (abort "Skip exporting model and start the REPL")
                    (with-model-transaction
                      (log.info "Calling EXPORT-MODEL with database ~A, connection-specification ~A" (database-of *model*) (connection-specification-of *model*))
                      (export-model)))
                  (awhen (load-and-eval-config-file project-system-name)
                    (log.info "Loaded config file ~A" it))
                  (sb-impl::toplevel-repl nil))
                (with-pid-file pid-file
                  (disable-debugger)
                  (handler-bind ((hu.dwim.rdbms:unconfirmed-alter-table-error
                                  ;; please note that this handler is not bound in the started threads
                                  ;; but EXPORT-MODEL is explicitly called from STARTUP-CLUSTER-NODE,
                                  ;; so this is not a problem.
                                  (lambda (error)
                                    (best-effort-log-error "Exiting because something was tried to be altered in the RDBMS schema at unattended startup: ~A" error))))
                    (log.info "Starting up cluster ~S" cluster-name)
                    (with-new-compiled-query-cache
                      #+nil
                      (hu.dwim.model:startup-cluster-node cluster-name wui-server))
                    (hu.dwim.wui:startup-server wui-server)
                    (with-save-core-and-die-restart
                      (awhen (load-and-eval-config-file project-system-name)
                        (log.info "Loaded config file ~A" it))
                      (block running
                        ;; TODO: put the timer stuff in hu.dwim.wui and remove dependency
                        (bind ((timer (hu.dwim.wui::timer-of wui-server)))
                          (flet ((running-signal-handler (signal code scp)
                                   (declare (ignore signal code scp))
                                   (log.info "SIGTERM/SIGINT was received, initiating shutdown")
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
                            (sb-sys:enable-interrupt sb-unix:sigterm #'running-signal-handler)
                            (sb-sys:enable-interrupt sb-unix:sigint #'running-signal-handler)
                            (log.info "Final signal handlers are installed, everything's started normally. Calling into DRIVE-TIMER now...")
                            (format *debug-io* "~A: Everything's started normally~%" (local-time:now))
                            (hu.dwim.wui::drive-timer timer))))
                      #+nil
                      (hu.dwim.model:shutdown-cluster-node)
                      (iter (until (ready-to-quit? wui-server))
                            (log.debug "Still not ready to quit, waiting...")
                            (sleep 1)))
                    (log.info "Everything's down, exiting normally")
                    (format *debug-io* "Everything's down, exiting normally~%"))))))
        (quit-production 0))
    (give-up nil
      :report (lambda (stream)
                (format stream "Give up starting the image and quit"))
      (quit-production 2))))
