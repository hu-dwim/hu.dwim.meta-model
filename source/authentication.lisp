;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Authentication model

(define-dynamic-context* authenticated-session () :create-class #f :export-symbols #t)

(def (constant e) +maximum-http-agent-identifier-length+ 256)

(def (constant e) +failed-authentication-warning-limit+ 10)

(def persistent-type http-agent-name ()
  `(text ,+maximum-http-agent-identifier-length+))

(def entity authentication-instrument ()
  ((number-of-failed-authentication-attempts 0 :type integer-16)
   (disabled #f :type boolean :accessor disabled?))
  (:abstract #t)
  (:documentation "Az azonosításhoz használható eszköz. Egy alany több és különféle ilyen eszközzel rendelkezhet."))

(def entity authenticated-session ()
  ((status
    :alive
    :type (member :alive :logged-out :expired :shutdown :crashed))
   (login-at
    :type (or null timestamp)
    :primary #t
    :reference #t
    :index #t)
   (logout-at
    :type (or null timestamp)
    :primary #t
    :reference #t
    :index #t)
   (remote-ip-address
    :type (or null ip-address)
    :primary #t)
   (http-agent
    :type (or null http-agent-name)
    :primary #t)
   (web-application
    :type (or null (text 64))
    :primary #t)
   (web-session-id
    ;; KLUDGE this is really #.hu.dwim.wui::+session-id-length+ but it's not yet available while loading
    :type (or null (text 40))))
  (:documentation "Egy a rendszer által azonosított belépés."))

(def print-object (authenticated-session :identity #f)
  (print-persistent-instance -self-)
  (write-string " of ")
  (princ (best-effort-slot-value -self- 'authenticated-subject)))

(def dimension authenticated-session
  :type authenticated-session
  :external #t
  :default-coordinate (current-authenticated-session))

(def association
  ((:class authenticated-session :slot impersolalized-sessions :type (set authenticated-session))
   (:class authenticated-session :slot parent-session :type (or null authenticated-session))))

(def association
  ((:class subject :slot authenticated-sessions :type (set authenticated-session))
   (:class authenticated-session :slot authenticated-subject :type (or null subject) :primary #t)))

(def association
  ((:class subject :slot effective-sessions :type (set authenticated-session))
   (:class authenticated-session :slot effective-subject :type (or null subject) :primary #t)))

(def association
  ((:class subject :slot authentication-instruments :type (set authentication-instrument))
   (:class authentication-instrument :slot subject :type subject :primary #t)))

(def association
  ((:class authentication-instrument :slot authenticated-sessions :type (set authenticated-session))
   (:class authenticated-session :slot authentication-instrument :type (or null authentication-instrument))))

(def localization en
  (type-name.ip-address "IP address")

  (class-name.authentication-instrument "authentiucation instrument")
  (class-name.authenticated-session "session")

  (status.alive "alive")
  (status.logged-out "logged out")
  (status.expired "expired")
  (status.shutdown "shutdown")
  (status.crashed "crashed")
  
  (slot-name.subject "subject")
  (slot-name.authentication-instruments "authenticated instruments")
  (slot-name.status "status")
  (slot-name.login-at "login at")
  (slot-name.logout-at "logout at")
  (authenticated-session.web-application "application")
  (slot-name.http-agent "http agent")
  (slot-name.authenticated-subject "authenticated subject")
  (slot-name.effective-subject "effective subject")
  (slot-name.authenticated-sessions "sessions")
  (slot-name.effective-sessions "effective sessions")
  (slot-name.remote-ip-address "remote IP address")
  (slot-name.web-session-id "web session ID"))

(def localization hu
  (type-name.ip-address "IP cím")

  (class-name.authentication-instrument "azonosító eszköz")
  (class-name.authenticated-session "belépés")

  (status.alive "élő")
  (status.logged-out "kilépve")
  (status.expired "lejárt")
  (status.shutdown "leállítva")
  (status.crashed "lefagyott")

  (slot-name.subject "alany")
  (slot-name.authentication-instruments "azonosító eszközök")
  (slot-name.status "állapot")
  (slot-name.login-at "belépés időpontja")
  (slot-name.logout-at "kilépés időpontja")
  (authenticated-session.web-application "alkalmazás")
  (slot-name.http-agent "http kliens")
  (slot-name.authenticated-subject "azonosított alany")
  (slot-name.effective-subject "effektív alany")
  (slot-name.authenticated-sessions "belépések")
  (slot-name.effective-sessions "effektív belépések")
  (slot-name.remote-ip-address "kliens IP címe")
  (slot-name.web-session-id "web session azonosító"))

(def (special-variable e) *always-reload-authenticated-session-on-transaction-boundary* #t)

(def function call-with-reloaded-authenticated-session (function)
  (if (and (not hu.dwim.perec::*exporting-to-rdbms*) ;; KLUDGE? but we should leave alone the authenticated session while we are exporting something.
           (boundp '*authenticated-session*)
           *authenticated-session*
           (persistent-p *authenticated-session*)
           (not (instance-in-current-transaction-p *authenticated-session*)))
      (if *always-reload-authenticated-session-on-transaction-boundary*
          (progn
            (authentication.debug "CALL-WITH-RELOADED-AUTHENTICATED-SESSION is loading an existing persistent authenticated-session")
            (with-authenticated-session (load-instance *authenticated-session*)
              (funcall function)))
          (with-new-local-instance-changed-listeners
            (bind ((authenticated-session *authenticated-session*))
              (when (persistent-p authenticated-session)
                ;; TODO every now and then the authenticated-session should be loaded from the db to propagate changes
                (authentication.debug "CALL-WITH-RELOADED-AUTHENTICATED-SESSION is cloning an existing persistent authenticated-session")
                (assert (not (hu.dwim.perec::cached-instance-of (oid-of authenticated-session))))
                ;; TODO: this copies somewhat deeper then really required for 90% percent of the requests
                (setf authenticated-session (load-instance authenticated-session :skip-existence-check #t :copy-cached-slot-values #t))
                (authentication.dribble "Registering instance change listeners on the authenticated-session")
                (labels ((cached-session-invalidator ()
                           (invalidate-cached-instance authenticated-session))
                         (listener (transaction instance transaction-event)
                           (assert (and transaction instance transaction-event))
                           (assert (transaction-of authenticated-session))
                           (hu.dwim.rdbms::register-hook-in-transaction (transaction-of authenticated-session) :before :always #'cached-session-invalidator)))
                  (hu.dwim.perec::map-cached-instances (lambda (instance)
                                               (register-local-instance-changed-listener instance #'listener)))))
              (with-authenticated-session authenticated-session
                (funcall function)))))
      (funcall function)))

(def method call-in-transaction :around (database (transaction transaction-mixin/dwim) function)
  (declare (ignore database transaction function))
  (call-with-reloaded-authenticated-session #'call-next-method))

;;;;;;
;;; Authenticate

(def (generic e) authenticate (authentication-information)
  (:documentation "Used to authenticate a subject communicating to the system. The provided authentication information is used to look up matching authentication instruments in the database.")

  (:method ((subject subject))
    (values #t nil subject)))

(def (function e) login (authentication-information &rest authenticated-session-initargs
                                                    &key (allow-parallel-sessions #t)
                                                    on-successful-authentication-callback
                                                    &allow-other-keys)
  "When successful it returns a new authenticated-session instance holding the result."
  (authentication.info "Logging in with authentication information ~A" authentication-information)
  (mark-transaction-for-commit-only)
  (remove-from-plistf authenticated-session-initargs :allow-parallel-sessions :on-successful-authentication-callback)
  (bind (((:values authenticated-p authentication-instrument subject) (authenticate authentication-information)))
    (flet ((fail (&optional reason)
             (authentication.info "Login failed for authentication information ~A~:[.~;, reason: ~S.~]" authentication-information reason reason)
             (when authentication-instrument
               (bind ((failed-attempts (incf (number-of-failed-authentication-attempts-of authentication-instrument))))
                 (when (> failed-attempts +failed-authentication-warning-limit+)
                   (audit.warn "Failed authentication count is ~A of instrument ~A, subject ~A" failed-attempts authentication-instrument (subject-of authentication-instrument))))
               ;; could disable the instrument or the (login-disabled-p subject) here
               )
             (return-from login nil)))
      (if authenticated-p
          (progn
            (unless subject
              (setf subject (subject-of authentication-instrument)))
            (authentication.debug "Authenticated instrument ~A, subject ~A" authentication-instrument subject)
            (when authentication-instrument
              (when (disabled? authentication-instrument)
                (fail "authenticated instrument is disabled"))
              (setf (number-of-failed-authentication-attempts-of authentication-instrument) 0))
            (when (login-disabled-p subject)
              (fail "subject's login is disabled"))
            (bind ((authenticated-session (apply #'make-instance
                                                 'authenticated-session
                                                 :effective-subject subject
                                                 :authenticated-subject subject
                                                 :authentication-instrument authentication-instrument
                                                 :login-at (transaction-timestamp)
                                                 authenticated-session-initargs))
                   (parallel-sessions nil))
              (login.info "Successful login by subject ~A using authentication-instument ~A into session ~A" subject authentication-instrument authenticated-session)
              (unless allow-parallel-sessions
                (setf parallel-sessions (select (session)
                                          (from (session authenticated-session))
                                          (where (and (eq (authenticated-subject-of session) subject)
                                                      (null (logout-at-of session))))))
                (iter (for parallel-session :in-sequence parallel-sessions)
                      (unless (eq parallel-session authenticated-session)
                        (authentication.info "Forcing logout of parallel session ~A, authenticated subject ~A" parallel-session (authenticated-subject-of parallel-session))
                        (setf (logout-at-of parallel-session) (transaction-timestamp)))))
              (when on-successful-authentication-callback
                (funcall on-successful-authentication-callback authenticated-session))
              (values authenticated-session parallel-sessions)))
          (fail "authenticate failed")))))

(def (function e) logout (&key (status :logged-out) (logout-at (transaction-timestamp)))
  "The logout date is stored in the current authenticated session."
  (login.info "Logged out authenticated session ~A" *authenticated-session*)
  (mark-transaction-for-commit-only)
  ;; these are not asserts because screwing up logout with signalled errors is a bad idea
  (unless (login-at-of *authenticated-session*)
    (authentication.error "There's some trouble with the authenticated session ~A at logout: login-at slot is NIL" *authenticated-session*))
  (unless (authenticated-subject-of *authenticated-session*)
    (authentication.error "There's some trouble with the authenticated session ~A at logout: authenticated-subject is NIL" *authenticated-session*))
  (awhen (logout-at-of *authenticated-session*)
    (authentication.error "There's some trouble with the authenticated session ~A at logout: logout-at slot is not NIL: ~A" *authenticated-session* it))
  (iter (with authenticated-subject = (authenticated-subject-of *authenticated-session*))
        (for session :first *authenticated-session* :then (parent-session-of session))
        (while session)
        (authentication.debug "Setting logout-at slot of authenticated session ~A" session)
        (assert (eq (authenticated-subject-of session) authenticated-subject))
        (setf (logout-at-of session) logout-at)
        (setf (status-of session) status)))

(def (function e) impersonalize (new-effective-subject &key web-session-id)
  "Creates a new impersonalized authenticated-session and returns it."
  (authentication.info "Impersonalizing effective subject ~A by authenticated subject ~A"
                       new-effective-subject
                       (authenticated-subject-of *authenticated-session*))
  (mark-transaction-for-commit-only)
  (assert (null (parent-session-of *authenticated-session*)))
  (assert (or (null web-session-id)
              (string= web-session-id (web-session-id-of *authenticated-session*))))
  (bind ((previous-authenticated-session *authenticated-session*))
    (prog1
        (with-reloaded-instance previous-authenticated-session
          (assert (null (logout-at-of previous-authenticated-session)))
          (setf *authenticated-session* (make-authenticated-session
                                         :web-session-id (web-session-id-of previous-authenticated-session)
                                         :parent-session previous-authenticated-session
                                         :login-at (transaction-timestamp)
                                         :authenticated-subject (authenticated-subject-of previous-authenticated-session)
                                         :effective-subject new-effective-subject
                                         :authentication-instrument (authentication-instrument-of previous-authenticated-session)
                                         :remote-ip-address (remote-ip-address-of previous-authenticated-session))))
      (invalidate-cached-instance previous-authenticated-session))))

(def (function e) cancel-impersonalization ()
  "Cancels the effect of a previous impersonalization and returns the parent session."
  (authentication.info "Cancelling impersonalization of effective subject ~A by authenticated subject ~A"
                       (effective-subject-of *authenticated-session*)
                       (authenticated-subject-of *authenticated-session*))
  (mark-transaction-for-commit-only)
  (bind ((previous-authenticated-session *authenticated-session*))
    (prog1
        (with-reloaded-instance previous-authenticated-session
          (bind ((parent-session (parent-session-of *authenticated-session*)))
            (assert parent-session)
            (assert (null (logout-at-of previous-authenticated-session)))
            (setf (logout-at-of previous-authenticated-session) (transaction-timestamp))
            (setf *authenticated-session* parent-session)))
      (invalidate-cached-instance previous-authenticated-session))))

(def (macro e) with-login-and-logout (authentication-instrument &body forms)
  ;; TODO only used by the (long dead) test code. delme?
  (once-only (authentication-instrument)
    (with-unique-names (authenticated-session)
      `(with-transaction
         (let ((,authenticated-session (login ,authentication-instrument)))
           (unless ,authenticated-session
             (error "Failed to login with ~A" ,authentication-instrument))
           (with-revived-instance ,authenticated-session
             (with-authenticated-session ,authenticated-session
               (multiple-value-prog1
                   (progn
                     ,@forms)
                 (logout)))))))))

(def (macro e) with-authenticated-and-effective-subject (subject &body forms)
  "Useful to unconditionally set a technical subject, for example when importing data. This inserts a new AUTHENTICATED-SESSION in the database, use accordingly..."
  (once-only (subject)
    (with-unique-names (in-transaction-p timestamp)
      `(bind ((,in-transaction-p (hu.dwim.rdbms:in-transaction-p))
              (,timestamp (if ,in-transaction-p
                              (transaction-timestamp)
                              (now))))
         (assert ,subject)
         (with-authenticated-session (make-authenticated-session
                                      :persistent ,in-transaction-p
                                      :effective-subject ,subject
                                      :authenticated-subject ,subject
                                      :login-at ,timestamp
                                      ;; since this is going to run in a single transaction what else could we set?
                                      :logout-at ,timestamp)
           ,@forms)))))

(def (function e) logged-in-p (&optional (authenticated-session (when (has-authenticated-session)
                                                                *authenticated-session*)))
  (and authenticated-session
       (and (authenticated-subject-of authenticated-session)
            (effective-subject-of authenticated-session)
            (null (logout-at-of authenticated-session)))))

(def (function ei) current-authenticated-subject ()
  (in-authenticated-session authenticated-session
    ;; TODO there should be a (revive-instance authenticated-session). think of nested transactions...
    (authenticated-subject-of authenticated-session)))

(def (function ei) current-effective-subject ()
  (in-authenticated-session authenticated-session
    ;; TODO there should be a (revive-instance authenticated-session). think of nested transactions...
    (effective-subject-of authenticated-session)))

(def function (setf current-effective-subject) (subject)
  (with-transaction
    (authentication.debug "Changing effective subject of ~A to ~A" *authenticated-session* subject)
    (setf (effective-subject-of *authenticated-session*) subject))
  (invalidate-cached-instance *authenticated-session*))

(def function select-authenticated-sessions-with-login-after-timestamp (timestamp)
  (select (authenticated-session)
    (from (authenticated-session authenticated-session))
    (where (timestamp>= (login-at-of authenticated-session) timestamp))))

(def function select-subjects-with-login-after-timestamp (timestamp)
  (select (subject)
    (from (authenticated-session authenticated-session)
          (subject subject))
    (where (and (eq subject (authenticated-subject-of authenticated-session))
                (timestamp>= (login-at-of authenticated-session) timestamp)))))

(def (function e) logout-stale-authenticated-sessions (seconds-since-login)
  (bind ((timestamp (local-time:now)))
    (adjust-timestamp! timestamp (:offset :sec (- seconds-since-login)))
    ;; TODO should use a do-instances construct
    (dolist (authenticated-session (select-instances (instance authenticated-session)
                                     (where (and (timestamp< (login-at-of instance) timestamp)
                                                 (null (logout-at-of instance))
                                                 (not (typep (authenticated-subject-of instance) 'technical-subject))))))
      (setf (logout-at-of authenticated-session) (transaction-timestamp)))))

(def function average-authenticated-session-time (authenticated-sessions)
  (iter (with count = 0)
        (for authenticated-session :in authenticated-sessions)
        (bind ((login (login-at-of authenticated-session))
               (logout (logout-at-of authenticated-session)))
          (if logout
              (progn
                (sum (local-time:timestamp-difference logout login) :into sum)
                (incf count))))
        (finally
         (return (values (coerce (/ sum 60) 'float)
                         count
                         (coerce (/ sum count 60) 'float))))))
