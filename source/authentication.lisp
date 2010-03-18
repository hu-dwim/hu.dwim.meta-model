;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

#+nil
(progn
  ;; TODO add an AUTHENTICATION entity which is instantiated every time an authentication happens. would provide support for re-authentication which is sometimes required by sensitive operations...

  (def (entity e) authentication ()
    (happened-at :type timestamp))

  (def association
    ((:slot authentications :type (set authentication))
     (:slot authenticated-session :type authenticated-session)))

  (def association
    ((:slot authentications :type (set authentication))
     (:slot authentication-instrument :type authentication-instrument))))

;;;;;;
;;; Authentication model

(define-dynamic-context* authenticated-session () :create-class #f :export-symbols #t)

(def (function e) current-authenticated-session ()
  ;; we keep this as a functional accessor to be able to add any needed asserts/logic later on
  *authenticated-session*)

(def (constant e) +maximum-http-agent-identifier-length+ 256)

(def (constant e) +failed-authentication-warning-limit+ 10)

(def persistent-type http-user-agent-name ()
  `(text ,+maximum-http-agent-identifier-length+))

(def (entity e) authentication-instrument ()
  ((number-of-failed-authentication-attempts 0 :type integer-16)
   (disabled #f :type boolean :accessor disabled?))
  (:abstract #t)
  (:documentation "Az azonosításhoz használható eszköz. Egy alany több és különféle ilyen eszközzel rendelkezhet."))

(def (entity e) authenticated-session ()
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
   (http-user-agent
    :type (or null http-user-agent-name)
    :primary #t)
   (web-application
    :type (or null (text 64))
    :primary #t)
   (web-session-id
    :type (or null (text #.hu.dwim.wui::+session-id-length+))))
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
  ((:slot impersolalized-sessions :type (set authenticated-session))
   (:slot parent-session :type (or null authenticated-session))))

(def association
  ((:slot authenticated-sessions :type (set authenticated-session))
   (:slot authenticated-subject :type (or null subject) :primary #t)))

(def association
  ((:slot effective-sessions :type (set authenticated-session))
   (:slot effective-subject :type (or null subject) :primary #t)))

(def association
  ((:slot authentication-instruments :type (set authentication-instrument))
   (:slot subject :type subject :primary #t)))

(def association
  ((:slot authenticated-sessions :type (set authenticated-session))
   (:slot authentication-instrument :type (or null authentication-instrument))))

(def (function e) current-authenticated-subject ()
  (authenticated-subject-of *authenticated-session*))

(def (function e) current-effective-subject ()
  (effective-subject-of *authenticated-session*))

(def function (setf current-effective-subject) (subject)
  (authentication.debug "Changing effective subject of ~A to ~A" *authenticated-session* subject)
  (mark-transaction-for-commit-only)
  (setf (effective-subject-of *authenticated-session*) subject)
  (invalidate-cached-instance *authenticated-session*))

(def (special-variable e) *always-reload-authenticated-session-on-transaction-boundary* #t)

(def (function e) call-with-reloaded-authenticated-session (function)
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

(def (function e) login/subject (subject authentication-instrument)
  (prog1-bind authenticated-session (make-instance 'authenticated-session
                                                   :effective-subject subject
                                                   :authenticated-subject subject
                                                   :authentication-instrument authentication-instrument
                                                   :login-at (transaction-timestamp))
    (login.info "Successful login by subject ~A using authentication-instument ~A into session ~A" subject authentication-instrument authenticated-session)))

(def (function e) login/authenticated-session (authentication-instrument &key (allow-parallel-sessions #f))
  (check-type authentication-instrument authentication-instrument)
  (bind ((subject (subject-of authentication-instrument)))
    (authentication.debug "LOGIN/AUTHENTICATED-SESSION of subject ~A, authentication-instrument ~A" subject authentication-instrument)
    (assert (in-transaction-p))
    (mark-transaction-for-commit-only)
    (flet ((fail (reason)
             (return-from login/authenticated-session (values nil reason))))
      (when (disabled? authentication-instrument)
        (fail "authentication instrument is disabled"))
      (setf (number-of-failed-authentication-attempts-of authentication-instrument) 0)
      (when (login-disabled? subject)
        (fail "subject's login is disabled"))
      (bind ((authenticated-session (login/subject subject authentication-instrument)))
        (unless allow-parallel-sessions
          (bind ((parallel-sessions (select (session)
                                      (from (session authenticated-session))
                                      (where (and (eq (authenticated-subject-of session) subject)
                                                  (null (logout-at-of session)))))))
            ;; TODO invalidate web sessions of the parallel authenticated sessions?
            (iter (for parallel-session :in-sequence parallel-sessions)
                  (unless (eq parallel-session authenticated-session)
                    (authentication.info "Forcing logout of parallel session ~A, authenticated subject ~A" parallel-session (authenticated-subject-of parallel-session))
                    (setf (logout-at-of parallel-session) (transaction-timestamp))))))
        authenticated-session))))

(def (function e) logout/authenticated-session (&key (status :logged-out) (logout-at (transaction-timestamp)))
  (login.info "Logging out authenticated session ~A" *authenticated-session*)
  (check-type status (member :logged-out :expired :shutdown :crashed))
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

(def (function e) valid-authenticated-session? (&optional (authenticated-session (when (has-authenticated-session)
                                                                                   *authenticated-session*)))
  (and (typep authenticated-session 'authenticated-session)
       (null (logout-at-of authenticated-session))))

(def (function e) impersonalize/authenticated-session (new-effective-subject)
  (authentication.info "Impersonalizing effective subject ~A by authenticated subject ~A" new-effective-subject (authenticated-subject-of *authenticated-session*))
  (mark-transaction-for-commit-only)
  (assert (null (parent-session-of *authenticated-session*)))
  (bind ((previous-authenticated-session *authenticated-session*))
    (prog1
        (with-reloaded-instance previous-authenticated-session
          (assert (null (logout-at-of previous-authenticated-session)))
          (setf *authenticated-session* (make-instance 'authenticated-session
                                                       ;; TODO :web-session-id (web-session-id-of previous-authenticated-session)
                                                       :parent-session previous-authenticated-session
                                                       :login-at (transaction-timestamp)
                                                       :authenticated-subject (authenticated-subject-of previous-authenticated-session)
                                                       :effective-subject new-effective-subject
                                                       :authentication-instrument (authentication-instrument-of previous-authenticated-session)
                                                       :remote-ip-address (remote-ip-address-of previous-authenticated-session))))
      (invalidate-cached-instance previous-authenticated-session))))

(def (function e) cancel-impersonalization/authenticated-session ()
  "Cancels the effect of a previous impersonalization and returns the parent session."
  (authentication.info "Cancelling impersonalization of effective subject ~A by authenticated subject ~A" (effective-subject-of *authenticated-session*) (authenticated-subject-of *authenticated-session*))
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

(def (macro e) with-authenticated-and-effective-subject (subject &body forms)
  "Useful to unconditionally set a technical subject, for example when importing data. This inserts a new AUTHENTICATED-SESSION in the database, use accordingly..."
  (once-only (subject)
    (with-unique-names (in-transaction? timestamp)
      `(bind ((,in-transaction? (hu.dwim.rdbms:in-transaction-p))
              (,timestamp (if ,in-transaction?
                              (transaction-timestamp)
                              (now))))
         (assert ,subject)
         (with-authenticated-session (make-instance 'authenticated-session
                                                    :persistent ,in-transaction?
                                                    :effective-subject ,subject
                                                    :authenticated-subject ,subject
                                                    :login-at ,timestamp
                                                    ;; since this is going to run in a single transaction what else could we set?
                                                    :logout-at ,timestamp)
           ,@forms)))))

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
