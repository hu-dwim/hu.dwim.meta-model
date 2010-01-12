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
  (slot-name.http-user-agent "http agent")
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
  (slot-name.http-user-agent "http kliens")
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
