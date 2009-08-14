;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Model

(def entity subject ()
  ((login-disabled #f :type boolean))
  (:abstract #t)
  (:documentation "Az alany a rendszer által azonosítható dolog. Ez lehet természetes személy, valamilyen szervezet, technikai jellegű alany illetve egy másik szoftver komponens."))

(def entity technical-subject (subject)
  ((name
    :type standard-text
    :primary #t
    :reference #t)
   (description
    :type (or null standard-text)
    :primary #t))
  (:documentation "Valamilyen technikai célt szolgáló többnyire a fejlesztők vagy rendszer által használt alany."))

(macrolet ((define-technical-subject (name)
             (bind ((variable-name (symbolicate '#:* (string-upcase name) '#:-technical-subject*)))
               `(progn
                  (def singleton-persistent-instance ,variable-name
                      (or (select-technical-subject :name ,name)
                          (make-technical-subject :name ,name)))
                  (def (macro e) ,(symbolicate '#:with- (string-upcase name) '#:-technical-subject) (&body forms)
                    (with-unique-names (subject)
                      `(bind ((,subject (if (hu.dwim.rdbms:in-transaction-p)
                                            ,',variable-name
                                            (with-transaction
                                              ,',variable-name))))
                         (with-authenticated-and-effective-subject ,subject
                           ,@forms))))))))
  (define-technical-subject "import")
  (define-technical-subject "scheduler"))

(def print-object (technical-subject :identity #f)
  (print-persistent-instance -self-)
  (write-string " ")
  (princ (best-effort-slot-value -self- 'name)))

(def (generic e) developer-p (subject)
  (:method (subject)
    #f))

;;;;;;
;;; Localization

(def localization en
  (class-name.subject "subject")
  (class-name.technical-subject "technical subject")

  (slot-name.name "name")
  (slot-name.description "description")
  (slot-name.login-disabled "login disabled"))

(def localization hu
  (class-name.subject "alany")
  (class-name.technical-subject "technikai alany")

  (slot-name.name "név")
  (slot-name.description "leírás")
  (slot-name.login-disabled "belépés letiltva"))

;;;;;;
;;; Functional

(def (function e) last-login-at (subject)
  (first
   (select ((login-at-of instance))
     (from (instance authenticated-session))
     (where (eq subject (authenticated-subject-of instance)))
     (order-by :descending (login-at-of instance))
     (limit 1))))
