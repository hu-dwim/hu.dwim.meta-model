;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

;; TODO: review this whole stuff
;; TODO: maybe we could live with only operation and authorization and the more complex examples would just be macros on top of that
;; TODO: in this case we will have to parse the condition forms to understand more deeply what is going on
(in-package :hu.dwim.meta-model)

(define-model-class operation (generalization-element model-element)
  ((arguments
    nil
    :type list
    :documentation "Specifies the names of the arguments taken by the operation."))
  (:documentation "Operations may generalize other operations but a specialized operation must include its parent operation parameters and may add new ones. An authorization rule that is defined on an operation will be inherited along the generalizations to the specialized operations."))

(define-copy-method (copy-one copy-query) ((operation operation) htable)
  operation)

(define-copy-method (copy-one hu.dwim.perec::copy-shallow) ((operation operation) htable)
  operation)

(def (macro e) defoperation (name supers args &optional documentation)
  `(progn
     (define-operation ',name
         :arguments ',args
         :documentation ,documentation)
     ,@(mapcar (lambda (super)
                 `(define-simple-generalization ',name ',super))
               supers)
     (export ',name)))

(def (definer e :available-flags "e") operation (name supers args &optional documentation)
  `(defoperation ,name ,supers ,args ,documentation))

(define-model-class authorization (model-element)
  ((arguments
    nil
    :type list
    :documentation "Specifies the names of the arguments taken by the authorization.")
   (condition
    :type (or atom list)
    :documentation "The condition used to decide whether the operation is permitted or not. The condition may refer to the arguments of the operation by their name.")
   (condition-function-definition
    :type cons
    :documentation "The lisp form used to compile the condition function.")
   (condition-function
    :type function
    :documentation "The compiled condition function.")))

(define-model-class operation-restricted-authorization (authorization)
  ((operation
    :type operation
    :documentation "The operation on which this authorization rule must be applied."))
  (:abstract #t))

(define-model-class subject-restricted-authorization (authorization owner-model-element)
  ((authenticated-subject-restriction
    nil
    :type argument-restriction
    :documentation "Additional restriction on the authenticated subject.")
   (effective-subject-restriction
    nil
    :type argument-restriction
    :documentation "Additional restriction on the effective subject."))
  (:abstract #t))

(define-model-class argument-restricted-authorization (authorization owner-model-element)
  ((argument-restrictions
    nil
    :type (list argument-restriction)
    :documentation "A list of argument restrictions applied to the operation arguments. An authorization rule matches an actual operation iff the arguments match the argument restrictions."))
  (:abstract #t)
  (:documentation "An authorization is mathcing an actual operation if the actual arguments are matching the corresponding argument restrictions including the authenticated and effective subject as being implicit arguments. An operation is permitted if all matching authorizations for the actual parameters returns true. Authorization rules strictly overridden by other authorization rules are not considered when granting permission."))

(define-model-class restricted-authorization (operation-restricted-authorization
                                              subject-restricted-authorization
                                              argument-restricted-authorization)
  ())

(define-model-class argument-restriction (owned-model-element)
  ((argument-type
    nil
    :type model-element
    :documentation "If present the argument must be an instance of the given type.")
   (argument-supertype
    nil
    :type model-element
    :documentation "If present the argument must be a subtype of the given type"))
  (:documentation "An argument restriction matches an argument if all restrictions provided by bound slots are fulfilled by the argument."))

(define-condition authorization-error (error)
  ())

(def (special-variable e) *disable-authorization* #f)

;;;;;;
;;; Defining authorizations

(def (definer e :available-flags "e") authorization (name args options &body body)
  `(defauthorization ,name ,args ,options ,@body))

(def (macro e) defauthorization (name args options &body body)
  (bind ((operation (getf options :operation))
         (operation-object (when operation (find-operation operation)))
         (object (getf options :object))
         (effective-subject (getf options :effective-subject))
         (authorization-definer
          (cond ((getf options :effective-subject)
                 'define-restricted-authorization)
                ((getf options :operation)
                 'define-operation-restricted-authorization)
                (t
                 'define-authorization))))
    `(,authorization-definer ,(if name
                                  `',name
                                  (authorization-name-for operation effective-subject 'anything))
                             ,@(when operation
                                     `(:operation (find-operation ',operation)))
                             :arguments ',(if operation
                                              (append (arguments-of operation-object) args)
                                              args)
                             ,@(when effective-subject
                                     `(:effective-subject-restriction (make-argument-restriction 'effective-subject-restriction
                                                                                                 :argument-type (find-entity ',effective-subject))))
                             ,@(when object
                                     `(:argument-restrictions (list (make-argument-restriction ',(first (arguments-of operation-object)))
                                                                    (make-argument-restriction ',(second (arguments-of operation-object))
                                                                                               :argument-type (find-entity ',object)))))
                             :condition ',@body)))

(def method shared-initialize :after ((authorization authorization) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (bind ((condition-function-name (condition-function-name-for-authorization authorization))
         (condition-function-arguments
          (append '(-operation- -authenticated-subject- -effective-subject- &rest args &key)
                  (arguments-of authorization)
                  '(&allow-other-keys)))
         (condition-function-arguments-names
          (remove-if [member !1 '(&rest &key &allow-other-keys)]
                     condition-function-arguments)))
    (clear-authorize-operation-cache)
    (setf (condition-function-definition-of authorization)
          `(def function ,condition-function-name ,condition-function-arguments
             (declare (ignorable ,@condition-function-arguments-names))
             ,(expand-authorization-calls
               authorization
               `(and
                 ,(matcher-condition-for-authorization authorization)
                 ,(condition-of authorization)))))
    (setf (condition-function-of authorization) (fdefinition (eval (condition-function-definition-of authorization))))
    (setf (get (condition-function-name-for-authorization authorization) :authorization) authorization)
    (eval `(define-query-macro ,condition-function-name ,condition-function-arguments
             ,(bind ((condition-function-definition (condition-function-definition-of authorization))
                     (condition (cddddr condition-function-definition))
                     (variable-names (remove-if [member !1 '(&rest &key &allow-other-keys)] condition-function-arguments)))
                    `(tree-substitute ,(cons 'list variable-names) ',variable-names ',(cons 'and condition)))))))

(def function apply-authorization (authorization-name operation authenticated-subject effective-subject args)
  (apply authorization-name operation authenticated-subject effective-subject args))

(define-query-macro apply-authorization (authorization-name &rest args)
  (bind ((normal-arguments (butlast args))
         (last-argument (ensure-list (lastcar args))))
    `(,(second authorization-name) ,@normal-arguments ,@last-argument)))

(def function expand-authorization-calls (authorization form)
  (let (applied-authorization)
    (cond ((listp form)
           (mapcar [expand-authorization-calls authorization !1] form))
          ((or (typep form 'authorization)
               (and (symbolp form)
                    (setf applied-authorization (find-authorization form :ignore-missing #t))))
           `(apply-authorization ',(condition-function-name-for-authorization (or applied-authorization form))
                                 -operation- -authenticated-subject- -effective-subject- args))
          (t form))))

(def function find-authorization-for-condition-function (function-name)
  (get function-name :authorization))

(def function condition-function-name-for-authorization (authorization)
  (bind ((name (element-name-of authorization)))
    (format-symbol (symbol-package name) "~A-P" name)))

(def function find-argument-restriction (owner-element-name argument-restriction-name)
  (find argument-restriction-name
        (argument-restrictions-of (find-authorization owner-element-name))
        :key #'element-name-of))

(def function authorization-name-for (operation-name &optional
                                                     (effective-subject-type-name nil effective-subject-type-name-p)
                                                     (instance-type-name nil instance-type-name-p))
  (format-symbol (symbol-package optional-name)
                 "~{~A~}"
                 (append
                  (list operation-name)
                  (when effective-subject-type-name-p
                    (list "-BY-" effective-subject-type-name))
                  (when instance-type-name-p
                    (list "-FOR-" instance-type-name)))))

;;;;;;
;;; Authorizing actual operations

;; TODO proper thread safety
(def special-variable *authorization-cache* (make-hash-table :test #'equal :synchronized #t))

(def function cached-authorize-operation (arguments)
  (gethash arguments *authorization-cache*))

(def function (setf cached-authorize-operation) (new-value arguments)
  (setf (gethash arguments *authorization-cache*) new-value))

(def function clear-authorize-operation-cache ()
  (clrhash *authorization-cache*))

(def generic condition-for-argument-restriction (restriction argument-name)
  (:method ((restriction (eql nil)) argument-name)
    nil)

  (:method ((restriction argument-restriction) argument-name)
    `((and
       ,@(awhen (argument-type-of restriction)
                `((typep ,argument-name ,it)))
       ,@(awhen (argument-supertype-of restriction)
                `((subtypep ,argument-name ,it)))))))

(def generic matcher-condition-for-authorization (authorization)
  (:method ((authorization authorization))
    #t)

  (:method ((authorization operation-restricted-authorization))
    `(and
      ,(call-next-method)
      (generalizesp -operation- ,(operation-of authorization))))

  (:method ((authorization restricted-authorization))
    `(and
      ,(call-next-method)
      ,@(condition-for-argument-restriction (authenticated-subject-restriction-of authorization)
                                            '-authenticated-subject-)
      ,@(condition-for-argument-restriction (effective-subject-restriction-of authorization)
                                            '-effective-subject-)
      ,@(mappend 'condition-for-argument-restriction
                 (argument-restrictions-of authorization)
                 (arguments-of authorization)))))

(def (generic e) authorize-operation (operation &rest args &key &allow-other-keys)
  (:documentation "Returns true or false indicating that the authorization failed or not.")

  (:method ((operation symbol) &rest args &key &allow-other-keys)
    (or *disable-authorization* (apply #'authorize-operation (find-operation operation) args)))

  (:method (operation &rest args &key &allow-other-keys)
    (in-authenticated-session (authenticated-subject effective-subject)
      (apply #'authorize-operation-for-subject operation authenticated-subject effective-subject args))))

(def generic authorize-operation-for-subject (operation authenticated-subject effective-subject &rest args &key &allow-other-keys)
  (:documentation "Returns true or false indicating that the authorization failed or not.")

  (:method ((operation symbol) authenticated-subject effective-subject &rest args &key &allow-other-keys)
    (apply #'authorize-operation-for-subject (find-operation operation) authenticated-subject effective-subject args))

  (:method (operation authenticated-subject effective-subject &rest args &key &allow-other-keys)
    (authorization.dribble "Authorizing operation ~A for authenticated subject ~A and effective subject ~A with arguments ~{~A ~}" operation authenticated-subject effective-subject args)
    (or *disable-authorization*
        (bind ((cache-arguments (unless (find :-instance- (arguments-of operation))
                                  (list* operation
                                         (class-of authenticated-subject)
                                         (class-of effective-subject)
                                         args)))
               ((:values cached-succeeded? found-in-cache?) (cached-authorize-operation cache-arguments))
               (succeeded? (if found-in-cache?
                               cached-succeeded?
                               (setf (cached-authorize-operation cache-arguments)
                                     (aif (find-authorization 'top-level-authorization)
                                          (apply (condition-function-name-for-authorization it)
                                                 operation
                                                 authenticated-subject
                                                 effective-subject
                                                 args)
                                          (cerror "No top level authorization rule found!" 'authorization-error))))))
          (authorization.debug "Access ~A for ~A with arguments ~{~A ~}. Authenticated subject is ~A, effective subject is ~A." (if succeeded? "granted" "denied ") operation args authenticated-subject effective-subject)
          succeeded?))))

(def (function e) authorize-query (operation query)
  (authorize-query! operation (copy-query query)))

(def (generic e) authorize-query! (operation query)
  (:documentation "Destructively modifies query according to the authorization rules.")

  (:method ((operation symbol) query)
    (if *disable-authorization* query (authorize-query! (find-operation operation) query)))

  (:method (operation query)
    (authorization.dribble "Authorizing operation ~A on query ~A" operation query)
    (unless *disable-authorization*
      (in-authenticated-session (authenticated-subject effective-subject) ;; TODO: add special variables to query instead
        (if-bind top-level-authorization (find-authorization 'top-level-authorization)
          (iter (for variable-name in (hu.dwim.perec::get-query-variable-names query))
                (authorization.debug "Adding assert for variable name ~A" variable-name)
                (add-assert query `(,(condition-function-name-for-authorization top-level-authorization)
                                     ,operation
                                     ,authenticated-subject
                                     ,effective-subject
                                     :-entity- (entity-of ,variable-name)
                                     :-instance- ,variable-name)))
          (cerror "No top level authorization rule found!" 'authorization-error))))
    query))

(def (function e) execute-authorized-query (query &rest lexical-variable-values)
  (apply #'execute-query (authorize-query 'read-instance-operation query) lexical-variable-values))
