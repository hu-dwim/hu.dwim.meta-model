;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Entity model class

(define-model-class entity (persistent-class computed-class association-element generalization-element)
  ((element-name
    (compute-as (class-name -self-)))
   (direct-properties
    (compute-as (collect-if (of-type 'direct-property) (class-direct-slots -self-)))
    :documentation "All direct properties including persistent and computed ones.")
   (effective-properties
    (compute-as (collect-if (of-type 'effective-property) (class-slots (ensure-finalized -self-))))
    :documentation "All effective properties including persistent and computed ones.")
   (effective-association-ends
    (compute-as (collect-if (of-type 'effective-binary-association-end) (effective-properties-of -self-)))
    :documentation "All effective association ends.")
   (statistics
    (make-statistics)
    :type statistics)
   ;; TODO drop these and implement arbitrary proerty groups (?)
   (primary-properties
    (compute-as (collect-if #'primary-p (effective-properties-of -self-)))
    :type (list effective-property))
   (reference-properties
    (compute-as (collect-if #'reference-p (effective-properties-of -self-)))
    :type (list effective-property))
   (to-be-flattened-properties
    (compute-as (collect-if #'to-be-flattened-p (effective-properties-of -self-)))
    :type (list effective-property)))
  (:maker #f)
  (:definer #f)
  (:documentation "An entity is a compound value holder keeping the identity of its instances for their whole life time. The life cycle of the entity and its properties are the same. An entity instance can be referred to with its unique instance identifier (oid) and can be reloaded in subsequent transactions based on the oid."))

(define-model-class entity-d (entity persistent-class-d)
  ()
  (:maker #f)
  (:definer #f)
  (:metaclass computed-class))

(define-model-class entity-h (entity persistent-class-h)
  ()
  (:maker #f)
  (:definer #f)
  (:metaclass computed-class))

;;;;;;
;;; Defining entities

;; TODO eliminate copy-paste...
(def (macro e) defentity (class-name super-entities slots &rest options)
  (bind ((metaclass (or (second (find :metaclass options :key 'first))
                        (if (find :dimensions slots :test #'member)
                            'entity-d
                            'entity)))
         (processed-options (remove-if [member (first !1) '(:metaclass :slot-definition-transformer)] options))
         (processed-slots (mapcar (lambda (slot)
                                    (bind ((slot-options (if (oddp (length slot))
                                                             (cdr slot)
                                                             (cddr slot))))
                                      (when (getf slot-options :compute-as)
                                        (clearf (getf slot-options :dimensions)))
                                      slot))
                                  slots)))
    `(progn
       (def persistent-class* ,class-name ,super-entities ,processed-slots
         ,@`((:metaclass ,metaclass)
             (:export-class-name-p #t)
             (:export-accessor-names-p #t)
             (:slot-definition-transformer
              (lambda (slot-definition)
                ,(awhen (second (find :slot-definition-transformer options :key 'first))
                        `(setf slot-definition (funcall ,it slot-definition)))
                (bind ((slot-name (car slot-definition))
                       (slot-options (cdr slot-definition)))
                  (list* slot-name
                         (aif (getf slot-options :compute-as)
                              (append (remove-from-plist slot-options :compute-as)
                                      `(:initform
                                        (compute-as/transaction-specific ,it)
                                        :persistent #f
                                        :editable #f)
                                      (if (eq (and (consp it)
                                                   (first it)) 'select)
                                          `(:property-query (prog1-bind query (make-query ',it)
                                                              (add-lexical-variable query '-self-)))))
                              slot-options))))))
            ,@processed-options)
       (unless (find-entity ',class-name :ignore-missing #t)
         (add-model-element (find-class ',class-name)))
       ,@(mapcar (lambda (super-entity)
                   `(when (find-entity ',super-entity :ignore-missing #t)
                      (define-simple-generalization ',class-name ',super-entity)))
                 super-entities)
       (find-class ',class-name))))

(def (definer e :available-flags "e") entity (class-name super-entities slots &rest options)
  (bind ((metaclass (or (second (find :metaclass options :key 'first))
                        (if (find :dimensions slots :test #'member)
                            'entity-d
                            'entity)))
         (processed-options (remove-if [member (first !1) '(:metaclass :slot-definition-transformer)] options))
         (processed-slots (mapcar (lambda (slot)
                                    (bind ((slot-options (if (oddp (length slot))
                                                             (cdr slot)
                                                             (cddr slot))))
                                      (when (getf slot-options :compute-as)
                                        ;; TODO FIXME err... are we here silently overriding some user defined values?
                                        (clearf (getf slot-options :dimensions)))
                                      slot))
                                  slots)))
    `(progn
       (def (persistent-class* ,@-options-) ,class-name ,super-entities ,processed-slots
         ,@`((:metaclass ,metaclass)
             (:export-class-name-p #t)
             (:export-accessor-names-p #t)
             (:slot-definition-transformer
              (lambda (slot-definition)
                ,(awhen (second (find :slot-definition-transformer options :key 'first))
                   `(setf slot-definition (funcall ,it slot-definition)))
                (bind ((slot-name (car slot-definition))
                       (slot-options (cdr slot-definition)))
                  (list* slot-name
                         (aif (getf slot-options :compute-as)
                              (progn
                                (assert (null (getf slot-options :initform)) () "You have given both an :INITFORM and a :COMPUTE-AS in the slot definition ~S of entity ~S" slot-definition ',class-name)
                                (append (remove-from-plist slot-options :compute-as :initform)
                                        `(:initform
                                          (compute-as/transaction-specific ,it)
                                          :persistent #f
                                          :editable #f)
                                        (when (eq (and (consp it)
                                                       (first it))
                                                  'select)
                                          `(:property-query (prog1-bind query (make-query ',it)
                                                              (add-lexical-variable query '-self-))))))
                              slot-options))))))
            ,@processed-options)
       (unless (find-entity ',class-name :ignore-missing #t)
         (add-model-element (find-class ',class-name)))
       ,@(mapcar (lambda (super-entity)
                   `(when (find-entity ',super-entity :ignore-missing #t)
                      (define-simple-generalization ',class-name ',super-entity)))
                 super-entities)
       (find-class ',class-name))))

(def function owner-entity-of (property)
  "Returns the exclusive owner as an entity of the given property."
  (aprog1
      (hu.dwim.perec::persistent-slot-definition-class property)
    (assert (typep it 'entity) () "A property must exclusively belong to an entity.")))

;; TODO: non portable CLOS program
(def method shared-initialize :around ((entity entity) slot-names &rest args &key (statistics nil statistics-p) &allow-other-keys)
  (bind ((processed-args
          (if statistics-p
              (append
               (list :statistics
                     (if (listp statistics)
                         (apply #'make-statistics statistics)
                         statistics))
               (remove-from-plist args :statistics))
              args)))
    (apply #'call-next-method entity slot-names processed-args)))

(def (method e) element-name-of ((class persistent-class))
  (class-name class))

;;;;;;
;;; Entity CLOS MOP related

(def method direct-slot-definition-class ((entity entity) &rest args &key instance editable state-machine &allow-other-keys)
  (bind ((class (apply #'call-next-method entity args))
         (meta-class (class-of class)))
    (when (eq meta-class (find-class 'standard-class))
      (setf meta-class (find-class 'hu.dwim.perec::identity-preserving-class)))
    (cond (instance
           (class-of instance))
          ((subtypep class 'persistent-association-end-direct-slot-definition)
           (ensure-property-class meta-class (list (find-class 'direct-binary-association-end) class)))
          ((subtypep class 'persistent-direct-slot-definition)
           (ensure-property-class meta-class (list (if state-machine
                                                       (find-class 'direct-state-property)
                                                       (find-class 'direct-property))
                                                   class)))
          ((subtypep class 'computed-direct-slot-definition)
           (ensure-property-class meta-class (list* (find-class 'direct-property)
                                                    class
                                                    (when editable
                                                      (list (find-class 'persistent-direct-slot-definition))))))
          (t class))))

(def method effective-slot-definition-class ((entity entity) &rest args &key instance editable state-machine &allow-other-keys)
  (bind ((class (apply #'call-next-method entity args))
         (meta-class (class-of class)))
    (when (eq meta-class (find-class 'standard-class))
      (setf meta-class (find-class 'computed-class)))
    (cond (instance
           (class-of instance))
          ((subtypep class 'persistent-association-end-effective-slot-definition)
           (ensure-property-class meta-class (list (find-class 'effective-binary-association-end) class)))
          ((subtypep class 'persistent-effective-slot-definition)
           (ensure-property-class meta-class (list (if state-machine
                                                       (find-class 'effective-state-property)
                                                       (find-class 'effective-property))
                                                   class)))
          ((subtypep class 'computed-effective-slot-definition)
           (ensure-property-class meta-class (list* (find-class 'effective-property)
                                                    class
                                                    (when editable
                                                      (list (find-class 'persistent-effective-slot-definition))))))
          (t class))))

(def method compute-slots :after ((entity entity))
  "Invalidates the cached slot value of persistent-effective-properties whenever the effective slots are recomputed, so that all dependent computed state will be invalidated and recomputed when requested."
  (invalidate-computed-slot entity 'direct-properties)
  (invalidate-computed-slot entity 'effective-properties))

(def method compute-effective-slot-definition ((entity entity) slot-name direct-slot-definitions)
  (bind ((effective-slot-definition (call-next-method)))
    (when (typep effective-slot-definition 'effective-property)
      (setf (direct-properties-of effective-slot-definition) direct-slot-definitions))
    effective-slot-definition))

(def condition* write-definitive-slot-error (error)
  ((instance
    :type persistent-object)
   (slot
    :type effective-property))
  (:report
   (lambda (error stream)
     (format stream "The definitive slot ~A in the instance ~A is already set to a value and thus it is forbidden to change it"
             (slot-definition-name (slot-of error)) (instance-of error)))))

(def function check-definitive-property-access (entity instance slot)
  (when (and (definitive-p slot)
             (persistent-p instance)
             (not (created-p instance))
             (slot-boundp-using-class entity instance slot))
    (error 'write-definitive-slot-error :instance instance :slot slot)))

(def method (setf slot-value-using-class) :before (new-value
                                                  (entity entity)
                                                  (instance persistent-object)
                                                  (slot effective-property))
  (check-definitive-property-access entity instance slot))

(def method slot-makunbound-using-class :before ((entity entity)
                                                (instance persistent-object)
                                                (slot effective-property))
  (check-definitive-property-access entity instance slot))

;;;;;;
;;; Helpful shortcuts

(def special-variable *propery-classes* (make-hash-table :test #'equal))

(def function find-property-class (meta-class super-classes)
  (gethash (list* meta-class super-classes) *propery-classes*))

(def function (setf find-property-class) (new-value meta-class super-classes)
  (setf (gethash (list* meta-class super-classes) *propery-classes*) new-value))

(def function ensure-property-class (meta-class super-classes)
  (aif (find-property-class meta-class super-classes)
       it
       (let ((class-name (format-symbol (find-package :hu.dwim.meta-model)
                                        "~{~A~}"
                                        (iter (for super-class in super-classes)
                                              (if (not (first-iteration-p))
                                                  (collect "+"))
                                              (collect (class-name super-class))))))
         (setf (find-property-class meta-class super-classes)
               (ensure-class class-name :metaclass meta-class :direct-superclasses super-classes)))))

(def function entity-p (instance)
  "Checks whether the given instance is an entity."
  (typep instance 'entity))

(def function entity-of (instance)
  "This is an alias for the class-of accessor specialized for the entity class meta object but checks if the returned class is an entity."
  (assert (typep instance 'persistent-object))
  (prog1-bind entity (class-of instance)
    (assert (typep entity 'entity))))
