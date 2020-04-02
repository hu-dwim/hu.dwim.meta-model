;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Association model class

(define-model-class association (relationship)
  ((element-name
    (compute-as (name-of -self-))))
  (:abstract #t)
  (:finder #f)
  (:documentation "An association is a special kind of relationship connecting arbitrary number of association ends. An association object at the model level means that instances of the associated entities at the object level may refer to each other and referential integrity will be kept between the association ends. A generalization between two associations means that any reference accessible through the sub association is also accessible through the super association."))

(define-model-class association-element (relationship-element)
  ()
  (:abstract #t)
  (:documentation "An element which can be used in an association."))

(define-model-class association-end (property relationship-end)
  ()
  (:abstract #t)
  (:documentation "An association end refers to an association element that is part of the association. The cardinality values of the association end tell how many instances of that association element might be associated with the other association ends."))

(define-slot-class direct-association-end (association-end direct-property)
  ())

(define-slot-class effective-association-end (association-end effective-property)
  ())

;;;;;;
;;; Binary association model class

(define-model-class binary-association (persistent-association association binary-relationship)
  ((relationship-ends
    (compute-as (hu.dwim.perec::association-ends-of -self-))
    :type (list binary-association-end)))
  (:documentation "An association that has exactly two association ends."))

(define-model-class binary-association-d (binary-association persistent-association-d)
  ()
  (:maker #f)
  (:definer #f)
  (:metaclass computed-class))

(define-model-class binary-association-end
    (association-end binary-relationship-end persistent-association-end-slot-definition)
  ((relationship-element
    (compute-as (hu.dwim.perec::associated-class-of -self-))
    :type association-element)
   (relationship
    (compute-as (hu.dwim.perec::association-of -self-))
    :type association)
   (statistics
    (make-statistics)
    :type statistics))
  (:maker #f)
  (:definer #f)
  (:documentation "A special assocation end which is part of a binary association."))

(define-slot-class direct-binary-association-end
    (direct-association-end binary-association-end persistent-association-end-direct-slot-definition)
  ()
  (:metaclass hu.dwim.perec::identity-preserving-class))

(define-slot-class effective-binary-association-end
    (effective-association-end binary-association-end persistent-association-end-effective-slot-definition)
  ())

;;;;;;
;;; Defining associations

(def definer association (&body association-ends)
  (bind ((options (cdr association-ends))
         (primary-association-end (first (first association-ends)))
         (secondary-association-end (second (first association-ends)))
         (metaclass (or (second (find :metaclass options :key 'first))
                        (if (find :dimensions options :test #'member)
                            'binary-association-d
                            'binary-association))))
    (labels ((class-for-type (type)
               (if (consp type)
                   (second type)
                   type))
             ;; KLUDGE: perec normalize-type-for cannot be used because it relies on the classes being already defined
             (normalized-type-for (type)
               (cond ((and (listp type)
                           (eq 'or (first type)))
                      (let ((type (remove-if [member !1 '(null unbound)] type)))
                        (assert (= (length type) 2))
                        (second type)))
                     (t type)))
             (process-association-end (association-end other-association-end)
               (bind ((class (getf association-end :class))
                      (other-type (normalized-type-for (getf other-association-end :type))))
                 (append
                  (unless class
                    (list :class (class-for-type other-type)))
                  association-end))))
      `(progn
         (def (persistent-association* ,@-options-)
           (,(process-association-end primary-association-end secondary-association-end)
             ,(process-association-end secondary-association-end primary-association-end))
           ,@options
           (:export-accessor-names-p #t)
           (:metaclass ,metaclass))
         ;; TODO: KLUDGE:
         (iter (for (key value) :in-hashtable hu.dwim.perec::*persistent-associations*)
               (when (and (typep value 'binary-association)
                          (not (member value (model-elements-of *model*))))
                 (add-model-element value)))))))

(def method primary-relationship-end-of ((association persistent-association))
  (hu.dwim.perec::primary-association-end-of association))

(def method secondary-relationship-end-of ((association persistent-association))
  (hu.dwim.perec::secondary-association-end-of association))

(def method relationship-ends-of ((association persistent-association))
  (hu.dwim.perec::association-ends-of association))

(def method relationship-element-of ((association-end persistent-association-end-slot-definition))
  (hu.dwim.perec::associated-class-of association-end))
