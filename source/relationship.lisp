;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Relationship model class

(define-model-class relationship (owner-model-element)
  ((relationship-ends
    (compute-as nil)
    :type (list relationship-end))
   (relationship-elements
    (compute-as (mapcar #'relationship-element-of (relationship-ends-of -self-)))
    :type (list relationship-element)
    :documentation "The relationship elements refererred by the ends of the relationship."))
  (:abstract #t)
  (:documentation "A relationship connects two or more model elements with each other. It is an abstract class without specific semantic meaning, so there are no direct relationship instances in a model. It is allowed to refer to the same model element several times in a single relationship. The life cycle of the relationship and its relationship ends are the same."))

(define-model-class relationship-element (model-element)
  ((declared-relationships
    (compute-as (compute-declared-relationships -self-))
    :type (list relationship))
   (relationships
    (compute-as (compute-relationships -self-))
    :type (list relationship)
    :documentation "A list of relationships where this relationship element is used."))
  (:abstract #t)
  (:documentation "An element which can be used in a relationship."))

(define-model-class relationship-end (owned-model-element)
  ((relationship-element
    :type relationship-element
    :computed-in compute-as
    :documentation "A relationship end refers to a model element. The model element will be related with other model elements referred by other relationship ends within the same relationship.")
   (relationship
    :type relationship
    :documentation "The relationship of which this relationship end is part of."))
  (:abstract #t)
  (:documentation "One end of a relationship corresponding to a single relationship element."))

;;;;;;
;;; Binary relationship model class

(define-model-class binary-relationship (relationship)
  ((primary-relationship-end
    (compute-as (first (relationship-ends-of -self-)))
    :type binary-relationship-end
    :documentation "In a binary relationship the first relationship end is called the primary relationship end.")
   (secondary-relationship-end
    (compute-as (second (relationship-ends-of -self-)))
    :type binary-relationship-end
    :documentation "In a binary relationship the second relationship end is called the secondary relationship end.")
   (primary-relationship-element
    (compute-as (relationship-element-of (primary-relationship-end-of -self-)))
    :type relationship-element
    :documentation "The relationship element of the primary relationship end.")
   (secondary-relationship-element
    (compute-as (relationship-element-of (secondary-relationship-end-of -self-)))
    :type relationship-element
    :documentation "The relationship element of the secondary relationship end."))
  (:abstract #t)
  (:documentation "A special relationship that has exactly two relationship ends."))

(define-model-class binary-relationship-end (relationship-end)
  ((primary-relationship-end
    (compute-as (eq (element-name-of -self-) (element-name-of (primary-relationship-end-of (relationship-of -self-)))))
    :type boolean
    :documentation "True if this end is the primary relationship end of its relationship.")
   (secondary-relationship-end
    (compute-as (eq (element-name-of -self-) (element-name-of (secondary-relationship-end-of (relationship-of -self-)))))
    :type boolean
    :documentation "True if this end is the primary relationship end of its relationship.")
   (other-relationship-end
    (compute-as (find-if [not (eq -self- !1)] (relationship-ends-of (relationship-of -self-))))
    ;;:type binary-relationship-end
    :documentation "A binary relationship has two ends each one referring to the other.")
   (other-relationship-element
    (compute-as (relationship-element-of (other-relationship-end-of -self-)))
    :type relationship-element))
  (:abstract #t)
  (:documentation "A special relationship end that is part of a binary relationship."))

;;;;;;
;;; N-ary relationship model class

(define-model-class n-ary-relationship (relationship)
  ()
  (:abstract #t)
  (:documentation "A special relationship that has arbitrary number of relationship ends."))

(define-model-class n-ary-relationship-end (relationship-end)
  ()
  (:abstract #t)
  (:documentation "A special relationship end that is part of a n-ary relationship."))

;;;;;;
;;; Localization

(def localization en
  (class-name.relationship "relationship")
  (class-name.relationship-element "relationship element")
  (class-name.relationship-end "relationship end")
  (class-name.binary-relationship "binary relationship")
  (class-name.binary-relationship-end "binary relationship end")
  (class-name.n-ary-relationship "n-ary relationship")
  (class-name.n-ary-relationship-end "n-ary relationship end"))

;;;;;;
;;; Defining relationships

(def generic compute-relationships (relationship-element)
  (:method ((relationship-element relationship-element))
           (collect-model-elements-if (lambda (model-element)
                                        (and (typep model-element 'relationship)
                                             (member relationship-element (relationship-elements-of model-element))))
                                      *model*)))

(def generic compute-declared-relationships (relationship-element)
  (:method ((relationship-element relationship-element))
           (collect-model-elements-if (lambda (model-element)
                                        (and (typep model-element 'relationship)
                                             (member relationship-element (relationship-elements-of model-element))))
                                      *model*
                                      :include-derived #f)))

(def method shared-initialize :after ((relationship relationship) slot-names &key &allow-other-keys)
  (dolist (relationship-end (relationship-ends-of relationship))
    (when (and relationship-end
               (typep relationship-end 'relationship-end))
      (setf (relationship-of relationship-end) relationship)
      (assert (relationship-element-of relationship-end) nil
              "A relationship end must refer to a relationship element"))))

(def method shared-initialize :after ((relationship binary-relationship) slot-names &key &allow-other-keys)
  (assert (= 2 (length (relationship-ends-of relationship))) nil
          "A binary relationship must have exactly two relationhip ends"))

;;;;;;
;;; Helper methods

(def (function e) collect-relationships-between-relationship-elements (relationship-elements)
  ;; KLUDGE: kill the stupid collect stuff
  (remove-duplicates
   (append (iter (for relationship in (collect-relationships))
                 (when (every [member !1 relationship-elements]
                              (relationship-elements-of relationship))
                   (collect relationship)))
           (iter (for (name association) :in-hashtable hu.dwim.perec::*persistent-associations*)
                 (when (every [member !1 relationship-elements]
                              (hu.dwim.perec::associated-classes-of association))
                   (collect association)))
           (iter outer
                 (for class :in relationship-elements)
                 (when (typep class 'persistent-class)
                   (iter (for superclass :in (hu.dwim.perec::persistent-direct-superclasses-of class))
                         (when (and (member superclass relationship-elements)
                                    (not (typep superclass 'entity)))
                           (in outer (collect (make-instance 'generalization
                                                             :generalization-ends (list (make-instance 'generalization-end
                                                                                                       :element-name 'super
                                                                                                       :generalization-element class)
                                                                                        (make-instance 'generalization-end
                                                                                                       :element-name 'sub
                                                                                                       :generalization-element superclass))))))))))))
