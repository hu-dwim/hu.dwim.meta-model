;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Basic model elements

(define-model-class element ()
  ((derived
    #f
    :type boolean
    :computed-in nil
    :documentation "Derived model elements are not directly defined by the user, but computed on demand based on some other higher level or more complex model elements, abstractions or parameters."))
  (:abstract #t)
  (:documentation "Base class of all elements in the model."))

(define-model-class named-element (element)
  ((element-name
    nil
    :type symbol
    :computed-in nil
    :documentation "The name of the element. Some model classes might require the names to be unique among its instances."))
  (:abstract #t)
  (:documentation "Allows elements to have names."))

(define-model-class documented-element (element)
  ((documentation
    nil
    :type (or null string function documentation)
    :computed-in nil
    :documentation "This is a one parameter function or a string or a list or a documentation object which provides the documentation for the element."))
  (:abstract #t)
  (:documentation "Allows elements to have documentation."))

(define-model-class owned-element (element)
  ((owner-element
    (compute-as nil)
    :type (or null owner-element)
    :documentation "The exclusive owner of this element."))
  (:abstract #t)
  (:documentation "An owned-element exclusively owned by an owner-element."))

(define-model-class owner-element (element)
  ((owned-elements
    (compute-as nil)
    :type (list owned-element)))
  (:abstract #t)
  (:documentation "An element that may exclusively own other elements."))

(define-model-class model-element (documented-element named-element)
  ((model-element-collections
    (compute-as (compute-model-element-collections -self-))
    :type (list model-element-collection)))
  (:abstract #t)
  (:documentation "Base class for all top level model-elements in the model. Elements directly under the model must all subclass from this class."))

(define-model-class owner-model-element (owner-element model-element)
  ()
  (:abstract #t)
  (:documentation "Base class for all top level owner model elements."))

(define-model-class owned-model-element (owned-element documented-element named-element)
  ()
  (:abstract #t)
  (:documentation "Base class for owned elements in model-elements."))

(define-model-class model-element-collection (element)
  ((model-elements
    (compute-as nil)
    :type (list model-element)))
  (:abstract #t)
  (:documentation "A non exculsive collection of model-elements for any purpose."))

;;;;;;
;;; Basic model element functions

(def print-object named-element
  (if (slot-boundp -self- 'element-name)
      (princ (element-name-of -self-))
      (princ "<<name unbound>>")))

(def function check-model-element-name (named-element)
  (bind ((name (element-name-of named-element))
         (name-package (symbol-package name)))
    (unless (or (eq *package* name-package) (eq name-package (find-package :hu.dwim.meta-model)))
      (error "Current package is ~A while model element name ~A is in the package ~A"
             *package* name name-package))))

(def generic compute-model-element-collections (model-element)
  (:method ((model-element model-element))
    (collect-model-elements-if (lambda (elt)
                                 (and (typep elt 'model-element-collection)
                                      (member model-element (model-elements-of elt))))
                               *model*)))
