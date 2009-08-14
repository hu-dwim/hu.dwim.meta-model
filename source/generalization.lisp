;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(define-model-class generalization (binary-relationship)
  ((relationship-ends                   ; override base slot
    :accessor generalization-ends-of
    :initarg :generalization-ends
    :type (list generalization-end))
   (relationship-elements               ; override base slot
    :accessor generalization-elements-of
    :initarg :generalization-elements
    :type (list relationship-element))
   (primary-relationship-end            ; override base slot
    :accessor sub-generalization-end-of
    :type generalization-end)
   (secondary-relationship-end          ; override base slot
    :accessor super-generalization-end-of
    :type generalization-end)
   (primary-relationship-element        ; override base slot
    :accessor sub-generalization-element-of
    :type generalization-element)
   (secondary-relationship-element      ; override base slot
    :accessor super-generalization-element-of
    :type generalization-element))
  (:documentation "A special relationship providing inheritance semantics on the class level model. If A generalizes B in the model, then any instance of B on the object level may act as an instance of A."))

(define-model-class generalization-element (relationship-element)
  ((declared-generalizations
    (compute-as (collect-if-typep 'generalization (declared-relationships-of -self-)))
    :type (list generalization))
   (generalizations
    (compute-as (collect-if-typep 'generalization (relationships-of -self-)))
    :type (list generalization)
    :documentation "A list of generalizations this element is part of.")
   (declared-direct-super-generalization-elements
    (compute-as (compute-declared-direct-super-generalization-elements -self-))
    :type (list generalization-element))
   (declared-direct-sub-generalization-elements
    (compute-as (compute-declared-direct-sub-generalization-elements -self-))
    :type (list generalization-element))
   (direct-super-generalization-elements
    (compute-as (compute-direct-super-generalization-elements -self-))
    :type (list generalization-element))
   (direct-sub-generalization-elements
    (compute-as (compute-direct-sub-generalization-elements -self-))
    :type (list generalization-element))
   (effective-super-generalization-elements
    (compute-as (compute-effective-super-generalization-elements -self-))
    :type (list generalization-element))
   (effective-sub-generalization-elements
    (compute-as (compute-effective-sub-generalization-elements -self-))
    :type (list generalization-element)))
  (:abstract #t)
  (:documentation "An element which can be used in a generalization either as supertype or as subtype."))

(define-model-class generalization-end (binary-relationship-end)
  ((primary-relationship-end            ; override base slot
    :accessor sub-generalization-end-p
    :type boolean)
   (secondary-relationship-end          ; override base slot 
    :accessor super-generalization-end-p
    :type boolean)
   (relationship-element                ; override base slot
    :accessor generalization-element-of
    :initarg :generalization-element
    :type generalization-element)
   (relationship                        ; override base slot
    :accessor generalization-of
    :type generalization)
   (other-relationship-end              ; override base slot
    :accessor other-generalization-end-of
    :type generalization-end)
   (other-relationship-element          ; override base slot 
    :type generalization-element
    :accessor other-generalization-element-of))
  (:documentation "A generalization-end refers to a generalization-element and further specifies how the element is treated in the generalization."))

;;;;;;
;;; Defining generalizations

(def method shared-initialize :before ((generalization-end generalization-end) slot-names
                                      &key generalization-element &allow-other-keys)
  (assert (typep generalization-element '(or generalization-element persistent-class))))

;;;;;;
;;; Generalization helpers

(def function simple-generalization-name (generalization-element-name-1 generalization-element-name-2)
  (format-symbol (symbol-package generalization-element-name-1) "~A-IS-~A" generalization-element-name-1 generalization-element-name-2))

(def (generic e) define-simple-generalization (generalization-element-name-1 generalization-element-name-2 &rest args)
  (:method ((generalization-element-name-1 symbol) (generalization-element-name-2 symbol) &rest args)
           (apply #'define-simple-generalization
                  (find-generalization-element generalization-element-name-1)
                  (find-generalization-element generalization-element-name-2)
                  args))

  (:method ((generalization-element-1 generalization-element) (generalization-element-2 generalization-element) &rest args)
           (apply #'define-generalization
                  (simple-generalization-name (element-name-of generalization-element-1) (element-name-of generalization-element-2))
                  :generalization-ends
                  (list
                   (make-generalization-end (element-name-of generalization-element-1)
                                            :generalization-element generalization-element-1)
                   (make-generalization-end (element-name-of generalization-element-2)
                                            :generalization-element generalization-element-2))
                  args)))

(def generic make-simple-generalization (generalization-element-name-1 generalization-element-name-2 &rest args)
  (:method ((generalization-element-name-1 symbol) (generalization-element-name-2 symbol) &rest args)
           (apply #'make-simple-generalization
                  (find-generalization-element generalization-element-name-1)
                  (find-generalization-element generalization-element-name-2)
                  args))

  (:method ((generalization-element-1 generalization-element) (generalization-element-2 generalization-element) &rest args)
           (apply #'make-generalization
                  (simple-generalization-name (element-name-of generalization-element-1) (element-name-of generalization-element-2))
                  :generalization-ends
                  (list
                   (make-generalization-end (element-name-of generalization-element-1)
                                            :generalization-element generalization-element-1)
                   (make-generalization-end (element-name-of generalization-element-2)
                                            :generalization-element generalization-element-2))
                  args)))

;; TODO: we shall rather use class-precedence-list?
(def generic direct-super-generalization-elements-for (generalization-element &key include-self include-derived)
  (:method ((generalization-element generalization-element)
            &key (include-self #f) (include-derived #t))
           (append
            (when include-self (list generalization-element))
            (iter (for generalization in (if include-derived
                                             (generalizations-of generalization-element)
                                             (declared-generalizations-of generalization-element)))
                  (when (eq generalization-element (sub-generalization-element-of generalization))
                    (collect (super-generalization-element-of generalization)))))))

(def generic effective-super-generalization-elements-for (generalization-element
                                                         &key include-self include-derived)
  (:method ((generalization-element generalization-element)
            &key (include-self #f) (include-derived #t))
           (append
            (when include-self (list generalization-element))
            (iter (for super-generalization-element in
                       (direct-super-generalization-elements-for generalization-element
                                                                 :include-derived include-derived))
                  (collect super-generalization-element)
                  (appending (effective-super-generalization-elements-for super-generalization-element
                                                                          :include-derived include-derived))))))

(def generic direct-sub-generalization-elements-for (generalization-element
                                                    &key include-self include-derived)
  (:method ((generalization-element generalization-element)
            &key (include-self #f) (include-derived #t))
           (append
            (when include-self (list generalization-element))
            (iter (for generalization in (if include-derived
                                             (generalizations-of generalization-element)
                                             (declared-generalizations-of generalization-element)))
                  (when (eq generalization-element (super-generalization-element-of generalization))
                    (collect (sub-generalization-element-of generalization)))))))

(def generic effective-sub-generalization-elements-for (generalization-element
                                                       &key include-self include-derived)
  (:method ((generalization-element generalization-element)
            &key (include-self #f) (include-derived #t))
           (append
            (when include-self (list generalization-element))
            (iter (for sub-generalization-element in
                       (direct-sub-generalization-elements-for generalization-element
                                                               :include-derived include-derived))
                  (collect sub-generalization-element)
                  (appending (effective-sub-generalization-elements-for sub-generalization-element
                                                                        :include-derived include-derived))))))

;; TODO: make this rely upon subtypep
(def (generic e) generalizesp (subtype supertype &key include-derived)
  (:method ((subtype generalization-element) (supertype generalization-element) &key (include-derived #t))
           (member subtype
                   (effective-sub-generalization-elements-for supertype
                                                              :include-self #t
                                                              :include-derived include-derived))))

(def generic compute-declared-direct-super-generalization-elements (generalization-element)
  (:method ((generalization-element generalization-element))
           (direct-super-generalization-elements-for generalization-element :include-derived #f)))

(def generic compute-declared-direct-sub-generalization-elements (generalization-element)
  (:method ((generalization-element generalization-element))
           (direct-sub-generalization-elements-for generalization-element :include-derived #f)))

(def generic compute-direct-super-generalization-elements (generalization-element)
  (:method ((generalization-element generalization-element))
           (direct-super-generalization-elements-for generalization-element)))

(def generic compute-direct-sub-generalization-elements (generalization-element)
  (:method ((generalization-element generalization-element))
           (direct-sub-generalization-elements-for generalization-element)))

(def generic compute-effective-super-generalization-elements (generalization-element)
  (:method ((generalization-element generalization-element))
           (effective-super-generalization-elements-for generalization-element))

  (:method ((generalization-element persistent-class))
           nil))

(def generic compute-effective-sub-generalization-elements (generalization-element)
  (:method ((generalization-element generalization-element))
           (effective-sub-generalization-elements-for generalization-element))

  (:method ((generalization-element persistent-class))
           nil))

(def generic find-generalization-end (generalization generalization)
  (:method ((generalization-name symbol) (generalization-end-name symbol))
           (if-bind generalization (find-generalization generalization-name)
             (find-generalization-end generalization generalization-end-name)
             (error "Cannot find generalization ~A" generalization-name)))

  (:method ((generalization generalization) (generalization-end-name symbol))
           (find generalization-end-name (generalization-ends-of generalization) :key #'element-name-of)))
