;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Property model class

(define-model-class property (owned-model-element)
  ((element-name
    (compute-as (slot-definition-name -self-)))
   (property-type
    (compute-as (aprog1
                    (parse-type (normalized-type-of -self-))
                  (unless it
                    (error "Unknown type ~S" (slot-definition-type -self-)))))
    :type persistent-type
    :documentation "Returns the object representation of the normalized type.")
   (unbound-subtype
    (compute-as (nth-value 1 (destructure-type (hu.dwim.perec::canonical-type-of -self-))))
    :type boolean
    :documentation "True means the slot may be unbound.")
   (null-subtype
    (compute-as (nth-value 2 (destructure-type (hu.dwim.perec::canonical-type-of -self-))))
    :type boolean
    :documentation "True means the slot may be assigned to nil.")
   (property-query
    :type query
    :documentation "The query used to compute the slot value or nil if not applicable.")
   (definitive
    :type boolean
    :documentation "True means that the property may only be assigned to a value once and for ever.")
   (editable
    :type boolean
    :documentation "True means that the value of this property may be changed.")
   (identity
    :type boolean
    :documentation "When the user interface shows an entity instance it has to display at least the identity properties by default.")
   (reference
    :type boolean
    :documentation "When the user interface shows a reference to an entity instance it will show the reference properties by default.")
   (primary
    :type boolean
    :documentation "When the user interface shows a list of an entity instances it will show the primary properties by default.")
   (flattened
    :type boolean
    :documentation "Should this property, holding an entity, be automatically flattened when being displayed? This makes sense only on a property having compound type such as a set or an entity.")
   (to-be-flattened
    :type boolean
    :documentation "When flattening an entity, should this member be flattened into the parent?"))
  (:maker #f))

(eval-always
  (mapc [pushnew !1 *allowed-slot-definition-properties*]
        '(:definitive :editable :identity :reference :primary :flattened :compute-as)))

;;;;;;
;;; Property slot class meta objects

(define-slot-class direct-property (property standard-direct-slot-definition)
  ()
  (:documentation "The base class of all persistent direct property classes."))

;; TODO this generates identity-of accessors due to the overridden :type boolean slots.
;; should add :accessor nil to all overridden slots, but it will be refactored anyway...
(define-slot-class effective-property (property standard-effective-slot-definition)
  ((direct-properties
    :type (list direct-property)
    :documentation "The list of direct properties used to compute this effective property during the class finalization procedure.")
   (property-query
    (compute-as (if (typep -self- 'persistent-association-end-effective-slot-definition)
                    (association-end-query-of -self-)
                    (inherited-slot-option -self- 'property-query nil))))
   (definitive
    (compute-as (inherited-slot-option -self- 'definitive #f)))
   (editable
    (compute-as (inherited-slot-option -self- 'editable #t)))
   (identity
    (compute-as (inherited-slot-option -self- 'identity #f)))
   (reference
    (compute-as (inherited-slot-option -self- 'reference #f)))
   (primary
    (compute-as (inherited-slot-option -self- 'primary #f)))
   (flattened
    (compute-as (inherited-slot-option -self- 'flattened (primary-p -self-))))
   (to-be-flattened
    (compute-as (inherited-slot-option -self- 'to-be-flattened (primary-p -self-)))))
  (:documentation "The base class of all persistent effective property classes."))

(define-computed-universe compute-as-in-transaction)

(def function inherited-slot-option (effective-slot slot-name &optional (default-value nil default-value-p))
  (iter (for direct-slot in (direct-properties-of effective-slot))
        (if (and (find-slot (class-of direct-slot) slot-name :otherwise #f)
                 (slot-boundp direct-slot slot-name))
            (return-from inherited-slot-option (slot-value direct-slot slot-name))))
  (if default-value-p
      default-value
      (error "Cannot determine slot option ~A for effective slot ~A" slot-name effective-slot)))

(def method hu.dwim.perec::compute-columns ((slot computed-effective-slot-definition))
  nil)

(def method hu.dwim.perec::compute-data-table-slot-p ((slot computed-effective-slot-definition))
  #f)

(def method element-name-of ((slot persistent-slot-definition))
  (slot-definition-name slot))

(def function reader-name-of (effective-slot)
  (first (some #'slot-definition-readers (direct-properties-of effective-slot))))

(def function writer-name-of (effective-slot)
  (first (some #'slot-definition-writers (direct-properties-of effective-slot))))

(def method hu.dwim.perec::normalized-type-of ((slot hu.dwim.computed-class::computed-slot-definition))
  (hu.dwim.perec::normalized-type-for (slot-definition-type slot)))

(def method hu.dwim.perec::canonical-type-of ((slot hu.dwim.computed-class::computed-slot-definition))
  (hu.dwim.perec::canonical-type-for (slot-definition-type slot)))
