;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; State machine property

(define-model-class state-property (property)
  ((state-machine
    :type finite-state-machine))
  (:maker #f)
  (:documentation "A state property links a finite state machine with an entity as a property. The state of the finite state machine can be queried via the state property as a property."))

;;;;;;
;;; State property slot class meta objects

(define-slot-class direct-state-property (state-property direct-property)
  ())

(define-slot-class effective-state-property (state-property effective-property)
  ((state-machine
    (compute-as (inherited-slot-option -self- 'state-machine)))))

(eval-always
  (pushnew :state-machine *allowed-slot-definition-properties*))

;;;;;;
;;; Defining state properties

(def function find-state-property (entity-or-name property-name &rest args)
  (apply #'find-slot entity-or-name property-name args))

(def method shared-initialize :around ((state-property direct-state-property) slot-names &rest args &key state-machine &allow-other-keys)
  (bind ((state-machine
          (aif (typep state-machine 'hu.dwim.util::finite-state-machine)
               state-machine
               (find-finite-state-machine state-machine))))
    (assert state-machine)
    (apply #'call-next-method state-property slot-names
           :state-machine state-machine
           :type (hu.dwim.util::state-type-of state-machine)
           args)))

(def method initialize-instance :after ((state-property state-property) &rest args)
  "Makes sure that the start state is filled in when a new instance of any state machine is created."
  (declare (ignore args))
  (bind ((start-state (hu.dwim.util::start-state-of (state-machine-of state-property))))
    (setf (slot-definition-initfunction state-property) (constantly start-state))
    (setf (slot-definition-initform state-property) start-state)))

(def method hu.dwim.perec::compute-persistent-effective-slot-definition-option ((class entity) (direct-slot direct-state-property) slot-option-name direct-slot-definitions)
  (if (member slot-option-name '(state-machine))
      (some [hu.dwim.perec::slot-initarg-and-value !1 slot-option-name] direct-slot-definitions)
      (call-next-method)))

(def method (setf slot-value-using-class) (new-value (class entity) (object persistent-object) (state-property effective-state-property))
  (if (typep new-value 'hu.dwim.util::state)
      (call-next-method)
      (let ((state (hu.dwim.util::find-state (state-machine-of state-property) new-value)))
        (assert (typep state 'hu.dwim.util::state))
        (call-next-method state class object state-property))))
