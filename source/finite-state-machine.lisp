;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Model

(define-model-class finite-state-machine (owner-model-element)
  ((states
    nil
    :type (list state))
   (events
    nil
    :type (list event))
   (transitions
    nil
    :type (list transition))
   (start-state
    :type start-state)
   (stop-states
    :type (list stop-state))
   (state-properties
    nil
    :type (list state-property))
   (state-type
    :type (or symbol list)))
  (:documentation "A finite state machine is a process described via states and transitions between these states fired upon incoming events."))

(define-model-class finite-state-machine-element (owned-model-element)
  ((state-machine
    :type finite-state-machine))
  (:abstract #t)
  (:documentation "Finite state machine elements exclusively belong to a finite state machine."))

(define-model-class state (finite-state-machine-element)
  ((on-enter-action
    :type function
    :documentation "The on enter action is called wheneber this state becomes the new state of a finite state machine.")
   (on-leave-action
    :type function
    :documentation "The on leave action is called whenever a finite state machine leaves this state.")
   (on-stay-action
    :type function
    :documentation "The on stay action is called whenever a finite state machine stays in this state during a transition.")
   (transitions
    nil
    :type (list transition))))

(define-model-class start-state (state)
  ()
  (:definer #f)
  (:documentation "There is only one start state in a finite state machine. This is the state of a new state machine instance when it is created."))

(define-model-class stop-state (state)
  ()
  (:definer #f)
  (:documentation "There might be several stop states in a finite state machine."))

(define-model-class transition (finite-state-machine-element)
  ((event
    :type event
    :documentation "The event triggers this transition if the finite state machine is in the source state.")
   (source
    :type state)
   (target
    :type state)
   (action
    :type function
    :documentation "The action is called when this transition fires."))
  (:documentation "A transition specifies what should happen if a specific event occurs when the finite state machine is in the source state."))

(define-model-class event (finite-state-machine-element)
  ()
  (:documentation "An event identifies a trigger."))

;;;;;;
;;; Defining

(def macro def-finite-state-machine (name &body forms)
  (bind ((start-state (second (find :start-state forms :key #'first)))
         (state-type-name (format-symbol (symbol-package name) "~A/STATE-TYPE" name))
         (stop-states (cdr (find :stop-states forms :key #'first)))
         (transitions (cdr (find :transitions forms :key #'first)))
         (states (union (delete-duplicates (mapcar #'second transitions))
                        (delete-duplicates (mapcar #'third transitions))))
         (events (delete-duplicates (mapcar #'first transitions))))
    `(progn
      (bind (,@(mapcar (lambda (state)
                         `(,state (,(cond
                                     ((eq state start-state)
                                      'make-start-state)
                                     ((member state stop-states)
                                      'make-stop-state)
                                     (t
                                      'make-state))
                                    ',state)))
                       states)
               ,@(mapcar (lambda (event)
                           `(,event (make-event ',event)))
                         events))
        (define-finite-state-machine ',name
            :state-type ',state-type-name
            :states (list ,@states)
            :events (list ,@events)
            :transitions
            (list
             ,@(mapcar (lambda (transition)
                         `(make-transition ',(format-symbol (symbol-package (second transition))
                                                            "~A->~A" (second transition) (third transition))
                                           :event ,(first transition)
                                           :source ,(second transition)
                                           :target ,(third transition)))
                       transitions))))
      (def ptype ,state-type-name ()
        `(member ,@(states-of (find-finite-state-machine ',name)))))))

(def (definer e :available-flags "e") finite-state-machine (name &body forms)
  (with-standard-definer-options name
    `(def-finite-state-machine ,name ,@forms)))

(def method shared-initialize :after ((fsm finite-state-machine) slot-names &key &allow-other-keys)
  ;; link up everything
  (dolist (element (append (states-of fsm) (events-of fsm) (transitions-of fsm)))
    (setf (state-machine-of element) fsm))
  ;; set start and stop states
  (setf (start-state-of fsm) (find-if (of-type 'start-state) (states-of fsm)))
  (setf (stop-states-of fsm) (collect-if-typep 'stop-state (states-of fsm))))

(define-copy-method (copy-one copy-query) ((state state) htable)
  state)

(define-copy-method (copy-one hu.dwim.perec::copy-shallow) ((state state) htable)
  state)

(def generic find-state (fsm state-name)
  (:method ((fsm-name symbol) (state-name symbol))
           (find-state (find-finite-state-machine fsm-name) state-name))

  (:method ((fsm finite-state-machine) (state-name symbol))
           (find state-name (states-of fsm) :key #'element-name-of)))

(def generic find-transition (fsm transition-name)
  (:method ((fsm-name symbol) (transition-name symbol))
           (find-transition (find-finite-state-machine fsm-name) transition-name))

  (:method ((fsm finite-state-machine) (transition-name symbol))
           (find transition-name (transitions-of fsm) :key #'element-name-of)))

(def generic find-event (fsm event-name)
  (:method ((fsm-name symbol) (event-name symbol))
           (find-event (find-finite-state-machine fsm-name) event-name))

  (:method ((fsm finite-state-machine) (event-name symbol))
           (find event-name (events-of fsm) :key #'element-name-of)))

(def (generic e) process-event (object state-property event &rest args)
  (:documentation "Process a single event and change the state according to the available transitions.")

  (:method ((object persistent-object) (state-property-name symbol) (event-name symbol) &rest args)
           (apply #'process-event
                  object
                  (find-state-property (class-of object) state-property-name
                                       :otherwise (lambda ()
                                                    (error "Cannot find state property in class ~A for name ~A"
                                                           (class-of object) state-property-name)))
                  event-name args))

  (:method ((object persistent-object) (state-property effective-property) (event-name symbol) &rest args)
           (bind ((fsm (state-machine-of state-property))
                  (state (slot-value-using-class (class-of object) object state-property))
                  (event (find event-name (events-of fsm) :key #'element-name-of))
                  ;; find the transtion to fire
                  (transition (find-if (lambda (transition)
                                         (and (eq (event-of transition) event)
                                              (eq (source-of transition) state)))
                                       (transitions-of fsm))))
             (if transition
                 ;; we have a valid transition here
                 (let ((new-state (target-of transition)))
                   (meta-model.debug "Firing transition ~A from finite state machine ~A for state property ~A in ~A"
                                     (element-name-of transition) (element-name-of fsm) state-property object)
                   ;; handle on stay action
                   (if (eq state new-state)
                       (when (slot-boundp state 'on-stay-action)
                         (apply (on-stay-action-of state) args))
                       (progn
                         ;; handle on leave action
                         (when (slot-boundp state 'on-leave-action)
                           (apply (on-leave-action-of state) args))
                         ;; handle transition action
                         (when (slot-boundp transition 'action)
                           (apply (action-of transition) args))
                         ;; handle on enter action
                         (when (slot-boundp new-state 'on-enter-action)
                           (apply (on-enter-action-of new-state) args))
                         ;; set new state
                         (setf (slot-value-using-class (class-of object) object state-property) new-state))))
                 ;; oops, we did not find a transition
                 (error "Transition not found for state ~A and event ~A in finite state machine ~A for object ~A"
                        state event fsm object)))))

(def generic process-event* (object event &rest args)
  (:method ((object persistent-object) (event-name symbol) &rest args)
           "Process a single event and change the state according to the currently available transitions. This variant searches for state-properties and fails if there's more than one."
           (let ((state-properties (collect-if-typep 'state-property (direct-properties-of (entity-of object)))))
             ;; if there is only one entity state associated with entity
             (if (= (length state-properties) 1)
                 (apply 'process-event object (slot-definition-name (first state-properties)) event-name args)
                 (error "More than one entity state for object ~A" object)))))
