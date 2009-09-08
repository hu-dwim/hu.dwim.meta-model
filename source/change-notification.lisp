;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(def (special-variable e) *instance-changed-listeners-enabled* #t)

(def special-variable *global-instance-changed-listeners* (make-hash-table :test #'eql))

(def special-variable *local-instance-changed-listeners-list* nil)

(def special-variable *instance-changed-listeners-lock* (make-recursive-lock "*instance-changed-listeners-lock*"))

(def (macro e) with-new-local-instance-changed-listeners (&body forms)
  `(bind ((*local-instance-changed-listeners-list* (cons (make-hash-table :test #'eql) *local-instance-changed-listeners-list*)))
     ,@forms))

(def (function e) register-global-instance-changed-listener (instance listener)
  (when *instance-changed-listeners-enabled*
    (notification.dribble "Registering global instance changed listener ~A for ~A" listener instance)
    (with-recursive-lock-held (*instance-changed-listeners-lock*)
      (push (make-weak-pointer listener) (hash-value-of instance *global-instance-changed-listeners*)))))

(def (function e) register-local-instance-changed-listener (instance listener)
  (when *instance-changed-listeners-enabled*
    (notification.dribble "Registering local instance changed listener ~A for ~A" listener instance)
    (push listener (hash-value-of instance (first *local-instance-changed-listeners-list*)))))

(def function notify-global-instance-changed-listeners (transaction instance transaction-event)
  (when *instance-changed-listeners-enabled*
    (notification.dribble "Notifying instance changed listeners of ~A" instance)
    (with-recursive-lock-held (*instance-changed-listeners-lock*)
      (bind (((:values weak-pointer-list foundp) (hash-value-of instance *global-instance-changed-listeners*)))
        (when foundp
          (iter (with processed-list = weak-pointer-list)
                (for cell :first weak-pointer-list :then (cdr cell))
                (for previous-cell :previous cell :initially nil)
                (while cell)
                (for listener = (weak-pointer-value (car cell)))
                (if listener
                    (progn
                      (notification.dribble "Calling a valid global instance changed listener ~A of ~A" listener instance)
                      (funcall listener transaction instance transaction-event))
                    (progn
                      (notification.dribble "Dropping an invalid global instance changed listener of ~A" instance)
                      (if previous-cell
                          (setf (cdr previous-cell) (cdr cell))
                          (setf processed-list (cdr cell)))))
                (finally (if processed-list
                             (setf (hash-value-of instance *global-instance-changed-listeners*) processed-list)
                             (remove-hash-value-of instance *global-instance-changed-listeners*)))))))))

(def function notify-local-instance-changed-listeners (transaction instance transaction-event)
  (when *instance-changed-listeners-enabled*
    (notification.dribble "Notifying instance changed listeners of ~A" instance)
    (iter (for instance-changed-listener :in *local-instance-changed-listeners-list*)
          (bind (((:values listener-list foundp) (hash-value-of instance instance-changed-listener)))
            (when foundp
              (dolist (listener listener-list)
                (notification.dribble "Calling a valid local instance changed listener ~A of ~A" listener instance)
                (funcall listener transaction instance transaction-event)))))))

(def (function e) notify-instance-changed-listeners (transaction instance transaction-event)
  (notify-local-instance-changed-listeners transaction instance transaction-event)
  (notify-global-instance-changed-listeners transaction instance transaction-event))

(def method before-committing-instance :before ((transaction transaction-mixin/dwim) (instance persistent-object) transaction-event)
  (when transaction-event
    (notify-instance-changed-listeners transaction instance transaction-event)))
