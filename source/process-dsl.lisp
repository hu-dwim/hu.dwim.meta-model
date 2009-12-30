;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Continuation

(def (function/cc e) process-save-point ()
  "Stores the current continuation and continues evaluation."
  (let/cc temporary-k
    (let/cc persistent-k
      (persistent-process-save-point *process* persistent-k)
      (kall temporary-k))
    ;; temporary-k is needed for skipping the following 'continue event when running through a save-point (as opposed to restarting from here)
    (process-event *process* 'process-state 'continue)
    (kall temporary-k)))

(def (function/cc e) yield-from-process ()
  "It is a save-point and a wait with no particular wait-reason at once."
  (let/cc k
    (persistent-process-save-point *process* k)
    (persistent-process-wait *process* nil)
    (values))
  (process-event *process* 'process-state 'continue)
  (values))

(def (function/cc e) revert-process ()
  "Rollback process state to the previous savepoint."
  (let/cc k
    (declare (ignore k))
    (process-event *process* 'process-state 'wait)
    (values)))

(def (function/cc e) process-wait (wait-reason &key (save-point #t))
  "Temporarily terminates execution and stores the given wait reason for the scheduler. Stores the current continuation to resume evaluation unless the opposite requested."
  (if save-point
      (let/cc temporary-k
        (let/cc persistent-k
          (persistent-process-save-point *process* persistent-k)
          (persistent-process-wait *process* wait-reason)
          (values))
        ;; temporary-k is needed for skipping the following 'continue event when running through a save-point (as opposed to restarting from here)
        (process-event *process* 'process-state 'continue)
        (kall temporary-k))
      (let/cc k
        (declare (ignore k))
        (persistent-process-wait *process* wait-reason)
        (values))))

;;;;;;
;;; Finish process

(def (function/cc e) finish-process (&optional result)
  "The persistent process successfully and permanently finishes execution with the given result"
  (let/cc k
    (declare (ignore k))
    (finish-persistent-process *process* result)))

(def (function/cc e) fail-process (&optional result)
  "The persistent process permanently failed with the given result"
  (let/cc k
    (declare (ignore k))
    (fail-persistent-process *process* result)))

;;;;;;
;;; Work with child processes

(def (macro e) start-process (&body forms)
  `(start-persistent-process/form ',@forms))

(def (macro e) with-parallel-child-processes (&body forms)
  "Starts several new child processes and waits until all of them terminates"
  (with-unique-names (process child-processes)
    `(bind ((,process *process*)
            (,child-processes
             (list ,@(iter (for form in forms)
                           (collect `(start-persistent-process/form
                                      ',form
                                      :start-running-in-same-transaction #t
                                      :parent-process ,process))))))
       (join-with-child-processes ,child-processes))))

(def (macro e) fork-child-process (&body forms)
  "Starts a new persistent process as a child of the current process"
  `(start-persistent-process/form
    '(progn
      ,@forms)
    :parent-process *process*
    :start-running-in-same-transaction #t))

(def function collect-in-progress-child-processes (parent-process &optional child-processes)
  ;; TODO: fix query compiler
  (select-instances (process persistent-process)
    (where (and (or (null child-processes)
                    (member process child-processes))
                (not (process-in-stop-state? process))
                (eq parent-process (parent-process-of process))))))

;; TODO: function or macro? &rest or list argument?
(def (function/cc e) join-with-child-processes (&optional processes)
  "Waits until all of the given child processes terminate."
  (iter (while (collect-in-progress-child-processes *process* processes))
        (yield) ;; TODO: this is inefficient, the process will be continued time by time randomly
        ;; TODO: recieve a message from a child that tells it has been finished
        #+nil
        (receive-message *process*)))

;;;;;;
;;; Milestone

(def (function e) milestone-reached (name)
  (incf (slot-value *process* name)))

(def (function e) before-milestone-p (name &optional (count 1))
  (< (slot-value *process* name) count))

(def (function e)  after-milestone-p (name &optional (count 1))
  (>= (slot-value *process* name) count))

;;;;;;
;;; Activity

(def (function e) begin-activity (name)
  (incf (slot-value *process* name)))

(def (function e) end-activity (name)
  (decf (slot-value *process* name)))

;; TODO: unwind protect when available in hu.dwim.delico
(def (macro e) with-activity (name &body forms)
  `(progn
     (begin-activity ,name)
     (multiple-value-prog1
         ,@forms
       (end-activity ,name))))

(def (function e) in-activity-p (name &optional (level 1))
  (>= (slot-value *process* name) level))

;; TODO: when handler-bind is available in hu.dwim.delico
;; TODO with-timeout is bordeaux-threads symbol!
#+nil
(def macro with-timeout (timeout &body forms)
  (declare (ignore timeout forms))
  `(not-yet-implemented))

;;;;;;
;;; Wait for

(def function/cc wait-for-next-schedule ()
  (process-wait nil))

(def function/cc wait-for-expression (expression)
  (process-wait (make-wait-for-expression :wait-for-subject #f :expression expression)))

(def function/cc wait-for-subject-expression (expression)
  (process-wait (make-wait-for-expression :wait-for-subject #t :expression expression)))

(def function/cc wait-for-subject (subject)
  (process-wait (make-wait-for-subject :subject subject)))

(def function/cc wait-for-subject-type (class-name)
  (process-wait (make-wait-for-subject-type :class-name class-name)))

(def function/cc wait-for-timestamp (timestamp)
  (process-wait (make-wait-for-timestamp :wait-until timestamp)))

(def function/cc wait-for-milestone (process name &optional (count 1))
  (process-wait (make-wait-for-milestone :persistent-process process :milestone name :count count)))

(def function/cc wait-for-activity (process name &optional (level 1))
  (process-wait (make-wait-for-activity :persistent-process process :activity name :level level)))
