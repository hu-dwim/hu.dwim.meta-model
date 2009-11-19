;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Worker threads

(def special-variable *debug-worker* #f)

(def class* worker-group ()
  ((worker-name "worker" :type string)
   (workers nil :type list)
   (worker-lock (make-lock "worker-lock"))
   (worker-condition-variable (make-condition-variable))
   (jobs nil :type list)
   (job-lock (make-lock "job-lock"))
   (scheduler-condition-variable (make-condition-variable)))
  (:documentation "Manages a set of worker threads to process a set of jobs simultanously."))

(def class* worker ()
  ((worker-group :type worker-group)
   (keep-on-running #t :type boolean)
   (thread)))

(def function worker-loop (worker-group worker)
  (unwind-protect
       (iter (while (keep-on-running-p worker))
             (for job = (pop-job worker-group
                                 (lambda ()
                                   (not (keep-on-running-p worker)))))
             (when job
               (block run-job
                 ;; TODO use hu.dwim.util:with-layered-error-handlers and other error handling stuff
                 (handler-bind
                     ((serious-condition
                       (lambda (condition)
                         (with-thread-name " / handling a serious condition"
                           (unless *debug-worker*
                             (meta-model.error "Got condition ~A within worker ~A in ~A skipping job ~A.~%~A"
                                               condition worker worker-group job
                                               ;; letting errors fly through here would not be funny...
                                               (ignore-errors
                                                 (with-output-to-string (str)
                                                   (print-backtrace str))))
                             (return-from run-job))))))
                   (with-thread-name " / running job"
                     (funcall job))))))
    (with-lock-held ((worker-lock-of worker-group))
      (deletef (workers-of worker-group) worker))
    (condition-notify (worker-condition-variable-of worker-group))))

(def function start-worker (worker-group &optional (worker-environment-function 'funcall))
  (with-lock-held ((worker-lock-of worker-group))
    (prog1-bind worker (make-instance 'worker :worker-group worker-group)
      (meta-model.debug "Staring new worker for ~A" worker-group)
      (setf (thread-of worker)
            (sb-thread:make-thread
             (lambda ()
               (funcall worker-environment-function
                        (lambda ()
                          (worker-loop worker-group worker))))
             :name (worker-name-of worker-group)))
      (push worker (workers-of worker-group)))))

(def function stop-worker (worker)
  (meta-model.debug "Stopping worker ~A" worker)
  (setf (keep-on-running-p worker) #f)
  (condition-notify (worker-condition-variable-of (worker-group-of worker))))

(def function stop-all-workers (worker-group)
  (with-lock-held ((worker-lock-of worker-group))
    (prog1-bind workers (workers-of worker-group)
      (dolist (worker workers)
        (setf (keep-on-running-p worker) #f))
      (condition-notify (worker-condition-variable-of worker-group)))))

(def function push-job (worker-group job)
  (with-lock-held ((job-lock-of worker-group))
    (meta-model.debug "Pushing new job ~A into ~A" job worker-group)
    (push job (jobs-of worker-group))
    (condition-notify (worker-condition-variable-of worker-group))))

(def function pop-job (worker-group &optional (exit-condition (constantly #f)))
  (bind ((job-lock (job-lock-of worker-group)))
    (with-lock-held (job-lock)
      (iter (until (funcall exit-condition))
            (when-bind job (pop (jobs-of worker-group))
              (meta-model.debug "Popping job ~A from ~A" job worker-group)
              (when (null (jobs-of worker-group))
                (condition-notify (scheduler-condition-variable-of worker-group)))
              (return-from pop-job job))
            (condition-wait (worker-condition-variable-of worker-group) job-lock)))))

(def function delete-all-jobs (worker-group)
  (with-lock-held ((job-lock-of worker-group))
    (setf (jobs-of worker-group) nil)
    (condition-notify (scheduler-condition-variable-of worker-group))))

(def function wait-until-all-jobs-are-finished (worker-group)
  (with-thread-name " / WAIT-UNTIL-ALL-JOBS-ARE-FINISHED"
    (bind ((job-lock (job-lock-of worker-group)))
      (with-lock-held (job-lock)
        (when (jobs-of worker-group)
          (condition-wait (scheduler-condition-variable-of worker-group) job-lock))))))
