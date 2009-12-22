;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Scheduler

(def special-variable *persistent-process-scheduler* nil)

(def special-variable *persistent-process-scheduler-keep-on-running* #f)

(def special-variable *persistent-process-worker-group*
  (make-worker-group "persistent processor worker"))

(def (function e) start-persistent-process-worker ()
  (bind ((compiled-query-cache (make-compiled-query-cache)))
    (start-worker *persistent-process-worker-group*
                  (lambda (worker-loop)
                    (with-compiled-query-cache compiled-query-cache
                      (funcall worker-loop))))))

(def (function e) stop-persistent-process-worker (worker)
  (stop-worker worker))

(def (function e) stop-all-persistent-process-workers ()
  (stop-all-workers *persistent-process-worker-group*))

(def function push-persistent-process-job (job)
  (push-job *persistent-process-worker-group* job))

(def (function e) start-persistent-process-scheduler (&optional (poll-time 1))
  (assert (not *persistent-process-scheduler*))
  (setf *persistent-process-scheduler-keep-on-running* #t)
  (setf *persistent-process-scheduler*
        (make-thread (lambda ()
                       (unwind-protect
                            (schedule-persistent-processes-continuously poll-time)
                         (setf *persistent-process-scheduler* nil)
                         (condition-notify (scheduler-condition-variable-of *persistent-process-worker-group*))))
                     :name "persistent process scheduler")))

(def (function e) is-persistent-process-scheduler-running? ()
  (not (null *persistent-process-scheduler*)))

(def (function e) stop-persistent-process-scheduler ()
  (assert *persistent-process-scheduler*)
  (setf *persistent-process-scheduler-keep-on-running* #f)
  (condition-notify (scheduler-condition-variable-of *persistent-process-worker-group*))
  *persistent-process-scheduler*)

(def function schedule-persistent-processes-continuously (poll-time)
  (with-model-database
    (with-new-compiled-query-cache
      (with-authenticated-session (with-transaction
                                    (login *scheduler-technical-subject*))
        (scheduler.info "Process scheduler starts up")
        ;; TODO it will only check for *persistent-process-scheduler-keep-on-running* based on poll-time
        ;; and due to this stopping the process is slow.
        ;; TODO implement a conditional variable instead of polling for *persistent-process-scheduler-keep-on-running*
        (iter (while *persistent-process-scheduler-keep-on-running*)
              (unless (with-simple-restart (continue "Leave alone this bunch of processes and go on")
                        (schedule-a-bunch-of-persistent-processes))
                (scheduler.dribble "Scheduler sleeping for poll time ~A" poll-time)
                (with-thread-name " / SLEEPING FOR POLL TIME"
                  (iter (repeat (round poll-time))
                        (while *persistent-process-scheduler-keep-on-running*)
                        (sleep 1))))
              (wait-until-all-jobs-are-finished *persistent-process-worker-group*))
        (scheduler.info "Process scheduler exits")
        (with-transaction
          (logout))))))

(def (function e) schedule-persistent-processes ()
  (iter (while (schedule-a-bunch-of-persistent-processes))))

(def function schedule-a-bunch-of-persistent-processes ()
  (with-thread-name " / SCHEDULING PERSISTENT PROCESSES"
    (scheduler.debug "Scheduling a bunch of persistent processes")
    (iter (with processes = (shuffle
                             (with-transaction
                               (scheduler.dribble "Selecting persistent processes ready to run")
                               (select-persistent-processes-ready-to-run))))
          (for process :in-sequence processes)
          (while *persistent-process-scheduler-keep-on-running*)
          (scheduler.debug "Scheduling persistent process ~A" process)
          ;; TODO: start/reuse worker thread (threads are already reused: then what?)
          (rebind (process)
            (push-persistent-process-job
             (bind ((authenticated-session *authenticated-session*))
               (lambda ()
                 (standalone-continue-persistent-process process authenticated-session)))))
          (finally (return processes)))))

(def function standalone-continue-persistent-process (process authenticated-session)
  (assert hu.dwim.perec::*compiled-query-cache*)
  (sb-ext:with-timeout (worker-timeout-of (class-of process))
    (with-model-database
      (with-authenticated-session authenticated-session
        (with-transaction
          (with-revived-instance process
            (if (lock-instance process :wait #f)
                (progn
                  ;; process state and continuation must be reloaded
                  (invalidate-cached-instance process)
                  (cond ((persistent-process-in-final-state-p process)
                         (scheduler.info "Another worker finished the persistent process ~A meanwhile" process))
                        ((not (persistent-process-ready-to-run-p process))
                         (scheduler.info "The persistent process ~A is not ready to run anymore" process))
                        (t (continue-persistent-process process))))
                (scheduler.info "Skipping persistent process ~A because lock failed" process))))))))
