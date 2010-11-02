;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

;; TODO: export persistent process variables into queryable persistent storage (class slots)
(in-package :hu.dwim.meta-model)

;;;;;;
;;; Definitions

(def constant +persistent-process-error-count-limit+ 3
  "Maximum number of serious-condition's a process may signal before putting it into broken state.")

(define-model-class persistent-process-class (entity)
  ((hu.dwim.util::form
    :type form
    :accessor nil)
   (closure
    nil
    :type hu.dwim.delico::closure/cc)
   (worker-timeout
    0
    :type integer)
   (error-count-limit
    +persistent-process-error-count-limit+
    :type (or null integer-16)))
  (:maker #f)
  (:definer #f)
  (:documentation "A persistent process is a stateful activity which might go on for an extremely long time. It might start other activities and may communicate with them. A persistent process might also communicate with arbitrary number of users. The users may or may not be online while the process is running, so the process might have to wait for a long time. Asynchronous event handling, such as timeouts are also to be supported."))

;;;;;;
;;; Model management

(def (definer e :available-flags "e") persistent-process-class (name super-classes slots &body forms)
  (with-standard-definer-options name
    (bind ((start-function-name (format-symbol (symbol-package name) "START-~A" name))
           (index (position-if [not (keywordp (first !1))] forms))
           (options (subseq forms 0 index))
           (lisp-forms (and index (subseq forms index))))
      `(progn
         (def (function e) ,start-function-name (&rest args)
           (declare (dynamic-extent args))
           (apply #'start-persistent-process ',name args))
         (defentity ,name ,super-classes ,slots
           (:metaclass persistent-process-class)
           ,@options
           ,(awhen lisp-forms
             `(:form (progn ,@lisp-forms))))))))

(def method initialize-instance :around ((persistent-process-class persistent-process-class) &rest args)
  (apply #'shared-ininitialize-around-persistent-process-class persistent-process-class #'call-next-method args))

(def method reinitialize-instance :around ((persistent-process-class persistent-process-class) &rest args)
  (apply #'shared-ininitialize-around-persistent-process-class persistent-process-class #'call-next-method
         :name (class-name persistent-process-class) args))

(def function shared-ininitialize-around-persistent-process-class (persistent-process-class call-next-method &rest args
                                                                   &key form worker-timeout error-count-limit &allow-other-keys)
  (let ((processed-args (append (when form
                                  (list :form (first form)
                                        :closure (make-process-closure (first form))))
                                (when worker-timeout
                                  (list :worker-timeout (first worker-timeout)))
                                (when error-count-limit
                                  (list :error-count-limit (first error-count-limit)))
                                (remove-from-plistf args :closure :worker-timeout :error-count-limit))))
    (apply call-next-method persistent-process-class processed-args)))

(def method hu.dwim.perec::persistent-class-default-superclasses ((class persistent-process-class) &key name direct-superclasses)
  (cond ((eq name 'persistent-process)
         (call-next-method))
        ((find-if (lambda (direct-superclass)
                    (ignore-errors (subtypep direct-superclass (find-class 'persistent-process))))
                  direct-superclasses)
         nil)
        (t
         (list (find-class 'persistent-process)))))

;;;;;;
;;; Standard persistent processes

(def (entity e) persistent-process (standard-process)
  ( ;; NOTE: the following slots are redefinitions of the slots of standard-process
   (hu.dwim.util::process-state
    :primary #t
    :state-machine hu.dwim.util::process-state-machine
    :index :bitmap)
   (hu.dwim.util::form
    :type (or unbound serialized)
    :prefetch #f
    :cache #t)
   (hu.dwim.util::continuation
    :type (or null serialized)
    :prefetch #f
    :cache #t)
   (hu.dwim.util::result
    :type t
    :accessor nil)
   (error-count
    0
    :type integer-16))
  (:metaclass persistent-process-class))

(def association
  ((:slot parent-process :type (or null persistent-process))
   (:slot child-processes :type (set persistent-process))))

(def association
  ((:slot created-processes :type (set persistent-process))
   (:slot creator :type (or null subject))))

(def constructor persistent-process
  ;; TODO: need to use slot-value, accessor don't work due to export
  (setf (hu.dwim.util::form-of -self-) (slot-value (class-of -self-) 'hu.dwim.util::form)))

(def method before-committing-instance :before (transaction (process persistent-process) event)
  (assert (not (process-running? process)) nil "Processes should not be comitted with running state"))

;;;;;;
;;; Messaging between processes

(def (entity e) message-queue ()
  ())

(def (entity e) persistent-message ()
  ())

(def association
  ((:slot message-queue :type (or null message-queue))
   (:slot persistent-process :type (or null persistent-process))))

(def association
  ((:slot persistent-messages :type (set persistent-message))
   (:slot message-queue :type (or null message-queue))))

;;;;;;
;;; Wait reasons

(def (entity e) wait-reason ()
  ()
  (:abstract #t))

(def association
  ((:slot wait-reason :type (or null wait-reason))
   (:slot persistent-processes :type (set persistent-process))))

(def (entity e) wait-for-expression (wait-reason)
  ((wait-for-subject :type boolean)
   (expression :type serialized :documentation "The expression will be evaluated to check whether the process is still waiting, all other fields are ignored")))

(def (entity e) wait-for-subject (wait-reason)
  ())

(def association
  ((:slot subject :type subject :primary #t :reference #t)
   (:slot wait-for-subjects :type (set wait-for-subject))))

(def (entity e) wait-for-subject-type (wait-reason)
  ((class-name :type symbol :primary #t :reference #t)))

(def (entity e) wait-for-timestamp (wait-reason)
  ((wait-until :type timestamp :primary #t :reference #t)))

(def persistent-type milestone ()
  'integer-32)

(def (entity e) wait-for-milestone (wait-reason)
  ((milestone :type symbol)
   (count :type integer-32)))

(def association
  ((:slot persistent-process :type persistent-process)
   (:slot wait-for-milestones :type (set wait-for-milestone))))

(def persistent-type activity ()
  'integer-32)

(def (entity e) wait-for-activity (wait-reason)
  ((activity :type symbol)
   (level :type integer-32)))

(def association
  ((:slot persistent-process :type persistent-process)
   (:slot wait-for-activities :type (set wait-for-activity))))

(def (entity e) wait-for-message (wait-reason)
  ((message-class-name :type symbol)))

;;;;;;
;;; Starting persistent processes

(def special-variable *debug-persistent-process* #f)

(def function handle-persistent-process-error (error)
  (process.error "Error in persistent process ~A: ~A" *process* error)
  ;; TODO clean up and use a public api for error handling
  (when *debug-persistent-process*
    (restart-case
      ;; FIXME we may not even be connected, see invoke-slime-debugger-if-possible
      (swank:swank-debugger-hook error nil)
      (continue ()
        :report "Record the error on the process instance and abort its execution."
        (values))))
  ;; this has to be done after the stack is unwound so that the other transaction is rolled back
  ;; and this transaction is not a nested one (otherwise it would be a deadlock)
  (let ((process *process*))
    (register-transaction-hook :after :always
      (handler-bind
          ((serious-condition
            (lambda (error)
              (process.fatal "Error occured during incrementing process error count of ~A: ~A" process error))))
        (with-transaction
          (with-reloaded-instance process
            (lock-instance process)
            (bind ((error-count (incf (error-count-of process)))
                   (error-count-limit (error-count-limit-of (class-of process))))
              (when (and error-count-limit
                         (= error-count
                            error-count-limit))
                (die-process process)))))))))

(def with-macro with-persistent-process-error-handler ()
  ;; TODO use util's with-layered-error-handler
  (handler-bind ((serious-condition 'handle-persistent-process-error))
    (-body-)))

(def (generic e) start-persistent-process (object &rest args)
  (:documentation "The main start persistent process method")

  (:method ((process-name symbol) &rest args)
    (apply #'start-persistent-process (find-persistent-process-class process-name) args))

  (:method ((class persistent-process-class) &rest args &key (parent-process nil) (start-running-in-same-transaction #t) &allow-other-keys)
    (remove-from-plistf args :start-running-in-same-transaction)
    (bind ((process (apply #'make-instance class
                           :parent-process (or parent-process
                                               (when (boundp '*process*)
                                                 *process*))
                           args))
           (*process* process))
      (%start-persistent-process process :start-running-in-same-transaction start-running-in-same-transaction)))

  (:method ((process persistent-process) &key (start-running-in-same-transaction #t))
    (bind ((*process* process))
      (%start-persistent-process process :start-running-in-same-transaction start-running-in-same-transaction))))

(def (function e) start-persistent-process/form (form &rest args &key &allow-other-keys)
  (apply #'start-persistent-process 'persistent-process :form form args))

(def function %start-persistent-process (process &key (start-running-in-same-transaction #t))
  (with-thread-name " / %START-PERSISTENT-PROCESS"
    (assert (process-initializing? process))
    (bind ((*debug-evaluate/cc* *debug-persistent-process*))
      (debug-only
        (assert (eq (transaction-of process) *transaction*)))
      (process.debug "Starting persistent process ~A" process)
      (if start-running-in-same-transaction
          (values process
                  (with-persistent-process-error-handler
                    (process-event process 'hu.dwim.util::process-state 'hu.dwim.util::start)
                    (%%start-persistent-process process)))
          ;; optimizing away storing the continuation
          (progn
            (process-event process 'hu.dwim.util::process-state 'hu.dwim.util::wait)
            process)))))

(def function %%start-persistent-process (process)
  (with-call/cc
    (funcall (make-process-closure (hu.dwim.util::form-of process)))))

;;;;;;
;;; Continuing persistent processes

(def (condition* e) persistent-process-error (error)
  ())

(def condition* simple-persistent-process-error (persistent-process-error simple-error)
  ())

(def condition* persistent-process-in-wrong-state-error (simple-persistent-process-error)
  ())

(def (function e) continue-persistent-process (process)
  "The main continue persistent process method."
  (bind ((*process* process))
    (%continue-persistent-process process)))

(def function %continue-persistent-process (process)
  (with-thread-name " / %CONTINUE-PERSISTENT-PROCESS"
    (assert (process-in-progress? process))
    (bind ((*debug-evaluate/cc* *debug-persistent-process*))
      (debug-only
        (assert (eq (transaction-of process) *transaction*)))
      (process.debug "Continuing persistent process ~A" process)
      (values process
              (with-persistent-process-error-handler
                (aif (hu.dwim.util::continuation-of process)
                     (kall it)
                     ;; optimizing away storing the continuation
                     (progn
                       (process-event process 'hu.dwim.util::process-state 'hu.dwim.util::continue)
                       (%%start-persistent-process process))))))))

;;;;;;
;;; Handle continuations

;; KLUDGE: eh, this is really hackish
(def method hu.dwim.delico::evaluate/cc ((var hu.dwim.delico::lexical-variable-reference-form) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (hu.dwim.delico::kontinue k (prog1-bind instance (hu.dwim.delico::lookup lex-env :let (name-of var) :error-p #t)
                                (if (and (typep instance 'persistent-object)
                                         (persistent-p instance))
                                    (revive-instance instance)))))

(def (function e) persistent-process-save-point (process k)
  (process.debug "Storing persistent process continuation for ~A" process)
  (setf (hu.dwim.util::continuation-of process) k))

(def (function e) persistent-process-wait (process wait-reason)
  (awhen (wait-reason-of process)
    (purge-instance it))
  (setf (wait-reason-of process) wait-reason)
  (process-event process 'hu.dwim.util::process-state 'hu.dwim.util::wait))

;;;;;;
;;; Messaging

(def (function e) send-message (message process)
  (unless (message-queue-of process)
    (setf (message-queue-of process)
          (make-instance 'message-queue)))
  (insert-item (persistent-messages-of* (message-queue-of process)) message))

(def (function/cc e) receive-message (process &optional (message-class-name 'persistent-message))
  ;; TODO: check if a message is already available
  (let/cc k
    (persistent-process-save-point process k)
    (persistent-process-wait process (make-wait-for-message :message-class-name message-class-name))
    k)
  (process-event process 'hu.dwim.util::process-state 'hu.dwim.util::continue))

;;;;;;
;;; Ready to run

(def (function e) persistent-process-ready-to-run? (process)
  (bind ((query (make-select-persistent-processes-ready-to-run-query :select-clause '((count process)))))
    (add-lexical-variable query 'instance)
    (add-assert query '(eq instance process))
    (= 1 (first (execute-query query process)))))

(def (function e) make-select-persistent-processes-ready-to-run-query (&key (process-variable-name 'process)
                                                                            (select-clause (list process-variable-name))
                                                                            (process-type 'persistent-process)
                                                                            (prefetch-mode :none))
  (make-query `(select (:prefetch-mode ,prefetch-mode) ,select-clause
                       (from (,process-variable-name ,process-type)
                             (wait-reason wait-reason))
                       (where (and (eq wait-reason (wait-reason-of ,process-variable-name))
                                   (eq (hu.dwim.util::process-state-of ,process-variable-name)
                                       (hu.dwim.util::find-state 'hu.dwim.util::process-state-machine 'hu.dwim.util::in-progress))
                                   (or (null wait-reason)
                                       (and (typep wait-reason 'wait-for-timestamp)
                                            ;; TODO: remove volatile, that should be the default for transaction-timestamp
                                            (timestamp<= (wait-until-of wait-reason) (volatile (transaction-timestamp))))))))))

(def (function e) make-select-for-failed-persistent-processes (&key (process-variable-name 'process)
                                                                    (select-clause (list process-variable-name))
                                                                    (process-type 'persistent-process)
                                                                    (prefetch-mode :none))
  (make-query `(select (:prefetch-mode ,prefetch-mode) ,select-clause
                       (from (,process-variable-name ,process-type))
                       (where (eq (hu.dwim.util::process-state-of ,process-variable-name)
                                  (hu.dwim.util::find-state 'hu.dwim.util::process-state-machine 'hu.dwim.util::failed))))))

;; TODO: add least recently used and limit
(def (function e) select-persistent-processes-ready-to-run (&rest args &key &allow-other-keys)
  (execute-query (apply #'make-select-persistent-processes-ready-to-run-query args)))

(def (function e) count-persistent-processes-ready-to-run (&rest args &key &allow-other-keys)
  (first-elt (execute-query (apply #'make-select-persistent-processes-ready-to-run-query :select-clause '((count process)) args))))

(def (function e) count-persistent-processes-with-state (state-name)
  (first-elt
   (select ((count process))
     (from (process persistent-process))
     (where (eq (hu.dwim.util::process-state-of process) (hu.dwim.util::find-state 'process-state-machine state-name))))))

(def (function e) count-failed-persistent-processes (&rest args &key &allow-other-keys)
  (first-elt (execute-query (apply #'make-select-for-failed-persistent-processes :select-clause '((count process)) args))))

(def function cancel-some-persistent-processes-ready-to-run (count)
  (iter (for processes = (select-persistent-processes-ready-to-run))
        (while processes)
        (dolist (process processes)
          (if (and (lock-instance process :wait #f)
                   (not (process-in-stop-state? process)))
              (progn
                (cancel-process process)
                (princ ".")
                (when (zerop (decf count))
                  (return-from cancel-some-persistent-processes-ready-to-run))))))
  (hu.dwim.rdbms:report-transaction-state))

(def function purge-all-persistent-processes ()
  (purge-instances 'persistent-process)
  (purge-instances 'persistent-message)
  (purge-instances 'message-queue)
  (purge-instances 'wait-reason)
  (hu.dwim.rdbms:report-transaction-state))

;;;;;;
;;; Waiting for subject

(def (function e) make-select-persistent-processes-waiting-for-subject-query (&key (authenticated-subject-variable-name '-authenticated-subject-)
                                                                                   (effective-subject-variable-name '-effective-subject-)
                                                                                   (process-variable-name 'process)
                                                                                   (select-clause (list process-variable-name))
                                                                                   (process-type 'persistent-process)
                                                                                   (prefetch-mode :none))
  (prog1-bind query
      (make-query `(select (:prefetch-mode ,prefetch-mode) ,select-clause
                           (from (,process-variable-name ,process-type)
                                 (wait-reason wait-reason))
                           (where (and (eq wait-reason (wait-reason-of process))
                                       (or (and (typep wait-reason 'wait-for-subject)
                                                (eq (subject-of wait-reason) ,effective-subject-variable-name))
                                           ;; TODO: fix bug in query compiler (adding this clause filters instance which would otherwise be in the result)
                                           #+nil
                                           (and (typep wait-reason 'wait-for-subject-type)
                                                (eq (class-name-of wait-reason) (class-name (class-of ,effective-subject-variable-name))))
                                           ,@(when-bind wait-for-expressions (select-instances (instance wait-for-expression)
                                                                               (where (wait-for-subject-p instance)))
                                                        `((and (typep wait-reason 'wait-for-expression)
                                                               (or ,@(iter (for wait-for-expression :in wait-for-expressions)
                                                                           (collect `(and (equal (oid-of wait-reason) ,(oid-of wait-for-expression))
                                                                                          ,(expression-of wait-for-expression)))))))))))))
    (add-lexical-variable query effective-subject-variable-name)))

(def (function e) select-persistent-processes-waiting-for-subject (subject &rest args &key &allow-other-keys)
  (execute-query (apply #'make-select-persistent-processes-waiting-for-subject-query args) subject))

(def (function e) count-persistent-processes-waiting-for-subject (subject &rest args &key &allow-other-keys)
  (first-elt (execute-query (apply #'make-select-persistent-processes-waiting-for-subject-query :select-clause '((count process)) args) subject)))
