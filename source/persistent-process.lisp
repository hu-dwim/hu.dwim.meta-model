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

(define-model-class persistent-process (entity)
  ((form
    :type form)
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
  (:documentation
   "A persistent process is a stateful activity which might go on for an extremely long time. It might start other activities and may communicate with them. A persistent process might also communicate with arbitrary number of users. The users may or may not be online while the process is running, so the process might have to wait for a long time. Asynchronous event handling, such as timeouts are also to be supported."))

(def (special-variable e) *process*)

;;;;;;
;;; Localization

(def localization en
  (slot-name.process-state "process state")

  (process-state.initializing "initializing")
  (process-state.running "running")
  (process-state.in-progress "in progress")
  (process-state.paused "paused")
  (process-state.finished "finished")
  (process-state.failed "failed")
  (process-state.cancelled "cancelled")
  (process-state.broken "broken"))

(def localization hu
  (slot-name.process-state "folyamat állapot")

  (process-state.initializing "felkészítés alatt")
  (process-state.running "fut")
  (process-state.in-progress "folyamatban")
  (process-state.paused "felfüggesztve")
  (process-state.finished "befeződött")
  (process-state.failed "hibás")
  (process-state.cancelled "elvetve")
  (process-state.broken "tönkrement"))

;;;;;;
;;; Model management

(def (definer e :available-flags "e") persistent-process (name super-classes slots &body forms)
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
                    (:metaclass persistent-process)
                    ,@options
                    ,(awhen lisp-forms
                            `(:form (progn ,@lisp-forms))))))))

(def method initialize-instance :around ((persistent-process persistent-process) &rest args)
  (apply #'shared-ininitialize-around-persistent-process persistent-process #'call-next-method args))

(def method reinitialize-instance :around ((persistent-process persistent-process) &rest args)
  (apply #'shared-ininitialize-around-persistent-process persistent-process #'call-next-method
         :name (class-name persistent-process) args))

(def function shared-ininitialize-around-persistent-process (persistent-process call-next-method &rest args
                                                             &key form worker-timeout error-count-limit &allow-other-keys)
  (let ((processed-args
         (append
          (when form
            (list :form (first form)
                  :closure (make-persistent-process-closure (first form))))
          (when worker-timeout
            (list :worker-timeout (first worker-timeout)))
          (when error-count-limit
            (list :error-count-limit (first error-count-limit)))
          (remove-from-plistf args :closure :worker-timeout :error-count-limit))))
    (apply call-next-method persistent-process processed-args)))

(def method hu.dwim.perec::persistent-class-default-superclasses ((class persistent-process) &key name direct-superclasses)
  (cond ((eq name 'standard-persistent-process)
         (call-next-method))
        ((find-if (lambda (direct-superclass)
                    (ignore-errors (subtypep direct-superclass (find-class 'standard-persistent-process))))
                  direct-superclasses)
         nil)
        (t
         (list (find-class 'standard-persistent-process)))))

(def function make-persistent-process-closure (form)
  (hu.dwim.delico::make-closure/cc
   (walk-form
    (with-unique-names (result)
      `(lambda ()
         (let ((,result ,form))
           (finish-persistent-process *process* ,result))
         ;; TODO: fix walker:
         #+nil
         (finish-persistent-process *process* ,form))))))

(def method before-committing-instance :before (transaction (process persistent-process) event)
  (assert (not (eq (element-name-of (process-state-of process)) 'running))
          () "Processes should not be comitted with running state"))

;;;;;;
;;; Standard persistent processes

;; TODO: put in the macro
;; initializing - a process will be in this state while setting its parameters and the scheduler will skip these
;; running - this state can only be seen from the transaction where the process is being run
;; in-progress - wait reason describes why the process stopped last time  
;; paused - intentionally paused by the user
;; cancelled - intentionally cancelled by the user
;; failed - set by the process (i.e. database has fatally changed since last restart) 
;; finished - normal end of execution
;; broken - process ended too many times with raising a condition and the scheduler decided to abort it to save resources
(def (finite-state-machine e) standard-persistent-process-state-machine
  (:start-state initializing)
  (:stop-states finished failed cancelled broken)
  (:transitions
   (start initializing running)
   (wait initializing in-progress)
   (wait running in-progress)
   (continue in-progress running)
   (pause in-progress paused)
   (pause paused in-progress)
   (cancel in-progress cancelled)
   (cancel paused cancelled)
   (finish running finished)
   (fail running failed)
   (die in-progress broken) ; this is only needed because the error handler needs to run in a new transaction to avoid deadlocks
   (die running broken)))

;; TODO rename to persistent-process-object?
(def entity standard-persistent-process ()
  ((process-state :primary #t :state-machine standard-persistent-process-state-machine :index :bitmap)
   (form :type (or unbound serialized) :prefetch #f :cache #t)
   (continuation :type (or null serialized) :prefetch #f :cache #t)
   (result :type t)
   (error-count 0 :type integer-16))
  (:metaclass persistent-process))

(export 'process-state)

(def association
  ((:slot parent-process :type (or null standard-persistent-process))
   (:slot child-processes :type (set standard-persistent-process))))

(def association
  ((:slot created-processes :type (set standard-persistent-process))
   (:slot creator :type (or null subject))))

;;;;;;
;;; Messaging between processes

(def entity message-queue ()
  ())

(def entity standard-message ()
  ())

(def association
  ((:type (or null message-queue))
   (:type (or null standard-persistent-process))))

(def association
  ((:type (set standard-message))
   (:type (or null message-queue))))

;;;;;;
;;; Wait reasons

(def entity wait-reason ()
  ()
  (:abstract #t))

(def association
  ((:type (or null wait-reason))
   (:type (set standard-persistent-process))))

(def entity wait-for-expression (wait-reason)
  ((wait-for-subject :type boolean)
   (expression :type serialized :documentation "The expression will be evaluated to check whether the process is still waiting, all other fields are ignored")))

(def entity wait-for-subject (wait-reason)
  ())

(def association
  ((:type (set wait-for-subject))
   (:type subject :primary #t :reference #t)))

(def entity wait-for-subject-type (wait-reason)
  ((class-name :type symbol :primary #t :reference #t)))

(def entity wait-for-timestamp (wait-reason)
  ((wait-until :type timestamp :primary #t :reference #t)))

(def ptype milestone ()
  'integer-32)

(def entity wait-for-milestone (wait-reason)
  ((milestone :type symbol)
   (count :type integer-32)))

(def association
  ((:type standard-persistent-process)
   (:type (set wait-for-milestone))))

(def ptype activity ()
  'integer-32)

(def entity wait-for-activity (wait-reason)
  ((activity :type symbol)
   (level :type integer-32)))

(def association
  ((:type standard-persistent-process)
   (:type (set wait-for-activity))))

(def entity wait-for-message (wait-reason)
  ((message-class-name :type symbol)))

;;;;;;
;;; Localization

(def localization en
  (class-name.standard-message "standard message")
  (class-name.message-queue "message queue")
  (class-name.wait-reason "wait reason")
  (class-name.wait-for-message "wait for message")
  (class-name.wait-for-subject "wait for subject")
  (class-name.wait-for-timestamp "wait for timestamp")
  (class-name.standard-persistent-process "standard persistent process"))

(def localization hu
  (class-name.standard-message "standard üzenet")
  (class-name.message-queue "üzenetsor")
  (class-name.wait-reason "várakozási ok")
  (class-name.wait-for-message "üzenetre várakozás")
  (class-name.wait-for-subject "alanyra várakozás")
  (class-name.wait-for-timestamp "időpontra várakozás")
  (class-name.standard-persistent-process "standard megszakítható folyamat"))

;;;;;;
;;; Starting persistent processes

(def special-variable *debug-persistent-process* #f)

(def function handle-persistent-process-error (error)
  (process.error "Error in persistent process ~A: ~A" *process* error)
  ;; TODO clean up and use a public api for error handling
  (when *debug-persistent-process*
    (restart-case
     ;; TODO this is bullshit, we may not even be connected, see invoke-slime-debugger-if-possible
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
                (die-persistent-process process)))))))))

(def with-macro with-persistent-process-error-handler ()
  (handler-bind
      ((serious-condition 'handle-persistent-process-error))
    (-body-)))

(def (function e) start-standard-persistent-process (form &rest args &key &allow-other-keys)
  (apply #'start-persistent-process 'standard-persistent-process :form form args))

(def (generic e) start-persistent-process (object &rest args)
  (:documentation "The main start persistent process method")

  (:method ((process-name symbol) &rest args)
    (bind ((persistent-process (find-persistent-process process-name)))
      (if (not persistent-process)
          (error "Persistent process ~A does not exist" process-name)
          (apply #'start-persistent-process persistent-process args))))

  (:method ((process-class persistent-process) &rest args &key (parent-process nil) (start-running-in-same-transaction #t) &allow-other-keys)
    (remove-from-plistf args :start-running-in-same-transaction)
    (bind ((process
            (apply #'make-instance process-class
                   :parent-process (or parent-process
                                       (when (boundp '*process*)
                                         *process*))
                   args))
           (*process* process))
      (%start-persistent-process process :start-running-in-same-transaction start-running-in-same-transaction)))

  (:method ((process standard-persistent-process) &key (start-running-in-same-transaction #t))
    (bind ((*process* process))
      (%start-persistent-process process :start-running-in-same-transaction start-running-in-same-transaction))))

(def function %start-persistent-process (process &key (start-running-in-same-transaction #t))
  (with-thread-name " / %START-PERSISTENT-PROCESS"
    (assert (persistent-process-initializing-p process))
    (bind ((*debug-evaluate/cc* *debug-persistent-process*))
      (debug-only
        (assert (eq (transaction-of process) *transaction*)))
      (process.debug "Starting persistent process ~A" process)
      (when (not (slot-boundp process 'form))
        (setf (form-of process) (form-of (class-of process))))
      (if start-running-in-same-transaction
          (values process
                  (with-persistent-process-error-handler
                    (process-event process 'process-state 'start)
                    (%%start-persistent-process process)))
          ;; optimizing away storing the continuation
          (progn
            (process-event process 'process-state 'wait)
            process)))))

(def function %%start-persistent-process (process)
  (with-call/cc
    (funcall (or (closure-of (class-of process))
                 (make-persistent-process-closure (form-of process))))))

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
    (assert (persistent-process-in-progress-p process))
    (bind ((*debug-evaluate/cc* *debug-persistent-process*))
      (debug-only
        (assert (eq (transaction-of process) *transaction*)))
      (process.debug "Continuing persistent process ~A" process)
      (values process
              (with-persistent-process-error-handler
                (aif (continuation-of process)
                     (kall it)
                     ;; optimizing away storing the continuation
                     (progn
                       (process-event process 'process-state 'continue)
                       (%%start-persistent-process process))))))))

;;;;;;
;;; Handle continuations

;; KLUDGE: eh, this is really hackish
(def method hu.dwim.delico::evaluate/cc ((var hu.dwim.delico::lexical-variable-reference-form) lex-env dyn-env k)
  (declare (ignore dyn-env))
  (hu.dwim.delico::kontinue k (prog1-bind instance (hu.dwim.delico::lookup lex-env :let (name-of var) :error-p t)
                                (if (and (typep instance 'persistent-object)
                                         (persistent-p instance))
                                    (revive-instance instance)))))

(def (function e) persistent-process-save-point (process k)
  (process.debug "Storing persistent process continuation for ~A" process)
  (setf (continuation-of process) k))

(def (function e) persistent-process-wait (process wait-reason)
  (awhen (wait-reason-of process)
    (purge-instance it))
  (setf (wait-reason-of process) wait-reason)
  (process-event process 'process-state 'wait))

(def function destroy-continuation (process)
  "Destroys the continuation of the persistent process"
  (setf (continuation-of process) nil))

(def (function e) k-of (process)
  "Provides the current continuation of the persistent process"
  (continuation-of process))

(def (function e) k-at-form-p (k form)
  "Checks an arnesi continuation to be at the given form"
  (do ((index 0 (1+ index))
       (frame (caadr k) (hu.dwim.delico::parent-of frame)))
      ;; TODO: maybe we need to have equal instead of eq?
      ;; when checking for the stored continuation we will clearly not have eq, right? 
      ((or (not frame) (eq form (hu.dwim.delico::source-of frame)))
       (when frame (if (= 0 index)
                       :current
                       :parent)))))

(def (function e) process-continuation-at-form-p (process form)
  "Checks the continuation as stored in the persistent storage."
  ;; TODO: it should read continuation within a nested new transaction
  (k-at-form-p (continuation-of process) form))

;;;;;;
;;; Put persistent process in final states and stop execution

(def function %finish-persistent-process (process result)
  (setf (result-of process) result)
  (destroy-continuation process)
  (awhen (parent-process-of process)
    ;; TODO: specialize message, do we really need this? the parent can look at its children anyway
    #+nil
    (send-message (make-standard-message) it))
  result)

;;;;;;
;;; Messaging

(def (function e) send-message (message process)
  (unless (message-queue-of process)
    (setf (message-queue-of process)
          (make-message-queue)))
  (insert-item (standard-messages-of* (message-queue-of process)) message))

(def (function/cc e) receive-message (process &optional (message-class-name 'standard-message))
  ;; TODO: check if a message is already available
  (let/cc k
    (persistent-process-save-point process k)
    (persistent-process-wait process
                             (make-wait-for-message :message-class-name message-class-name))
    k)
  (process-event process 'process-state 'continue))

;;;;;;
;;; Put persistent process in final states and stop execution

;; TODO this should be called automatically by the statemachine
(def generic process-state-changed (process)
  (:method ((process standard-persistent-process))
    (values)))

;; TODO shouldn't the invocation of these be attached as callbacks on the state-machine transitions?
;; in tandem with a shorter alias for (process-event process 'process-state ...)?
(def generic pause-persistent-process (process)
  (:method (process)
           (process.debug "Pausing persistent process ~A" process)
           (process-event process 'process-state 'pause)))

(def (generic e) finish-persistent-process (process &optional result)
  (:method ((process standard-persistent-process) &optional result)
    (process.debug "Finishing persistent process ~A" process)
    (process-event process 'process-state 'finish)
    (process-state-changed process)
    (%finish-persistent-process process result)))

(def generic fail-persistent-process (process &optional result)
  (:method ((process standard-persistent-process) &optional result)
           (process.debug "Failing persistent process ~A" process)
           (process-event process 'process-state 'fail)
           (process-state-changed process)
           (%finish-persistent-process process result)))

(def (generic e) cancel-persistent-process (process &optional result)
  (:method (process &optional result)
           (process.debug "Cancelling persistent process ~A" process)
           (process-event process 'process-state 'cancel)
           (%finish-persistent-process process result)))

(def generic die-persistent-process (process)
  (:method ((process standard-persistent-process))
           (process.debug "Dying persistent process ~A" process)
           (process-event process 'process-state 'die)
           (process-state-changed process)
           (slot-makunbound process 'result)
           (destroy-continuation process)
           (values)))

(export '(process-state-changed
          die-persistent-process cancel-persistent-process
          fail-persistent-process finish-persistent-process))

(def (function e) persistent-process-initializing-p (process)
  (eq (element-name-of (process-state-of process)) 'initializing))

(def (function e) persistent-process-in-progress-p (process)
  (eq (element-name-of (process-state-of process)) 'in-progress))

(def (function e) persistent-process-running-p (process)
  (eq (element-name-of (process-state-of process)) 'running))

(def (function e) persistent-process-paused-p (process)
  (eq (element-name-of (process-state-of process)) 'paused))

(def (function e) persistent-process-finished-p (process)
  (eq (element-name-of (process-state-of process)) 'finished))

(def (function e) persistent-process-in-final-state-p (process)
  (typep (process-state-of process) 'stop-state))

(def (function e) count-persistent-processes-with-state (state-name)
  (first-elt
   (select ((count process))
     (from (process standard-persistent-process))
     (where (eq (process-state-of process) (find-state 'standard-persistent-process-state-machine state-name))))))

;;;;;;
;;; Ready to run

(def (function e) persistent-process-ready-to-run-p (process)
  (bind ((query (make-select-persistent-processes-ready-to-run-query :select-clause '((count process)))))
    (add-lexical-variable query 'instance)
    (add-assert query '(eq instance process))
    (= 1 (first (execute-query query process)))))

(def (function e) make-select-persistent-processes-ready-to-run-query (&key (process-variable-name 'process)
                                                                            (select-clause (list process-variable-name))
                                                                            (process-type 'standard-persistent-process)
                                                                            (prefetch-mode :none))
  (make-query `(select (:prefetch-mode ,prefetch-mode) ,select-clause
                       (from (,process-variable-name ,process-type)
                             (wait-reason wait-reason))
                       (where (and (eq wait-reason (wait-reason-of ,process-variable-name))
                                   (eq (process-state-of ,process-variable-name)
                                       (find-state 'standard-persistent-process-state-machine 'in-progress))
                                   (or (null wait-reason)
                                       (and (typep wait-reason 'wait-for-timestamp)
                                            ;; TODO: remove volatile, that should be the default for transaction-timestamp
                                            (timestamp<= (wait-until-of wait-reason) (volatile (transaction-timestamp))))))))))

(def (function e) make-select-for-failed-persistent-processes (&key (process-variable-name 'process)
                                                                    (select-clause (list process-variable-name))
                                                                    (process-type 'standard-persistent-process)
                                                                    (prefetch-mode :none))
  (make-query `(select (:prefetch-mode ,prefetch-mode) ,select-clause
                       (from (,process-variable-name ,process-type))
                       (where (eq (process-state-of ,process-variable-name)
                                  (find-state 'standard-persistent-process-state-machine 'failed))))))

;; TODO: add least recently used and limit
(def (function e) select-persistent-processes-ready-to-run (&rest args &key &allow-other-keys)
  (execute-query (apply #'make-select-persistent-processes-ready-to-run-query args)))

(def (function e) count-persistent-processes-ready-to-run (&rest args &key &allow-other-keys)
  (first-elt (execute-query (apply #'make-select-persistent-processes-ready-to-run-query :select-clause '((count process)) args))))

(def (function e) count-failed-persistent-processes (&rest args &key &allow-other-keys)
  (first-elt (execute-query (apply #'make-select-for-failed-persistent-processes :select-clause '((count process)) args))))

(def function cancel-some-persistent-processes-ready-to-run (count)
  (iter (for processes = (select-persistent-processes-ready-to-run))
        (while processes)
        (dolist (process processes)
          (if (and (lock-instance process :wait #f)
                   (not (persistent-process-in-final-state-p process)))
              (progn
                (cancel-persistent-process process)
                (princ ".")
                (when (zerop (decf count))
                  (return-from cancel-some-persistent-processes-ready-to-run))))))
  (hu.dwim.rdbms:report-transaction-state))

(def function purge-all-persistent-processes ()
  (purge-instances 'standard-persistent-process)
  (purge-instances 'standard-message)
  (purge-instances 'message-queue)
  (purge-instances 'wait-reason)
  (hu.dwim.rdbms:report-transaction-state))

;;;;;;
;;; Waiting for subject

(def (function e) make-select-persistent-processes-waiting-for-subject-query (&key (authenticated-subject-variable-name '-authenticated-subject-)
                                                                                   (effective-subject-variable-name '-effective-subject-)
                                                                                   (process-variable-name 'process)
                                                                                   (select-clause (list process-variable-name))
                                                                                   (process-type 'standard-persistent-process)
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
