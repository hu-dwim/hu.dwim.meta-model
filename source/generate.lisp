;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Variables

(def special-variable *generate-args*)

(def special-variable *generate-random-state* (make-random-state))

(def special-variable *generated-instances*)

(def special-variable *generated-instance-count*)

(def special-variable *generated-instance-counts*)

;;;;;;
;;; Top level

(def (function e) generate-instances (&rest args &key
                                 (entities (collect-entities))
                                 (purge-instances #f)
                                 &allow-other-keys)
  "Generates instances according to the model. The number of generated instances and their properties and associations tries to fulfill the statistical requirements present in the model as much as possible."
  (when purge-instances
    (purge-instances 'persistent-object))
  (bind ((*generate-args* args)
         (*generate-random-state* (make-random-state *generate-random-state*))
         (*generated-instances* nil)
         (*generated-instance-count* 0)
         (*generated-instance-counts* (make-hash-table))
         (start-time (get-internal-run-time)))
    ;; first phase
    (dolist (entity entities)
      (iter (repeat (required-number-of-instances entity))
            (generate-instance entity)))
    ;; second phase
    (bind ((*generated-instance-counts* (make-hash-table)))
      (iter (for instance = (pop *generated-instances*))
            (while (and instance (< *generated-instance-count* (getf *generate-args* :max 1e6))))
            (generate-associations instance)))
    (log.debug "Generated ~a instances in ~,3f seconds"
               *generated-instance-count* (/ (- (get-internal-run-time) start-time) internal-time-units-per-second))
    *generated-instance-count*))

;;;;;;
;;; Entities

(def function instance-count-for (entity)
  (or (gethash entity *generated-instance-counts*)
      (setf (instance-count-for entity) 0)))

(def function (setf instance-count-for) (new-value entity)
  (setf (gethash entity *generated-instance-counts*) new-value))

(def generic generate-instance (entity)
  (:method ((entity-name symbol))
           (generate-instance (find-entity entity-name)))

  (:method ((entity entity))
           (if (abstract-p entity)
               (aif (find-instanciable-subclass entity)
                    (generate-instance it))
               (bind ((instance (apply 'make-instance entity (collect-property-values entity))))
                 (log.debug "Generated ~a" instance)
                 (generate-properties instance)
                 (push instance *generated-instances*)
                 (incf *generated-instance-count*)
                 (incf (instance-count-for entity))
                 instance)))

  (:method ((persistent-process persistent-process))
           nil))

;;;;;;
;;; Properties

(def function collect-property-values (entity)
  (iter (for property in (effective-properties-of entity))
        (when (and (not (typep property 'computed-effective-slot-definition))
                   (not (typep property 'effective-binary-association-end))
                   (not (slot-definition-initfunction property))
                   (slot-definition-initargs property))
          (nconcing (list (first (slot-definition-initargs property))
                          (generate-property property))))))

(def function generate-properties (instance)
  (dolist (property (effective-properties-of (class-of instance)))
    (when (and (not (typep property 'computed-effective-slot-definition))
               (not (slot-definition-initargs property))
               (not (slot-definition-initfunction property)))
      (bind ((values (multiple-value-list (generate-property property))))
        (when values
          (setf (slot-value-using-class (entity-of instance) instance property)
                (first values)))))))

(def generic generate-property (property)
  (:method ((property effective-property))
           (cond
             ((string= (slot-definition-name property) "NAME")
              (generate-name (symbol-name (element-name-of (owner-entity-of property)))))
             (t (random-value (property-type-of property)))))

  (:method ((property effective-state-property))
           (bind ((states (states-of (state-machine-of property))))
             (element-name-of (random-element states)))))

;;;;;;
;;; Associations

(def function generate-associations (instance)
  (bind ((entity (entity-of instance)))
    (dolist (association-end (effective-association-ends-of entity))
      (log.debug "Generating at association end ~a for instance ~a" association-end instance)
      (link-instance-with-associated-instances instance association-end))))

(def generic link-instance-with-associated-instances (instance association-end)
  (:method :around (instance association-end)
           (with-simple-restart (continue "Skip LINK-INSTANCE-WITH-ASSOCIATED-INSTANCES for instance ~A association ~A"
                                          instance association-end)
             (call-next-method)))

  (:method ((instance persistent-object) (association-end association-end))
           (not-yet-implemented))

  (:method ((instance persistent-object) (association-end binary-association-end))
           (bind ((min-cardinality (min-number-of-associated-instances association-end))
                  (max-cardinality (max-number-of-associated-instances association-end))
                  (required-cardinality (if (to-many-association-end-p association-end)
                                            (floor
                                             (+ min-cardinality
                                                (* (random* (1+ (- max-cardinality min-cardinality)))
                                                   (expt 0.9 (instance-count-for (entity-of instance))))))
                                            (random-integer min-cardinality max-cardinality)))
                  (current-cardinality (current-cardinality instance association-end)))
             (when (< current-cardinality required-cardinality)
               (log.debug "Required cardinality at ~A ~A is ~A" instance association-end required-cardinality)
               (bind ((instances (collect-or-create-free-instances
                                  (- required-cardinality current-cardinality)
                                  (hu.dwim.perec::other-effective-association-end-for (hu.dwim.perec::associated-class-of association-end) association-end))))
                 (if (to-many-association-end-p association-end)
                     (insert-list (with-lazy-slot-value-collections (slot-value instance (slot-definition-name association-end))) instances)
                     (setf (slot-value instance (slot-definition-name association-end)) (first instances))))))))

(def function max-cardinality-for (association-end)
  (or (get-statistics-param association-end :max)
      (and (slot-boundp association-end 'hu.dwim.perec::max-cardinality)
           (hu.dwim.perec::max-cardinality-of association-end))))

(def function current-cardinality (instance association-end)
  (bind ((slot-name (slot-definition-name association-end)))
    (if (to-many-association-end-p association-end)
        (size (slot-value instance slot-name))
        (if (and (slot-boundp instance slot-name)
                 (slot-value instance slot-name))
            1
            0))))

;;;;;;
;;; Queries

(def function collect-or-create-free-instances (n association-end)
  (bind ((max-cardinality (max-cardinality-for association-end))
         (accessor (reader-name-of association-end))
         (query (make-query
                 `(select (instance)
                   (from instance)
                   (where
                    (and
                     (typep instance ',(class-name (owner-entity-of association-end)))
                     ,(cond
                       ((or (not max-cardinality)
                            (eq :n max-cardinality))
                        #t)
                       ((to-many-association-end-p association-end)
                        `(< (length (,accessor instance)) ,max-cardinality))
                       ((= max-cardinality 1) `(null (,accessor instance)))
                       (t #t)))))))
         (instances (execute-query query)))
    (collect-or-create-random-instances (owner-entity-of association-end)
                                        instances
                                        n)))

(def function collect-or-create-random-instances (entity instances n)
  (let* ((instances (collect-random-instances instances n)))
    (iter (repeat (- n (length instances)))
          (awhen (generate-instance entity)
            (push it instances)))
    instances))

(def function collect-random-instances (instances n)
  (iter (repeat n)
        (for len from (length instances) downto 1)
        (for i = (random* len))
        (for instance = (nth i instances))
        (deletef instances instance)
        (collect instance)))

(def function find-instanciable-subclass (entity)
  (find-if (lambda (element) (not (abstract-p element)))
           (effective-sub-generalization-elements-for entity :include-self #t)))

;;;;;;
;;; Statistics interface

(def function required-number-of-instances (entity)
  (get-statistics-param entity :min 0))

(def function min-number-of-associated-instances (association-end)
  (max (get-statistics-param association-end :min 0)
       (hu.dwim.perec::min-cardinality-of association-end)))

(def function max-number-of-associated-instances (association-end)
  (bind ((max-cardinality (hu.dwim.perec::max-cardinality-of association-end)))
    (min (get-statistics-param association-end :max most-positive-fixnum)
         (if (eq :n max-cardinality)
             10
             max-cardinality))))

(def generic get-statistics-param (model-element param-name &optional default)
  (:method ((entity entity) param-name &optional default)
           (or (getf *generate-args* (concatenate-keyword param-name (element-name-of entity)))
               (slot-value (statistics-of entity) (slot-symbol 'statistics param-name))
               default))
  (:method ((association-end binary-association-end) param-name &optional default)
           (or (getf *generate-args* (concatenate-keyword (element-name-of (hu.dwim.perec::association-of association-end))
                                                          "/" (element-name-of association-end)))
               (slot-value (statistics-of association-end) (slot-symbol 'statistics param-name))
               default)))

;;;;;;
;;; Name 

(def function generate-name (prefix)
  (bind ((counter-fn (get-or-create-counter prefix)))
    (string+ prefix "-" (write-to-string (funcall counter-fn)))))

(def special-variable *counters* (make-hash-table :test 'equal))

(def function get-or-create-counter (id)
  (bind ((fn (gethash id *counters*)))
    (when (not fn)
      (setf fn (setf (gethash id *counters*)
                     (bind ((counter 0))
                       (lambda ()
                         (incf counter))))))
    fn))

;;;;;;
;;; Values for various types

(def generic random-value (type &key &allow-other-keys)
  (:documentation "Creates a random value of the given type.")

  ;; TODO: perec
  #+nil
  (:method ((type symbol-type) &key &allow-other-keys)
           (random-symbol))
  
  (:method ((type integer-type) &key (min 0) (max 1000) &allow-other-keys)
           (random-integer min max))

  (:method ((type (eql (find-type 'float-32))) &key min max &allow-other-keys)
           (random-number (or min most-negative-single-float) (or max most-positive-single-float)))

  (:method ((type (eql (find-type 'float-64))) &key min max &allow-other-keys)
           (random-number (or min most-negative-double-float) (or max most-positive-double-float)))
  
  (:method ((type member-type) &key &allow-other-keys)
           (if (eq type (find-type 'boolean))
               (random-boolean)
               (random-element (members-of type))))

  (:method ((type string-type) &key (length 10) &allow-other-keys)
           (random-string length "abcdefghijklmnopqrstuvwxyz "))

  (:method ((type form-type) &key &allow-other-keys)
           (list "form"))

  (:method ((type serialized-type) &key &allow-other-keys)
           (random-value (find-type t)))

  (:method ((type (eql (find-type 'serialized))) &key &allow-other-keys)
           (random-value (find-type t)))

  (:method ((type (eql (find-type t))) &key &allow-other-keys)
           (case (random* 3)
             (0 (random-integer -100 100))
             (1 (random-number -100.0 100.0))
             (2 (random-string 10 "abcdefghijklmnopqrstuvwxyz "))))

  (:method ((type text-type) &key (length 10) &allow-other-keys)
           (random-string length "abcdefghijklmnopqrstuvwxyz "))

  (:method ((type html-text-type) &key (length 10) &allow-other-keys)
           (random-string length "abcdefghijklmnopqrstuvwxyz "))

  (:method ((type (eql (find-type 'percentage))) &key &allow-other-keys)
           (random-number 0.0 1.0))

  (:method ((type (eql (find-type 'date))) &key &allow-other-keys)
           (random-date))
  
  (:method ((type (eql (find-type 'timestamp))) &key &allow-other-keys)
           (random-timestamp))

  (:method ((type persistent-class) &key &allow-other-keys)
           (random-instance type)))

;;;;;;
;;; Random generators

(def function random* (limit)
  (random limit *generate-random-state*))

(def function random-boolean ()
  (> (random* 2) 0))

(def function random-integer (min max)
  (+ min (random* (1+ (- max min)))))

(def function random-number (min max)
  (setf min (rational min))
  (setf max (rational max))
  (bind ((factor (random-integer 1 100)))
    (/ (random-integer (* factor min) (* factor max)) factor)))

(def function random-string (length &optional (alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (bind ((str (make-string length)))
    (loop for i from 0 below length
          do (setf (char str i) (random-char alphabet)))
    str))

(def function random-symbol ()
  'random-symbol)

(def function random-timestamp ()
  (bind ((date (random-date)))
    (multiple-value-bind (nsec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
        (decode-timestamp date)
      (declare (ignore nsec sec min hour day-of-week daylight-saving-time-p original-timezone))
      (encode-timestamp 0 (random* 59) (random* 59) (random* 23) day month year :offset 0))))

(def function random-date ()
  (bind ((year (+ 2000 (random* 10)))
         (month (1+ (random* 12)))
         (day (1+ (random* 31))))
    (encode-timestamp 0 0 0 0 day month year :offset 0)))

(def function random-element (lst)
  (nth (random* (length lst)) lst))

(def function random-char (alphabet)
  (if alphabet
      (char alphabet (random* (length alphabet)))
      (code-char (random-integer 32 128))))

(def function random-instance (class)
  (bind ((query (make-query
                 `(select (:result-type scroll) (instance)
                   (from instance)
                   (where (typep instance ,class)))))
         (scroll (execute-query query)))
    (setf (page-size scroll) 1)
    (bind ((count (element-count scroll)))
      (if (zerop count)
          (generate-instance class)
          (progn
            (setf (page scroll) (random* count))
            (elt (elements scroll) 0))))))
