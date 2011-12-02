;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Top level model entry points

(define-model-class model (model-element-collection)
  ((model-element-cache
    (make-hash-table :test #'equal :synchronized #t)
    :documentation "A map from (cons TYPE NAME) to model elements"))
  (:documentation "The top level container of model elements. There is only one instance of this class and there should never be more. The reason for this is to avoid intoducing superfluous barriers between models and model elements. If you ever want to have several instances, then you probably should introduce a new meta class instead and use it as part of the model."))

;;;;;;
;;; Generic model functions

;; TODO delme? but where should we collect model-elements then? but do we need to collect anything on top of what perec already keeps track of?
;; but even if we do, why don't we just have a bunch of globals, e.g. (def namespace ...)-es?
(def special-variable *model* (make-model)
  "This is the singleton top level model object which contains all the defined model elements.")

(def (macro e) with-model-database (&body body)
  (simple-style-warning "FIXME: ~S is obsolete and only expands to a simple PROGN" 'with-model-database)
  `(progn
     ,@body))

(def class* postgresql/dwim (hu.dwim.perec:database-mixin hu.dwim.rdbms.postgresql:postgresql)
  ())

(def method hu.dwim.rdbms:transaction-mixin-class list ((self postgresql/dwim))
  'transaction-mixin/dwim)

(def computed-universe computed-universe/transaction ()
  ()
  (:computed-state-factory-name compute-as/transaction-specific)
  (:universe-accessor-form (computed-universe-of *transaction*)))

(def (class* e) transaction-mixin/dwim (hu.dwim.perec:transaction-mixin transaction-with-hooks-mixin)
  ((computed-universe (make-instance 'computed-universe/transaction))))

(def method hu.dwim.rdbms:notify-transaction-event ((transaction transaction-mixin/dwim) event)
  (unless (eq event :select)
    (incf (hu.dwim.computed-class::pulse-of (computed-universe-of transaction)))))

;;;;;;
;;; Collecting and finding elements

(def function cached-model-element (type name)
  (gethash (cons type name) (model-element-cache-of *model*)))

(def function (setf cached-model-element) (new-value type name)
  (setf (gethash (cons type name) (model-element-cache-of *model*)) new-value))

(def (generic e) collect-model-elements-if (predicate element &key &allow-other-keys)
  (:documentation "Collects all model elements including declared and derived model elements which match the given predicate.")

  (:method-combination append)

  (:method :around (predicate element &rest args &key (include-derived #t) &allow-other-keys)
           (delete-duplicates
            (apply #'call-next-method predicate
                   element
                   (if (getf args :include-derived)
                       args
                       (append (list :include-derived include-derived) args)))))

  (:method append (predicate element &key &allow-other-keys)
           ;; KLUDGE: kill this stupid collect stuff
           (ignore-errors
             (when (funcall predicate element)
               (list element))))

  (:method append (predicate (model-element model-element) &key &allow-other-keys)
           (when (funcall predicate model-element)
             (list model-element)))

  (:method append (predicate (model-element-collection model-element-collection) &rest args &key &allow-other-keys)
           (mappend [apply #'collect-model-elements-if predicate !1 args]
                    (model-elements-of model-element-collection))))

(def (function e) collect-model-elements (&rest args &key (model-element-collection *model*) &allow-other-keys)
  (apply #'collect-model-elements-if #'identity model-element-collection args))

(def (function e) collect-model-elements-if-typep (element-type-name &rest args &key (model-element-collection *model*) &allow-other-keys)
  ;; TODO optimize: add a function model-element-type-predicate and a case inside it for element-type-name covering the often used
  ;; ones so that the typep's can be optimized
  (apply #'collect-model-elements-if [typep !1 element-type-name] model-element-collection args))

(def (function e) find-model-element-if (predicate &rest args &key element-name (model-element-collection *model*) (ignore-missing #f) &allow-other-keys)
  (let ((elements (apply #'collect-model-elements-if predicate model-element-collection args)))
    (if (< 1 (length elements))
        (error "Found more than one model element")
        (aif (first elements)
             it
             (unless ignore-missing
               (error "Did not find model element: ~A" element-name))))))

(def (function e) find-model-element (model-element-name &rest args &key (ignore-missing #f) &allow-other-keys)
  (aprog1
      (apply #'find-model-element-if [eq model-element-name (element-name-of !1)]
             :ignore-missing #t :element-name model-element-name args)
    (when (and (not it)
               (not ignore-missing))
      (error "Could not find model element ~S" model-element-name))))

(def (function e) find-model-element-if-typep (model-element-or-name element-type-name &rest args &key &allow-other-keys)
  (cond
    ((typep model-element-or-name element-type-name) model-element-or-name)
    ((symbolp model-element-or-name)
     (bind (((:values model-element found-in-cache?) (cached-model-element element-type-name model-element-or-name)))
       (if found-in-cache?
           model-element
           (awhen (apply #'find-model-element-if
                         (lambda (element)
                           ;; TODO optimize, see todo above
                           (and (typep element element-type-name)
                                (eq model-element-or-name (element-name-of element))))
                         :element-name model-element-or-name
                         :type (find-class element-type-name)
                         args)
             (setf (cached-model-element element-type-name model-element-or-name) it)))))
    (t
     (error "Invalid parameter"))))

;;;;;;
;;; Modifing the model

(def function add-model-element (model-element &optional (model-element-collection *model*))
  (assert (not (member model-element (model-elements-of model-element-collection))))
  (assert (not (find (element-name-of model-element) (model-elements-of model-element-collection) :key #'element-name-of)))
  (push model-element (model-elements-of model-element-collection))
  model-element)

(def function delete-model-element (model-element &optional (model-element-collection *model*))
  (assert (member model-element (model-elements-of model-element-collection)))
  (deletef model-element (model-elements-of model-element-collection))
  model-element)
