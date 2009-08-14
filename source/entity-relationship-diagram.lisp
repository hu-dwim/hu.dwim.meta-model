;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(define-model-class entity-relationship-diagram (structure-diagram)
  ()
  (:documentation "An entity relationship diagram is a collection of entities and relationships between them."))

(def (function e) define-simple-entity-relationship-diagram (diagram-name model-element-names &rest args)
  (bind ((model-elements (mapcar #'find-class model-element-names)))
    (apply 'define-entity-relationship-diagram diagram-name
           :model-elements (append model-elements (collect-relationships-between-relationship-elements model-elements))
           args)))

(def (macro e) def-simple-entity-relationship-diagram (diagram-name model-element-names &rest args)
  `(define-simple-entity-relationship-diagram ',diagram-name ',model-element-names ,@args))

(def (definer e) simple-entity-relationship-diagram (diagram-name model-element-names &rest args)
  `(def-simple-entity-relationship-diagram ,diagram-name ,model-element-names ,@args))
