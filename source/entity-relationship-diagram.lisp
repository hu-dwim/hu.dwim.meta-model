;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; entity-relationship-diagram

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

;;;;;;
;;; t/inspector

(def layered-method make-alternatives ((component t/inspector) (class standard-class) (prototype entity-relationship-diagram) (value entity-relationship-diagram))
  (list* (make-instance 'entity-relationship-diagram/documentation/inspector
                        :component-value value
                        :component-value-type (component-value-type-of component))
         (make-instance 'entity-relationship-diagram/graph/inspector
                        :component-value value
                        :component-value-type (component-value-type-of component))
         (call-next-method)))

;;;;;;
;;; entity-relationship-diagram/documentation/inspector

(def (component e) entity-relationship-diagram/documentation/inspector (t/documentation/inspector)
  ())

(def method make-documentation ((component entity-relationship-diagram/documentation/inspector) (class standard-class) (prototype entity-relationship-diagram) (value entity-relationship-diagram))
  (documentation-of value))

;;;;;;
;;; entity-relationship-diagram/graph/inspector

(def (component e) entity-relationship-diagram/graph/inspector (inspector/style content/widget)
  ())

(def refresh-component entity-relationship-diagram/graph/inspector
  (bind (((:slots hu.dwim.wui::content component-value) -self-))
    (setf hu.dwim.wui::content (build-graph-for-entity-relationship-diagram component-value))))

(def (function e) build-graph-for-complete-entity-relationship-diagram ()
  (let ((entities (collect-entities)))
    (build-graph-for-structure-diagram-by-elements
     entities
     (collect-relationships-between-relationship-elements entities))))

(def (function e) build-graph-for-entity-relationship-diagram (entity-relationship-diagram)
  (build-graph-for-structure-diagram entity-relationship-diagram))

(def generic relationship-end-arrow (relationship-end)
  (:method ((association-end hu.dwim.perec::persistent-association-end-slot-definition))
    (when (eq (hu.dwim.perec::cardinality-kind-of association-end) :n)
      (make-instance 'arrow/widget :shape :reverse-arrow-with-line)))

  (:method ((generalization-end generalization-end))
    (when (super-generalization-end-p generalization-end)
      (make-instance 'arrow/widget :shape :empty-arrow))))

(def generic relationship-end-dot-arrow (relationship-end)
  (:method ((association-end hu.dwim.perec::persistent-association-end-slot-definition))
    (if (eq (hu.dwim.perec::cardinality-kind-of association-end) :n)
        :invempty
        :none))

  (:method ((generalization-end generalization-end))
    (if (super-generalization-end-p generalization-end)
        :empty
        :none)))

(def generic make-relationship-element-vertex (graph relationship-element &key &allow-other-keys)
  (:method (graph (relationship-element relationship-element) &key &allow-other-keys)
    (bind ((name (hu.dwim.wui::localized-class-name relationship-element :capitalize-first-letter #t)))
      (cl-graph:add-vertex graph
                           (element-name-of relationship-element)
                           :dot-attributes `(:shape :box :label ,name)
                           :content name)))

  (:method (graph (class persistent-class) &key &allow-other-keys)
    (bind ((name (hu.dwim.wui::localized-class-name class :capitalize-first-letter #t)))
      (cl-graph:add-vertex graph
                           (class-name class)
                           :dot-attributes `(:shape :box :label ,name)
                           :content name))))

(def generic make-relationship-edge (graph relationship relationship-element-name-vertex-map)
  (:method (graph relationship relationship-element-name-vertex-map)
    (cl-graph:add-edge-between-vertexes graph
                                        (gethash (element-name-of (relationship-element-of (secondary-relationship-end-of relationship)))
                                                 relationship-element-name-vertex-map)
                                        (gethash (element-name-of (relationship-element-of (primary-relationship-end-of relationship)))
                                                 relationship-element-name-vertex-map)
                                        :if-duplicate-do :force
                                        :dot-attributes `(:label ,"" ;; TODO: pass in relationship name?
                                                          :arrowhead ,(relationship-end-dot-arrow (primary-relationship-end-of relationship))
                                                          :arrowtail ,(relationship-end-dot-arrow (secondary-relationship-end-of relationship)))
                                        :head-arrow (relationship-end-arrow (primary-relationship-end-of relationship))
                                        :tail-arrow (relationship-end-arrow (secondary-relationship-end-of relationship)))))

(def (function e) build-graph-for-complete-structure-diagram ()
  (build-graph-for-structure-diagram-by-elements (collect-relationship-elements) (collect-relationships)))

(def (function e) build-graph-for-structure-diagram (structure-diagram &key omit-slots)
  (build-graph-for-structure-diagram-by-elements
   (collect-model-elements-if-typep 'persistent-class :model-element-collection structure-diagram)
   (collect-model-elements-if-typep '(or persistent-association generalization) :model-element-collection structure-diagram)
   :omit-slots omit-slots))

(def (function e) build-graph-for-structure-diagram-by-elements (relationship-elements relationships &key omit-slots)
  (let* ((graph (make-instance 'graph/widget))
         (relationship-element-name-vertex-map (make-hash-table))
	 (vertexes nil)
	 (edges nil))
    (dolist (relationship-element relationship-elements)
      (push (setf (gethash (class-name relationship-element) relationship-element-name-vertex-map)
                  (make-relationship-element-vertex graph relationship-element :omit-slots omit-slots))
	    vertexes))
    (dolist (relationship relationships)
      (push (make-relationship-edge graph relationship relationship-element-name-vertex-map)
            edges))
    graph))
