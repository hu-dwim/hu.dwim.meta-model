;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(define-model-class structure-diagram (model-element model-element-collection)
  ()
  (:documentation
   "An structure diagram is a collection of relationship elements and relationships between them."))
