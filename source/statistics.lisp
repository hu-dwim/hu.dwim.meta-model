;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(define-model-class statistics (element)
  ((min
    0
    :type integer
    :documentation "Minimum of the distribution")
   (max
    nil
    :type integer
    :documentation "Maximum of the distribution"))
  (:documentation "Describes the distribution of a quantity"))
