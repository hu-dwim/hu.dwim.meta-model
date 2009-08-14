;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; These definitions need to be available by the time we are reading other files, therefore they are in a standalone file.

(def function setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-lambda-with-bang-args-syntax))

(register-readtable-for-swank
 '(:hu.dwim.meta-model :hu.dwim.meta-model-test) 'setup-readtable)
