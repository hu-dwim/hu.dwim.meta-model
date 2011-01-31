;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.meta-model.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.meta-model.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "meta-model" :depends-on ("package"))
                             (:file "package")))))
