;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.meta-model.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.meta-model
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))))))
