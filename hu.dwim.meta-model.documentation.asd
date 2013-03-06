;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.meta-model.documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :depends-on (:hu.dwim.meta-model.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "meta-model" :depends-on ("package"))
                             (:file "package")))))
