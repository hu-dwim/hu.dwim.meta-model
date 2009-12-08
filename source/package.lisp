;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def package :hu.dwim.meta-model
  (:use :bordeaux-threads
        :cl-containers
        :cl-l10n
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.computed-class
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.delico
        :hu.dwim.logger
        :hu.dwim.perec
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        ;; there shouldn't be many references to :hu.dwim.walker, so just prefix the usages...
        :local-time
        :metacopy-with-contextl
        :trivial-garbage)

  (:shadowing-import-from :iterate
                          #:finish)

  (:shadow #:login
           #:model
           #:element
           #:parent
           #:code
           #:result
           #:transaction-mixin)

  (:import-from :hu.dwim.perec
                #:compute-as)

  (:export #:-instance-
           #:-entity-
           #:-property-
           #:-authenticated-subject-
           #:-effective-subject-
           #:top-level-authorization)

  (:readtable-setup
   (enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-lambda-with-bang-args-syntax)))
