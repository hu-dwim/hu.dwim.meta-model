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
        :hu.dwim.walker
        :local-time
        :metacopy-with-contextl
        :trivial-garbage)

  (:shadowing-import-from :iterate
                          #:finish)

  (:shadowing-import-from :hu.dwim.perec
                          #:set)
  
  (:shadow #:defassociation
           #:login
           #:model
           #:element
           #:random-string
           #:parent
           #:code
           #:result
           #:image
           #:color
           #:transaction-mixin
           #:find-association)

  (:import-from :hu.dwim.perec
                #:compute-as)

  (:export #:login
           #:logout

           ;; authorization
           #:-operation-
           #:-instance-
           #:-entity-
           #:-property-
           #:-authenticated-subject-
           #:-effective-subject-
           #:top-level-authorization

           ;; these are defined in wui and not in any dwim file
           #:show-maybe
           #:show-to-subject
           #:show-to-current-effective-subject
           #:show-to-subjects-matching-expression

           ;; needed by dwim-presentation
           #:identifier-of)

  (:readtable-setup
   (enable-standard-hu.dwim-syntaxes)
   (hu.dwim.syntax-sugar:enable-lambda-with-bang-args-syntax)))
