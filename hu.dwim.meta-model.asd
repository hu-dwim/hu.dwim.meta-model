;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.meta-model
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Various meta model classes and behavior"
  :depends-on (:cl-l10n
               :hu.dwim.common-lisp
               :hu.dwim.computed-class
               :hu.dwim.def
               :hu.dwim.def+cl-l10n
               :hu.dwim.defclass-star
               :hu.dwim.delico
               :hu.dwim.perec
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :hu.dwim.walker)
  :components ((:module "source"
                :components ((:file "association" :depends-on ("statistics" "relationship" "generalization" "property"))
                             (:file "authentication" :depends-on ("entity" "subject" "change-notification"))
                             (:file "authorization" :depends-on ("authentication"))
                             (:file "change-notification" :depends-on ("model"))
                             (:file "configuration" :depends-on ("package"))
                             (:file "entity" :depends-on ("statistics" "generalization" "association" "type"))
                             (:file "entity-relationship-diagram" :depends-on ("entity" "structure-diagram"))
                             (:file "finite-state-machine" :depends-on ("entity"))
                             (:file "generalization" :depends-on ("relationship"))
                             (:file "generate" :depends-on ("state-property" "persistent-process"))
                             (:file "logger" :depends-on ("configuration"))
                             (:file "meta-model" :depends-on ("util"))
                             (:file "model" :depends-on ("model-element"))
                             (:file "model-element" :depends-on ("meta-model"))
                             (:file "package")
                             (:file "persistent-process" :depends-on ("entity" "state-property" "authentication" "type" "finite-state-machine"))
                             (:file "process-dsl" :depends-on ("persistent-process"))
                             (:file "process-scheduler" :depends-on ("worker-group" "persistent-process" "subject"))
                             (:file "property" :depends-on ("model"))
                             (:file "relationship" :depends-on ("model"))
                             (:file "state-property" :depends-on ("entity" "finite-state-machine"))
                             (:file "statistics" :depends-on ("model"))
                             (:file "structure-diagram" :depends-on ("relationship"))
                             (:file "subject" :depends-on ("entity"))
                             (:file "type" :depends-on ("configuration"))
                             (:file "util" :depends-on ("configuration"))
                             (:file "worker-group" :depends-on ("configuration"))))))
