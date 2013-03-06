;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.meta-model
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Various meta model classes."
  :depends-on (:cl-containers
               :command-line-arguments
               :hu.dwim.common
               :hu.dwim.computed-class
               :hu.dwim.def+cl-l10n
               :hu.dwim.defclass-star
               :hu.dwim.delico
               :hu.dwim.graphviz ; TODO this is a heavy dependency with a .so, consider eliminating somehow
               :hu.dwim.logger
               :hu.dwim.perec+iolib
               :hu.dwim.perec.postgresql
               :hu.dwim.syntax-sugar
               :hu.dwim.util.production+swank
               :hu.dwim.util.worker-group
               :hu.dwim.walker
               :hu.dwim.presentation
               :hu.dwim.presentation+cl-graph+cl-typesetting
               :iolib.sockets
               :local-time
               :metacopy-with-contextl
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "association" :depends-on ("statistics" "relationship" "generalization" "property"))
                             (:file "authentication" :depends-on ("entity" "subject" "change-notification"))
                             (:file "change-notification" :depends-on ("model"))
                             (:file "entity" :depends-on ("statistics" "generalization" "association" "type"))
                             (:file "entity-relationship-diagram" :depends-on ("entity" "structure-diagram"))
                             (:file "generalization" :depends-on ("relationship"))
                             (:file "generate" :depends-on ("state-property" "persistent-process"))
                             (:file "logger" :depends-on ("package"))
                             (:file "localization" :depends-on ("package"))
                             (:file "meta-model" :depends-on ("util"))
                             (:file "model" :depends-on ("model-element" "logger"))
                             (:file "model-element" :depends-on ("meta-model"))
                             (:file "package")
                             (:file "persistent-process" :depends-on ("entity" "state-property" "authentication" "type"))
                             (:file "process-dsl" :depends-on ("persistent-process"))
                             (:file "process-scheduler" :depends-on ("persistent-process" "subject"))
                             (:file "property" :depends-on ("model"))
                             (:file "relationship" :depends-on ("model"))
                             (:file "state-property" :depends-on ("entity"))
                             (:file "statistics" :depends-on ("model"))
                             (:file "structure-diagram" :depends-on ("relationship"))
                             (:file "subject" :depends-on ("entity" "property"))
                             (:file "type" :depends-on ("package"))
                             (:file "util" :depends-on ("package"))))))
