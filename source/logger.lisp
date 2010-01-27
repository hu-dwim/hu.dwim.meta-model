;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(def logger meta-model ())

(def logger notification (meta-model))

(def logger process (meta-model))

(def logger scheduler (process) :runtime-level +info+)

(def (logger e) authentication (meta-model))

(def (logger e) authorization (meta-model))

;; a separate branch for audit messages, only delegating to standard-logger.
;; later on, one can install a persistent appender to store audit messages in the database.
(def (logger e) audit ())
(def (logger :export :printers) login (audit))

;; TODO: move?
(def hu.dwim.wui:localization-loader-callback meta-model-localization-loader :hu.dwim.meta-model "localization/")
