;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(def logger log ())

(def logger loading (log))

(def logger image (log))

(def logger notification (log))

(def logger process (log))

(def logger scheduler (process) :level +info+)

(def logger authentication (log))

(def logger authorization (log))

(def (logger e) audit (log))

(def logger login (audit))

(export '(audit.dribble audit.debug audit.warn audit.info audit.error))

