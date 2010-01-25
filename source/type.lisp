;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Predefined types

(def (persistent-type e) alphanumeric-text (&optional maximum-length minimum-length)
  `(text ,maximum-length ,minimum-length "qwertzuiopasdfghjklyxcvbnmQWERTZUIOPASDFGHJKLLYXCVBNM0123456789"))

(def (persistent-type e) standard-text ()
  "A resonable length text."
  '(text 100))

(def function percentage? (value)
  (<= 0.0 value 1.0))

(def (persistent-type e) percentage ()
  "This is a value in the range [0, 1] representing a percentage."
  '(and float-32
        (satisfies percentage?)))

(def (persistent-type e) url-text (&optional maximum-length)
  "Uniform Resource Locator"
  `(text ,maximum-length))

(def (persistent-type e) standard-url-text ()
  "A resonable length Uniform Resource Locator"
  '(url-text 100))

(def (persistent-type e) rgb-code ()
  'integer-32)

(def (persistent-type e) email-address ()
  '(text 100))

(def (persistent-type e) phone-number ()
  '(text 16 0 "1234567890+-/ ()"))

(def (persistent-type e) image-from-static-file (base-path)
  "A type that will be rendered as <img href='base-path/value'/>"
  (declare (ignore base-path))
  '(text 100))

;;;;;;
;;; SI

(def (persistent-type e) length ()
  "In meters"
  'number)

(def (persistent-type e) area ()
  "In square meters"
  'number)

(def (persistent-type e) volume ()
  "In cubic meters"
  'number)

(def (persistent-type e) velocity ()
  "In meter per seconds"
  'number)
