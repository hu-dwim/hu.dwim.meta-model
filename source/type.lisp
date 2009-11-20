;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Predefined types

(def persistent-type alphanumeric-text (&optional maximum-length minimum-length)
  `(text ,maximum-length ,minimum-length "qwertzuiopasdfghjklyxcvbnmQWERTZUIOPASDFGHJKLLYXCVBNM0123456789"))

(def persistent-type standard-text ()
  "A resonable length text."
  '(text 100))

(def function percentage? (value)
  (<= 0.0 value 1.0))

(def persistent-type percentage ()
  "This is a value in the range [0, 1] representing a percentage."
  '(and float-32
        (satisfies percentage?)))

(def persistent-type url (&optional maximum-length)
  "Uniform Resource Locator"
  `(text ,maximum-length))

(def persistent-type standard-url ()
  "A resonable length Uniform Resource Locator"
  '(url 100))

(def persistent-type rgb-code ()
  'integer-32)

(def persistent-type email-address ()
  '(text 100))

(def persistent-type phone-number ()
  '(text 16 0 "1234567890+-/ ()"))

(def persistent-type image-from-static-file (base-path)
  "A type that will be rendered as <img href='base-path/value'/>"
  (declare (ignore base-path))
  '(text 100))

;;;;;;
;;; SI

(def persistent-type length ()
  "In meters"
  'number)

(def persistent-type area ()
  "In square meters"
  'number)

(def persistent-type volume ()
  "In cubic meters"
  'number)

(def persistent-type velocity ()
  "In meter per seconds"
  'number)

;;;;;;
;; Localizations

(def localization en
  (type-name.t "anything")
  (type-name.serialized "serialized")

  (type-name.member "member type")
  
  (type-name.boolean "boolean")
  (type-name.integer-16 "16 bits integer")
  (type-name.integer-32 "32 bits integer")
  (type-name.integer-64 "64 bits integer")
  (type-name.integer "integer")
  (type-name.number "number")

  (type-name.standard-text "standard text")
  (type-name.text "text")

  (type-name.symbol "symbol")

  (type-name.date "date")
  (type-name.time "time")
  (type-name.timestamp "timestamp")
  
  (type-name.email-address "email address")
  (type-name.url "URL")

  (type-name.phone-number "phone number"))

(def localization hu
  (type-name.t "bármi")
  (type-name.serialized "sorosított")

  (type-name.member "felsorolt típus")
  
  (type-name.boolean "logikai érték")
  (type-name.integer-16 "16 bites egész szám")
  (type-name.integer-32 "32 bites egész szám")
  (type-name.integer-64 "64 bites egész szám")
  (type-name.integer "egész szám")
  (type-name.number "szám")

  (type-name.standard-text "rövid szöveg")
  (type-name.text "szöveg")

  (type-name.symbol "szimbólum")

  (type-name.date "dátum")
  (type-name.time "idő")
  (type-name.timestamp "időpont")
  
  (type-name.email-address "email cím")
  (type-name.url "URL")

  (type-name.phone-number "telefonszám"))
