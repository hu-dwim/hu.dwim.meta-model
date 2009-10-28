;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

(def function english-plural-symbol-for (symbol)
  (bind ((name (string-downcase (symbol-name symbol))))
    (intern (string-upcase (if (>= (length name) 2)
                               (english-plural-of name)
                               (concatenate 'string name "s")))
            (symbol-package symbol))))

(def function hungarian-plural-symbol-for (symbol)
  (bind ((name (string-downcase (symbol-name symbol))))
    (intern (string-upcase (hungarian-plural-of name))
            (symbol-package symbol))))

(defun hash-value-of (key hashtable)
  (gethash (key-for key) hashtable))

(defun (setf hash-value-of) (value key hashtable)
  (setf (gethash (key-for key) hashtable) value))

(defun remove-hash-value-of (key hashtable)
  (remhash (key-for key) hashtable))
