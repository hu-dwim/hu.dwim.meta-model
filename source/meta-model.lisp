;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.meta-model)

;;;;;;
;;; Defining meta classes

(def (special-variable e) *model*)

(def special-variable *redefine-if-exists* #t)

(def special-variable *ignore-if-does-not-exist* #t)

(def function meta-class-process-slot (slot)
  (bind ((options (if (oddp (length slot)) (cdr slot) (cddr slot)))
         (type (getf options :type)))
    ;; KLUDGE :type commented out due to SBCL complaining when loading from fasl
    (unless (eq 'boolean (getf options :type))
      (setf (getf options :type) t))
    (if (and (listp type) (eq (first type) 'list))
        (setf (getf options :type) 'list)))
  slot)

(def macro define-class (class-name super-class-names slots &rest options)
  `(def class* ,class-name ,super-class-names ,slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass computed-class)))
              '((:export-class-name-p t)
                (:export-accessor-names-p t))
              (remove-if [eq :abstract (first !1)]
                         options))))

(def macro define-slot-class (class-name super-class-names slots &rest options)
  `(define-class ,class-name ,super-class-names ,slots
                 ,@(append (unless (find :metaclass options :key #'first)
                             '((:metaclass computed-class))) options)))

(def macro define-model-class (class-name super-class-names slots &rest options)
  (flet ((subclassp (superclass-name)
           (find (find-class superclass-name nil)
                 (mapcar [find-class !1 nil] super-class-names)
                 :test [and !2 (subtypep !2 !1)])))
    (bind ((package (symbol-package class-name))
           (class-name-as-string (string-downcase (symbol-name class-name)))
           (finder-function-name (format-symbol package "FIND-~A" class-name))
           (finder-p (aif (find :finder options :key 'first) (second it) #t))
           (collector-function-name (format-symbol package "COLLECT-~A" (english-plural-symbol-for class-name)))
           (definer-function-name (format-symbol package "DEFINE-~A" class-name))
           (definer-p (aif (find :definer options :key 'first) (second it) #t))
           (maker-function-name (format-symbol package "MAKE-~A" class-name))
           (maker-p (aif (find :maker options :key 'first) (second it) #t))
           (abstract-p (second (find :abstract options :key 'first)))
           (subtype-of-named-element-p (subclassp 'named-element))
           (subtype-of-owned-model-element-p (subclassp 'owned-model-element))
           (subtype-of-model-element-p (subclassp 'model-element))
           (processed-slots (mapcar #'meta-class-process-slot slots)))
      `(progn
        ;; define meta class
        (eval-always
          (define-class ,class-name ,super-class-names ,processed-slots ,@options))
        (eval-when (:load-toplevel)
          ,(when subtype-of-model-element-p
                 `(progn
                   ;; finder
                   ,(when finder-p
                          `(def function ,finder-function-name (object-or-name &rest args &key &allow-other-keys)
                            (apply #'find-model-element-if-typep object-or-name ',class-name args)))
                   ;; collector
                   (def function ,collector-function-name (&rest args)
                     (apply #'collect-model-elements-if-typep ',class-name args))
                   ,@(when finder-p
                           `((export ',finder-function-name)))
                   (export ',collector-function-name)))
          ,@(unless abstract-p
                    (append
                     ;; maker
                     (when maker-p
                       (if subtype-of-named-element-p
                           `((def function ,maker-function-name (name &rest args)
                               (apply #'make-instance ',class-name :element-name name args)))
                           `((def function ,maker-function-name (&rest args)
                               (apply #'make-instance ',class-name args)))))
                     (when maker-p
                       `((export ',maker-function-name)))
                     ;; definer
                     (when (and definer-p subtype-of-model-element-p)
                       `((def function ,definer-function-name (name &rest args
                                                             &key (redefine-if-exists *redefine-if-exists*) &allow-other-keys)
                           (let ((object (,finder-function-name name :ignore-missing #t :include-derived #f)))
                             (if (and (not redefine-if-exists) object)
                                 (error ,(concatenate 'string class-name-as-string " already exists"))
                                 (if object
                                     (apply 'reinitialize-instance object args)
                                     (add-model-element (apply ',maker-function-name name args) *model*)))))))
                     ;; exports
                     (when (and definer-p (or subtype-of-owned-model-element-p subtype-of-model-element-p))
                       `((export ',definer-function-name))))))
        (find-class ',class-name)))))
