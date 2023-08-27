(defpackage :utils
  (:use :common-lisp)
  (:export :with-export))

(in-package :utils)

(defmacro with-export (&body body)
  ;; Exports symbols created in the `body'. Works for defun and defmacro.
  `(progn ,@(apply #'append (loop for form in body collect
                                 (if (and
                                      (listp form)
                                      (not (null form))
                                      (or (eq (first form) 'defun)
                                          (eq (first form) 'defmacro)))
                                     `(,form (export ',(second form))) `(,form))))))
