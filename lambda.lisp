(in-package :utils)

;; Graham
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                      :from-end t
                      :initial-value (apply fn1 args))))
      #'identity))
(export 'compose)

;; Graham
(defun fint (fn &rest fns)
  "Creates a lambda function of one argument that combiles (and (f1 x) (f2 x) (f3 x) ...)"
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))
(export 'fint)

;; Graham
(defun fun (fn &rest fns)
  "Creates a lambda function of one argument that combiles (or (f1 x) (f2 x) (f3 x) ...)"
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))
(export 'fun)

(defun string=p (str)
  "Creates a lambda function for use as a predicate that needs more than one argument."
  #'(lambda (x) (string= str x)))
(export 'string=p)

(defun unary (function arg)
  #'(lambda (x) (funcall function x arg)))
(export 'unary)

(defun lambda-arg (function &rest args)
  "Creates a lambda function of one argument that calls a function with multiple arguments. Useful examples: (lambda-arg #'> 0)"
  #'(lambda (x) (apply function x args)))
(export 'lambda-arg)
