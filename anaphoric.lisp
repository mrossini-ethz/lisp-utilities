(in-package :utils)

;; Graham
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
(export 'aif)

;; Graham
(defmacro awhen (test &body forms)
  `(let ((it ,test))
     (when it ,@forms)))
(export 'awhen)

;; Graham
(defmacro alambda (args &body body)
  `(labels ((self ,args ,@body))
     #'self))
(export 'alambda)
