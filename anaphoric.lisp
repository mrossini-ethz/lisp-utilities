(in-package :utils)

;; Graham
(defmacro aif (test then &optional else)
  (with-interned-symbols (it)
    `(let ((,it ,test))
       (if ,it ,then ,else))))
(export 'aif)

;; Graham
(defmacro awhen (test &body forms)
  (with-interned-symbols (it)
    `(let ((,it ,test))
       (when ,it ,@forms))))
(export 'awhen)

;; Graham
(defmacro alambda (args &body body)
  (with-interned-symbols (self)
    `(labels ((,self ,args ,@body))
       #',self)))
(export 'alambda)
