(in-package :utils)

(defmacro letn (bindings &body body)
  `(let ,bindings
     ,@body
     ,@(when (consp bindings)
         (list (if (consp (car (last bindings)))
                   (car (car (last bindings)))
                   (car (last bindings)))))))
(export 'letn)
