(in-package :utils)

(defmacro letn (bindings &body body)
  `(let ,bindings
     ,@body
     ,@(when (consp bindings)
         (list (if (consp (last-1 bindings))
                   (car (last-1 bindings))
                   (last-1 bindings))))))
(export 'letn)
