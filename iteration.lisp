(in-package :utils)

(with-export
  (defmacro dolist-idx ((item index list &optional result) &body body)
    "Iterates over the given list but also provides a list index along with the list item."
    `(let ((,index 0))
       (dolist (,item ,list ,result)
         ,@body
         (incf ,index))))

  (defmacro until (condition &body body)
    `(do () (,condition)
      ,@body))

  (defmacro while (condition &body body)
    `(until (not ,condition)
      ,@body))

  (defmacro forever (&body body)
    `(do () (nil) ,@body))

  (defmacro cloop (&rest keywords-and-forms)
    `(apply #'append (loop ,@keywords-and-forms)))
)
;;; GRAHAM UTILITIES!!!!!!
