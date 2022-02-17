(in-package :utils)

(defmacro if-hash ((key hash-table &key var place) then &optional else)
  (with-gensyms (value success)
    (declare (ignorable value))
    `(symbol-macrolet (,@(if var `((,var ,(if place `(gethash ,key ,hash-table) value)))))
       (multiple-value-bind (,value ,success) (gethash ,key ,hash-table)
         (declare (ignorable ,value))
         (if ,success ,then ,else)))))
