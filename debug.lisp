(in-package :utils)

(defmacro dp (&rest objects)
  `(funcall ,(formatter "~{~s = ~s~^, ~}~%") *standard-output* (list ,@(apply #'append (loop for obj in objects collect `(',obj ,obj))))))
(export 'dp)

(defmacro dpl (list)
  `(apply ,(formatter "~s:~%~{~s~%~}") *standard-output* (list ',list ,list)))
(export 'dpl)

(defmacro with-print-retval (form)
  (with-gensyms (result)
    `(let ((,result ,form))
       (funcall ,(formatter "~a => ~a~%") *standard-output* ',form ,result)
       ,result)))

(defmacro progn-dp (&body body)
  `(progn
     ,@(loop for form in body collect `(with-print-retval ,form))))
(export 'progn-dp)
