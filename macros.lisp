(in-package :utils)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym (concatenate 'string (symbol-name ',n) "-"))))
     ,@body))
(export 'with-gensyms)

(defmacro macroletexpand-1 (form &environment env)
  "Prints the expansion of a macrolet. Expands at compile time, prints at runtime."
  `(format t "~s~%" ',(macroexpand-1 form env)))
(export 'macroletexpand-1)

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms for n in names collect `(,g (gensym (concatenate 'string ,(symbol-name n) "-")))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))
(export 'once-only)
