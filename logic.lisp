(in-package :utils)

(with-export
  (defun xor (&rest forms)
    (oddp (count-if #'identity forms)))

  (defun xnor (&rest forms)
    (evenp (count-if #'identity forms)))

  (defmacro nand (&rest forms)
    `(not (and ,@forms)))

  (defmacro nor (&rest forms)
    `(not (or ,@forms)))
)
