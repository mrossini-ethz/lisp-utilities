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

  (defun logrotate (integer shift bits)
    (cond
      ((plusp shift) (logior (ash (ldb (byte (- bits shift) 0) integer) shift) (ldb (byte shift (- bits shift)) integer)))
      ((minusp shift) (logior (ash (ldb (byte (- shift) 0) integer) (+ bits shift)) (ldb (byte (+ bits shift) (- shift)) integer)))
      (t integer)))
)
