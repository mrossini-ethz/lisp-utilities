(in-package :utils)

(defun xor (&rest forms)
  (oddp (count-if #'identity forms)))
(export 'xor)

(defun xnor (&rest forms)
  (evenp (count-if #'identity forms)))
(export 'nor)

(defmacro nand (&rest forms)
  `(not (and ,@forms)))
(export 'nand)

(defmacro nor (&rest forms)
  `(not (or ,@forms)))
(export 'nor)

(defun logrotate (integer shift bits)
  (cond
    ((plusp shift) (logior (ash (ldb (byte (- bits shift) 0) integer) shift) (ldb (byte shift (- bits shift)) integer)))
    ((minusp shift) (logior (ash (ldb (byte (- shift) 0) integer) (+ bits shift)) (ldb (byte (+ bits shift) (- shift)) integer)))
    (t integer)))
(export 'logrotate)
