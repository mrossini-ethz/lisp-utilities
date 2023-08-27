(in-package :utils)

(defun initialize-random-seed (&optional (state t))
  (setf *random-state* (make-random-state state)))
(export 'initialize-random-seed)

(defun random-integer (min max)
  (+ (random (1+ (- max min))) min))
(export 'random-integer)

(defun random-select (seq)
  ;; Randomly selects an item from the given sequence
  (elt seq (random (length seq))))
(export 'random-select)

(defun random-select-multiple (seq num)
  ;; Randomly selects `num' items fom the given sequence. Duplicate selection is possible.
  (loop for i below num collect (random-select seq)))
(export 'random-select-multiple)

(defun shuffle (list)
  "Creates a new list in which the items of the given list are shuffled"
  ;; Algorithm by Donald Knuth
  (let ((n (list-length list)) (result (copy-list list)))
    (loop for i below (- n 1) do
      (rotatef (nth i result) (nth (+ i (random (- n i))) result))
          finally (return result))))
(export 'shuffle)

(defun random-unique-indices (num limit &key random-order)
  "Randomly selects `num' distinct indices from the range [0,limit)"
  ;; This is selection without replacement
  ;; The result is a sorted list of indices"
  ;; Algorithm by Robert W. Floyd
  (assert (<= num limit))
  (let (set)
    (loop for j from (- limit num) below limit do
      (let ((r (random (1+ j))))
        (if (find r set)
            ;; Is in the set
            (setf set (append set (list j)))
            ;; Is not in the set
            (setf set (append set (list r))))))
    (if random-order
        (shuffle set)
        (sort set #'<))))
(export 'random-unique-indices)

(defun random-subset (list num &key random-order)
  "Returns a random subset of the given list with `num' items."
  (loop for i in (random-unique-indices num (list-length list) :random-order random-order) collect (nth i list)))
(export 'random-subset)

(defun nonzero-random (arg)
  (let ((result (random arg)))
    (do () ((plusp result) result)
      (setf result (random arg)))))
(export 'nonzero-random)

(defparameter *random-gauss-save* nil)
;; not exported

(defun random-gauss (mean stdev)
  (if *random-gauss-save*
      (prog1
          *random-gauss-save*
        (setf *random-gauss-save* nil))
      (let ((u1 (nonzero-random 1d0)) (u2 (nonzero-random 1d0)))
        (setf *random-gauss-save* (* (sqrt (* -2 (log u1))) (sin (* 2 pi u2))))
        (+ mean (* (* (sqrt (* -2 (log u1))) (cos (* 2 pi u2))) stdev)))))
(export 'random-gauss)

(defun random-list (n random-function &rest function-args)
  (loop for i below n collect (apply random-function function-args)))
(export 'random-list)

(defun random-vector (n random-function &rest function-args)
  (let ((result (make-array (list n) :element-type 'double-float)))
    (loop for i below n do (setf (elt result i) (apply random-function function-args)))
    result))
(export 'random-vector)

(defun random-hash (&optional (length 8))
  (loop for i below length collect (random 256)))
(export 'random-hash)

(defun random-hash-str (&optional (bytes 8))
  (string-downcase (format nil "~{~2,'0x~}" (random-hash bytes))))
(export 'random-hash-str)
