(in-package :utils)

(defun average (&rest values)
  ;; Calculates the average of the given values
  (/ (loop for v in values summing v) (list-length values)))
(export 'average)

(defun s0s1s2 (&rest values)
  ;; Iterates over the given values counting them, summing them and summing the squares
  (loop for v in values summing 1 into s0 summing v into s1 summing (* v v) into s2 finally (return (values s0 s1 s2))))
;; not exported

(defun stdev-running (s0 s1 s2)
  ;; Calculates the standard deviation from the three sums: value^i with i = 0, 1, 2
  (sqrt (/ (- (* s0 s2) (* s1 s1)) (* s0 (- s0 1)))))
;; not exported

(defun num-avg-stdev (&rest values)
  ;; Calculates the number of items, the average and standard deviation of the given values in a single iteration
  (multiple-value-bind (s0 s1 s2) (apply #'s0s1s2 values)
    (values s0 (/ s1 s0) (stdev-running s0 s1 s2))))
(export 'num-avg-stdev)

(defun stdev (&rest values)
  (apply #'stdev-running (multiple-value-list (apply #'s0s1s2 values))))
(export 'stdev)

(defun median (&rest numbers)
  (let ((n (list-length numbers)) (s (sort numbers #'<)))
    (if (oddp n)
        (nth (truncate n 2) s)
        (/ (+ (nth (truncate n 2) s) (nth (- (truncate n 2) 1) s)) 2))))
(export 'median)
