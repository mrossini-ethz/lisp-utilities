(in-package :utils)

(with-export
    (defun /+ (&rest numbers)
      "Calculates 1/result = 1/a + 1/b + 1/c + ... (e.g. parallel resistors)"
      (/ (loop for n in numbers summing (/ n))))
  
  (defun /- (&rest numbers)
    "Calculates 1/result = 1/a - 1/b - 1/c + ... (e.g. parallel resistors)"
    (/ (- (/ (first numbers)) (loop for n in (rest numbers) summing (/ n)))))
  )

(defun hypot (&rest kathetes)
  "Computes sqrt(a^2 + b^2 + c^2 + ...) in a numerially stable way."
  (let ((m (apply #'max (mapcar #'abs kathetes))))
    (if (plusp m)
        (* m (sqrt (loop for n in kathetes sum (expt (/ n m) 2))))
        0.0)))
(export 'hypot)

(defun kath (hypotenuse &rest kathetes)
  "Computes sqrt(z^2 - a^2 + b^2 + ...) in a numerially stable way."
  (let ((m (apply #'max (abs hypotenuse) (mapcar #'abs kathetes))))
    (if (not (zerop m))
        (* m (sqrt (- (expt (/ hypotenuse m) 2) (loop for n in kathetes sum (expt (/ n m) 2)))))
        0.0)))
(export 'kath)
