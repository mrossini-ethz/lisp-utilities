(in-package :utils)

(defun /+/ (&rest numbers)
  "Calculates 1/result = 1/a + 1/b + 1/c + ... (e.g. parallel resistors)"
  (/ (loop for n in numbers summing (/ n))))
(export '/+/)

(defun /-/ (&rest numbers)
  "Calculates 1/result = 1/a - 1/b - 1/c + ... (e.g. parallel resistors)"
  (/ (- (/ (first numbers)) (loop for n in (rest numbers) summing (/ n)))))
(export '/-/)

(defun pyt (&rest numbers)
  "Computes sqrt(a^2 + b^2 + c^2 + ...) in a numerially stable way."
  (let ((m (apply #'max (mapcar #'abs numbers))))
    (if (plusp m)
        (* m (sqrt (loop for n in numbers sum (expt (/ n m) 2))))
        0.0)))
(export 'pyt)

(defun ipyt (number &rest more-numbers)
  "Computes sqrt(z^2 - a^2 + b^2 + ...) in a numerially stable way."
  (let ((m (apply #'max (abs number) (mapcar #'abs more-numbers))))
    (if (not (zerop m))
        (* m (sqrt (- (expt (/ number m) 2) (loop for n in more-numbers sum (expt (/ n m) 2)))))
        0.0)))
(export 'ipyt)

(defun square (number)
  (* number number))
(export 'square)

(defun cube (number)
  (* number number number))
(export 'cube)

(defun rad->deg (x)
  (* x (/ 180 pi)))
(export 'rad->deg)

(defun deg->rad (x)
  (* x (/ pi 180)))
(export 'deg->rad)

(defun sind (x)
  (sin (* x (/ pi 180))))
(export 'sind)

(defun cosd (x)
  (cos (* x (/ pi 180))))
(export 'cosd)

(defun tand (x)
  (tan (* x (/ pi 180))))
(export 'tand)

(defun asind (x)
  (* (asin x) (/ 180 pi)))
(export 'asind)

(defun acosd (x)
  (* (acos x) (/ 180 pi)))
(export 'acosd)

(defun atand (y &optional (x 1))
  (* (atan y x) (/ 180 pi)))
(export 'atand)
