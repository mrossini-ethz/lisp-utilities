(in-package :utils)

(defun package-symbols (package)
  ;; Returns a list of all exported symbols from the given package
  (let (symbols)
    (do-external-symbols (s (find-package (symbol-name package)))
      (setf symbols (append symbols (list s))))
    (sort symbols #'string< :key #'symbol-name)))
(export 'package-symbols)

(defun print-symbols (package)
  ;; Prints a list of all exported symbols from the given package
  (format t "~{~a~%~}" (package-symbols package)))
(export 'print-symbols)

;; Graham
(defun symb (&rest args)
  (values (intern (apply (lambda (&rest args) (with-output-to-string (s) (dolist (arg args) (princ arg s)))) args))))
(export 'symb)
