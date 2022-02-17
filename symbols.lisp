(in-package :utils)

(with-export
  (defun package-symbols (package)
    ;; Returns a list of all exported symbols from the given package
    (let (symbols)
      (do-external-symbols (s (find-package (symbol-name package)))
        (setf symbols (append symbols (list s))))
        (sort symbols #'string< :key #'symbol-name)))

  (defun print-symbols (package)
    ;; Prints a list of all exported symbols from the given package
    (format t "~{~a~%~}" (package-symbols package)))

  ;; Graham
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))  
)
