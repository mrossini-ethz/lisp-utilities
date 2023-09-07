(in-package :utils)

(defun timestamp-fmt (&rest elements)
  (let ((elts (multiple-value-list (get-decoded-time))))
    (apply #'concatenate 'string (loop for e in elements collect (format nil "~2,'0d" (nth e elts))))))
;; not exported

(defun timestamp (&optional (format "YYYYMMDDhhmmss"))
  (cond
    ((string= format "YYYYMMDDhhmmss") (timestamp-fmt 5 4 3 2 1 0))
    ((string= format "YYYYMMDD") (timestamp-fmt 5 4 3))
    ((string= format "hhmmss") (timestamp-fmt 2 1 0))
    ((string= format "hhmm") (timestamp-fmt 2 1))
    (t (error "Unknown format ~a" format))))
(export 'timestamp)
