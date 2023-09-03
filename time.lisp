(in-package :utils)

(defun timestamp-elements ()
  (multiple-value-bind (second minute hour date month year day daylight zone) (get-decoded-time)
    (declare (ignore day daylight zone))
    (list
     (format nil "~2,'0d" second)
     (format nil "~2,'0d" minute)
     (format nil "~2,'0d" hour)
     (format nil "~2,'0d" date)
     (format nil "~2,'0d" month)
     (format nil "~4,'0d" year))))
;; not exported

(defun timestamp (&optional (format "YYYYMMDDhhmmss"))
  (cond
    ((string= format "YYYYMMDDhhmmss") (apply #'concatenate 'string (elements (timestamp-elements) 5 4 3 2 1 0)))
    (t (error "Unknown format ~a" format))))
(export 'timestamp)
