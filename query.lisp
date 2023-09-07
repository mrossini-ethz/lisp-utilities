(in-package :utils)

(defun prompt (str &optional (stream *standard-output*))
  (format stream "~a" str)
  (force-output stream))
(export 'prompt)

(defun prompt-and-read (str &optional (stream *standard-output*))
  (prompt str stream)
  (read-line))
(export 'prompt-and-read)

(defun query-integer (prompt &key min max default)
  (flet ((ask () (format t "~a~:[~;~:*[~d] ~]" prompt default) (force-output)))
    (ask)
    (loop for n upfrom 0 with continue = t with answer while continue do
         (handler-case (let ((line (read-line)))
                         (cond
                           ((and (zerop (length line)) default) (setf answer default continue nil))
                           ((and (zerop (length line)) (not default)) (ask))
                           (t
                            (setf answer (parse-integer line))
                            (if (and (or (not min) (>= answer min)) (or (not max) (<= answer max)))
                                (setf continue nil)
                                (ask)))))
           (sb-int:simple-parse-error () (ask)))
       finally (return answer))))
(export 'query-integer)

(defun query-string (prompt &key trim empty)
  (loop for input = (prompt-and-read prompt) while (or empty (= (length (if trim (string-trim '(#\newline #\return #\space #\tab) input) input)) 0)) finally (return input)))
(export 'query-string)

(defun query-choice (prompt default &rest items)
  (let ((width (1+ (floor (log (length items) 10)))))
    (format t "~a~%" prompt)
    (loop for item in items for i upfrom 1 do (format t " ~vd: ~a~%" width i item))
    (if default
        (query-integer (format nil "Choice: ") :default default :min 1 :max (length items))
        (query-integer (format nil "Choice: ") :min 1 :max (length items)))))
(export 'query-choice)
