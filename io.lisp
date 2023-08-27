(in-package :utils)

(defun printn (object &optional (stream *standard-output*))
  (prin1 object stream)
  (terpri stream))
(export 'printn)
