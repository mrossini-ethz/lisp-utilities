(in-package :utils)

(with-export
  (defun printn (object &optional (stream *standard-output*))
    (prin1 object stream)
    (terpri stream))
)
