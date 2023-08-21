(use-package :fiveam)

(fiveam:def-suite utils)

(defun utils-test ()
  (fiveam:run! 'utils))
