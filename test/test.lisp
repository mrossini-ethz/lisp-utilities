(use-package :fiveam)

(fiveam:def-suite utils-test)

(defun utils-test ()
  (fiveam:run! 'utils-test))
