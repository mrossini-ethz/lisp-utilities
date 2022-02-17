(in-package :utils)

(with-export
  (defun items-unique-p (seq &key (test #'eql) key)
    ;; Checks whether all items in the sequence are unique.
    ;; FIXME: very inefficient
    ;; FIXME: works only for lists
    (loop for n from 1 for item in seq when (find item seq :start n :test test :key key) do (return nil) finally (return t)))
)
