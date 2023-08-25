(in-package :utils)

(with-export
  (defmacro with-lengths ((&rest variables) (&rest sequences) &body body)
    (when (/= (list-length variables) (list-length sequences))
      (error "Error macro-expanding with-lengths: Number of variables does not match number of sequences."))
    `(let ,(loop for v in variables for s in sequences collect `(,v (length ,s)))
       ,@body))

  (defun slice (sequence a &optional b)
    (with-lengths (n) (sequence)
      (subseq sequence (if a (if (minusp a) (+ n a) a) 0) (if b (if (minusp b) (+ n b) b) n))))

  (defmacro sbind ((&rest variables) sequence &body body)
    (with-gensyms (seq)
      `(let ((,seq ,sequence))
         (when (/= (length ,seq) ,(length variables))
           (error "Error binding sequence ~s to ~a variables." ',sequence ,(length variables)))
         (let ,(loop for v in variables for i upfrom 0 collect `(,v (elt ,seq ,i)))
           (declare (ignorable ,@variables))
           ,@body))))

  (defun items-unique-p (seq &key (test #'eql) key)
    ;; Checks whether all items in the sequence are unique.
    ;; FIXME: very inefficient
    ;; FIXME: works only for lists
    (loop for n from 1 for item in seq when (find item seq :start n :test test :key key) do (return nil) finally (return t)))

  (defun same (test sequence &key (key #'identity))
    ;; Checks whether every item in the sequence is the same according to test (using key)
    (let ((first (funcall key (elt sequence 0))))
      (every #'(lambda (x) (funcall test (funcall key x) first)) sequence)))

  (defun have (item sequence &key (test #'eql) (key #'identity))
    ;; Checks whether the given item is in the list
    (some #'(lambda (x) (funcall test item (funcall key x))) sequence))

  (defun remove-nth (n sequence)
    (let ((len (length sequence)))
      (when (or (minusp n) (>= n len))
        (error "Unable to remove element ~a in a list of length ~a" n len))
      (typecase sequence
        (list (append (subseq sequence 0 n) (if (l> sequence (1+ n)) (subseq sequence (1+ n)))))
        (string (concatenate 'string (subseq sequence 0 n) (subseq sequence (1+ n))))
        (vector (concatenate 'vector (subseq sequence 0 n) (subseq sequence (1+ n))))
        (t (error "Unable to remove item in sequence of type ~a" (type-of sequence))))))
)
