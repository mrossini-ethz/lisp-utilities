(in-package :utils)

(defmacro with-lengths ((&rest variables) (&rest sequences) &body body)
  (when (/= (list-length variables) (list-length sequences))
    (error "Error macro-expanding with-lengths: Number of variables does not match number of sequences."))
  `(let ,(loop for v in variables for s in sequences collect `(,v (length ,s)))
     ,@body))
(export 'with-lengths)

;; --- s= family of functions ------------------------------------------------------------------------------

(declaim (inline s=))
(defun s= (sequence length)
  (= (length sequence) length))
(export 's=)

(declaim (inline s/=))
(defun s/= (sequence length)
  (/= (length sequence) length))
(export 's/=)

(declaim (inline s>))
(defun s> (sequence length)
  (> (length sequence) length))
(export 's>)

(declaim (inline s<))
(defun s< (sequence length)
  (< (length sequence) length))
(export 's<)

(declaim (inline s>=))
(defun s>= (sequence length)
  (>= (length sequence) length))
(export 's>=)

(declaim (inline s<=))
(defun s<= (sequence length)
  (<= (length sequence) length))
(export 's<=)

;; --- Even more compact functions to the s= family --------------------------------------------------------

(declaim (inline s=0))
(defun s=0 (sequence)
  (zerop (length sequence)))
(export 's=0)

(declaim (inline s=1))
(defun s=1 (sequence)
  (= (length sequence) 1))
(export 's=1)

(declaim (inline s=2))
(defun s=2 (sequence)
  (= (length sequence) 2))
(export 's=2)

(declaim (inline s=3))
(defun s=3 (sequence)
  (= (length sequence) 3))
(export 's=3)

(declaim (inline s=4))
(defun s=4 (sequence)
  (= (length sequence) 4))
(export 's=4)

(declaim (inline s/=0))
(defun s/=0 (sequence)
  (plusp (length sequence)))
(export 's/=0)

(declaim (inline s/=1))
(defun s/=1 (sequence)
  (/= (length sequence) 1))
(export 's/=1)

(declaim (inline s/=2))
(defun s/=2 (sequence)
  (/= (length sequence) 2))
(export 's/=2)

(declaim (inline s/=3))
(defun s/=3 (sequence)
  (/= (length sequence) 3))
(export 's/=3)

(declaim (inline s/=4))
(defun s/=4 (sequence)
  (/= (length sequence) 4))
(export 's/=4)

(declaim (inline s>0))
(defun s>0 (sequence)
  (plusp (length sequence)))
(export 's>0)

(declaim (inline s>1))
(defun s>1 (sequence)
  (> (length sequence) 1))
(export 's>1)

(declaim (inline s>2))
(defun s>2 (sequence)
  (> (length sequence) 2))
(export 's>2)

(declaim (inline s>3))
(defun s>3 (sequence)
  (> (length sequence) 3))
(export 's>3)

(declaim (inline s>4))
(defun s>4 (sequence)
  (> (length sequence) 4))
(export 's>4)

(declaim (inline s<1))
(defun s<1 (sequence)
  (< (length sequence) 1))
(export 's<1)

(declaim (inline s<2))
(defun s<2 (sequence)
  (< (length sequence) 2))
(export 's<2)

(declaim (inline s<3))
(defun s<3 (sequence)
  (< (length sequence) 3))
(export 's<3)

(declaim (inline s<4))
(defun s<4 (sequence)
  (< (length sequence) 4))
(export 's<4)

(declaim (inline s<5))
(defun s<5 (sequence)
  (< (length sequence) 5))
(export 's<5)

;; --- ss= family of functions -----------------------------------------------------------------------------

(declaim (inline ss=))
(defun ss= (sequence-a sequence-b)
  (= (length sequence-a) (length sequence-b)))
(export 'ss=)

(declaim (inline ss/=))
(defun ss/= (sequence-a sequence-b)
  (/= (length sequence-a) (length sequence-b)))
(export 'ss/=)

(declaim (inline ss>))
(defun ss> (sequence-a sequence-b)
  (> (length sequence-a) (length sequence-b)))
(export 'ss>)

(declaim (inline ss<))
(defun ss< (sequence-a sequence-b)
  (< (length sequence-a) (length sequence-b)))
(export 'ss<)

(declaim (inline ss>=))
(defun ss>= (sequence-a sequence-b)
  (>= (length sequence-a) (length sequence-b)))
(export 'ss>=)

(declaim (inline ss<=))
(defun ss<= (sequence-a sequence-b)
  (<= (length sequence-a) (length sequence-b)))
(export 'ss<=)

(defun slice (sequence a &optional b)
  (with-lengths (n) (sequence)
    (subseq sequence (if a (if (minusp a) (+ n a) a) 0) (if b (if (minusp b) (+ n b) b) n))))
(export 'slice)

(defmacro sbind ((&rest variables) sequence &body body)
  (with-gensyms (seq)
    `(let ((,seq ,sequence))
       (when (/= (length ,seq) ,(length variables))
         (error "Error binding sequence ~s to ~a variables." ',sequence ,(length variables)))
       (let ,(loop for v in variables for i upfrom 0 collect `(,v (elt ,seq ,i)))
         (declare (ignorable ,@variables))
         ,@body))))
(export 'sbind)

(defun items-unique-p (seq &key (test #'eql) key)
  ;; Checks whether all items in the sequence are unique.
  ;; FIXME: very inefficient
  ;; FIXME: works only for lists
  (loop for n from 1 for item in seq when (find item seq :start n :test test :key key) do (return nil) finally (return t)))
(export 'items-unique-p)

(defun same (test sequence &key (key #'identity))
  ;; Checks whether every item in the sequence is the same according to test (using key)
  ;; Assumes the test is commutative and transitive, i.e. if A = B and B = C, then it is assumed that B = A and A = C.
  (or (zerop (length sequence))
      (let ((first (funcall key (elt sequence 0))))
        (every #'(lambda (x) (funcall test (funcall key x) first)) sequence))))
(export 'same)

(defun have (item sequence &key (test #'eql) (key #'identity))
  ;; Checks whether the given item is in the list
  (some #'(lambda (x) (funcall test item (funcall key x))) sequence))
(export 'have)

(defun remove-nth (n sequence)
  (let ((len (length sequence)))
    (when (or (minusp n) (>= n len))
      (error "Unable to remove element ~a in a sequence of length ~a" n len))
    (typecase sequence
      (list (concatenate 'list (subseq sequence 0 n) (subseq sequence (1+ n))))
      (string (concatenate 'string (subseq sequence 0 n) (subseq sequence (1+ n))))
      (vector (concatenate 'vector (subseq sequence 0 n) (subseq sequence (1+ n))))
      (t (error "Unable to remove item in sequence of type ~a" (type-of sequence))))))
(export 'remove-nth)

(defun remove-if-index (test sequence)
  (let ((n (length sequence)) (m 0) result)
    ;; Determine number of items for resulting sequence
    (loop for i below n when (funcall test i) do (incf m))
    ;; Create sequence of same type with the right number of items
    (setf result (subseq sequence 0 (- n m)))
    ;; Copy the elements
    (loop for i below n for x = (funcall test i) with j = 0 when (not x) do
      (setf (elt result j) (elt sequence i))
      (incf j))
    ;; Return it
    result))
(export 'remove-if-index)

(defun elements (sequence &rest indices)
  (loop for i in indices collect (elt sequence i)))
(export 'elements)

(defun range (a &optional b (step 1) (type 'list))
  (unless b
    (setf b a)
    (setf a 0))
  (when (or (zerop step) (and (< a b) (minusp step)) (and (> a b) (plusp step)))
    (error "Invalid range step."))
  (if (plusp step)
      (let ((result (make-sequence type (ceiling (- b a) step))))
        (loop for v upfrom a below b by step for i upfrom 0 do (setf (elt result i) v))
        result)
      (let ((result (make-sequence type (ceiling (- a b) (- step)))))
        (loop for v downfrom a above b by (- step) for i upfrom 0 do (setf (elt result i) v))
        result)))
(export 'range)

(defun zeros (n &key (type 'list))
  (make-sequence type n :initial-element 0))
(export 'zeros)

(defun ones (n &key (type 'list))
  (make-sequence type n :initial-element 1))
(export 'ones)

(defun linspace (a b n &key (type 'list))
  (check-type n (integer 2))
  (let ((step (/ (coerce (- b a) 'double-float) (1- n))))
    (let ((result (make-sequence type n)))
      (loop for i below n do (setf (elt result i) (+ a (* i step))))
      result)))
(export 'linspace)
