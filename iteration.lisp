(in-package :utils)

(defmacro with-escape (escape-symbol return-value &body body)
  "Defines a block and a symbol macro to escape from the block"
  (with-gensyms (tag)
    `(symbol-macrolet ((,escape-symbol (return-from ,tag ,return-value)))
       (block ,tag
         ,@body))))
(export 'with-escape)

(defmacro for-range ((var a &optional (b nil b-supplied-p) (step 1)) &body body)
  (unless b-supplied-p
    (setf b a a 0))
  (with-interned-symbols (continue break)
    (with-gensyms (itervar)
      (once-only (a b step)
        `(with-escape ,break ,itervar
           (do ((,itervar ,a (+ ,itervar ,step))) ((or (zerop ,step) (and (plusp ,step) (>= ,itervar ,b)) (and (minusp ,step) (<= ,itervar ,b))) ,itervar)
             (with-escape ,continue ,itervar (let ((,var ,itervar)) (declare (ignorable ,var)) ,@body))))))))
(export 'for-range)

(defun list-iterator (list &optional (start 0) (stop #.most-positive-fixnum) (step 1))
  (check-type start (integer 0))
  (check-type stop (integer 0))
  (check-type step (integer 1))
  ;; Move to start position if necessary
  (when (plusp start)
    (setf list (nthcdr start list))
    (setf stop (- stop start)))
  ;; Differentiate stepping cases for more efficiency
  (cond
    ((= step 1)
     (let ((pos 0))
       (declare (fixnum pos stop))
       (lambda ()
         (declare (optimize (speed 3) (safety 0)))
         (multiple-value-prog1 (if (and list (< pos stop)) (values (car list) t))
           (setf list (cdr list))
           (incf pos)))))
    ((and (> step 1))
     (let ((pos 0))
       (declare (fixnum pos stop step))
       (lambda ()
         (multiple-value-prog1 (if (and list (< pos stop)) (values (car list) t))
           (setf list (nthcdr step list) pos (+ pos step))))))))
(export 'list-iterator)

(defun vector-iterator (vector &optional (start 0) (stop #.most-positive-fixnum) (step 1))
  (check-type start (integer 0))
  (check-type stop (integer 0))
  (when (minusp (* step (- stop start)))
    (error "Invalid vector iterator argument combination."))
  (setf stop (min stop (length vector)))
  (let ((pos start))
    (lambda ()
      (multiple-value-prog1 (if (< pos stop) (values (elt vector pos) t))
        (incf pos step)))))
(export 'vector-iterator)

(defun sequence-iterator (sequence &optional (start 0) (stop #.most-positive-fixnum) (step 1))
  (if (listp sequence)
      (list-iterator sequence start stop step)
      (vector-iterator sequence start stop step)))
(export 'sequence-iterator)

(defmacro loop-iterator ((var iterator) &body body)
  (once-only (iterator)
    (with-gensyms (fun proceed last)
      `(let (,last)
         (labels ((,fun ()
                    (multiple-value-bind (,var ,proceed)  (funcall ,iterator)
                      (declare (ignorable ,var))
                      (if ,proceed (progn ,@body (setf ,last ,var) (,fun)) ,last))))
           (,fun))))))
(export 'loop-iterator)

(defmacro foreach ((item sequence) &body body)
  "Iterates over each sequence item and executes the given code."
  (with-interned-symbols (continue break)
    (with-gensyms (iterator)
      `(let ((,iterator (sequence-iterator ,sequence)))
         (with-escape ,break ,item
           (loop-iterator (,item ,iterator) (with-escape ,continue ,item ,@body)))))))
(export 'foreach)

(defmacro enumerate ((index item) sequence &body body)
  "Iterates over the given list but also provides a list index along with the list item."
  (with-gensyms (idx)
    `(let ((,idx -1))
       (foreach (,item) ,sequence
         (incf ,idx)
         (let ((,index ,idx))
           ,@body))
       ,idx)))
(export 'enumerate)

(defmacro enumerate-unpack ((index &rest items) sequence &body body)
  (with-gensyms (item)
    `(enumerate (,index ,item) ,sequence
       (sbind ,items ,item
         ,@body))))
(export 'enumerate-unpack)

(defmacro enumerate-zip ((index &rest items) (&rest sequences) &body body)
  (when (/= (length items) (length sequences))
    (error "Error expanding enumerate-zip: number of items does not match number of sequences."))
  (with-gensyms (item)
    `(enumerate (,index ,item) (map 'list #'vector ,@sequences)
       (declare (ignorable ,index))
       (let ,(loop for var in items for i upfrom 0 collect `(,var (elt ,item ,i)))
         ,@body))))
(export 'enumerate-zip)

(defmacro until (condition &body body)
  `(with-escape break nil
     (do () (,condition)
       (with-escape continue nil ,@body))))
(export 'until)

(defmacro while (condition &body body)
  `(until (not ,condition)
     ,@body))
(export 'while)

(defmacro do-until (condition &body body)
  `(with-escape break nil
     (progn
       (with-escape continue nil ,@body)
       (do () (,condition)
         (with-escape continue nil ,@body)))))
(export 'do-until)

(defmacro do-while (condition &body body)
  `(with-escape break nil
     (progn
       (with-escape continue nil ,@body)
       (do () ((not ,condition))
         (with-escape continue nil ,@body)))))
(export 'do-while)

(defmacro while-let (variable condition expression &body body)
  `(let ((,variable ,expression))
     (while ,condition
       ,@body
       (setf ,variable ,expression))))
(export 'while-let)

(defmacro forever (&body body)
  `(until nil ,@body))
(export 'forever)

(defmacro cloop (&rest keywords-and-forms)
  `(apply #'append (loop ,@keywords-and-forms)))
(export 'cloop)

;;; GRAHAM UTILITIES!!!!!!
