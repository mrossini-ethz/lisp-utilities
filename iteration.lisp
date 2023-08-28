(in-package :utils)

(defmacro with-escape (escape-symbol &body body)
  "Defines a block and a symbol macro to escape from the block"
  (with-gensyms (tag)
    `(symbol-macrolet ((,escape-symbol (return-from ,tag)))
       (block ,tag
         ,@body))))
(export 'with-escape)

(defmacro for-range ((var a &optional (b nil b-supplied-p) (step 1)) &body body)
  (unless b-supplied-p
    (setf b a)
    (setf a 0))
  (once-only (a b step)
    `(cond
       ((and (< ,a ,b) (plusp ,step))
        (do ((,var ,a (+ ,var ,step))) ((>= ,var ,b)) ,@body))
       ((and (> ,a ,b) (minusp ,step))
        (do ((,var ,a (+ ,var ,step))) ((<= ,var ,b)) ,@body))
       (t (error "Invalid range")))))

(defmacro foreach ((item) sequence &body body)
  "Iterates over each sequence item and executes the given code."
  `(with-escape break
     (map nil (lambda (,item) (with-escape continue ,@body)) ,sequence)))
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
  `(with-escape break
     (do () (,condition)
       (with-escape continue ,@body))))
(export 'until)

(defmacro while (condition &body body)
  `(until (not ,condition)
     ,@body))
(export 'while)

(defmacro do-until (condition &body body)
  `(with-escape break
     (progn
       (with-escape continue ,@body)
       (do () (,condition)
         (with-escape continue ,@body)))))
(export 'do-until)

(defmacro do-while (condition &body body)
  `(with-escape break
     (progn
       (with-escape continue ,@body)
       (do () ((not ,condition))
         (with-escape continue ,@body)))))
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
