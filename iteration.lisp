(in-package :utils)

(with-export
  (defmacro with-escape (escape-symbol &body body)
    "Defines a block and a symbol macro to escape from the block"
    (with-gensyms (tag)
      `(symbol-macrolet ((,escape-symbol (return-from ,tag)))
         (block ,tag
           ,@body))))

  (defmacro foreach ((item) sequence &body body)
    "Iterates over each sequence item and executes the given code."
    `(with-escape break
       (map nil (lambda (,item) (with-escape continue ,@body)) ,sequence)))

  (defmacro enumerate ((index item) sequence &body body)
    "Iterates over the given list but also provides a list index along with the list item."
    (with-gensyms (idx)
      `(let ((,idx -1))
         (foreach (,item) ,sequence
           (incf ,idx)
           (let ((,index ,idx))
             ,@body))
         ,idx)))

  (defmacro enumerate-unpack ((index &rest items) sequence &body body)
    (with-gensyms (item)
      `(enumerate (,index ,item) ,sequence
         (sbind ,items ,item
           ,@body))))

  (defmacro enumerate-zip ((index &rest items) (&rest sequences) &body body)
    (when (/= (length items) (length sequences))
      (error "Error expanding enumerate-zip: number of items does not match number of sequences."))
    (with-gensyms (item)
      `(enumerate (,index ,item) (map 'list #'vector ,@sequences)
         (declare (ignorable ,index))
         (let ,(loop for var in items for i upfrom 0 collect `(,var (elt ,item ,i)))
           ,@body))))

  (defmacro until (condition &body body)
    `(do () (,condition)
       ,@body))

  (defmacro while (condition &body body)
    `(until (not ,condition)
       ,@body))

  (defmacro do-until (condition &body body)
    `(progn
       ,@body
       (until ,condition
         ,@body)))

  (defmacro do-while (condition &body body)
    `(progn
       ,@body
       (while ,condition
         ,@body)))

  (defmacro while-let (variable condition expression &body body)
    `(let ((,variable ,expression))
       (while ,condition
         ,@body
         (setf ,variable ,expression))))

  (defmacro forever (&body body)
    `(do () (nil) ,@body))

  (defmacro cloop (&rest keywords-and-forms)
    `(apply #'append (loop ,@keywords-and-forms)))
)
;;; GRAHAM UTILITIES!!!!!!
