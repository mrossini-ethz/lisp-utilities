(in-package :utils)

(defmacro lambda-case ((arg &body body) &body cases)
  (with-gensyms (fun)
    `(let ((,fun (lambda (,arg) ,@body)))
       (cond ,@(loop for c in cases collect (if (eql (first c) t) `(t ,@(rest c)) `((funcall ,fun ,(first c)) ,@(rest c))))))))

(defmacro string-case (keystring &body cases)
  (once-only (keystring)
    `(cond ,@(loop for c in cases collect (if (eql t (first c)) c `((string= ,keystring ,(first c)) ,@(rest c)))))))

(defmacro string-equal-case (keystring &body cases)
  (once-only (keystring)
    `(cond ,@(loop for c in cases collect (if (eql t (first c)) c `((string-equal ,keystring ,(first c)) ,@(rest c)))))))

(defmacro case-table ((&rest keyforms) &body cases)
  ;; FIXME: Keyforms should be evaluated only once and in order!
  (let ((num (list-length keyforms)))
    `(cond
       ,@(loop for cas in cases collect
              `((and ,@(loop for n below num collect
                            (if (eql (nth n (first cas)) t)
                                t
                                `(eql ,(nth n keyforms) ,(nth n (first cas))))))
                (progn ,@(rest cas)))))))
(export 'case-table)

(defmacro truth-table ((&rest boolean-forms) &rest action-forms)
  "Conditional expression that evaluates all boolean forms. For each of the possible outcomes (2^n) there has to be an action-form that will then be evaluated. Action-forms are sorted like binary numbers according to the boolean-form states: 000 001 010 011 100 101 110 111 (1 = t, 0 = nil)."
  (when (/= (expt 2 (list-length boolean-forms)) (list-length action-forms))
    (error (format nil "Invalid number of action forms given (should be ~a)." (expt 2 (list-length boolean-forms)))))
  (let (symbols)
    ;; Ensure that each boolean form is only evaluated once
    (setf symbols (loop repeat (list-length boolean-forms) collecting (gensym)))
    `(let ,(loop for i from 0 for form in boolean-forms for symb in symbols collecting `(,symb ,form))
       (cond
         ,@(loop for i from 0 below (list-length action-forms) collecting
                 `((and ,@(loop for j from (- (list-length boolean-forms) 1) downto 0 for symb in symbols
                                collecting (if (plusp (logand (expt 2 j) i)) symb `(not ,symb))))
                   ,(nth i action-forms)))))))
(export 'truth-table)

(defmacro case-test (keyform test &body cases)
  "Same as (case ...), but the test can be specified (lambda function of two arguments)."
  (with-gensyms (key)
    `(let ((,key ,keyform))
       (cond
         ,@(loop for case in cases collect
                (destructuring-bind (k &rest b) case
                  (if (eql k t)
                      `(t ,@b)
                      `((,test ,key ,k) ,@b))))))))
(export 'case-test)
