(in-package :utils)

(defvar *test-name* nil)
(defvar *test-failures* 0)

(defmacro condition= (form condition)
  "Tests whether the execution of the form results in the given condition (returning T). If no condition or a different condition occurs, NIL is returned."
  `(handler-case (and ,form nil)
     (,condition () t)
     (t () nil)))
(export 'condition=)

(defmacro define-test (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (when (zerop (list-length *test-name*))
       (setf *test-failures* 0))
     (let ((*test-name* (append *test-name* (list ',name))) (test-failures-save *test-failures*))
       (format t "~V<~>Testing ~{~a~^:~} ...~%" (- (list-length *test-name*) 1) *test-name*)
       ,@body
       (when (> *test-failures* test-failures-save)
         (format t "~V<~>Total number of tests failed in ~{~a~^:~}: ~a~%" (- (list-length *test-name*) 1) *test-name* (- *test-failures* test-failures-save))))))
(export 'define-test)

(defun report-result (result form expanded-form)
  "Report the results of a single test case. Called by 'check'."
  (when (not result)
    (incf *test-failures*)
    (format t "~V<~> ~:[Failed~;Passed~]: ~s~@[ => ~*~s~]~%" (- (list-length *test-name*) 1) result form (not (equal form expanded-form)) expanded-form))
  result)
;; not exported

(defmacro combine-results (&body forms)
  "Logical AND operation of the given forms, but without short-circuiting. This ensures that each form is evaluated exactly once."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))
;; not exported

(defmacro check (&body forms)
  "Run each expression in 'forms' once and reports whether succeded (t) or failed (nil)."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f ,@(if (and (listp f) (not (eql 'condition= (first f)))) `((list ',(first f) ,@(rest f))) `(',f))))))
(export 'check)
