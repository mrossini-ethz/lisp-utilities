(require :asdf)
(asdf:load-system :utils)

(define-test test-l* ()
  ;; Define a macro to test many different cases
  (macrolet ((multi (func comp n) `(check ,@(cloop for i below n collect (loop for j below n collect `(xnor (funcall ,func (make-list ,i) ,j) (funcall ,comp ,i ,j)))))))
    ;; Use the macro to test the < > <= >= = and /= operators
    (multi #'l< #'< 5)
    (multi #'l> #'> 5)
    (multi #'l<= #'<= 5)
    (multi #'l>= #'>= 5)
    (multi #'l= #'= 5)
    (multi #'l/= #'/= 5)))

(define-test test-ll* ()
  ;; Define a macro to test many different cases
  (macrolet ((multi (func comp n) `(check ,@(cloop for i below n collect (loop for j below n collect `(xnor (funcall ,func (make-list ,i) (make-list ,j)) (funcall ,comp ,i ,j)))))))
    ;; Use the macro to test the < > <= >= = and /= operators
    (multi #'ll< #'< 5)
    (multi #'ll> #'> 5)
    (multi #'ll<= #'<= 5)
    (multi #'ll>= #'>= 5)
    (multi #'ll= #'= 5)
    (multi #'ll/= #'/= 5)))

(define-test test-lists ()
  (check
    (test-l*)
    (test-ll*)))

;(test-lists)
