(in-suite utils)

(def-suite* lists :in utils)

(test appendf
  (is (let (x) (utils:appendf x) (equal x nil)))
  (is (let (x) (utils:appendf x nil) (equal x '(nil))))
  (is (let (x) (utils:appendf x 1) (equal x '(1))))
  (is (let (x) (utils:appendf x 1 2 3) (equal x '(1 2 3))))
  (is (let ((x '(1 2 3 4))) (utils:appendf x) (equal x '(1 2 3 4))))
  (is (let ((x '(1 2 3 4))) (utils:appendf x 5) (equal x '(1 2 3 4 5))))
  (is (let ((x '(1 2 3 4))) (utils:appendf x 5 6) (equal x '(1 2 3 4 5 6))))
  (is (let ((x '(1 2 3 4))) (utils:appendf x '(5 6)) (equal x '(1 2 3 4 (5 6))))))

(test append-1
  (is (equal (utils:append-1 nil nil) '(nil)))
  (is (equal (utils:append-1 1 nil) '(1)))
  (is (equal (utils:append-1 '(1 2) nil) '((1 2))))
  (is (equal (utils:append-1 nil '(1 2 3)) '(1 2 3 nil)))
  (is (equal (utils:append-1 4 '(1 2 3)) '(1 2 3 4)))
  (is (equal (utils:append-1 '(4 5) '(1 2 3)) '(1 2 3 (4 5)))))

(test prepend-1
  (is (equal (utils:prepend-1 nil nil) '(nil)))
  (is (equal (utils:prepend-1 1 nil) '(1)))
  (is (equal (utils:prepend-1 '(1 2) nil) '((1 2))))
  (is (equal (utils:prepend-1 nil '(1 2 3)) '(nil 1 2 3)))
  (is (equal (utils:prepend-1 0 '(1 2 3)) '(0 1 2 3)))
  (is (equal (utils:prepend-1 '(0 1) '(2 3 4)) '((0 1) 2 3 4))))

(test last-1
  (is (= (utils:last-1 '(1 2 3 4 5)) 5))
  (is (= (utils:last-1 '(1 2 3 4)) 4))
  (is (= (utils:last-1 '(1 2 3)) 3))
  (is (= (utils:last-1 '(1 2)) 2))
  (is (= (utils:last-1 '(1)) 1))
  (is (null (utils:last-1 ()))))

(test mklist
  (is (equal (utils:mklist 3) '(3)))
  (is (equal (utils:mklist '()) '()))
  (is (equal (utils:mklist '(3 4)) '(3 4)))
  (is (equalp (utils:mklist #(3 4)) '(#(3 4)))))

(defmacro l=</>-tester (function test)
  `(locally
     (declare (notinline ,function))
     (loop for i below 10 for lst = (make-list i :initial-element 1) do
       (loop for j below 10 do
         (if (,test i j)
             (is (,function lst j))
             (is-false (,function lst j)))))
     ;; Is a function
     (is (identity (function ,function)))
     ;; Invalid argument type
     (signals error (,function 3 3))
     (signals error (,function #(1 2 3) 3))
     (signals error (,function "123" 3))))

(test l=
  (l=</>-tester utils:l= =))

(test l/=
  (l=</>-tester utils:l/= /=))

(test l>
  (l=</>-tester utils:l> >))

(test l<
  (l=</>-tester utils:l< <))

(test l>=
  (l=</>-tester utils:l>= >=))

(test l<=
  (l=</>-tester utils:l<= <=))

(defmacro l=</>x-tester (function test number)
  `(locally
     (declare (notinline ,function))
     (loop for i below 10 for lst = (make-list i :initial-element 1) do
       (if (,test i ,number)
           (is (,function lst))
           (is-false (,function lst))))
     (signals error (,function ,number))))

(test l=0
  (l=</>x-tester utils:l=0 = 0))

(test l=1
  (l=</>x-tester utils:l=1 = 1))

(test l=2
  (l=</>x-tester utils:l=2 = 2))

(test l=3
  (l=</>x-tester utils:l=3 = 3))

(test l=4
  (l=</>x-tester utils:l=4 = 4))

(test l/=0
  (l=</>x-tester utils:l/=0 /= 0))

(test l/=1
  (l=</>x-tester utils:l/=1 /= 1))

(test l/=2
  (l=</>x-tester utils:l/=2 /= 2))

(test l/=3
  (l=</>x-tester utils:l/=3 /= 3))

(test l/=4
  (l=</>x-tester utils:l/=4 /= 4))

(test l>0
  (l=</>x-tester utils:l>0 > 0))

(test l>1
  (l=</>x-tester utils:l>1 > 1))

(test l>2
  (l=</>x-tester utils:l>2 > 2))

(test l>3
  (l=</>x-tester utils:l>3 > 3))

(test l>4
  (l=</>x-tester utils:l>4 > 4))

(test l<1
  (l=</>x-tester utils:l<1 < 1))

(test l<2
  (l=</>x-tester utils:l<2 < 2))

(test l<3
  (l=</>x-tester utils:l<3 < 3))

(test l<4
  (l=</>x-tester utils:l<4 < 4))

(test l<5
  (l=</>x-tester utils:l<5 < 5))

(defmacro ll=</>-tester (function test)
  `(locally
     (declare (notinline ,function))
     ;; Test various combinations of sequence lengths
     (loop for i below 6 for lst-a = (make-list i :initial-element 1) do
       (loop for j below 6 for lst-b = (make-list j :initial-element 2) do
         (if (,test i j)
             (is (,function lst-a lst-b))
             (is-false (,function lst-a lst-b)))))
     ;; Is a function
     (is (identity (function ,function)))
     ;; Invalid argument type
     (signals error (,function 3 '(1 2 3)))
     (signals error (,function '(1 2 3) 3))
     (signals error (,function 3 0))))

(test ll=
  (ll=</>-tester utils:ll= =))

(test ll/=
  (ll=</>-tester utils:ll/= /=))

(test ll>
  (ll=</>-tester utils:ll> >))

(test ll<
  (ll=</>-tester utils:ll< <))

(test ll>=
  (ll=</>-tester utils:ll>= >=))

(test ll<=
  (ll=</>-tester utils:ll<= <=))

(test list-index-valid-p
  (is-false (utils:list-index-valid-p 0 nil))

  (is-false (utils:list-index-valid-p -1 '(1)))
  (is (utils:list-index-valid-p 0 '(1)))
  (is-false (utils:list-index-valid-p 1 '(1)))

  (is-false (utils:list-index-valid-p -1 '(1 2)))
  (is (utils:list-index-valid-p 0 '(1 2)))
  (is (utils:list-index-valid-p 1 '(1 2)))
  (is-false (utils:list-index-valid-p 2 '(1 2)))

  (is-false (utils:list-index-valid-p -1 '(1 2 3)))
  (is (utils:list-index-valid-p 0 '(1 2 3)))
  (is (utils:list-index-valid-p 1 '(1 2 3)))
  (is (utils:list-index-valid-p 2 '(1 2 3)))
  (is-false (utils:list-index-valid-p 3 '(1 2 3)))

  (is-false (utils:list-index-valid-p -1 '(1 2 3 4)))
  (is (utils:list-index-valid-p 0 '(1 2 3 4)))
  (is (utils:list-index-valid-p 1 '(1 2 3 4)))
  (is (utils:list-index-valid-p 2 '(1 2 3 4)))
  (is (utils:list-index-valid-p 3 '(1 2 3 4)))
  (is-false (utils:list-index-valid-p 4 '(1 2 3 4))))

(test list-flat-p
  (is (utils:list-flat-p nil))
  (is (utils:list-flat-p '(nil)))
  (is (utils:list-flat-p '(nil nil)))
  (is (utils:list-flat-p '(nil nil nil)))
  (is (utils:list-flat-p '(1)))
  (is (utils:list-flat-p '(1 2)))
  (is (utils:list-flat-p '(1 2 3)))
  (is (utils:list-flat-p '(1 2 3 4)))
  (is (utils:list-flat-p '(1 2 3 4 5)))
  (is-false (utils:list-flat-p '((nil))))
  (is-false (utils:list-flat-p '((1))))
  (is-false (utils:list-flat-p '((1) 2 3 4 5)))
  (is-false (utils:list-flat-p '(1 2 3 4 (5)))))

(test group
  (signals type-error (utils:group '(1 2 3) -1))
  (signals simple-error (utils:group '(1 2 3) 0))
  (is (equalp (utils:group '(1 2 3 4 5 6 7 8 9 10 11 12) 1) '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7 8 9 10 11 12) 12) '((1 2 3 4 5 6 7 8 9 10 11 12))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7) 1) '((1) (2) (3) (4) (5) (6) (7))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7) 2) '((1 2) (3 4) (5 6) (7))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7) 3) '((1 2 3) (4 5 6) (7))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7) 4) '((1 2 3 4) (5 6 7))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7) 5) '((1 2 3 4 5) (6 7))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7) 6) '((1 2 3 4 5 6) (7))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7) 7) '((1 2 3 4 5 6 7))))
  (is (equalp (utils:group '(1 2 3 4 5 6 7) 8) '((1 2 3 4 5 6 7)))))

(test flatten
  (is (equal (utils:flatten nil) nil))
  (is (equal (utils:flatten '(1)) '(1)))
  (is (equal (utils:flatten '(1 2)) '(1 2)))
  (is (equal (utils:flatten '(1 2 3 4 5)) '(1 2 3 4 5)))
  (is (equal (utils:flatten '(1 2 nil 4 5)) '(1 2 4 5)))
  (is (equal (utils:flatten '(1 2 (3 4) 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '((1 2) 3 4 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 (3 4) 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 3 4 (5 6))) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '((1 2 3 4) 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 (3 4 5 6))) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(((1) 2) 3 4 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '((1 (2)) 3 4 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(((1) (2)) 3 4 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(((1 2)) 3 4 (5) 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 ((3) 4) 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 (3 (4)) 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 ((3) (4)) 5 6)) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 ((3 4)) 5 (6))) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 3 4 ((5) 6))) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 3 4 (5 (6)))) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 2 3 4 ((5) (6)))) '(1 2 3 4 5 6)))
  (is (equal (utils:flatten '(1 (2) 3 4 ((5 6)))) '(1 2 3 4 5 6))))

(test prune
  (is (equalp (utils:prune #'evenp nil) nil))
  (is (equalp (utils:prune #'evenp '(1)) '(1)))
  (is (equalp (utils:prune #'oddp '(1)) nil))
  (is (equalp (utils:prune #'evenp '(1 2 3 4 5 6 7 8)) '(1 3 5 7)))
  (is (equalp (utils:prune #'oddp '(1 2 3 4 5 6 7 8)) '(2 4 6 8)))
  (is (equalp (utils:prune #'evenp '(1 (2 3) 4 5 6 7 8)) '(1 (3) 5 7)))
  (is (equalp (utils:prune #'oddp '(1 2 3 4 (5 6 7) 8)) '(2 4 (6) 8)))
  (is (equalp (utils:prune #'evenp '(1 (2 3 (4 5) (6 7)) 8)) '(1 (3 (5) (7))))))

(test list-comp
  (is (equal (utils:list-comp x '(1 2 3 4) (1+ x)) '(2 3 4 5)))
  (is (equal (utils:list-comp x '(1 2 3 4) (* x x)) '(1 4 9 16)))
  (is (equal (utils:list-comp x '(1 2 3 4) (* x x) (1- x)) '(0 1 2 3))))
