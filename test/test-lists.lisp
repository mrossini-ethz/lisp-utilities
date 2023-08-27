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
  (is (equal (utils:append-1 '(1 2 3) nil) '(1 2 3 nil)))
  (is (equal (utils:append-1 '(1 2 3) 4) '(1 2 3 4)))
  (is (equal (utils:append-1 '(1 2 3) '(4 5)) '(1 2 3 (4 5)))))

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

(test l=
  (is (utils:l= '() 0))
  (is (utils:l= '(1) 1))
  (is (utils:l= '(1 2) 2))
  (is (utils:l= '(1 2 3) 3))
  (is (utils:l= '(1 2 3 4) 4))
  (is-false (utils:l= '() 1))
  (is-false (utils:l= '(1) 2))
  (is-false (utils:l= '(1 2) 3))
  (is-false (utils:l= '(1 2 3) 4))
  (is-false (utils:l= '(1 2 3 4) 5))
  (is-false (utils:l= '() -1))
  (is-false (utils:l= '(1) 0))
  (is-false (utils:l= '(1 2) 1))
  (is-false (utils:l= '(1 2 3) 2))
  (is-false (utils:l= '(1 2 3 4) 3))
  (signals type-error (utils:l= #(1 2 3 4) 4)))

(test l/=
  (is-false (utils:l/= '() 0))
  (is-false (utils:l/= '(1) 1))
  (is-false (utils:l/= '(1 2) 2))
  (is-false (utils:l/= '(1 2 3) 3))
  (is-false (utils:l/= '(1 2 3 4) 4))
  (is (utils:l/= '() 1))
  (is (utils:l/= '(1) 2))
  (is (utils:l/= '(1 2) 3))
  (is (utils:l/= '(1 2 3) 4))
  (is (utils:l/= '(1 2 3 4) 5))
  (is (utils:l/= '() -1))
  (is (utils:l/= '(1) 0))
  (is (utils:l/= '(1 2) 1))
  (is (utils:l/= '(1 2 3) 2))
  (is (utils:l/= '(1 2 3 4) 3))
  (signals type-error (utils:l/= #(1 2 3 4) 3)))

(test l>
  (is-false (utils:l> '() 0))
  (is-false (utils:l> '(1) 1))
  (is-false (utils:l> '(1 2) 2))
  (is-false (utils:l> '(1 2 3) 3))
  (is-false (utils:l> '(1 2 3 4) 4))
  (is-false (utils:l> '() 1))
  (is-false (utils:l> '(1) 2))
  (is-false (utils:l> '(1 2) 3))
  (is-false (utils:l> '(1 2 3) 4))
  (is-false (utils:l> '(1 2 3 4) 5))
  (is (utils:l> '() -1))
  (is (utils:l> '(1) 0))
  (is (utils:l> '(1 2) 1))
  (is (utils:l> '(1 2 3) 2))
  (is (utils:l> '(1 2 3 4) 3))
  (signals type-error (utils:l> #(1 2 3 4) 3)))

(test l<=
  (is (utils:l<= '() 0))
  (is (utils:l<= '(1) 1))
  (is (utils:l<= '(1 2) 2))
  (is (utils:l<= '(1 2 3) 3))
  (is (utils:l<= '(1 2 3 4) 4))
  (is (utils:l<= '() 1))
  (is (utils:l<= '(1) 2))
  (is (utils:l<= '(1 2) 3))
  (is (utils:l<= '(1 2 3) 4))
  (is (utils:l<= '(1 2 3 4) 5))
  (is-false (utils:l<= '() -1))
  (is-false (utils:l<= '(1) 0))
  (is-false (utils:l<= '(1 2) 1))
  (is-false (utils:l<= '(1 2 3) 2))
  (is-false (utils:l<= '(1 2 3 4) 3))
  (signals type-error (utils:l<= #(1 2 3 4) 5)))

(test l<
  (is-false (utils:l< '() 0))
  (is-false (utils:l< '(1) 1))
  (is-false (utils:l< '(1 2) 2))
  (is-false (utils:l< '(1 2 3) 3))
  (is-false (utils:l< '(1 2 3 4) 4))
  (is (utils:l< '() 1))
  (is (utils:l< '(1) 2))
  (is (utils:l< '(1 2) 3))
  (is (utils:l< '(1 2 3) 4))
  (is (utils:l< '(1 2 3 4) 5))
  (is-false (utils:l< '() -1))
  (is-false (utils:l< '(1) 0))
  (is-false (utils:l< '(1 2) 1))
  (is-false (utils:l< '(1 2 3) 2))
  (is-false (utils:l< '(1 2 3 4) 3))
  (signals type-error (utils:l< #(1 2 3 4) 5)))

(test l>=
  (is (utils:l>= '() 0))
  (is (utils:l>= '(1) 1))
  (is (utils:l>= '(1 2) 2))
  (is (utils:l>= '(1 2 3) 3))
  (is (utils:l>= '(1 2 3 4) 4))
  (is-false (utils:l>= '() 1))
  (is-false (utils:l>= '(1) 2))
  (is-false (utils:l>= '(1 2) 3))
  (is-false (utils:l>= '(1 2 3) 4))
  (is-false (utils:l>= '(1 2 3 4) 5))
  (is (utils:l>= '() -1))
  (is (utils:l>= '(1) 0))
  (is (utils:l>= '(1 2) 1))
  (is (utils:l>= '(1 2 3) 2))
  (is (utils:l>= '(1 2 3 4) 3))
  (signals type-error (utils:l>= #(1 2 3 4) 4)))

(test single
  (is (utils:single '(3)))
  (is-false (utils:single '()))
  (is-false (utils:single '(3 4))))

(test ll=
  (is-false (utils:ll= nil '(1)))
  (is-false (utils:ll= '(1) '(2 3)))
  (is-false (utils:ll= '(1 2) '(2 3 4)))
  (is-false (utils:ll= '(1 2 3) '(2 3 4 5)))
  (is-false (utils:ll= '(1 2 3 4) '(2 3 4 5 6)))
  (is-false (utils:ll= '(1 2 3 4 5) '(2 3 4 5 6 7)))
  (is (utils:ll= nil nil))
  (is (utils:ll= '(1) '(2)))
  (is (utils:ll= '(1 2) '(2 3)))
  (is (utils:ll= '(1 2 3) '(2 3 4)))
  (is (utils:ll= '(1 2 3 4) '(2 3 4 5)))
  (is (utils:ll= '(1 2 3 4 5) '(2 3 4 5 6)))
  (is-false (utils:ll= '(1) nil))
  (is-false (utils:ll= '(1 2) '(2)))
  (is-false (utils:ll= '(1 2 3) '(2 3)))
  (is-false (utils:ll= '(1 2 3 4) '(2 3 4)))
  (is-false (utils:ll= '(1 2 3 4 5) '(2 3 4 5)))
  (is-false (utils:ll= '(1 2 3 4 5 6) '(2 3 4 5 6))))

(test l/=
  (is (utils:ll/= nil '(1)))
  (is (utils:ll/= '(1) '(2 3)))
  (is (utils:ll/= '(1 2) '(2 3 4)))
  (is (utils:ll/= '(1 2 3) '(2 3 4 5)))
  (is (utils:ll/= '(1 2 3 4) '(2 3 4 5 6)))
  (is (utils:ll/= '(1 2 3 4 5) '(2 3 4 5 6 7)))
  (is-false (utils:ll/= nil nil))
  (is-false (utils:ll/= '(1) '(2)))
  (is-false (utils:ll/= '(1 2) '(2 3)))
  (is-false (utils:ll/= '(1 2 3) '(2 3 4)))
  (is-false (utils:ll/= '(1 2 3 4) '(2 3 4 5)))
  (is-false (utils:ll/= '(1 2 3 4 5) '(2 3 4 5 6)))
  (is (utils:ll/= '(1) nil))
  (is (utils:ll/= '(1 2) '(2)))
  (is (utils:ll/= '(1 2 3) '(2 3)))
  (is (utils:ll/= '(1 2 3 4) '(2 3 4)))
  (is (utils:ll/= '(1 2 3 4 5) '(2 3 4 5)))
  (is (utils:ll/= '(1 2 3 4 5 6) '(2 3 4 5 6))))

(test ll>
  (is-false (utils:ll> nil '(1)))
  (is-false (utils:ll> '(1) '(2 3)))
  (is-false (utils:ll> '(1 2) '(2 3 4)))
  (is-false (utils:ll> '(1 2 3) '(2 3 4 5)))
  (is-false (utils:ll> '(1 2 3 4) '(2 3 4 5 6)))
  (is-false (utils:ll> '(1 2 3 4 5) '(2 3 4 5 6 7)))
  (is-false (utils:ll> nil nil))
  (is-false (utils:ll> '(1) '(2)))
  (is-false (utils:ll> '(1 2) '(2 3)))
  (is-false (utils:ll> '(1 2 3) '(2 3 4)))
  (is-false (utils:ll> '(1 2 3 4) '(2 3 4 5)))
  (is-false (utils:ll> '(1 2 3 4 5) '(2 3 4 5 6)))
  (is (utils:ll> '(1) nil))
  (is (utils:ll> '(1 2) '(2)))
  (is (utils:ll> '(1 2 3) '(2 3)))
  (is (utils:ll> '(1 2 3 4) '(2 3 4)))
  (is (utils:ll> '(1 2 3 4 5) '(2 3 4 5)))
  (is (utils:ll> '(1 2 3 4 5 6) '(2 3 4 5 6))))

(test ll<=
  (is (utils:ll<= nil '(1)))
  (is (utils:ll<= '(1) '(2 3)))
  (is (utils:ll<= '(1 2) '(2 3 4)))
  (is (utils:ll<= '(1 2 3) '(2 3 4 5)))
  (is (utils:ll<= '(1 2 3 4) '(2 3 4 5 6)))
  (is (utils:ll<= '(1 2 3 4 5) '(2 3 4 5 6 7)))
  (is (utils:ll<= nil nil))
  (is (utils:ll<= '(1) '(2)))
  (is (utils:ll<= '(1 2) '(2 3)))
  (is (utils:ll<= '(1 2 3) '(2 3 4)))
  (is (utils:ll<= '(1 2 3 4) '(2 3 4 5)))
  (is (utils:ll<= '(1 2 3 4 5) '(2 3 4 5 6)))
  (is-false (utils:ll<= '(1) nil))
  (is-false (utils:ll<= '(1 2) '(2)))
  (is-false (utils:ll<= '(1 2 3) '(2 3)))
  (is-false (utils:ll<= '(1 2 3 4) '(2 3 4)))
  (is-false (utils:ll<= '(1 2 3 4 5) '(2 3 4 5)))
  (is-false (utils:ll<= '(1 2 3 4 5 6) '(2 3 4 5 6))))

(test ll<
  (is (utils:ll< nil '(1)))
  (is (utils:ll< '(1) '(2 3)))
  (is (utils:ll< '(1 2) '(2 3 4)))
  (is (utils:ll< '(1 2 3) '(2 3 4 5)))
  (is (utils:ll< '(1 2 3 4) '(2 3 4 5 6)))
  (is (utils:ll< '(1 2 3 4 5) '(2 3 4 5 6 7)))
  (is-false (utils:ll< nil nil))
  (is-false (utils:ll< '(1) '(2)))
  (is-false (utils:ll< '(1 2) '(2 3)))
  (is-false (utils:ll< '(1 2 3) '(2 3 4)))
  (is-false (utils:ll< '(1 2 3 4) '(2 3 4 5)))
  (is-false (utils:ll< '(1 2 3 4 5) '(2 3 4 5 6)))
  (is-false (utils:ll< '(1) nil))
  (is-false (utils:ll< '(1 2) '(2)))
  (is-false (utils:ll< '(1 2 3) '(2 3)))
  (is-false (utils:ll< '(1 2 3 4) '(2 3 4)))
  (is-false (utils:ll< '(1 2 3 4 5) '(2 3 4 5)))
  (is-false (utils:ll< '(1 2 3 4 5 6) '(2 3 4 5 6))))

(test ll>=
  (is-false (utils:ll>= nil '(1)))
  (is-false (utils:ll>= '(1) '(2 3)))
  (is-false (utils:ll>= '(1 2) '(2 3 4)))
  (is-false (utils:ll>= '(1 2 3) '(2 3 4 5)))
  (is-false (utils:ll>= '(1 2 3 4) '(2 3 4 5 6)))
  (is-false (utils:ll>= '(1 2 3 4 5) '(2 3 4 5 6 7)))
  (is (utils:ll>= nil nil))
  (is (utils:ll>= '(1) '(2)))
  (is (utils:ll>= '(1 2) '(2 3)))
  (is (utils:ll>= '(1 2 3) '(2 3 4)))
  (is (utils:ll>= '(1 2 3 4) '(2 3 4 5)))
  (is (utils:ll>= '(1 2 3 4 5) '(2 3 4 5 6)))
  (is (utils:ll>= '(1) nil))
  (is (utils:ll>= '(1 2) '(2)))
  (is (utils:ll>= '(1 2 3) '(2 3)))
  (is (utils:ll>= '(1 2 3 4) '(2 3 4)))
  (is (utils:ll>= '(1 2 3 4 5) '(2 3 4 5)))
  (is (utils:ll>= '(1 2 3 4 5 6) '(2 3 4 5 6))))

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
