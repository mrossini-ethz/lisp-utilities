(in-suite utils)

(def-suite* sequence :in utils)

(test with-lengths
  ;; Zero operands
  (is (= (utils:with-lengths () () 3) 3))
  ;; One operand
  (is (= (utils:with-lengths (n) ('()) n) 0))
  (is (= (utils:with-lengths (n) ('(1)) n) 1))
  (is (= (utils:with-lengths (n) ('(1 2)) n) 2))
  (is (= (utils:with-lengths (n) ('(1 2 3)) n) 3))
  ;; Two operands
  (is (= (utils:with-lengths (n m) ('() '()) (declare (ignorable m)) n) 0))
  (is (= (utils:with-lengths (n m) ('() '(1)) (declare (ignorable m)) n) 0))
  (is (= (utils:with-lengths (n m) ('() '(1 2)) (declare (ignorable m)) n) 0))
  (is (= (utils:with-lengths (n m) ('() '(1 2 3)) (declare (ignorable m)) n) 0))
  (is (= (utils:with-lengths (n m) ('(1) '()) (declare (ignorable m)) n) 1))
  (is (= (utils:with-lengths (n m) ('(1) '(1)) (declare (ignorable m)) n) 1))
  (is (= (utils:with-lengths (n m) ('(1) '(1 2)) (declare (ignorable m)) n) 1))
  (is (= (utils:with-lengths (n m) ('(1) '(1 2 3)) (declare (ignorable m)) n) 1))
  (is (= (utils:with-lengths (n m) ('(1 2) '()) (declare (ignorable m)) n) 2))
  (is (= (utils:with-lengths (n m) ('(1 2) '(1)) (declare (ignorable m)) n) 2))
  (is (= (utils:with-lengths (n m) ('(1 2) '(1 2)) (declare (ignorable m)) n) 2))
  (is (= (utils:with-lengths (n m) ('(1 2) '(1 2 3)) (declare (ignorable m)) n) 2))
  (is (= (utils:with-lengths (n m) ('(1 2 3) '()) (declare (ignorable m)) n) 3))
  (is (= (utils:with-lengths (n m) ('(1 2 3) '(1)) (declare (ignorable m)) n) 3))
  (is (= (utils:with-lengths (n m) ('(1 2 3) '(1 2)) (declare (ignorable m)) n) 3))
  (is (= (utils:with-lengths (n m) ('(1 2 3) '(1 2 3)) (declare (ignorable m)) n) 3))
  (is (= (utils:with-lengths (n m) ('() '()) (declare (ignorable n)) m) 0))
  (is (= (utils:with-lengths (n m) ('(1) '()) (declare (ignorable n)) m) 0))
  (is (= (utils:with-lengths (n m) ('(1 2) '()) (declare (ignorable n)) m) 0))
  (is (= (utils:with-lengths (n m) ('(1 2 3) '()) (declare (ignorable n)) m) 0))
  (is (= (utils:with-lengths (n m) ('() '(1)) (declare (ignorable n)) m) 1))
  (is (= (utils:with-lengths (n m) ('(1) '(1)) (declare (ignorable n)) m) 1))
  (is (= (utils:with-lengths (n m) ('(1 2) '(1)) (declare (ignorable n)) m) 1))
  (is (= (utils:with-lengths (n m) ('(1 2 3) '(1)) (declare (ignorable n)) m) 1))
  (is (= (utils:with-lengths (n m) ('() '(1 2)) (declare (ignorable n)) m) 2))
  (is (= (utils:with-lengths (n m) ('(1) '(1 2)) (declare (ignorable n)) m) 2))
  (is (= (utils:with-lengths (n m) ('(1 2) '(1 2)) (declare (ignorable n)) m) 2))
  (is (= (utils:with-lengths (n m) ('(1 2 3) '(1 2)) (declare (ignorable n)) m) 2))
  (is (= (utils:with-lengths (n m) ('() '(1 2 3)) (declare (ignorable n)) m) 3))
  (is (= (utils:with-lengths (n m) ('(1) '(1 2 3)) (declare (ignorable n)) m) 3))
  (is (= (utils:with-lengths (n m) ('(1 2) '(1 2 3)) (declare (ignorable n)) m) 3))
  (is (= (utils:with-lengths (n m) ('(1 2 3) '(1 2 3)) (declare (ignorable n)) m) 3))
  ;; Invalid number of operands
  (signals error (eval '(utils:with-lengths () (nil) t)))
  (signals error (eval '(utils:with-lengths () ('(1 2 3)) t)))
  (signals error (eval '(utils:with-lengths (n) () t)))
  (signals error (eval '(utils:with-lengths (n m) ('(1 2 3)) t))))

(test s=
  (declare (notinline utils:s=))
  ;; Zero length
  (is-false (utils:s= '() -1))
  (is-false (utils:s= #() -1))
  (is-false (utils:s= "" -1))
  (is (utils:s= '() 0))
  (is (utils:s= #() 0))
  (is (utils:s= "" 0))
  (is-false (utils:s= '() 1))
  (is-false (utils:s= #() 1))
  (is-false (utils:s= "" 1))
  ;; Length 1
  (is-false (utils:s= '(1) 0))
  (is-false (utils:s= #(1) 0))
  (is-false (utils:s= "1" 0))
  (is (utils:s= '(1) 1))
  (is (utils:s= #(1) 1))
  (is (utils:s= "1" 1))
  (is-false (utils:s= '(1) 2))
  (is-false (utils:s= #(1) 2))
  (is-false (utils:s= "1" 2))
  ;; Length 2
  (is-false (utils:s= '(1 2) 1))
  (is-false (utils:s= #(1 2) 1))
  (is-false (utils:s= "12" 1))
  (is (utils:s= '(1 2) 2))
  (is (utils:s= #(1 2) 2))
  (is (utils:s= "12" 2))
  (is-false (utils:s= '(1 2) 3))
  (is-false (utils:s= #(1 2) 3))
  (is-false (utils:s= "12" 3))
  ;; Length 3
  (is-false (utils:s= '(1 2 3) 2))
  (is-false (utils:s= #(1 2 3) 2))
  (is-false (utils:s= "123" 2))
  (is (utils:s= '(1 2 3) 3))
  (is (utils:s= #(1 2 3) 3))
  (is (utils:s= "123" 3))
  (is-false (utils:s= '(1 2 3) 4))
  (is-false (utils:s= #(1 2 3) 4))
  (is-false (utils:s= "123" 4))
  ;; Length 4
  (is-false (utils:s= '(1 2 3 4) 3))
  (is-false (utils:s= #(1 2 3 4) 3))
  (is-false (utils:s= "1234" 3))
  (is (utils:s= '(1 2 3 4) 4))
  (is (utils:s= #(1 2 3 4) 4))
  (is (utils:s= "1234" 4))
  (is-false (utils:s= '(1 2 3 4) 5))
  (is-false (utils:s= #(1 2 3 4) 5))
  (is-false (utils:s= "1234" 5))
  ;; Is a function
  (is (identity (function utils:s=)))
  ;; Invalid argument
  (signals error (utils:s= 3 0)))

(test s>
  (declare (notinline utils:s>))
  ;; Zero length
  (is (utils:s> '() -1))
  (is (utils:s> #() -1))
  (is (utils:s> "" -1))
  (is-false (utils:s> '() 0))
  (is-false (utils:s> #() 0))
  (is-false (utils:s> "" 0))
  (is-false (utils:s> '() 1))
  (is-false (utils:s> #() 1))
  (is-false (utils:s> "" 1))
  ;; Length 1
  (is (utils:s> '(1) 0))
  (is (utils:s> #(1) 0))
  (is (utils:s> "1" 0))
  (is-false (utils:s> '(1) 1))
  (is-false (utils:s> #(1) 1))
  (is-false (utils:s> "1" 1))
  (is-false (utils:s> '(1) 2))
  (is-false (utils:s> #(1) 2))
  (is-false (utils:s> "1" 2))
  ;; Length 2
  (is (utils:s> '(1 2) 1))
  (is (utils:s> #(1 2) 1))
  (is (utils:s> "12" 1))
  (is-false (utils:s> '(1 2) 2))
  (is-false (utils:s> #(1 2) 2))
  (is-false (utils:s> "12" 2))
  (is-false (utils:s> '(1 2) 3))
  (is-false (utils:s> #(1 2) 3))
  (is-false (utils:s> "12" 3))
  ;; Length 3
  (is (utils:s> '(1 2 3) 1))
  (is (utils:s> #(1 2 3) 1))
  (is (utils:s> "123" 1))
  (is-false (utils:s> '(1 2 3) 3))
  (is-false (utils:s> #(1 2 3) 3))
  (is-false (utils:s> "123" 3))
  (is-false (utils:s> '(1 2 3) 4))
  (is-false (utils:s> #(1 2 3) 4))
  (is-false (utils:s> "123" 4))
  ;; Length 4
  (is (utils:s> '(1 2 3 4) 3))
  (is (utils:s> #(1 2 3 4) 3))
  (is (utils:s> "1234" 3))
  (is-false (utils:s> '(1 2 3 4) 4))
  (is-false (utils:s> #(1 2 3 4) 4))
  (is-false (utils:s> "1234" 4))
  (is-false (utils:s> '(1 2 3 4) 5))
  (is-false (utils:s> #(1 2 3 4) 5))
  (is-false (utils:s> "1234" 5))
  ;; Is a function
  (is (identity (function utils:s>)))
  ;; Invalid argument
  (signals error (utils:s> 3 0)))

(test s<
  (declare (notinline utils:s<))
  ;; Zero length
  (is-false (utils:s< '() -1))
  (is-false (utils:s< #() -1))
  (is-false (utils:s< "" -1))
  (is-false (utils:s< '() 0))
  (is-false (utils:s< #() 0))
  (is-false (utils:s< "" 0))
  (is (utils:s< '() 1))
  (is (utils:s< #() 1))
  (is (utils:s< "" 1))
  ;; Length 1
  (is-false (utils:s< '(1) 0))
  (is-false (utils:s< #(1) 0))
  (is-false (utils:s< "1" 0))
  (is-false (utils:s< '(1) 1))
  (is-false (utils:s< #(1) 1))
  (is-false (utils:s< "1" 1))
  (is (utils:s< '(1) 2))
  (is (utils:s< #(1) 2))
  (is (utils:s< "1" 2))
  ;; Length 2
  (is-false (utils:s< '(1 2) 1))
  (is-false (utils:s< #(1 2) 1))
  (is-false (utils:s< "12" 1))
  (is-false (utils:s< '(1 2) 2))
  (is-false (utils:s< #(1 2) 2))
  (is-false (utils:s< "12" 2))
  (is (utils:s< '(1 2) 3))
  (is (utils:s< #(1 2) 3))
  (is (utils:s< "12" 3))
  ;; Length 3
  (is-false (utils:s< '(1 2 3) 1))
  (is-false (utils:s< #(1 2 3) 1))
  (is-false (utils:s< "123" 1))
  (is-false (utils:s< '(1 2 3) 3))
  (is-false (utils:s< #(1 2 3) 3))
  (is-false (utils:s< "123" 3))
  (is (utils:s< '(1 2 3) 4))
  (is (utils:s< #(1 2 3) 4))
  (is (utils:s< "123" 4))
  ;; -falseLength 4
  (is-false (utils:s< '(1 2 3 4) 3))
  (is-false (utils:s< #(1 2 3 4) 3))
  (is-false (utils:s< "1234" 3))
  (is-false (utils:s< '(1 2 3 4) 4))
  (is-false (utils:s< #(1 2 3 4) 4))
  (is-false (utils:s< "1234" 4))
  (is (utils:s< '(1 2 3 4) 5))
  (is (utils:s< #(1 2 3 4) 5))
  (is (utils:s< "1234" 5))
  ;; Is a function
  (is (identity (function utils:s<)))
  ;; Invalid argument
  (signals error (utils:s< 3 0)))

(test s>=
  (declare (notinline utils:s>=))
  ;; Zero length
  (is (utils:s>= '() -1))
  (is (utils:s>= #() -1))
  (is (utils:s>= "" -1))
  (is (utils:s>= '() 0))
  (is (utils:s>= #() 0))
  (is (utils:s>= "" 0))
  (is-false (utils:s>= '() 1))
  (is-false (utils:s>= #() 1))
  (is-false (utils:s>= "" 1))
  ;; Length 1
  (is (utils:s>= '(1) 0))
  (is (utils:s>= #(1) 0))
  (is (utils:s>= "1" 0))
  (is (utils:s>= '(1) 1))
  (is (utils:s>= #(1) 1))
  (is (utils:s>= "1" 1))
  (is-false (utils:s>= '(1) 2))
  (is-false (utils:s>= #(1) 2))
  (is-false (utils:s>= "1" 2))
  ;; Length 2
  (is (utils:s>= '(1 2) 1))
  (is (utils:s>= #(1 2) 1))
  (is (utils:s>= "12" 1))
  (is (utils:s>= '(1 2) 2))
  (is (utils:s>= #(1 2) 2))
  (is (utils:s>= "12" 2))
  (is-false (utils:s>= '(1 2) 3))
  (is-false (utils:s>= #(1 2) 3))
  (is-false (utils:s>= "12" 3))
  ;; Length 3
  (is (utils:s>= '(1 2 3) 1))
  (is (utils:s>= #(1 2 3) 1))
  (is (utils:s>= "123" 1))
  (is (utils:s>= '(1 2 3) 3))
  (is (utils:s>= #(1 2 3) 3))
  (is (utils:s>= "123" 3))
  (is-false (utils:s>= '(1 2 3) 4))
  (is-false (utils:s>= #(1 2 3) 4))
  (is-false (utils:s>= "123" 4))
  ;; Length 4
  (is (utils:s>= '(1 2 3 4) 3))
  (is (utils:s>= #(1 2 3 4) 3))
  (is (utils:s>= "1234" 3))
  (is (utils:s>= '(1 2 3 4) 4))
  (is (utils:s>= #(1 2 3 4) 4))
  (is (utils:s>= "1234" 4))
  (is-false (utils:s>= '(1 2 3 4) 5))
  (is-false (utils:s>= #(1 2 3 4) 5))
  (is-false (utils:s>= "1234" 5))
  ;; Is a function
  (is (identity (function utils:s>=)))
  ;; Invalid argument
  (signals error (utils:s>= 3 0)))

(test s<=
  (declare (notinline utils:s<=))
  ;; Zero length
  (is-false (utils:s<= '() -1))
  (is-false (utils:s<= #() -1))
  (is-false (utils:s<= "" -1))
  (is (utils:s<= '() 0))
  (is (utils:s<= #() 0))
  (is (utils:s<= "" 0))
  (is (utils:s<= '() 1))
  (is (utils:s<= #() 1))
  (is (utils:s<= "" 1))
  ;; Length 1
  (is-false (utils:s<= '(1) 0))
  (is-false (utils:s<= #(1) 0))
  (is-false (utils:s<= "1" 0))
  (is (utils:s<= '(1) 1))
  (is (utils:s<= #(1) 1))
  (is (utils:s<= "1" 1))
  (is (utils:s<= '(1) 2))
  (is (utils:s<= #(1) 2))
  (is (utils:s<= "1" 2))
  ;; Length 2
  (is-false (utils:s<= '(1 2) 1))
  (is-false (utils:s<= #(1 2) 1))
  (is-false (utils:s<= "12" 1))
  (is (utils:s<= '(1 2) 2))
  (is (utils:s<= #(1 2) 2))
  (is (utils:s<= "12" 2))
  (is (utils:s<= '(1 2) 3))
  (is (utils:s<= #(1 2) 3))
  (is (utils:s<= "12" 3))
  ;; Length 3
  (is-false (utils:s<= '(1 2 3) 1))
  (is-false (utils:s<= #(1 2 3) 1))
  (is-false (utils:s<= "123" 1))
  (is (utils:s<= '(1 2 3) 3))
  (is (utils:s<= #(1 2 3) 3))
  (is (utils:s<= "123" 3))
  (is (utils:s<= '(1 2 3) 4))
  (is (utils:s<= #(1 2 3) 4))
  (is (utils:s<= "123" 4))
  ;; Length 4
  (is-false (utils:s<= '(1 2 3 4) 3))
  (is-false (utils:s<= #(1 2 3 4) 3))
  (is-false (utils:s<= "1234" 3))
  (is (utils:s<= '(1 2 3 4) 4))
  (is (utils:s<= #(1 2 3 4) 4))
  (is (utils:s<= "1234" 4))
  (is (utils:s<= '(1 2 3 4) 5))
  (is (utils:s<= #(1 2 3 4) 5))
  (is (utils:s<= "1234" 5))
  ;; Is a function
  (is (identity (function utils:s<=)))
  ;; Invalid argument
  (signals error (utils:s<= 3 0)))

(test s=0
  (declare (notinline utils:s=0))
  ;; Length 0
  (is (utils:s=0 '()))
  (is (utils:s=0 #()))
  (is (utils:s=0 ""))
  ;; Length 1
  (is-false (utils:s=0 '(1)))
  (is-false (utils:s=0 #(1)))
  (is-false (utils:s=0 "1"))
  ;; Length 2
  (is-false (utils:s=0 '(1 2)))
  (is-false (utils:s=0 #(1 2)))
  (is-false (utils:s=0 "12"))
  ;; Length 3
  (is-false (utils:s=0 '(1 2 3)))
  (is-false (utils:s=0 #(1 2 3)))
  (is-false (utils:s=0 "123"))
  ;; Is a function
  (is (identity (function utils:s=0)))
  ;; Invalid argument
  (signals error (utils:s=0 3)))

(test s=1
  (declare (notinline utils:s=1))
  ;; Length 0
  (is-false (utils:s=1 '()))
  (is-false (utils:s=1 #()))
  (is-false (utils:s=1 ""))
  ;; Length 1
  (is (utils:s=1 '(1)))
  (is (utils:s=1 #(1)))
  (is (utils:s=1 "1"))
  ;; Length 2
  (is-false (utils:s=1 '(1 2)))
  (is-false (utils:s=1 #(1 2)))
  (is-false (utils:s=1 "12"))
  ;; Length 3
  (is-false (utils:s=1 '(1 2 3)))
  (is-false (utils:s=1 #(1 2 3)))
  (is-false (utils:s=1 "123"))
  ;; Is a function
  (is (identity (function utils:s=1)))
  ;; Invalid argument
  (signals error (utils:s=1 3)))

(test s=2
  (declare (notinline utils:s=2))
  ;; Length 0
  (is-false (utils:s=2 '()))
  (is-false (utils:s=2 #()))
  (is-false (utils:s=2 ""))
  ;; Length 1
  (is-false (utils:s=2 '(1)))
  (is-false (utils:s=2 #(1)))
  (is-false (utils:s=2 "1"))
  ;; Length 2
  (is (utils:s=2 '(1 2)))
  (is (utils:s=2 #(1 2)))
  (is (utils:s=2 "12"))
  ;; Length 3
  (is-false (utils:s=2 '(1 2 3)))
  (is-false (utils:s=2 #(1 2 3)))
  (is-false (utils:s=2 "123"))
  ;; Is a function
  (is (identity (function utils:s=2)))
  ;; Invalid argument
  (signals error (utils:s=2 3)))

(test s=3
  (declare (notinline utils:s=3))
  ;; Length 0
  (is-false (utils:s=3 '()))
  (is-false (utils:s=3 #()))
  (is-false (utils:s=3 ""))
  ;; Length 1
  (is-false (utils:s=3 '(1)))
  (is-false (utils:s=3 #(1)))
  (is-false (utils:s=3 "1"))
  ;; Length 2
  (is-false (utils:s=3 '(1 2)))
  (is-false (utils:s=3 #(1 2)))
  (is-false (utils:s=3 "12"))
  ;; Length 3
  (is (utils:s=3 '(1 2 3)))
  (is (utils:s=3 #(1 2 3)))
  (is (utils:s=3 "123"))
  ;; Length 4
  (is-false (utils:s=3 '(1 2 3 4)))
  (is-false (utils:s=3 #(1 2 3 4)))
  (is-false (utils:s=3 "12345"))
  ;; Is a function
  (is (identity (function utils:s=3)))
  ;; Invalid argument
  (signals error (utils:s=3 3)))

(test s=4
  (declare (notinline utils:s=4))
  ;; Length 0
  (is-false (utils:s=4 '()))
  (is-false (utils:s=4 #()))
  (is-false (utils:s=4 ""))
  ;; Length 1
  (is-false (utils:s=4 '(1)))
  (is-false (utils:s=4 #(1)))
  (is-false (utils:s=4 "1"))
  ;; Length 2
  (is-false (utils:s=4 '(1 2)))
  (is-false (utils:s=4 #(1 2)))
  (is-false (utils:s=4 "12"))
  ;; Length 3
  (is-false (utils:s=4 '(1 2 3)))
  (is-false (utils:s=4 #(1 2 3)))
  (is-false (utils:s=4 "123"))
  ;; Length 4
  (is (utils:s=4 '(1 2 3 4)))
  (is (utils:s=4 #(1 2 3 4)))
  (is (utils:s=4 "1234"))
  ;; Length 5
  (is-false (utils:s=4 '(1 2 3 4 5)))
  (is-false (utils:s=4 #(1 2 3 4 5)))
  (is-false (utils:s=4 "12345"))
  ;; Is a function
  (is (identity (function utils:s=4)))
  ;; Invalid argument
  (signals error (utils:s=4 3)))

(test s>0
  (declare (notinline utils:s>0))
  ;; Length 0
  (is-false (utils:s>0 '()))
  (is-false (utils:s>0 #()))
  (is-false (utils:s>0 ""))
  ;; Length 1
  (is (utils:s>0 '(1)))
  (is (utils:s>0 #(1)))
  (is (utils:s>0 "1"))
  ;; Length 2
  (is (utils:s>0 '(1 2)))
  (is (utils:s>0 #(1 2)))
  (is (utils:s>0 "12"))
  ;; Length 3
  (is (utils:s>0 '(1 2 3)))
  (is (utils:s>0 #(1 2 3)))
  (is (utils:s>0 "123"))
  ;; Is a function
  (is (identity (function utils:s>0)))
  ;; Invalid argument
  (signals error (utils:s>0 3)))

(test s>1
  (declare (notinline utils:s>1))
  ;; Length 0
  (is-false (utils:s>1 '()))
  (is-false (utils:s>1 #()))
  (is-false (utils:s>1 ""))
  ;; Length 1
  (is-false (utils:s>1 '(1)))
  (is-false (utils:s>1 #(1)))
  (is-false (utils:s>1 "1"))
  ;; Length 2
  (is (utils:s>1 '(1 2)))
  (is (utils:s>1 #(1 2)))
  (is (utils:s>1 "12"))
  ;; Length 3
  (is (utils:s>1 '(1 2 3)))
  (is (utils:s>1 #(1 2 3)))
  (is (utils:s>1 "123"))
  ;; Is a function
  (is (identity (function utils:s>1)))
  ;; Invalid argument
  (signals error (utils:s>1 3)))

(test s>2
  (declare (notinline utils:s>2))
  ;; Length 0
  (is-false (utils:s>2 '()))
  (is-false (utils:s>2 #()))
  (is-false (utils:s>2 ""))
  ;; Length 1
  (is-false (utils:s>2 '(1)))
  (is-false (utils:s>2 #(1)))
  (is-false (utils:s>2 "1"))
  ;; Length 2
  (is-false (utils:s>2 '(1 2)))
  (is-false (utils:s>2 #(1 2)))
  (is-false (utils:s>2 "12"))
  ;; Length 3
  (is (utils:s>2 '(1 2 3)))
  (is (utils:s>2 #(1 2 3)))
  (is (utils:s>2 "123"))
  ;; Is a function
  (is (identity (function utils:s>2)))
  ;; Invalid argument
  (signals error (utils:s>2 3)))

(test s>3
  (declare (notinline utils:s>3))
  ;; Length 0
  (is-false (utils:s>3 '()))
  (is-false (utils:s>3 #()))
  (is-false (utils:s>3 ""))
  ;; Length 1
  (is-false (utils:s>3 '(1)))
  (is-false (utils:s>3 #(1)))
  (is-false (utils:s>3 "1"))
  ;; Length 2
  (is-false (utils:s>3 '(1 2)))
  (is-false (utils:s>3 #(1 2)))
  (is-false (utils:s>3 "12"))
  ;; Length 3
  (is-false (utils:s>3 '(1 2 3)))
  (is-false (utils:s>3 #(1 2 3)))
  (is-false (utils:s>3 "123"))
  ;; Length 4
  (is (utils:s>3 '(1 2 3 4)))
  (is (utils:s>3 #(1 2 3 4)))
  (is (utils:s>3 "1234"))
  ;; Is a function
  (is (identity (function utils:s>3)))
  ;; Invalid argument
  (signals error (utils:s>3 3)))

(test s>4
  (declare (notinline utils:s>4))
  ;; Length 0
  (is-false (utils:s>4 '()))
  (is-false (utils:s>4 #()))
  (is-false (utils:s>4 ""))
  ;; Length 1
  (is-false (utils:s>4 '(1)))
  (is-false (utils:s>4 #(1)))
  (is-false (utils:s>4 "1"))
  ;; Length 2
  (is-false (utils:s>4 '(1 2)))
  (is-false (utils:s>4 #(1 2)))
  (is-false (utils:s>4 "12"))
  ;; Length 3
  (is-false (utils:s>4 '(1 2 3)))
  (is-false (utils:s>4 #(1 2 3)))
  (is-false (utils:s>4 "123"))
  ;; Length 4
  (is-false (utils:s>4 '(1 2 3 4)))
  (is-false (utils:s>4 #(1 2 3 4)))
  (is-false (utils:s>4 "1234"))
  ;; Length 5
  (is (utils:s>4 '(1 2 3 4 5)))
  (is (utils:s>4 #(1 2 3 4 5)))
  (is (utils:s>4 "12345"))
  ;; Is a function
  (is (identity (function utils:s>4)))
  ;; Invalid argument
  (signals error (utils:s>4 3)))

(test s<1
  (declare (notinline utils:s<1))
  ;; Length 0
  (is (utils:s<1 '()))
  (is (utils:s<1 #()))
  (is (utils:s<1 ""))
  ;; Length 1
  (is-false (utils:s<1 '(1)))
  (is-false (utils:s<1 #(1)))
  (is-false (utils:s<1 "1"))
  ;; Length 2
  (is-false (utils:s<1 '(1 2)))
  (is-false (utils:s<1 #(1 2)))
  (is-false (utils:s<1 "12"))
  ;; Length 3
  (is-false (utils:s<1 '(1 2 3)))
  (is-false (utils:s<1 #(1 2 3)))
  (is-false (utils:s<1 "123"))
  ;; Is a function
  (is (identity (function utils:s<1)))
  ;; Invalid argument
  (signals error (utils:s<1 3)))

(test s<2
  (declare (notinline utils:s<2))
  ;; Length 0
  (is (utils:s<2 '()))
  (is (utils:s<2 #()))
  (is (utils:s<2 ""))
  ;; Length 1
  (is (utils:s<2 '(1)))
  (is (utils:s<2 #(1)))
  (is (utils:s<2 "1"))
  ;; Length 2
  (is-false (utils:s<2 '(1 2)))
  (is-false (utils:s<2 #(1 2)))
  (is-false (utils:s<2 "12"))
  ;; Length 3
  (is-false (utils:s<2 '(1 2 3)))
  (is-false (utils:s<2 #(1 2 3)))
  (is-false (utils:s<2 "123"))
  ;; Is a function
  (is (identity (function utils:s<2)))
  ;; Invalid argument
  (signals error (utils:s<2 3)))

(test s<3
  (declare (notinline utils:s<3))
  ;; Length 0
  (is (utils:s<3 '()))
  (is (utils:s<3 #()))
  (is (utils:s<3 ""))
  ;; Length 1
  (is (utils:s<3 '(1)))
  (is (utils:s<3 #(1)))
  (is (utils:s<3 "1"))
  ;; Length 2
  (is (utils:s<3 '(1 2)))
  (is (utils:s<3 #(1 2)))
  (is (utils:s<3 "12"))
  ;; Length 3
  (is-false (utils:s<3 '(1 2 3)))
  (is-false (utils:s<3 #(1 2 3)))
  (is-false (utils:s<3 "123"))
  ;; Length 4
  (is-false (utils:s<3 '(1 2 3 4)))
  (is-false (utils:s<3 #(1 2 3 4)))
  (is-false (utils:s<3 "1234"))
  ;; Is a function
  (is (identity (function utils:s<3)))
  ;; Invalid argument
  (signals error (utils:s<3 3)))

(test s<4
  (declare (notinline utils:s<4))
  ;; Length 0
  (is (utils:s<4 '()))
  (is (utils:s<4 #()))
  (is (utils:s<4 ""))
  ;; Length 1
  (is (utils:s<4 '(1)))
  (is (utils:s<4 #(1)))
  (is (utils:s<4 "1"))
  ;; Length 2
  (is (utils:s<4 '(1 2)))
  (is (utils:s<4 #(1 2)))
  (is (utils:s<4 "12"))
  ;; Length 3
  (is (utils:s<4 '(1 2 3)))
  (is (utils:s<4 #(1 2 3)))
  (is (utils:s<4 "123"))
  ;; Length 4
  (is-false (utils:s<4 '(1 2 3 4)))
  (is-false (utils:s<4 #(1 2 3 4)))
  (is-false (utils:s<4 "1234"))
  ;; Length 5
  (is-false (utils:s<4 '(1 2 3 4 5)))
  (is-false (utils:s<4 #(1 2 3 4 5)))
  (is-false (utils:s<4 "12345"))
  ;; Is a function
  (is (identity (function utils:s<4)))
  ;; Invalid argument
  (signals error (utils:s<4 3)))

(test ss=
  (declare (notinline utils:ss=))
  ;; Zero length
  (is (utils:ss= '() '()))
  (is (utils:ss= #() '()))
  (is (utils:ss= "" '()))
  (is-false (utils:ss= '() '(5)))
  (is-false (utils:ss= #() #(5)))
  (is-false (utils:ss= "" "5"))
  ;; Length 1
  (is-false (utils:ss= '(1) '()))
  (is-false (utils:ss= #(1) #()))
  (is-false (utils:ss= "1" ""))
  (is (utils:ss= '(1) '(5)))
  (is (utils:ss= #(1) #(5)))
  (is (utils:ss= "1" "5"))
  (is-false (utils:ss= '(1) '(5 6)))
  (is-false (utils:ss= #(1) #(5 6)))
  (is-false (utils:ss= "1" "56"))
  ;; Length 2
  (is-false (utils:ss= '(1 2) '(5)))
  (is-false (utils:ss= #(1 2) #(5)))
  (is-false (utils:ss= "12" "5"))
  (is (utils:ss= '(1 2) '(5 6)))
  (is (utils:ss= #(1 2) #(5 6)))
  (is (utils:ss= "12" "56"))
  (is-false (utils:ss= '(1 2) '(7 8 9)))
  (is-false (utils:ss= #(1 2) #(7 8 9)))
  (is-false (utils:ss= "12" "789"))
  ;; Length 3
  (is-false (utils:ss= '(1 2 3) '(5 6)))
  (is-false (utils:ss= #(1 2 3) #(5 6)))
  (is-false (utils:ss= "123" "56"))
  (is (utils:ss= '(1 2 3) '(5 6 7)))
  (is (utils:ss= #(1 2 3) #(5 6 7)))
  (is (utils:ss= "123" "567"))
  (is-false (utils:ss= '(1 2 3) '(5 6 7 8)))
  (is-false (utils:ss= #(1 2 3) #(5 6 7 8)))
  (is-false (utils:ss= "123" "5678"))
  ;; Length 4
  (is-false (utils:ss= '(1 2 3 4) '(5 6 7)))
  (is-false (utils:ss= #(1 2 3 4) #(5 6 7)))
  (is-false (utils:ss= "1234" "567"))
  (is (utils:ss= '(1 2 3 4) '(5 6 7 8)))
  (is (utils:ss= #(1 2 3 4) #(5 6 7 8)))
  (is (utils:ss= "1234" "5678"))
  (is-false (utils:ss= '(1 2 3 4) '(5 6 7 8 9)))
  (is-false (utils:ss= #(1 2 3 4) #(5 6 7 8 9)))
  (is-false (utils:ss= "1234" "56789"))
  ;; Is a function
  (is (identity (function utils:ss=)))
  ;; Invalid argument
  (signals error (utils:ss= 3 '(1 2 3)))
  (signals error (utils:ss= '(1 2 3) 3))
  (signals error (utils:ss= 3 0)))

(test ss>
  (declare (notinline utils:ss>))
  ;; Zero length
  (is-false (utils:ss> '() '()))
  (is-false (utils:ss> #() '()))
  (is-false (utils:ss> "" '()))
  (is-false (utils:ss> '() '(5)))
  (is-false (utils:ss> #() #(5)))
  (is-false (utils:ss> "" "5"))
  ;; Length 1
  (is (utils:ss> '(1) '()))
  (is (utils:ss> #(1) #()))
  (is (utils:ss> "1" ""))
  (is-false (utils:ss> '(1) '(5)))
  (is-false (utils:ss> #(1) #(5)))
  (is-false (utils:ss> "1" "5"))
  (is-false (utils:ss> '(1) '(5 6)))
  (is-false (utils:ss> #(1) #(5 6)))
  (is-false (utils:ss> "1" "56"))
  ;; Length 2
  (is (utils:ss> '(1 2) '(5)))
  (is (utils:ss> #(1 2) #(5)))
  (is (utils:ss> "12" "5"))
  (is-false (utils:ss> '(1 2) '(5 6)))
  (is-false (utils:ss> #(1 2) #(5 6)))
  (is-false (utils:ss> "12" "56"))
  (is-false (utils:ss> '(1 2) '(7 8 9)))
  (is-false (utils:ss> #(1 2) #(7 8 9)))
  (is-false (utils:ss> "12" "789"))
  ;; Length 3
  (is (utils:ss> '(1 2 3) '(5 6)))
  (is (utils:ss> #(1 2 3) #(5 6)))
  (is (utils:ss> "123" "56"))
  (is-false (utils:ss> '(1 2 3) '(5 6 7)))
  (is-false (utils:ss> #(1 2 3) #(5 6 7)))
  (is-false (utils:ss> "123" "567"))
  (is-false (utils:ss> '(1 2 3) '(5 6 7 8)))
  (is-false (utils:ss> #(1 2 3) #(5 6 7 8)))
  (is-false (utils:ss> "123" "5678"))
  ;; Length 4
  (is (utils:ss> '(1 2 3 4) '(5 6 7)))
  (is (utils:ss> #(1 2 3 4) #(5 6 7)))
  (is (utils:ss> "1234" "567"))
  (is-false (utils:ss> '(1 2 3 4) '(5 6 7 8)))
  (is-false (utils:ss> #(1 2 3 4) #(5 6 7 8)))
  (is-false (utils:ss> "1234" "5678"))
  (is-false (utils:ss> '(1 2 3 4) '(5 6 7 8 9)))
  (is-false (utils:ss> #(1 2 3 4) #(5 6 7 8 9)))
  (is-false (utils:ss> "1234" "56789"))
  ;; Is a function
  (is (identity (function utils:ss>)))
  ;; Invalid argument
  (signals error (utils:ss> 3 '(1 2 3)))
  (signals error (utils:ss> '(1 2 3) 3))
  (signals error (utils:ss> 3 0)))

(test ss<
  (declare (notinline utils:ss<))
  ;; Zero length
  (is-false (utils:ss< '() '()))
  (is-false (utils:ss< #() '()))
  (is-false (utils:ss< "" '()))
  (is (utils:ss< '() '(5)))
  (is (utils:ss< #() #(5)))
  (is (utils:ss< "" "5"))
  ;; Length 1
  (is-false (utils:ss< '(1) '()))
  (is-false (utils:ss< #(1) #()))
  (is-false (utils:ss< "1" ""))
  (is-false (utils:ss< '(1) '(5)))
  (is-false (utils:ss< #(1) #(5)))
  (is-false (utils:ss< "1" "5"))
  (is (utils:ss< '(1) '(5 6)))
  (is (utils:ss< #(1) #(5 6)))
  (is (utils:ss< "1" "56"))
  ;; Length 2
  (is-false (utils:ss< '(1 2) '(5)))
  (is-false (utils:ss< #(1 2) #(5)))
  (is-false (utils:ss< "12" "5"))
  (is-false (utils:ss< '(1 2) '(5 6)))
  (is-false (utils:ss< #(1 2) #(5 6)))
  (is-false (utils:ss< "12" "56"))
  (is (utils:ss< '(1 2) '(7 8 9)))
  (is (utils:ss< #(1 2) #(7 8 9)))
  (is (utils:ss< "12" "789"))
  ;; Length 3
  (is-false (utils:ss< '(1 2 3) '(5 6)))
  (is-false (utils:ss< #(1 2 3) #(5 6)))
  (is-false (utils:ss< "123" "56"))
  (is-false (utils:ss< '(1 2 3) '(5 6 7)))
  (is-false (utils:ss< #(1 2 3) #(5 6 7)))
  (is-false (utils:ss< "123" "567"))
  (is (utils:ss< '(1 2 3) '(5 6 7 8)))
  (is (utils:ss< #(1 2 3) #(5 6 7 8)))
  (is (utils:ss< "123" "5678"))
  ;; Length 4
  (is-false (utils:ss< '(1 2 3 4) '(5 6 7)))
  (is-false (utils:ss< #(1 2 3 4) #(5 6 7)))
  (is-false (utils:ss< "1234" "567"))
  (is-false (utils:ss< '(1 2 3 4) '(5 6 7 8)))
  (is-false (utils:ss< #(1 2 3 4) #(5 6 7 8)))
  (is-false (utils:ss< "1234" "5678"))
  (is (utils:ss< '(1 2 3 4) '(5 6 7 8 9)))
  (is (utils:ss< #(1 2 3 4) #(5 6 7 8 9)))
  (is (utils:ss< "1234" "56789"))
  ;; Is a function
  (is (identity (function utils:ss<)))
  ;; Invalid argument
  (signals error (utils:ss< 3 '(1 2 3)))
  (signals error (utils:ss< '(1 2 3) 3))
  (signals error (utils:ss< 3 0)))

(test ss>=
  (declare (notinline utils:ss>=))
  ;; Zero length
  (is (utils:ss>= '() '()))
  (is (utils:ss>= #() '()))
  (is (utils:ss>= "" '()))
  (is-false (utils:ss>= '() '(5)))
  (is-false (utils:ss>= #() #(5)))
  (is-false (utils:ss>= "" "5"))
  ;; Length 1
  (is (utils:ss>= '(1) '()))
  (is (utils:ss>= #(1) #()))
  (is (utils:ss>= "1" ""))
  (is (utils:ss>= '(1) '(5)))
  (is (utils:ss>= #(1) #(5)))
  (is (utils:ss>= "1" "5"))
  (is-false (utils:ss>= '(1) '(5 6)))
  (is-false (utils:ss>= #(1) #(5 6)))
  (is-false (utils:ss>= "1" "56"))
  ;; Length 2
  (is (utils:ss>= '(1 2) '(5)))
  (is (utils:ss>= #(1 2) #(5)))
  (is (utils:ss>= "12" "5"))
  (is (utils:ss>= '(1 2) '(5 6)))
  (is (utils:ss>= #(1 2) #(5 6)))
  (is (utils:ss>= "12" "56"))
  (is-false (utils:ss>= '(1 2) '(7 8 9)))
  (is-false (utils:ss>= #(1 2) #(7 8 9)))
  (is-false (utils:ss>= "12" "789"))
  ;; Length 3
  (is (utils:ss>= '(1 2 3) '(5 6)))
  (is (utils:ss>= #(1 2 3) #(5 6)))
  (is (utils:ss>= "123" "56"))
  (is (utils:ss>= '(1 2 3) '(5 6 7)))
  (is (utils:ss>= #(1 2 3) #(5 6 7)))
  (is (utils:ss>= "123" "567"))
  (is-false (utils:ss>= '(1 2 3) '(5 6 7 8)))
  (is-false (utils:ss>= #(1 2 3) #(5 6 7 8)))
  (is-false (utils:ss>= "123" "5678"))
  ;; Length 4
  (is (utils:ss>= '(1 2 3 4) '(5 6 7)))
  (is (utils:ss>= #(1 2 3 4) #(5 6 7)))
  (is (utils:ss>= "1234" "567"))
  (is (utils:ss>= '(1 2 3 4) '(5 6 7 8)))
  (is (utils:ss>= #(1 2 3 4) #(5 6 7 8)))
  (is (utils:ss>= "1234" "5678"))
  (is-false (utils:ss>= '(1 2 3 4) '(5 6 7 8 9)))
  (is-false (utils:ss>= #(1 2 3 4) #(5 6 7 8 9)))
  (is-false (utils:ss>= "1234" "56789"))
  ;; Is a function
  (is (identity (function utils:ss>=)))
  ;; Invalid argument
  (signals error (utils:ss>= 3 '(1 2 3)))
  (signals error (utils:ss>= '(1 2 3) 3))
  (signals error (utils:ss>= 3 0)))

(test ss<=
  (declare (notinline utils:ss<=))
  ;; Zero length
  (is (utils:ss<= '() '()))
  (is (utils:ss<= #() '()))
  (is (utils:ss<= "" '()))
  (is (utils:ss<= '() '(5)))
  (is (utils:ss<= #() #(5)))
  (is (utils:ss<= "" "5"))
  ;; Length 1
  (is-false (utils:ss<= '(1) '()))
  (is-false (utils:ss<= #(1) #()))
  (is-false (utils:ss<= "1" ""))
  (is (utils:ss<= '(1) '(5)))
  (is (utils:ss<= #(1) #(5)))
  (is (utils:ss<= "1" "5"))
  (is (utils:ss<= '(1) '(5 6)))
  (is (utils:ss<= #(1) #(5 6)))
  (is (utils:ss<= "1" "56"))
  ;; Length 2
  (is-false (utils:ss<= '(1 2) '(5)))
  (is-false (utils:ss<= #(1 2) #(5)))
  (is-false (utils:ss<= "12" "5"))
  (is (utils:ss<= '(1 2) '(5 6)))
  (is (utils:ss<= #(1 2) #(5 6)))
  (is (utils:ss<= "12" "56"))
  (is (utils:ss<= '(1 2) '(7 8 9)))
  (is (utils:ss<= #(1 2) #(7 8 9)))
  (is (utils:ss<= "12" "789"))
  ;; Length 3
  (is-false (utils:ss<= '(1 2 3) '(5 6)))
  (is-false (utils:ss<= #(1 2 3) #(5 6)))
  (is-false (utils:ss<= "123" "56"))
  (is (utils:ss<= '(1 2 3) '(5 6 7)))
  (is (utils:ss<= #(1 2 3) #(5 6 7)))
  (is (utils:ss<= "123" "567"))
  (is (utils:ss<= '(1 2 3) '(5 6 7 8)))
  (is (utils:ss<= #(1 2 3) #(5 6 7 8)))
  (is (utils:ss<= "123" "5678"))
  ;; Length 4
  (is-false (utils:ss<= '(1 2 3 4) '(5 6 7)))
  (is-false (utils:ss<= #(1 2 3 4) #(5 6 7)))
  (is-false (utils:ss<= "1234" "567"))
  (is (utils:ss<= '(1 2 3 4) '(5 6 7 8)))
  (is (utils:ss<= #(1 2 3 4) #(5 6 7 8)))
  (is (utils:ss<= "1234" "5678"))
  (is (utils:ss<= '(1 2 3 4) '(5 6 7 8 9)))
  (is (utils:ss<= #(1 2 3 4) #(5 6 7 8 9)))
  (is (utils:ss<= "1234" "56789"))
  ;; Is a function
  (is (identity (function utils:ss<=)))
  ;; Invalid argument
  (signals error (utils:ss<= 3 '(1 2 3)))
  (signals error (utils:ss<= '(1 2 3) 3))
  (signals error (utils:ss<= 3 0)))

(test slice-0
  (let ((n 0) (test-list '()))
    ;; Both indices specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (loop for j upfrom (- (1+ n)) upto (1+ n) for jj = (if (minusp j) (+ n j) j) do
        (cond
          ((> ii jj) (signals error (utils:slice test-list i j)))
          ((minusp ii) (signals error (utils:slice test-list i j)))
          ((minusp jj) (signals error (utils:slice test-list i j)))
          ((> ii n) (signals error (utils:slice test-list i j)))
          ((> jj n) (signals error (utils:slice test-list i j)))
          (t (is (equal (utils:slice test-list i j) (subseq test-list ii jj)))))))
    ;; Only first index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list i) (subseq test-list ii n))))))
    ;; Only second index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list nil i) (subseq test-list 0 ii))))))
    ;; No index specified
    (is (equal (utils:slice test-list nil) test-list))))

(test slice-1
  (let ((n 1) (test-list '(1)))
    ;; Both indices specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (loop for j upfrom (- (1+ n)) upto (1+ n) for jj = (if (minusp j) (+ n j) j) do
        (cond
          ((> ii jj) (signals error (utils:slice test-list i j)))
          ((minusp ii) (signals error (utils:slice test-list i j)))
          ((minusp jj) (signals error (utils:slice test-list i j)))
          ((> ii n) (signals error (utils:slice test-list i j)))
          ((> jj n) (signals error (utils:slice test-list i j)))
          (t (is (equal (utils:slice test-list i j) (subseq test-list ii jj)))))))
    ;; Only first index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list i) (subseq test-list ii n))))))
    ;; Only second index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list nil i) (subseq test-list 0 ii))))))
    ;; No index specified
    (is (equal (utils:slice test-list nil) test-list))))

(test slice-2
  (let ((n 2) (test-list '(1 2)))
    ;; Both indices specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (loop for j upfrom (- (1+ n)) upto (1+ n) for jj = (if (minusp j) (+ n j) j) do
        (cond
          ((> ii jj) (signals error (utils:slice test-list i j)))
          ((minusp ii) (signals error (utils:slice test-list i j)))
          ((minusp jj) (signals error (utils:slice test-list i j)))
          ((> ii n) (signals error (utils:slice test-list i j)))
          ((> jj n) (signals error (utils:slice test-list i j)))
          (t (is (equal (utils:slice test-list i j) (subseq test-list ii jj)))))))
    ;; Only first index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list i) (subseq test-list ii n))))))
    ;; Only second index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list nil i) (subseq test-list 0 ii))))))
    ;; No index specified
    (is (equal (utils:slice test-list nil) test-list))))

(test slice-3
  (let ((n 3) (test-list '(1 2 3)))
    ;; Both indices specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (loop for j upfrom (- (1+ n)) upto (1+ n) for jj = (if (minusp j) (+ n j) j) do
        (cond
          ((> ii jj) (signals error (utils:slice test-list i j)))
          ((minusp ii) (signals error (utils:slice test-list i j)))
          ((minusp jj) (signals error (utils:slice test-list i j)))
          ((> ii n) (signals error (utils:slice test-list i j)))
          ((> jj n) (signals error (utils:slice test-list i j)))
          (t (is (equal (utils:slice test-list i j) (subseq test-list ii jj)))))))
    ;; Only first index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list i) (subseq test-list ii n))))))
    ;; Only second index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list nil i) (subseq test-list 0 ii))))))
    ;; No index specified
    (is (equal (utils:slice test-list nil) test-list))))

(test slice-4
  (let ((n 4) (test-list '(1 2 3 4)))
    ;; Both indices specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (loop for j upfrom (- (1+ n)) upto (1+ n) for jj = (if (minusp j) (+ n j) j) do
        (cond
          ((> ii jj) (signals error (utils:slice test-list i j)))
          ((minusp ii) (signals error (utils:slice test-list i j)))
          ((minusp jj) (signals error (utils:slice test-list i j)))
          ((> ii n) (signals error (utils:slice test-list i j)))
          ((> jj n) (signals error (utils:slice test-list i j)))
          (t (is (equal (utils:slice test-list i j) (subseq test-list ii jj)))))))
    ;; Only first index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list i) (subseq test-list ii n))))))
    ;; Only second index specified
    (loop for i upfrom (- (1+ n)) upto (1+ n) for ii = (if (minusp i) (+ n i) i) do
      (cond
        ((minusp ii) (signals error (utils:slice test-list i)))
        ((> ii n) (signals error (utils:slice test-list i)))
        (t (is (equal (utils:slice test-list nil i) (subseq test-list 0 ii))))))
    ;; No index specified
    (is (equal (utils:slice test-list nil) test-list))))

(test sbind
  ;; Length 0
  (is (utils:sbind () '() t))
  (is (utils:sbind () #() t))
  (is (utils:sbind () "" t))
  (signals error (utils:sbind (x) '() x))
  (signals error (utils:sbind (x) #() x))
  (signals error (utils:sbind (x) "" x))
  ;; Length 1
  (signals error (utils:sbind () '(1) t))
  (signals error (utils:sbind () #(1) t))
  (signals error (utils:sbind () "1" t))
  (is (= (utils:sbind (x) '(1) x) 1))
  (is (= (utils:sbind (x) #(1) x) 1))
  (is (char= (utils:sbind (x) "1" x) #\1))
  (signals error (utils:sbind (x y) '(1) x))
  (signals error (utils:sbind (x y) #(1) x))
  (signals error (utils:sbind (x y) "1" x))
  ;; Length 1
  (signals error (utils:sbind (x) '(1 2) t))
  (signals error (utils:sbind (x) #(1 2) t))
  (signals error (utils:sbind (x) "12" t))
  (is (equal (utils:sbind (x y) '(1 2) (list y x)) '(2 1)))
  (is (equal (utils:sbind (x y) #(1 2) (list y x)) '(2 1)))
  (is (equal (utils:sbind (x y) "12" (list y x)) '(#\2 #\1)))
  (signals error (utils:sbind (x y z) '(1 2) x))
  (signals error (utils:sbind (x y z) #(1 2) x))
  (signals error (utils:sbind (x y z) "12" x)))

(test items-unique-p
  (is (utils:items-unique-p '(1 2 3 4 5)))
  (is (utils:items-unique-p '(1 2 3 4 5) :test #'=))
  (is (utils:items-unique-p '(1 2 3 4 5) :key #'identity))
  (is-false (utils:items-unique-p '(1 1 3 4 5)))
  (is-false (utils:items-unique-p '(1 2 1 4 5)))
  (is-false (utils:items-unique-p '(1 2 3 1 5)))
  (is-false (utils:items-unique-p '(1 2 3 4 1))))

(test remove-nth
  (is (equal (utils:remove-nth 0 '(1)) nil))
  (is (equal (utils:remove-nth 0 '(1 2)) '(2)))
  (is (equal (utils:remove-nth 1 '(1 2)) '(1))))
