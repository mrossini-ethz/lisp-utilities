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

(defmacro s=<>-tester (function test)
  `(locally
     (declare (notinline ,function))
     (loop for i below 6 for lst = (make-list i :initial-element 1) for vec = (make-array `(,i) :initial-element 1) for str = (make-string i :initial-element #\1) do
       (loop for j below 6 do
         (if (,test i j)
             (progn
               (is (,function lst j))
               (is (,function vec j))
               (is (,function str j)))
             (progn
               (is-false (,function lst j))
               (is-false (,function vec j))
               (is-false (,function str j))))))
     ;; Is a function
     (is (identity (function ,function)))
     ;; Invalid argument type
     (signals error (,function 3 1))))

(test s=
  (s=<>-tester utils:s= =))

(test s>
  (s=<>-tester utils:s> >))

(test s<
  (s=<>-tester utils:s< <))

(test s>=
  (s=<>-tester utils:s>= >=))

(test s<=
  (s=<>-tester utils:s<= <=))

(defmacro s=<>x-tester (function test number)
  `(locally
     (declare (notinline ,function))
     (loop for i below 10 for lst = (make-list i :initial-element 1) for vec = (make-array `(,i) :initial-element 1) for str = (make-string i :initial-element #\1) do
       (if (,test i ,number)
           (progn
             (is (,function lst))
             (is (,function lst))
             (is (,function lst)))
           (progn
             (is-false (,function lst))
             (is-false (,function lst))
             (is-false (,function lst)))))
     (signals error (,function 5))))

(test s=0
  (s=<>x-tester utils:s=0 = 0))

(test s=1
  (s=<>x-tester utils:s=1 = 1))

(test s=2
  (s=<>x-tester utils:s=2 = 2))

(test s=3
  (s=<>x-tester utils:s=3 = 3))

(test s=4
  (s=<>x-tester utils:s=4 = 4))

(test s>0
  (s=<>x-tester utils:s>0 > 0))

(test s>1
  (s=<>x-tester utils:s>1 > 1))

(test s>2
  (s=<>x-tester utils:s>2 > 2))

(test s>3
  (s=<>x-tester utils:s>3 > 3))

(test s>4
  (s=<>x-tester utils:s>4 > 4))

(test s<1
  (s=<>x-tester utils:s<1 < 1))

(test s<2
  (s=<>x-tester utils:s<2 < 2))

(test s<3
  (s=<>x-tester utils:s<3 < 3))

(test s<4
  (s=<>x-tester utils:s<4 < 4))

(test s<5
  (s=<>x-tester utils:s<5 < 5))

(defmacro ss=<>-tester (function test)
  `(locally
     (declare (notinline ,function))
     ;; Test various combinations of sequence lengths
     (loop for i below 6 for lst-a = (make-list i :initial-element 1) for vec-a = (make-array `(,i) :initial-element 1) for str-a = (make-string i :initial-element #\1) do
       (loop for j below 6 for lst-b = (make-list j :initial-element 2) for vec-b = (make-array `(,j) :initial-element 2) for str-b = (make-string j :initial-element #\2) do
         (if (,test i j)
             (progn
               (is (,function lst-a lst-b))
               (is (,function vec-a vec-b))
               (is (,function str-a vec-b)))
             (progn
               (is-false (,function lst-a lst-b))
               (is-false (,function vec-a vec-b))
               (is-false (,function str-a vec-b))))))
     ;; Is a function
     (is (identity (function ,function)))
     ;; Invalid argument type
     (signals error (,function 3 '(1 2 3)))
     (signals error (,function '(1 2 3) 3))
     (signals error (,function 3 0))))

(test ss=
  (ss=<>-tester utils:ss= =))

(test ss>
  (ss=<>-tester utils:ss> >))

(test ss<
  (ss=<>-tester utils:ss< <))

(test ss>=
  (ss=<>-tester utils:ss>= >=))

(test ss<=
  (ss=<>-tester utils:ss<= <=))

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

(test same
  (is (utils:same #'= '()))
  (is (utils:same #'= '(1 1 1 1 1)))
  (is (utils:same #'char= '(#\1 #\1 #\1 #\1 #\1)))
  (is-false (utils:same #'= '(2 1 1 1 1)))
  (is-false (utils:same #'= '(1 2 1 1 1)))
  (is-false (utils:same #'= '(1 1 2 1 1)))
  (is-false (utils:same #'= '(1 1 1 2 1)))
  (is-false (utils:same #'= '(1 1 1 1 2)))
  (is (utils:same #'= '((1 1) (2 1) (3 1) (4 1) (5 1)) :key #'second))
  (is-false (utils:same #'= '((1 1) (2 1) (3 1) (4 1) (5 1)) :key #'first)))

(test have
  (is (utils:have 1 '(1 2 3 4 5) :test #'=))
  (is (utils:have 2 '(1 2 3 4 5) :test #'=))
  (is (utils:have 3 '(1 2 3 4 5) :test #'=))
  (is (utils:have 4 '(1 2 3 4 5) :test #'=))
  (is (utils:have 5 '(1 2 3 4 5) :test #'=))
  (is-false (utils:have 6 '(1 2 3 4 5) :test #'=))
  (is (utils:have 'hello '(hello world)))
  (is (utils:have 'world '(hello world)))
  (is (utils:have "hello" '("hello" "world") :test #'string=))
  (is (utils:have "world" '("hello" "world") :test #'string=))
  (is (utils:have "HELLO" '("hello" "world") :test #'string-equal))
  (is (utils:have "WORLD" '("hello" "world") :test #'string-equal))
  (is (utils:have "hel" '("hello" "world") :test #'string= :key (lambda (x) (subseq x 0 3))))
  (is (utils:have "wor" '("hello" "world") :test #'string= :key (lambda (x) (subseq x 0 3)))))

(test remove-nth
  ;; Invalid indices
  (signals error (utils:remove-nth 0 '()))
  (signals error (utils:remove-nth -1 '(1)))
  ;; List
  (is (equal (utils:remove-nth 0 '(1)) nil))
  (is (equal (utils:remove-nth 0 '(1 2)) '(2)))
  (is (equal (utils:remove-nth 1 '(1 2)) '(1)))
  ;; String
  (is (string= (utils:remove-nth 0 "a") ""))
  (is (string= (utils:remove-nth 0 "ab") "b"))
  (is (string= (utils:remove-nth 1 "ab") "a"))
  ;; Vector
  (is (equalp (utils:remove-nth 0 #(1)) #()))
  (is (equalp (utils:remove-nth 0 #(1 2)) #(2)))
  (is (equalp (utils:remove-nth 1 #(1 2)) #(1)))
  ;; Invalid type
  (signals error (utils:remove-nth 0 3)))

(test remove-if-index
  ;; Remove some
  (is (equal (utils:remove-if-index #'oddp '(0 1 2 3 4 5 6 7)) '(0 2 4 6)))
  (is (equal (utils:remove-if-index #'oddp '(1 2 3 4 5 6 7 8)) '(1 3 5 7)))
  (is (equalp (utils:remove-if-index #'oddp #(0 1 2 3 4 5 6 7)) #(0 2 4 6)))
  (is (equalp (utils:remove-if-index #'oddp #(1 2 3 4 5 6 7 8)) #(1 3 5 7)))
  (is (string= (utils:remove-if-index #'oddp "a1b2c3d4e5") "abcde"))
  (is (string= (utils:remove-if-index #'evenp "a1b2c3d4e5") "12345"))
  ;; Remove all
  (is (equal (utils:remove-if-index (constantly t) '(0 1 2 3 4 5 6 7)) '()))
  ;; Remove none
  (is (equal (utils:remove-if-index (constantly nil) '(0 1 2 3 4 5 6 7)) '(0 1 2 3 4 5 6 7))))

(test range
  ;; Invalid inputs
  (signals error (utils:range -1))
  (signals error (utils:range 5 1))
  (signals error (utils:range 5 1 2))
  (signals error (utils:range 1 5 -2))
  ;; List output
  (is (equal (utils:range 0 5 1 'list) '(0 1 2 3 4)))
  ;; Vector output
  (is (equalp (utils:range 0 5 1 'vector) #(0 1 2 3 4)))
  ;; Single input
  (is (equal (utils:range 0) '()))
  (is (equal (utils:range 1) '(0)))
  (is (equal (utils:range 2) '(0 1)))
  (is (equal (utils:range 3) '(0 1 2)))
  (is (equal (utils:range 4) '(0 1 2 3)))
  ;; Double input
  (is (equal (utils:range 0 0) '()))
  (is (equal (utils:range 0 1) '(0)))
  (is (equal (utils:range 0 2) '(0 1)))
  (is (equal (utils:range 0 3) '(0 1 2)))
  (is (equal (utils:range 0 4) '(0 1 2 3)))
  (is (equal (utils:range 1 1) '()))
  (is (equal (utils:range 1 2) '(1)))
  (is (equal (utils:range 1 3) '(1 2)))
  (is (equal (utils:range 1 4) '(1 2 3)))
  (is (equal (utils:range 2 2) '()))
  (is (equal (utils:range 2 3) '(2)))
  (is (equal (utils:range 2 4) '(2 3)))
  (is (equal (utils:range 3 3) '()))
  (is (equal (utils:range 3 4) '(3)))
  (is (equal (utils:range 4 4) '()))
  ;; Triple input (step 2)
  (is (equal (utils:range 0 0 2) '()))
  (is (equal (utils:range 0 1 2) '(0)))
  (is (equal (utils:range 0 2 2) '(0)))
  (is (equal (utils:range 0 3 2) '(0 2)))
  (is (equal (utils:range 0 4 2) '(0 2)))
  (is (equal (utils:range 1 1 2) '()))
  (is (equal (utils:range 1 2 2) '(1)))
  (is (equal (utils:range 1 3 2) '(1)))
  (is (equal (utils:range 1 4 2) '(1 3)))
  (is (equal (utils:range 2 2 2) '()))
  (is (equal (utils:range 2 3 2) '(2)))
  (is (equal (utils:range 2 4 2) '(2)))
  (is (equal (utils:range 3 3 2) '()))
  (is (equal (utils:range 3 4 2) '(3)))
  (is (equal (utils:range 4 4 2) '()))
  ;; Triple input (step 3)
  (is (equal (utils:range 0 0 3) '()))
  (is (equal (utils:range 0 1 3) '(0)))
  (is (equal (utils:range 0 2 3) '(0)))
  (is (equal (utils:range 0 3 3) '(0)))
  (is (equal (utils:range 0 4 3) '(0 3)))
  (is (equal (utils:range 0 5 3) '(0 3)))
  (is (equal (utils:range 0 6 3) '(0 3)))
  (is (equal (utils:range 0 7 3) '(0 3 6)))
  (is (equal (utils:range 0 8 3) '(0 3 6)))
  (is (equal (utils:range 0 9 3) '(0 3 6)))
  (is (equal (utils:range 1 1 3) '()))
  (is (equal (utils:range 1 2 3) '(1)))
  (is (equal (utils:range 1 3 3) '(1)))
  (is (equal (utils:range 1 4 3) '(1)))
  (is (equal (utils:range 1 5 3) '(1 4)))
  (is (equal (utils:range 1 6 3) '(1 4)))
  (is (equal (utils:range 1 7 3) '(1 4)))
  (is (equal (utils:range 1 8 3) '(1 4 7)))
  (is (equal (utils:range 1 9 3) '(1 4 7)))
  (is (equal (utils:range 2 2 3) '()))
  (is (equal (utils:range 2 3 3) '(2)))
  (is (equal (utils:range 2 4 3) '(2)))
  (is (equal (utils:range 2 5 3) '(2)))
  (is (equal (utils:range 2 6 3) '(2 5)))
  (is (equal (utils:range 2 7 3) '(2 5)))
  (is (equal (utils:range 2 8 3) '(2 5)))
  (is (equal (utils:range 2 9 3) '(2 5 8)))
  (is (equal (utils:range 3 3 3) '()))
  (is (equal (utils:range 3 4 3) '(3)))
  (is (equal (utils:range 3 5 3) '(3)))
  (is (equal (utils:range 3 6 3) '(3)))
  (is (equal (utils:range 3 7 3) '(3 6)))
  (is (equal (utils:range 3 8 3) '(3 6)))
  (is (equal (utils:range 3 9 3) '(3 6)))
  (is (equal (utils:range 4 4 3) '()))
  (is (equal (utils:range 4 5 3) '(4)))
  (is (equal (utils:range 4 6 3) '(4)))
  (is (equal (utils:range 4 7 3) '(4)))
  (is (equal (utils:range 4 8 3) '(4 7)))
  (is (equal (utils:range 4 9 3) '(4 7)))
  ;; Triple input (step -2)
  (is (equal (utils:range 2 +2 -2) '()))
  (is (equal (utils:range 2 +1 -2) '(2)))
  (is (equal (utils:range 2 +0 -2) '(2)))
  (is (equal (utils:range 2 -1 -2) '(2 0)))
  (is (equal (utils:range 2 -2 -2) '(2 0)))
  (is (equal (utils:range 1 +1 -2) '()))
  (is (equal (utils:range 1 +0 -2) '(1)))
  (is (equal (utils:range 1 -1 -2) '(1)))
  (is (equal (utils:range 1 -2 -2) '(1 -1)))
  (is (equal (utils:range 1 -3 -2) '(1 -1)))
  (is (equal (utils:range 0 +0 -2) '()))
  (is (equal (utils:range 0 -1 -2) '(0)))
  (is (equal (utils:range 0 -2 -2) '(0)))
  (is (equal (utils:range 0 -3 -2) '(0 -2)))
  (is (equal (utils:range 0 -4 -2) '(0 -2)))
  ;; Triple input (step -3)
  (is (equal (utils:range +2 +2 -3) '()))
  (is (equal (utils:range +2 +1 -3) '(2)))
  (is (equal (utils:range +2 +0 -3) '(2)))
  (is (equal (utils:range +2 -1 -3) '(2)))
  (is (equal (utils:range +2 -2 -3) '(2 -1)))
  (is (equal (utils:range +2 -3 -3) '(2 -1)))
  (is (equal (utils:range +2 -4 -3) '(2 -1)))
  (is (equal (utils:range +2 -5 -3) '(2 -1 -4)))
  (is (equal (utils:range +2 -6 -3) '(2 -1 -4)))
  (is (equal (utils:range +2 -7 -3) '(2 -1 -4)))
  (is (equal (utils:range +1 +1 -3) '()))
  (is (equal (utils:range +1 +0 -3) '(1)))
  (is (equal (utils:range +1 -1 -3) '(1)))
  (is (equal (utils:range +1 -2 -3) '(1)))
  (is (equal (utils:range +1 -3 -3) '(1 -2)))
  (is (equal (utils:range +1 -4 -3) '(1 -2)))
  (is (equal (utils:range +1 -5 -3) '(1 -2)))
  (is (equal (utils:range +1 -6 -3) '(1 -2 -5)))
  (is (equal (utils:range +1 -7 -3) '(1 -2 -5)))
  (is (equal (utils:range +0 +0 -3) '()))
  (is (equal (utils:range +0 -1 -3) '(0)))
  (is (equal (utils:range +0 -2 -3) '(0)))
  (is (equal (utils:range +0 -3 -3) '(0)))
  (is (equal (utils:range +0 -4 -3) '(0 -3)))
  (is (equal (utils:range +0 -5 -3) '(0 -3)))
  (is (equal (utils:range +0 -6 -3) '(0 -3)))
  (is (equal (utils:range +0 -7 -3) '(0 -3 -6)))
  (is (equal (utils:range -1 -1 -3) '()))
  (is (equal (utils:range -1 -2 -3) '(-1)))
  (is (equal (utils:range -1 -3 -3) '(-1)))
  (is (equal (utils:range -1 -4 -3) '(-1)))
  (is (equal (utils:range -1 -5 -3) '(-1 -4)))
  (is (equal (utils:range -1 -6 -3) '(-1 -4)))
  (is (equal (utils:range -1 -7 -3) '(-1 -4)))
  (is (equal (utils:range -2 -2 -3) '()))
  (is (equal (utils:range -2 -3 -3) '(-2)))
  (is (equal (utils:range -2 -4 -3) '(-2)))
  (is (equal (utils:range -2 -5 -3) '(-2)))
  (is (equal (utils:range -2 -6 -3) '(-2 -5)))
  (is (equal (utils:range -2 -7 -3) '(-2 -5))))
