(in-suite utils)

(def-suite* iteration :in utils)

(defmacro with-var= ((&rest var-def-tests) &body body)
  `(let ,(loop for v in var-def-tests collect `(,(first v) ,(second v)))
     ,@body
     ,(if (= (length var-def-tests) 1)
          `(is ,(third (car var-def-tests)))
          `(is (and ,@(mapcar #'third var-def-tests))))))

(test for-range
  ;; Stop only, integer, no interruption

  ;; Return value
  (is (= 0 (utils:for-range (i 0) nil)))
  (is (= 1 (utils:for-range (i 1) nil)))
  (is (= 2 (utils:for-range (i 2) nil)))
  (is (= 5 (utils:for-range (i 5) nil)))
  ;; Sum iterated variable
  (with-var= ((x 0 (= 5050 x)))
    (utils:for-range (i 100) (incf x (1+ i))))
  ;; Collect iterated variable
  (with-var= ((x nil (equal '(0 1 2 3 4) x)))
    (utils:for-range (i 5) (setf x (append x (list i)))))
  ;; Change itarated variable
  (with-var= ((x 0 (= 5050 x)))
    (utils:for-range (i 100) (incf i) (incf x i)))

  ;; Stop only, integer, break

  ;; Return value
  (is (= 0 (utils:for-range (i 0) break)))
  (is (= 0 (utils:for-range (i 1) break)))
  (is (= 1 (utils:for-range (i 2) (when (= i 1) break))))
  (is (= 3 (utils:for-range (i 5) (when (= i 3) break))))
  ;; Sum iterated variable
  (with-var= ((x 0 (= 5050 x)))
    (utils:for-range (i 200) (when (= i 100) break) (incf x (1+ i))))
  ;; Collect iterated variable
  (with-var= ((x nil (equal '(0 1 2 3 4) x)))
    (utils:for-range (i 10) (when (= i 5) break) (setf x (append x (list i)))))
  ;; Change itarated variable
  (with-var= ((x 0 (= 5050 x)))
    (utils:for-range (i 200) (incf i) (when (= i 101) break) (incf x i)))

  ;; Stop only, integer, continue

  ;; Return value
  (is (= 0 (utils:for-range (i 0) continue)))
  (is (= 1 (utils:for-range (i 1) continue)))
  (is (= 2 (utils:for-range (i 2) (when (= i 1) continue))))
  (is (= 5 (utils:for-range (i 5) (when (= i 4) continue))))
  ;; Sum iterated variable
  (with-var= ((x 0 (= 5000 x)))
    (utils:for-range (i 100) (when (= i 49) continue) (incf x (1+ i))))
  ;; Collect iterated variable
  (with-var= ((x nil (equal '(0 1 3 4) x)))
    (utils:for-range (i 5) (when (= i 2) continue) (setf x (append x (list i)))))
  ;; Change itarated variable
  (with-var= ((x 0 (= 5000 x)))
    (utils:for-range (i 100) (incf i) (when (= i 50) continue) (incf x i)))

  ;; Start, stop, step, integer, no interruption

  ;; Return value
  (is (= 0 (utils:for-range (i 0 0 0) nil)))
  (is (= 5 (utils:for-range (i 3 4 2) nil)))
  (is (= 7 (utils:for-range (i 3 7 2) nil)))
  (is (= -11 (utils:for-range (i 5 -10 -2) nil)))
  ;; Sum iterated variable
  (with-var= ((x 0 (= (loop for k upfrom 10 below 100 by 5 sum k) x)))
    (utils:for-range (i 10 100 5) (incf x i)))
  ;; Collect iterated variable
  (with-var= ((x nil (equal '(15 13 11 9 7 5 3 1 -1 -3 -5 -7 -9) x)))
    (utils:for-range (i 15 -10 -2) (setf x (append x (list i)))))
  ;; Change itarated variable
  (with-var= ((x 0 (= (loop for k upfrom -20 below 53 by 4 sum (1+ k)) x)))
    (utils:for-range (i -20 53 4) (incf i) (incf x i)))

  ;; Start, stop, step, integer, continue

  ;; Return value
  (is (= 0 (utils:for-range (i 0 0 0) (when (= i 0) continue))))
  (is (= 5 (utils:for-range (i 3 4 2) (when (= i 3) continue))))
  (is (= 7 (utils:for-range (i 3 7 2) (when (= i 5) continue))))
  (is (= -11 (utils:for-range (i 5 -10 -2) (when (= i -1) continue))))
  ;; Sum iterated variable
  (with-var= ((x 0 (= (- (loop for k upfrom 10 below 100 by 5 sum k) 65) x)))
    (utils:for-range (i 10 100 5) (when (= i 65) continue) (incf x i)))
  ;; Collect iterated variable
  (with-var= ((x nil (equal '(15 13 11 9 5 3 1 -1 -3 -5 -7 -9) x)))
    (utils:for-range (i 15 -10 -2) (when (= i 7) continue) (setf x (append x (list i)))))
  ;; Change itarated variable
  (with-var= ((x 0 (= (- (loop for k upfrom -20 below 53 by 4 sum (1+ k)) -11) x)))
    (utils:for-range (i -20 53 4) (incf i) (when (= i -11) continue) (incf x i)))

  ;; Start, stop, step, integer, break

  ;; Return value
  (is (= 0 (utils:for-range (i 0 0 0) break)))
  (is (= 3 (utils:for-range (i 3 4 2) break)))
  (is (= 5 (utils:for-range (i 3 7 2) (when (= i 5) break))))
  (is (= -1 (utils:for-range (i 5 -10 -2) (when (= i -1) break))))
  ;; Sum iterated variable
  (with-var= ((x 0 (= (loop for k upfrom 10 below 65 by 5 sum k) x)))
    (utils:for-range (i 10 100 5) (when (= i 65) break) (incf x i)))
  ;; Collect iterated variable
  (with-var= ((x nil (equal '(15 13 11 9 7 5 3 1 -1 -3) x)))
    (utils:for-range (i 15 -10 -2) (when (= i -5) break) (setf x (append x (list i)))))
  ;; Change itarated variable
  (with-var= ((x 0 (= (loop for k upfrom -20 below 12 by 4 sum (1+ k)) x)))
    (utils:for-range (i -20 53 4) (incf i) (when (= i 13) break) (incf x i)))

  ;; Float
  (with-var= ((x nil (equal (loop for f downfrom 5d0 above -5d0 by 0.125d0 collect f) x)))
    (utils:for-range (f 5d0 -5d0 -0.125d0) (setf x (append x (list f))))))

(test foreach
  ;; Empty sequence

  ;; Return value
  (is (null (utils:foreach (i '()) nil)))
  (is (null (utils:foreach (i #()) nil)))
  (is (null (utils:foreach (i "") nil)))
  ;; Sum iterated variable
  (with-var= ((x 0 (= 0 x)))
    (utils:foreach (i '()) (incf x i)))
  (with-var= ((x 0 (= 0 x)))
    (utils:foreach (i #()) (incf x i)))
  ;; Collect iterated variable
  (with-var= ((x nil (equal '() x)))
    (utils:foreach (i '()) (setf x (append x (list i)))))
  (with-var= ((x nil (equal '() x)))
    (utils:foreach (i #()) (setf x (append x (list i)))))
  (with-var= ((x "" (string= "" x)))
    (utils:foreach (i "") (concatenate #'string x (string i))))
  ;; Change itarated variable
  (with-var= ((x 0 (= 0 x)))
    (utils:foreach (i '()) (incf i) (incf x i)))
  (with-var= ((x 0 (= 0 x)))
    (utils:foreach (i #()) (incf i) (incf x i)))

  ;; Nonempty sequence

  ;; Return value
  (is (null (utils:foreach (i '(1 2 3 4 5)) nil))) ; FIXME
  (is (null (utils:foreach (i #(5 6 7 8 9)) nil))) ; FIXME
  (is (null (utils:foreach (i "abcde") nil))) ; FIXME
  ;; Sum iterated variable
  (with-var= ((x 0 (= 15 x)))
    (utils:foreach (i '(1 2 3 4 5)) (incf x i)))
  (with-var= ((x 0 (= 35 x)))
    (utils:foreach (i #(5 6 7 8 9)) (incf x i)))
  ;; Collect iterated variable
  (with-var= ((x nil (equal '(1 2 3 4 5) x)))
    (utils:foreach (i '(1 2 3 4 5)) (setf x (append x (list i)))))
  (with-var= ((x nil (equal '(5 6 7 8 9) x)))
    (utils:foreach (i #(5 6 7 8 9)) (setf x (append x (list i)))))
  (with-var= ((x "" (string= "abcdef" x)))
    (utils:foreach (i "abcdef") (setf x (concatenate 'string x (string i)))))
  ;; Change itarated variable
  (with-var= ((x 0 (= 20 x)))
    (utils:foreach (i '(1 2 3 4 5)) (incf i) (incf x i)))
  (with-var= ((x 0 (= 40 x)))
    (utils:foreach (i #(5 6 7 8 9)) (incf i) (incf x i)))

  ;; FIXME
  )
