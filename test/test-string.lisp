(in-suite utils)

(def-suite* string :in utils)

(test string=
  (is (string= (utils:mkstr 1 2 3) "123"))
  (is (string= (utils:strcat "") ""))
  (is (string= (utils:strcat "abc") "abc"))
  (is (string= (utils:strcat "abc" "def") "abcdef"))
  (is (string= (utils:strcat "abc" "def" "ghi" "jkl" "mno" "..." "xyz") "abcdefghijklmno...xyz")))

(test strsplit
  ;; delimiter is default
  (is (equal (utils:strsplit "") '("")))
  (is (equal (utils:strsplit "  ") '("" "" "")))
  (is (equal (utils:strsplit "abc") '("abc")))
  (is (equal (utils:strsplit "a b c") '("a" "b" "c")))
  ;; delimiter is char
  (is (equal (utils:strsplit "" #\,) '("")))
  (is (equal (utils:strsplit ",," #\,) '("" "" "")))
  (is (equal (utils:strsplit "a b c" #\,) '("a b c")))
  (is (equal (utils:strsplit "a,b,c" #\,) '("a" "b" "c")))
  (is (equal (utils:strsplit "a,b.c" #\,) '("a" "b.c")))
  (is (equal (utils:strsplit "a, b, c" #\,) '("a" " b" " c")))
  ;; delimiter is string
  (is (equal (utils:strsplit "" ",") '("")))
  (is (equal (utils:strsplit ",," ",") '("" "" "")))
  (is (equal (utils:strsplit "a b c" ",") '("a b c")))
  (is (equal (utils:strsplit "a,b,c" ",") '("a" "b" "c")))
  (is (equal (utils:strsplit "a,b.c" ",") '("a" "b.c")))
  (is (equal (utils:strsplit "a, b, c" ",") '("a" " b" " c")))
  (is (equal (utils:strsplit "a<->b<->c" "<->") '("a" "b" "c")))
  ;; delimiter is list of characters
  (is (equal (utils:strsplit "" '(#\, #\. #\;)) '("")))
  (is (equal (utils:strsplit ",," '(#\, #\. #\;)) '("" "" "")))
  (is (equal (utils:strsplit ".." '(#\, #\. #\;)) '("" "" "")))
  (is (equal (utils:strsplit ";;" '(#\, #\. #\;)) '("" "" "")))
  (is (equal (utils:strsplit ",.;" '(#\, #\. #\;)) '("" "" "" "")))
  (is (equal (utils:strsplit ".,;" '(#\, #\. #\;)) '("" "" "" "")))
  (is (equal (utils:strsplit ",;." '(#\, #\. #\;)) '("" "" "" "")))
  (is (equal (utils:strsplit ".;," '(#\, #\. #\;)) '("" "" "" "")))
  (is (equal (utils:strsplit ";,." '(#\, #\. #\;)) '("" "" "" "")))
  (is (equal (utils:strsplit ";.," '(#\, #\. #\;)) '("" "" "" "")))
  (is (equal (utils:strsplit "a;b;c" '(#\, #\. #\;)) '("a" "b" "c")))
  (is (equal (utils:strsplit "a,b,c" '(#\, #\. #\;)) '("a" "b" "c")))
  (is (equal (utils:strsplit "a.b.c" '(#\, #\. #\;)) '("a" "b" "c")))
  (is (equal (utils:strsplit "a b c" '(#\, #\. #\;)) '("a b c")))
  ;; delimiter is test function
  (is (equal (utils:strsplit "" #'digit-char-p) '("")))
  (is (equal (utils:strsplit "11" #'digit-char-p) '("" "" "")))
  (is (equal (utils:strsplit "55" #'digit-char-p) '("" "" "")))
  (is (equal (utils:strsplit "99" #'digit-char-p) '("" "" "")))
  (is (equal (utils:strsplit "19" #'digit-char-p) '("" "" "")))
  (is (equal (utils:strsplit "91" #'digit-char-p) '("" "" "")))
  (is (equal (utils:strsplit "a1b2c3" #'digit-char-p) '("a" "b" "c" "")))
  (is (equal (utils:strsplit "1a2b3c" #'digit-char-p) '("" "a" "b" "c"))))

(test strjoin
  (is (equal (utils:strjoin "," "") ""))
  (is (equal (utils:strjoin "," "a") "a"))
  (is (equal (utils:strjoin "," "a" "b" "c" "d") "a,b,c,d"))
  (is (equal (utils:strjoin "<->" "a" "b" "c" "d") "a<->b<->c<->d")))

(test ljust
  (loop for j below 5 do
    (loop for i below 5 for str = (make-string i :initial-element #\a) do
      (is (string= (utils:ljust str j) (concatenate 'string str (make-string (max 0 (- j i)) :initial-element #\Space))))))
  (loop for j below 5 do
    (loop for i below 5 for str = (make-string i :initial-element #\a) do
      (is (string= (utils:ljust str j #\0) (concatenate 'string str (make-string (max 0 (- j i)) :initial-element #\0)))))))

(test rjust
  (loop for j below 5 do
    (loop for i below 5 for str = (make-string i :initial-element #\a) do
      (is (string= (utils:rjust str j) (concatenate 'string (make-string (max 0 (- j i)) :initial-element #\Space) str)))))
  (loop for j below 5 do
    (loop for i below 5 for str = (make-string i :initial-element #\a) do
      (is (string= (utils:rjust str j #\0) (concatenate 'string (make-string (max 0 (- j i)) :initial-element #\0) str))))))

(test remove-prefix
  (is (string= (utils:remove-prefix "" "") ""))
  (is (string= (utils:remove-prefix "" "a") ""))
  (is (string= (utils:remove-prefix "" "ab") ""))
  (is (string= (utils:remove-prefix "" "abc") ""))
  (is (string= (utils:remove-prefix "abcdefghabcdefgh" "") "abcdefghabcdefgh"))
  (is (string= (utils:remove-prefix "abcdefghabcdefgh" "b") "abcdefghabcdefgh"))
  (is (string= (utils:remove-prefix "abcdefghabcdefgh" "bc") "abcdefghabcdefgh"))
  (is (string= (utils:remove-prefix "abcdefghabcdefgh" "bcd") "abcdefghabcdefgh"))
  (is (string= (utils:remove-prefix "abcdefghabcdefgh" "a") "bcdefghabcdefgh"))
  (is (string= (utils:remove-prefix "abcdefghabcdefgh" "ab") "cdefghabcdefgh"))
  (is (string= (utils:remove-prefix "abcdefghabcdefgh" "abc") "defghabcdefgh"))
  (is (string= (utils:remove-prefix "abcdefghabcdefgh" "abcd") "efghabcdefgh")))

(test remove-suffix
  (is (string= (utils:remove-suffix "" "") ""))
  (is (string= (utils:remove-suffix "" "h") ""))
  (is (string= (utils:remove-suffix "" "gh") ""))
  (is (string= (utils:remove-suffix "" "fgh") ""))
  (is (string= (utils:remove-suffix "abcdefghabcdefgh" "") "abcdefghabcdefgh"))
  (is (string= (utils:remove-suffix "abcdefghabcdefgh" "g") "abcdefghabcdefgh"))
  (is (string= (utils:remove-suffix "abcdefghabcdefgh" "fg") "abcdefghabcdefgh"))
  (is (string= (utils:remove-suffix "abcdefghabcdefgh" "efg") "abcdefghabcdefgh"))
  (is (string= (utils:remove-suffix "abcdefghabcdefgh" "h") "abcdefghabcdefg"))
  (is (string= (utils:remove-suffix "abcdefghabcdefgh" "gh") "abcdefghabcdef"))
  (is (string= (utils:remove-suffix "abcdefghabcdefgh" "fgh") "abcdefghabcde"))
  (is (string= (utils:remove-suffix "abcdefghabcdefgh" "efgh") "abcdefghabcd")))

(test is-empty-string
  (is (utils:empty-string-p ""))
  (is-false (utils:empty-string-p "a"))
  (is-false (utils:empty-string-p ()))
  (is-false (utils:empty-string-p #())))

(test strtrim
  (is (string= (utils:strtrim "abc") "abc"))
  (is (string= (utils:strtrim " abc") "abc"))
  (is (string= (utils:strtrim "  abc") "abc"))
  (is (string= (utils:strtrim "abc ") "abc"))
  (is (string= (utils:strtrim "abc  ") "abc"))
  (is (string= (utils:strtrim " abc ") "abc"))
  (is (string= (utils:strtrim "  abc  ") "abc"))
  (is (string= (utils:strtrim (utils:mkstr #\Newline "abc")) "abc"))
  (is (string= (utils:strtrim (utils:mkstr "abc" #\Newline)) "abc"))
  (is (string= (utils:strtrim (utils:mkstr #\Return "abc")) "abc"))
  (is (string= (utils:strtrim (utils:mkstr "abc" #\Return)) "abc"))
  (is (string= (utils:strtrim (utils:mkstr #\Tab "abc")) "abc"))
  (is (string= (utils:strtrim (utils:mkstr "abc" #\Tab)) "abc")))

(test string^=
  (is (utils:string^= "abcdef" ""))
  (is (utils:string^= "abcdef" "a"))
  (is (utils:string^= "abcdef" "ab"))
  (is (utils:string^= "abcdef" "abc"))
  (is (utils:string^= "abcdef" "abcd"))
  (is (utils:string^= "abcdef" "abcde"))
  (is (utils:string^= "abcdef" "abcdef"))
  (is-false (utils:string^= "abcdef" "abcdefg"))
  (is-false (utils:string^= " abcdef" "ab"))
  (is-false (utils:string^= "abcdef" "ac")))

(test string$=
  (is (utils:string$= "abcdef" ""))
  (is (utils:string$= "abcdef" "f"))
  (is (utils:string$= "abcdef" "ef"))
  (is (utils:string$= "abcdef" "def"))
  (is (utils:string$= "abcdef" "cdef"))
  (is (utils:string$= "abcdef" "bcdef"))
  (is (utils:string$= "abcdef" "abcdef"))
  (is-false (utils:string$= "abcdef" "xabcdef"))
  (is-false (utils:string$= "abcdef " "ef"))
  (is-false (utils:string$= "abcdef" "df")))

(test substrp
  (is (utils:substrp "" "abcd"))
  (is (utils:substrp "a" "abcd"))
  (is (utils:substrp "b" "abcd"))
  (is (utils:substrp "c" "abcd"))
  (is (utils:substrp "d" "abcd"))
  (is (utils:substrp "ab" "abcd"))
  (is (utils:substrp "bc" "abcd"))
  (is (utils:substrp "cd" "abcd"))
  (is (utils:substrp "abc" "abcd"))
  (is (utils:substrp "bcd" "abcd"))
  (is (utils:substrp "abcd" "abcd"))
  (is-false (utils:substrp "e" "abcd"))
  (is-false (utils:substrp "ad" "abcd"))
  (is-false (utils:substrp "ac" "abcd"))
  (is-false (utils:substrp "bd" "abcd"))
  (is-false (utils:substrp "abcde" "abcd")))

(test string-replace
  ;; Degenerate calls
  (is (string= (utils:string-replace "" "" "") ""))
  (is (string= (utils:string-replace "abcdef" "" "") "abcdef"))
  (is (string= (utils:string-replace "abcdef" "" "XYZ") "abcdef"))
  ;; Normal calls, some without actual replacement
  (is (string= (utils:string-replace "" "abc" "") ""))
  (is (string= (utils:string-replace "abc" "abc" "") ""))
  (is (string= (utils:string-replace "abd" "abc" "") "abd"))
  (is (string= (utils:string-replace "xyz" "abc" "") "xyz"))
  (is (string= (utils:string-replace "" "abc" "uvw") ""))
  (is (string= (utils:string-replace "abc" "abc" "uvw") "uvw"))
  (is (string= (utils:string-replace "abd" "abc" "uvw") "abd"))
  (is (string= (utils:string-replace "xyz" "abc" "uvw") "xyz"))
  ;; Replacement is shorter
  (is (string= (utils:string-replace "abc01234567" "abc" "") "01234567"))
  (is (string= (utils:string-replace "0123abc4567" "abc" "") "01234567"))
  (is (string= (utils:string-replace "01234567abc" "abc" "") "01234567"))
  ;; Replacement is exactly the same
  (is (string= (utils:string-replace "abc01234567" "abc" "abc") "abc01234567"))
  (is (string= (utils:string-replace "0123abc4567" "abc" "abc") "0123abc4567"))
  (is (string= (utils:string-replace "01234567abc" "abc" "abc") "01234567abc"))
  ;; Replacement is the same but twice
  (is (string= (utils:string-replace "abc01234567" "abc" "abcabc") "abcabc01234567"))
  (is (string= (utils:string-replace "0123abc4567" "abc" "abcabc") "0123abcabc4567"))
  (is (string= (utils:string-replace "01234567abc" "abc" "abcabc") "01234567abcabc"))
  ;; Replacement is longer
  (is (string= (utils:string-replace "abc01234567" "abc" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz01234567"))
  (is (string= (utils:string-replace "0123abc4567" "abc" "abcdefghijklmnopqrstuvwxyz") "0123abcdefghijklmnopqrstuvwxyz4567"))
  (is (string= (utils:string-replace "01234567abc" "abc" "abcdefghijklmnopqrstuvwxyz") "01234567abcdefghijklmnopqrstuvwxyz"))
  ;; Multiple replacements
  (is (string= (utils:string-replace "abc0abc1abc2abc3abc4abc5abc6abc7abc" "abc" "") "01234567"))
  (is (string= (utils:string-replace "abc0abc1abc2abc3abc4abc5abc6abc7abc" "a" "") "bc0bc1bc2bc3bc4bc5bc6bc7bc")))

(test parse-float
  ;; Nothing
  (signals error (utils:parse-float ""))
  ;; Single dividing character
  (signals error (utils:parse-float "."))
  (signals error (utils:parse-float "e"))
  (signals error (utils:parse-float "E"))
  ;; Only integer
  (is (= (utils:parse-float "3") 3d0))
  (is (= (utils:parse-float "+3") 3d0))
  (is (= (utils:parse-float "-3") -3d0))
  ;; Only integer with period
  (is (= (utils:parse-float "3.") 3d0))
  (is (= (utils:parse-float "+3.") 3d0))
  (is (= (utils:parse-float "-3.") -3d0))
  ;; Only decimals
  (is (= (utils:parse-float ".5") 0.5d0))
  (is (= (utils:parse-float "+.5") 0.5d0))
  (is (= (utils:parse-float "-.5") -0.5d0))
  (is (= (utils:parse-float "3.5") 3.5d0))
  (is (= (utils:parse-float "+3.5") 3.5d0))
  (is (= (utils:parse-float "-3.5") -3.5d0))
  (is (= (utils:parse-float "34.125") 34.125d0))
  (is (= (utils:parse-float "+34.125") 34.125d0))
  (is (= (utils:parse-float "-34.125") -34.125d0))
  ;; Only exponent
  (signals error (utils:parse-float "e1"))
  (signals error (utils:parse-float "E1"))
  (signals error (utils:parse-float "e+1"))
  (signals error (utils:parse-float "E+1"))
  (signals error (utils:parse-float "e-1"))
  (signals error (utils:parse-float "E-1"))
  ;; Partial exponent
  (signals error (utils:parse-float "e"))
  (signals error (utils:parse-float "E"))
  (signals error (utils:parse-float "1e"))
  (signals error (utils:parse-float "1E"))
  (signals error (utils:parse-float ".1e"))
  (signals error (utils:parse-float ".1E"))
  ;; Integer and exponent
  (is (= (utils:parse-float "5e0") 5d0))
  (is (= (utils:parse-float "+5e0") 5d0))
  (is (= (utils:parse-float "-5e0") -5d0))
  (is (= (utils:parse-float "5e2") 5d2))
  (is (= (utils:parse-float "+5e2") 5d2))
  (is (= (utils:parse-float "-5e2") -5d2))
  (is (= (utils:parse-float "5e+2") 5d2))
  (is (= (utils:parse-float "+5e+2") 5d2))
  (is (= (utils:parse-float "-5e+2") -5d2))
  (is (= (utils:parse-float "5e-2") 5d-2))
  (is (= (utils:parse-float "+5e-2") 5d-2))
  (is (= (utils:parse-float "-5e-2") -5d-2))
  ;; Decimals and exponent
  (is (= (utils:parse-float ".5e0") 0.5d0))
  (is (= (utils:parse-float "+.5e0") 0.5d0))
  (is (= (utils:parse-float "-.5e0") -0.5d0))
  (is (= (utils:parse-float ".5e2") 0.5d2))
  (is (= (utils:parse-float "+.5e2") 0.5d2))
  (is (= (utils:parse-float "-.5e2") -0.5d2))
  (is (= (utils:parse-float ".5e+2") 0.5d2))
  (is (= (utils:parse-float "+.5e+2") 0.5d2))
  (is (= (utils:parse-float "-.5e+2") -0.5d2))
  (is (= (utils:parse-float ".5e-2") 0.5d-2))
  (is (= (utils:parse-float "+.5e-2") 0.5d-2))
  (is (= (utils:parse-float "-.5e-2") -0.5d-2))
  ;; Full expression
  (is (= (utils:parse-float "23.125e2") 23.125d2))
  (is (= (utils:parse-float "+23.125e2") 23.125d2))
  (is (= (utils:parse-float "-23.125e2") -23.125d2))
  (is (= (utils:parse-float "23.125e+2") 23.125d2))
  (is (= (utils:parse-float "+23.125e+2") 23.125d2))
  (is (= (utils:parse-float "-23.125e+2") -23.125d2))
  (is (= (utils:parse-float "23.125e-2") 23.125d-2))
  (is (= (utils:parse-float "+23.125e-2") 23.125d-2))
  (is (= (utils:parse-float "-23.125e-2") -23.125d-2))
  ;; Excess #\. and #\e or #\E
  (signals error (utils:parse-float "+23.12.5e-2"))
  (signals error (utils:parse-float "+23.12e5e-2"))
  (signals error (utils:parse-float "+23.12E5e-2"))
  (signals error (utils:parse-float "+2e3.125e-2"))
  (signals error (utils:parse-float "+2E3.125e-2"))
  ;; Invalid characters
  (signals error (utils:parse-float "+2x3.125e-2"))
  (signals error (utils:parse-float "+23.12x5e-2"))
  (signals error (utils:parse-float "+23.125e-2x")))
