(in-suite utils)

(def-suite* string :in utils)

(test string=
  (is (string= (utils:mkstr 1 2 3) "123"))
  (is (string= (utils:strcat "") ""))
  (is (string= (utils:strcat "abc") "abc"))
  (is (string= (utils:strcat "abc" "def") "abcdef"))
  (is (string= (utils:strcat "abc" "def" "ghi" "jkl" "mno" "..." "xyz") "abcdefghijklmno...xyz")))

(test strsplit
  (is (equal (utils:strsplit "a b c" ",") '("a b c")))
  (is (equal (utils:strsplit "a,b,c" ",") '("a" "b" "c")))
  (is (equal (utils:strsplit "a,b.c" ",") '("a" "b.c")))
  (is (equal (utils:strsplit "a, b, c" ",") '("a" " b" " c")))
  (is (equal (utils:strsplit "a<->b<->c" "<->") '("a" "b" "c"))))

(test strjoin
  (is (equal (utils:strjoin "," "") ""))
  (is (equal (utils:strjoin "," "a") "a"))
  (is (equal (utils:strjoin "," "a" "b" "c" "d") "a,b,c,d"))
  (is (equal (utils:strjoin "<->" "a" "b" "c" "d") "a<->b<->c<->d")))

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
