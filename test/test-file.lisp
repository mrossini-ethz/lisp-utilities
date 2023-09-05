(in-suite utils)

(def-suite* file :in utils)

;;; Helper functions

(defun cat (&rest args)
  (apply #'concatenate 'string (mapcar #'string args)))

(defun file= (content filename)
  (with-open-file (f filename :element-type '(unsigned-byte 8))
    (when (= (file-length f) (length content))
      (loop for b1 across content for b2 = (read-byte f) always (= b1 b2)))))

(defun hexstr->bytes (hexstr)
  (let* ((hexstr (remove #\Space hexstr)) (n (/ (length hexstr) 2)) (result (make-array `(,n) :element-type '(unsigned-byte 8))))
    (loop for i below n do (setf (elt result i) (parse-integer hexstr :start (* i 2) :end (+ (* i 2) 2) :radix 16)))
    result))

(defun save-hexfile (filename hexstr)
  (with-open-file (f filename :direction :output :if-exists :error :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (write-sequence (hexstr->bytes hexstr) f)))

(defmacro with-hexfile ((filevar hexstr) &body body)
  `(let ((,filevar "testfile.txt"))
     (save-hexfile ,filevar ,hexstr)
     ,@body
     (delete-file ,filevar)))

(defmacro is-file= ((filevar hexstr) &body body)
  (let ((datavar (gensym)))
    `(let ((,filevar "testfile.txt") (,datavar (hexstr->bytes ,hexstr)))
       (when (probe-file ,filevar)
         (error "File ~a exists" ,filevar))
       ,@body
       (is (file= ,datavar ,filevar))
       (delete-file ,filevar))))

;;; Actual tests

(test read-file-to-string
  (let* ((non-ascii-chars-1 (cat #\LATIN_SMALL_LETTER_A_WITH_DIAERESIS #\LATIN_SMALL_LETTER_O_WITH_DIAERESIS #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\MICRO_SIGN #\GREEK_SMALL_LETTER_SIGMA))
         (non-ascii-chars-2 (cat #\LATIN_SMALL_LETTER_A_WITH_DIAERESIS #\LATIN_SMALL_LETTER_O_WITH_DIAERESIS #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\MICRO_SIGN))
         (non-ascii-string-1-lf (cat "hello world" #\Newline "foo bar baz" #\Newline non-ascii-chars-1 #\Newline))
         (non-ascii-string-2-lf (cat "hello world" #\Newline "foo bar baz" #\Newline non-ascii-chars-2 #\Newline))
         (non-ascii-string-1-crlf (cat "hello world" #\Return #\Newline "foo bar baz" #\Return #\Newline non-ascii-chars-1 #\Return #\Newline))
         (non-ascii-string-2-crlf (cat "hello world" #\Return #\Newline "foo bar baz" #\Return #\Newline non-ascii-chars-2 #\Return #\Newline)))
    ;; File does not exist
    (signals error (utils:read-file-to-string "this-file-does-not-exist"))
    ;; Default encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a") ; utf-8, LF
      (is (string= (utils:read-file-to-string f) non-ascii-string-1-lf)))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CRLF
      (is (string= (utils:read-file-to-string f) non-ascii-string-1-crlf)))
    ;; UTF-8 encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a") ; utf-8, LF
      (is (string= (utils:read-file-to-string f :encoding :utf-8) non-ascii-string-1-lf)))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CRLF
      (is (string= (utils:read-file-to-string f :encoding :utf-8) non-ascii-string-1-crlf)))
    ;; ISO-8869-1 encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a e4 f6 fc b5 0a") ; iso-8859-1, LF
      (is (string= (utils:read-file-to-string f :encoding :iso-8859-1) non-ascii-string-2-lf)))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a e4 f6 fc b5 0d 0a") ; iso-8859-1, CRLF
      (is (string= (utils:read-file-to-string f :encoding :iso-8859-1) non-ascii-string-2-crlf)))))

(test write-file-from-string
  ;; Default encoding
  (is-file= (f "")
    (utils:write-file-from-string f ""))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64")
    (utils:write-file-from-string f (cat "hello world")))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a")
    (utils:write-file-from-string f (cat "hello world" #\Newline)))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a")
    (utils:write-file-from-string f (cat "hello world" #\Return #\Newline)))
  (is-file= (f "c3 bc 0a")
    (utils:write-file-from-string f (cat #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\Newline)))
  ;; UTF-8 encoding
  (is-file= (f "")
    (utils:write-file-from-string f "" :encoding :utf-8))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64")
    (utils:write-file-from-string f (cat "hello world") :encoding :utf-8))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a")
    (utils:write-file-from-string f (cat "hello world" #\Newline) :encoding :utf-8))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a")
    (utils:write-file-from-string f (cat "hello world" #\Return #\Newline) :encoding :utf-8))
  (is-file= (f "c3 bc 0a")
    (utils:write-file-from-string f (cat #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\Newline) :encoding :utf-8))
  ;; ISO-8859-1 encoding
  (is-file= (f "")
    (utils:write-file-from-string f "" :encoding :iso-8859-1))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64")
    (utils:write-file-from-string f (cat "hello world") :encoding :iso-8859-1))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a")
    (utils:write-file-from-string f (cat "hello world" #\Newline) :encoding :iso-8859-1))
  (is-file= (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a")
    (utils:write-file-from-string f (cat "hello world" #\Return #\Newline) :encoding :iso-8859-1))
  (is-file= (f "fc 0a")
    (utils:write-file-from-string f (cat #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\Newline) :encoding :iso-8859-1)))

(test update-file-from-string
  ;; Empty -> Empty
  (with-hexfile (f "")
    (utils:update-file-from-string f "")
    (is (file= #() f)))
  ;; Empty -> Some
  (with-hexfile (f "")
    (utils:update-file-from-string f "hello world")
    (is (file= (hexstr->bytes "68 65 6c 6c 6f 20 77 6f 72 6c 64") f)))
  ;; Some -> Empty
  (with-hexfile (f "66 6f 6f 0a 62 61 72 0a 62 61 7a 0a")
    (utils:update-file-from-string f "")
    (is (file= #() f)))
  ;; A -> A
  (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64")
    (utils:update-file-from-string f "hello world")
    (is (file= (hexstr->bytes "68 65 6c 6c 6f 20 77 6f 72 6c 64") f)))
  ;; B -> A
  (with-hexfile (f "66 6f 6f 0a 62 61 72 0a 62 61 7a 0a")
    (utils:update-file-from-string f "hello world")
    (is (file= (hexstr->bytes "68 65 6c 6c 6f 20 77 6f 72 6c 64") f))))

(test read-file-to-lines
  (let ((non-ascii-chars-1 (cat #\LATIN_SMALL_LETTER_A_WITH_DIAERESIS  #\LATIN_SMALL_LETTER_O_WITH_DIAERESIS  #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\MICRO_SIGN #\GREEK_SMALL_LETTER_SIGMA))
        (non-ascii-chars-2 (cat #\LATIN_SMALL_LETTER_A_WITH_DIAERESIS  #\LATIN_SMALL_LETTER_O_WITH_DIAERESIS  #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\MICRO_SIGN)))
    ;; File does not exist
    (signals error (utils:read-file-to-lines "this-file-does-not-exist"))
    ;; Default encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a") ; utf-8, LF
      (is (equalp (utils:read-file-to-lines f) (list "hello world" "" "foo bar baz" non-ascii-chars-1))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CRLF
      (is (equalp (utils:read-file-to-lines f) (list "hello world" "" "foo bar baz" non-ascii-chars-1))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, mixed LF and CRLF
      (is (equalp (utils:read-file-to-lines f) (list "hello world" "" "foo bar baz" non-ascii-chars-1))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CR + CRLF
      (is (equalp (utils:read-file-to-lines f) (list "hello world" "" (cat "foo bar baz" #\Return) non-ascii-chars-1))))
    ;; UTF-8 encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a") ; utf-8, LF
      (is (equalp (utils:read-file-to-lines f :encoding :utf-8) (list "hello world" "" "foo bar baz" non-ascii-chars-1))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CRLF
      (is (equalp (utils:read-file-to-lines f :encoding :utf-8) (list "hello world" "" "foo bar baz" non-ascii-chars-1))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, mixed LF and CRLF
      (is (equalp (utils:read-file-to-lines f :encoding :utf-8) (list "hello world" "" "foo bar baz" non-ascii-chars-1))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CR + CRLF
      (is (equalp (utils:read-file-to-lines f :encoding :utf-8) (list "hello world" "" (cat "foo bar baz" #\Return) non-ascii-chars-1))))
    ;; ISO-8859-1 encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a e4 f6 fc b5 0a") ; iso-8859-1, LF
      (is (equalp (utils:read-file-to-lines f :encoding :iso-8859-1) (list "hello world" "" "foo bar baz" non-ascii-chars-2))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a e4 f6 fc b5 0d 0a") ; iso-8859-1, CRLF
      (is (equalp (utils:read-file-to-lines f :encoding :iso-8859-1) (list "hello world" "" "foo bar baz" non-ascii-chars-2))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a e4 f6 fc b5 0d 0a") ; iso-8859-1, mixed LF and CRLF
      (is (equalp (utils:read-file-to-lines f :encoding :iso-8859-1) (list "hello world" "" "foo bar baz" non-ascii-chars-2))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0d 0a e4 f6 fc b5 0d 0a") ; iso-8859-1, CR + CRLF
      (is (equalp (utils:read-file-to-lines f :encoding :iso-8859-1) (list "hello world" "" (cat "foo bar baz" #\Return) non-ascii-chars-2))))
    ))

(test write-file-from-lines
  (let ((line-list (list "Hello" "World" (cat #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS))))
    ;; Default EOL style
    (is-file= (filename (cat "48 65 6c 6c 6f" #+WIN32 "0d" "0a 57 6f 72 6c 64" #+WIN32 "0d" "0a c3 bc" #+WIN32 "0d" "0a")) ; utf-8, native
      (utils:write-file-from-lines filename line-list))
    (is-file= (filename (cat "48 65 6c 6c 6f" #+WIN32 "0d" "0a 57 6f 72 6c 64" #+WIN32 "0d" "0a c3 bc" #+WIN32 "0d" "0a")) ; utf-8, native
      (utils:write-file-from-lines filename line-list :encoding :utf-8))
    (is-file= (filename (cat "48 65 6c 6c 6f" #+WIN32 "0d" "0a 57 6f 72 6c 64" #+WIN32 "0d" "0a fc" #+WIN32 "0d" "0a")) ; iso-8859-1, native
      (utils:write-file-from-lines filename line-list :encoding :iso-8859-1))
    ;; Native EOL style
    (is-file= (filename (cat "48 65 6c 6c 6f" #+WIN32 "0d" "0a 57 6f 72 6c 64" #+WIN32 "0d" "0a c3 bc" #+WIN32 "0d" "0a")) ; utf-8, native
      (utils:write-file-from-lines filename line-list :eol-style :native))
    (is-file= (filename (cat "48 65 6c 6c 6f" #+WIN32 "0d" "0a 57 6f 72 6c 64" #+WIN32 "0d" "0a c3 bc" #+WIN32 "0d" "0a")) ; utf-8, native
      (utils:write-file-from-lines filename line-list :eol-style :native :encoding :utf-8))
    (is-file= (filename (cat "48 65 6c 6c 6f" #+WIN32 "0d" "0a 57 6f 72 6c 64" #+WIN32 "0d" "0a fc" #+WIN32 "0d" "0a")) ; iso-8859-1, native
      (utils:write-file-from-lines filename line-list :eol-style :native :encoding :iso-8859-1))
    ;; Unix EOL style
    (is-file= (filename (cat "48 65 6c 6c 6f" "0a 57 6f 72 6c 64 0a c3 bc 0a")) ; utf-8, unix
      (utils:write-file-from-lines filename line-list :eol-style :unix))
    (is-file= (filename (cat "48 65 6c 6c 6f" "0a 57 6f 72 6c 64 0a c3 bc 0a")) ; utf-8, unix
      (utils:write-file-from-lines filename line-list :eol-style :unix :encoding :utf-8))
    (is-file= (filename (cat "48 65 6c 6c 6f" "0a 57 6f 72 6c 64 0a fc 0a")) ; iso-8859-1, unix
      (utils:write-file-from-lines filename line-list :eol-style :unix :encoding :iso-8859-1))
    ;; LF EOL style
    (is-file= (filename (cat "48 65 6c 6c 6f" "0a 57 6f 72 6c 64 0a c3 bc 0a")) ; utf-8, unix
      (utils:write-file-from-lines filename line-list :eol-style :LF))
    (is-file= (filename (cat "48 65 6c 6c 6f" "0a 57 6f 72 6c 64 0a c3 bc 0a")) ; utf-8, unix
      (utils:write-file-from-lines filename line-list :eol-style :LF :encoding :utf-8))
    (is-file= (filename (cat "48 65 6c 6c 6f" "0a 57 6f 72 6c 64 0a fc 0a")) ; iso-8859-1, unix
      (utils:write-file-from-lines filename line-list :eol-style :LF :encoding :iso-8859-1))
    ;; Windows EOL style
    (is-file= (filename (cat "48 65 6c 6c 6f 0d 0a 57 6f 72 6c 64 0d 0a c3 bc 0d 0a")) ; utf-8, windows
      (utils:write-file-from-lines filename line-list :eol-style :windows))
    (is-file= (filename (cat "48 65 6c 6c 6f 0d 0a 57 6f 72 6c 64 0d 0a c3 bc 0d 0a")) ; utf-8, windows
      (utils:write-file-from-lines filename line-list :eol-style :windows :encoding :utf-8))
    (is-file= (filename (cat "48 65 6c 6c 6f 0d 0a 57 6f 72 6c 64 0d 0a fc 0d 0a")) ; iso-8859-1, windows
      (utils:write-file-from-lines filename line-list :eol-style :windows :encoding :iso-8859-1))
    ;; CRLF EOL style
    (is-file= (filename (cat "48 65 6c 6c 6f 0d 0a 57 6f 72 6c 64 0d 0a c3 bc 0d 0a")) ; utf-8, windows
      (utils:write-file-from-lines filename line-list :eol-style :CRLF))
    (is-file= (filename (cat "48 65 6c 6c 6f 0d 0a 57 6f 72 6c 64 0d 0a c3 bc 0d 0a")) ; utf-8, windows
      (utils:write-file-from-lines filename line-list :eol-style :CRLF :encoding :utf-8))
    (is-file= (filename (cat "48 65 6c 6c 6f 0d 0a 57 6f 72 6c 64 0d 0a fc 0d 0a")) ; iso-8859-1, windows
      (utils:write-file-from-lines filename line-list :eol-style :CRLF :encoding :iso-8859-1))))

(test read-file-to-vector
  ;; File does not exist
  (signals error (utils:read-file-to-vector "this-file-does-not-exist"))
  ;; Empty file
  (with-hexfile (f "")
    (is (equalp (utils:read-file-to-vector f) #())))
  ;; Non-empty file
  (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a") ; utf-8, LF
    (is (equalp (utils:read-file-to-vector f) (hexstr->bytes "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a")))))

(test write-file-from-vector
  ;; Empty file
  (is-file= (f "")
    (utils:write-file-from-vector f #()))
  ;; Non-empty file
  (is-file= (f "01 02 03 04 05")
    (utils:write-file-from-vector f #(1 2 3 4 5))))

(test for-line-in-file
  (let ((non-ascii-chars-1 (cat #\LATIN_SMALL_LETTER_A_WITH_DIAERESIS  #\LATIN_SMALL_LETTER_O_WITH_DIAERESIS  #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\MICRO_SIGN #\GREEK_SMALL_LETTER_SIGMA))
        (non-ascii-chars-2 (cat #\LATIN_SMALL_LETTER_A_WITH_DIAERESIS  #\LATIN_SMALL_LETTER_O_WITH_DIAERESIS  #\LATIN_SMALL_LETTER_U_WITH_DIAERESIS #\MICRO_SIGN)))
    ;; File does not exist
    (signals error (utils:for-line-in-file (line "this-file-does-not-exist") line))

    ;; Default encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a") ; utf-8, LF
      (let (result)
        (utils:for-line-in-file (line f)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-1) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CRLF
      (let (result)
        (utils:for-line-in-file (line f)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-1) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, mixed LF and CRLF
      (let (result)
        (utils:for-line-in-file (line f)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-1) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CR + CRLF
      (let (result)
        (utils:for-line-in-file (line f)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" (cat "foo bar baz" #\Return) non-ascii-chars-1) result))))
    ;; UTF-8 encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a") ; utf-8, LF
      (let (result)
        (utils:for-line-in-file (line f :encoding :utf-8)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-1) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CRLF
      (let (result)
        (utils:for-line-in-file (line f :encoding :utf-8)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-1) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, mixed LF and CRLF
      (let (result)
        (utils:for-line-in-file (line f :encoding :utf-8)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-1) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0d 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0d 0a") ; utf-8, CR + CRLF
      (let (result)
        (utils:for-line-in-file (line f :encoding :utf-8)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" (cat "foo bar baz" #\Return) non-ascii-chars-1) result))))
    ;; ISO-8859-1 encoding
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a e4 f6 fc b5 0a") ; iso-8859-1, LF
      (let (result)
        (utils:for-line-in-file (line f :encoding :iso-8859-1)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-2) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0a e4 f6 fc b5 0d 0a") ; iso-8859-1, CRLF
      (let (result)
        (utils:for-line-in-file (line f :encoding :iso-8859-1)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-2) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a e4 f6 fc b5 0d 0a") ; iso-8859-1, mixed LF and CRLF
      (let (result)
        (utils:for-line-in-file (line f :encoding :iso-8859-1)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" "foo bar baz" non-ascii-chars-2) result))))
    (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0d 0a 0d 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0d 0d 0a e4 f6 fc b5 0d 0a") ; iso-8859-1, CR + CRLF
      (let (result)
        (utils:for-line-in-file (line f :encoding :iso-8859-1)
          (setf result (append result (list line)))
          (setf line "nonsense"))
        (is (equal (list "hello world" "" (cat "foo bar baz" #\Return) non-ascii-chars-2) result))))))

(test file-name
  (is (null (utils:file-name "")))
  (is (null (utils:file-name "some/dir/")))
  (is (string= "file.1.dat" (utils:file-name "file.1.dat")))
  (is (string= "file.1.dat" (utils:file-name "./file.1.dat")))
  (is (string= "file.1.dat" (utils:file-name "some/dir/file.1.dat"))))

(test file-basename
  (is (null (utils:file-basename "")))
  (is (null (utils:file-basename "some/dir/")))
  (is (string= "file.1" (utils:file-basename "file.1.dat")))
  (is (string= "file.1" (utils:file-basename "./file.1.dat")))
  (is (string= "file.1" (utils:file-basename "some/dir/file.1.dat"))))

(test file-suffix
  (is (null (utils:file-suffix "")))
  (is (null (utils:file-suffix "some/dir/")))
  (is (string= "dat" (utils:file-suffix "file.1.dat")))
  (is (string= "dat" (utils:file-suffix "./file.1.dat")))
  (is (string= "dat" (utils:file-suffix "some/dir/file.1.dat"))))

(test file-directory
  (is (string= "./" (utils:file-directory "")))
  (is (string= "some/dir/" (utils:file-directory "some/dir/")))
  (is (string= "./" (utils:file-directory "file.1.dat")))
  (is (string= "./" (utils:file-directory "./file.1.dat")))
  (is (string= "some/dir/" (utils:file-directory "some/dir/file.1.dat"))))

(test filepath-split
  (is (equal '("./" nil nil) (multiple-value-list (utils:filepath-split ""))))
  (is (equal '("some/dir/" nil nil) (multiple-value-list (utils:filepath-split "some/dir/"))))
  (is (equal '("./" "file.1" "dat") (multiple-value-list (utils:filepath-split "file.1.dat"))))
  (is (equal '("./" "file.1" "dat") (multiple-value-list (utils:filepath-split "./file.1.dat"))))
  (is (equal '("some/dir/" "file.1" "dat") (multiple-value-list (utils:filepath-split "some/dir/file.1.dat")))))

(test with-filepath
  (is (equal '("./" nil nil) (utils:with-filepath (d f s) "" (list d f s))))
  (is (equal '("some/dir/" nil nil) (utils:with-filepath (d f s) "some/dir/" (list d f s))))
  (is (equal '("./" "file.1" "dat") (utils:with-filepath (d f s) "file.1.dat" (list d f s))))
  (is (equal '("./" "file.1" "dat") (utils:with-filepath (d f s) "./file.1.dat" (list d f s))))
  (is (equal '("some/dir/" "file.1" "dat") (utils:with-filepath (d f s) "some/dir/file.1.dat" (list d f s))))
  (is (equal '("./" nil) (utils:with-filepath (d f) "" (list d f))))
  (is (equal '("some/dir/" nil) (utils:with-filepath (d f) "some/dir/" (list d f))))
  (is (equal '("./" "file.1.dat") (utils:with-filepath (d f) "file.1.dat" (list d f))))
  (is (equal '("./" "file.1.dat") (utils:with-filepath (d f) "./file.1.dat" (list d f))))
  (is (equal '("some/dir/" "file.1.dat") (utils:with-filepath (d f) "some/dir/file.1.dat" (list d f)))))

(test file-exists-p
  (is (null (utils:file-exists-p "this-file-does-not-exist")))
  (with-hexfile (f "68 65 6c 6c 6f 20 77 6f 72 6c 64 0a 0a 66 6f 6f 20 62 61 72 20 62 61 7a 0a c3 a4 c3 b6 c3 bc c2 b5 cf 83 0a") ; utf-8, LF
    (is (utils:file-exists-p f))))
