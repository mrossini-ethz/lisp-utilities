(in-package :utils)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
(export 'mkstr)

(defun strcat (&rest strings)
  "Concatenates the given strings"
  (apply #'concatenate 'string strings))
(export 'strcat)

(defun strsplit (str &optional (delimiter '(#\Space #\Tab)))
  "Splits the string at each occurrence of `delimiter'."
  (typecase delimiter
    (string
     (let ((pos (search delimiter str)) (len (length delimiter)))
       (if pos
           (append (list (subseq str 0 pos)) (strsplit (subseq str (+ pos len)) delimiter))
           (list str))))
    (character
     (let ((pos (position delimiter str)))
       (if pos
           (append (list (subseq str 0 pos)) (strsplit (subseq str (1+ pos)) delimiter))
           (list str))))
    (list
     (let ((pos (position-if (lambda (c) (some (lambda (x) (char= x c)) delimiter)) str)))
       (if pos
           (append (list (subseq str 0 pos)) (strsplit (subseq str (1+ pos)) delimiter))
           (list str))))
    (function
     (let ((pos (position-if delimiter str)))
       (if pos
           (append (list (subseq str 0 pos)) (strsplit (subseq str (1+ pos)) delimiter))
           (list str))))))
(export 'strsplit)

(defun strjoin (separator &rest strings)
  "Joins multiple strings, separated with the 'separator' sequence."
  (let ((result (if strings (first strings) "")))
    (dolist (str (rest strings) result)
      (setf result (concatenate 'string result separator str)))))
(export 'strjoin)

(defun newline-join (&rest strings)
  (apply #'strjoin (append (list (make-string 1 :initial-element #\newline)) strings)))
(export 'newline-join)

(defun empty-string-p (str)
  (and (stringp str) (zerop (length str))))
(export 'empty-string-p)

(defun strtrim (str)
  (string-trim '(#\newline #\return #\space #\tab) str))
(export 'strtrim)

(defun ljust (str n &optional (fill-char #\Space))
  (let ((len (length str)))
    (if (< len n)
        (concatenate 'string str (make-sequence 'string (- n len) :initial-element fill-char))
        str)))
(export 'ljust)

(defun rjust (str n &optional (fill-char #\Space))
  (let ((len (length str)))
    (if (< len n)
        (concatenate 'string (make-sequence 'string (- n len) :initial-element fill-char) str)
        str)))
(export 'rjust)

(defun remove-prefix (string prefix &key (test #'string=))
  (let ((m (length prefix)) (n (length string)))
    (if (and (>= n m) (funcall test (subseq string 0 m) prefix))
        (subseq string m)
        string)))
(export 'remove-prefix)

(defun remove-suffix (string suffix &key (test #'string=))
  (let ((m (length suffix)) (n (length string)))
    (if (and (>= n m) (funcall test (subseq string (- n m)) suffix))
        (subseq string 0 (- n m))
        string)))
(export 'remove-suffix)

(defun string^= (string beginning)
  "Tests whether 'string' begins with 'beginning'"
  (let ((n (length string)) (m (length beginning)))
    (and (>= n m) (string= (subseq string 0 m) beginning))))
(export 'string^=)

(defun string$= (string ending)
  "Tests whether 'string' ends with 'ending'"
  (let ((n (length string)) (m (length ending)))
    (and (>= n m) (string= (subseq string (- n m)) ending))))
(export 'string$=)

(defun substrp (substring string &key (test #'char=))
  (search substring string :test test))
(export 'substrp)

(defun string-replace (string old new &optional (test #'string=))
  (let ((n (length old)) (m (length new)) (i 0))
    (if (plusp n)
        (do () ((not (setf i (search old string :test test :start2 i))))
          (setf string (concatenate 'string (subseq string 0 i) new (subseq string (+ i n))))
          (incf i m))
        string))
  string)
(export 'string-replace)

(defmacro multiline-format (stream &body lines)
  `(progn
     ,@(loop for line in lines append
                               (list (if (listp line)
                                         `(format ,stream ,@line)
                                         `(format ,stream ,line))
                                     `(terpri ,stream)))))
(export 'multiline-format)

(defun parse-float (float-str)
  (when (or (zerop (length float-str)) (string= float-str "."))
    (error "Error parsing floating point number."))
  (let ((sign 1) (pre-decimal 0) (post-decimal 0) (exponent 0))
    ;; Get the sign (if it exists)
    (cond
      ((char= (elt float-str 0) #\+) (setf float-str (subseq float-str 1)))
      ((char= (elt float-str 0) #\-) (setf float-str (subseq float-str 1) sign -1)))
    ;; Get the exponent (if it exists)
    (let ((pos (position #\e float-str :test #'char-equal)))
      (when pos
        (when (zerop pos)
          (error "Error parsing floating point number."))
        (handler-case
            (setf exponent (parse-integer (subseq float-str (1+ pos))))
          (t () (error "Error parsing floating point number.")))
        (setf float-str (subseq float-str 0 pos))))
    ;; Get the decimals (if they exist)
    (let ((pos (position #\. float-str :test #'char=)))
      (when pos
        (let ((substr (subseq float-str (1+ pos))))
          (unless (every #'digit-char-p substr)
            (error "Error parsing floating point number."))
          (when (plusp (length substr))
            (setf post-decimal (* (parse-integer (subseq float-str (1+ pos))) (expt 10d0 (- pos -1 (length float-str))))))
          (setf float-str (subseq float-str 0 pos)))))
    ;; Get the integer before the decimal point
    (when (plusp (length float-str))
      (setf pre-decimal (parse-integer float-str)))
    (* sign (+ pre-decimal post-decimal) (expt 10d0 exponent))))
(export 'parse-float)
