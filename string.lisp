(in-package :utils)

(with-export
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun strcat (&rest strings)
    "Concatenates the given strings"
    (apply #'concatenate 'string strings))

  (defun strsplit (str delimiter)
    "Splits the string at each occurrence of `delimiter'."
    (let (seq (pos 0) newpos (len (length str)))
      (do () ((>= pos len) seq)
        (setf newpos (search delimiter str :start2 pos))
        (unless newpos
          (setf newpos len))
        (when (/= newpos pos)
          (setf seq (append seq (list (subseq str pos newpos)))))
        (setf pos (+ newpos (length delimiter))))
      (if seq seq '(""))))

  (defun strjoin (separator &rest strings)
    "Joins multiple strings, separated with the 'separator' sequence."
    (let ((result (if strings (first strings) "")))
      (dolist (str (rest strings) result)
        (setf result (concatenate 'string result separator str)))))

  (defun newline-join (&rest strings)
    (apply #'strjoin (append (list (make-string 1 :initial-element #\newline)) strings)))

  (defun empty-string-p (str)
    (and (stringp str) (zerop (length str))))

  (defun strtrim (str)
    (string-trim '(#\newline #\return #\space #\tab) str))

  (defun string^= (string beginning)
    "Tests whether 'string' begins with 'beginning'"
    (let ((n (length string)) (m (length beginning)))
      (and (>= n m) (string= (subseq string 0 m) beginning))))

  (defun string$= (string ending)
    "Tests whether 'string' ends with 'ending'"
    (let ((n (length string)) (m (length ending)))
      (and (>= n m) (string= (subseq string (- n m)) ending))))

  (defun substrp (substring string &key (test #'char=))
    (search substring string :test test))

  (defmacro multiline-format (stream &body lines)
    `(progn
       ,@(loop for line in lines append
            (list (if (listp line)
                      `(format ,stream ,@line)
                      `(format ,stream ,line))
                  `(terpri ,stream)))))
)

(defun parse-float (float-str)
  (let ((pre-decimal 0) (post-decimal 0) (exponent 0))
    ;; Get the exponent (if it exists)
    (let ((pos (position #\e float-str :test #'char-equal)))
      (when pos
        (handler-case
            (setf exponent (parse-integer (subseq float-str (1+ pos))))
          (t () (error "Error parsing exponent")))
        (setf float-str (subseq float-str 0 pos))))
    ;; Get the decimals (if they exist)
    (let ((pos (position #\. float-str :test #'char=)))
      (when pos
        (let ((substr (subseq float-str (1+ pos))))
          (unless (every #'digit-char-p substr)
            (error "Error parsing decimals"))
          (when (plusp (length substr))
            (handler-case
                (setf post-decimal (* (parse-integer (subseq float-str (1+ pos))) (expt 10d0 (- pos -1 (length float-str)))))
              (t () (error "Error parsing decimal number"))))
          (setf float-str (subseq float-str 0 pos)))))
    ;; Get the integer before the decimal point
    (when (plusp (length float-str))
      (setf pre-decimal (parse-integer float-str)))
    (* (+ pre-decimal post-decimal) (expt 10d0 exponent))))
