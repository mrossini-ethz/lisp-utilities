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
    (zerop (length str)))
)

(defun split (list string delimiter delimiter-length)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type simple-string string delimiter))
  (declare (type fixnum delimiter-length))
  (let (pos)
    (setf pos (search delimiter string))
    (if (null pos)
        (append-1 list string)
        (split (append-1 list (subseq string 0 pos)) (subseq string (+ pos delimiter-length)) delimiter delimiter-length))))

;(disassemble 'split)
;(split nil "abcikbb3bsdfsdb2" "b" 1)

;(defun str-split (string delimiter)
;  (flet ((split (list string delimiter)
;           if (search

;(strsplit "abcbd" "b")

(defun strtrim (str)
  (string-trim '(#\newline #\space) str))
(export 'strtrim)

(defmacro multiline-format (stream &body lines)
  `(progn
     ,@(loop for line in lines append
            (list (if (listp line)
                      `(format ,stream ,@line)
                      `(format ,stream ,line))
                  `(terpri ,stream)))))
(export 'multiline-format)
