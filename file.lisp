(in-package :utils)

;;; File <-> String (text)

(defun read-file-to-string (filename &key (encoding :utf-8))
  "Reads an entire text file into a string."
  (with-open-file (f filename :external-format encoding)
    (let* ((n (file-length f)) (content (make-string n)) (m (read-sequence content f)))
      (if (= n m) content (subseq content 0 m)))))
(export 'read-file-to-string)

(defun write-file-from-string (filename content &key (if-exists :supersede) (if-does-not-exist :create) (encoding :utf-8))
  "Writes content into a text file."
  (with-open-file (file filename :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist :external-format encoding)
    (write-sequence content file)))
(export 'write-file-from-string)

(defun update-file-from-string (path content &key (if-does-not-exist :create) (encoding :utf-8))
  "Updates a text file only if the new content is different."
  (let (previous-content)
    ;; Read possibly existing file for comparison with new content
    (setf previous-content (read-file-to-string path :encoding encoding)) ;; FIXME: problem when file does not exist
    ;; Write new content if it is not identical
    (unless (string= previous-content content)
      (write-file-from-string path content :if-exists :supersede :if-does-not-exist if-does-not-exist :encoding encoding))))
(export 'update-file-from-string)

;;; File <-> Sequence of lines (text)

(defun remove-trailing-carriage-return (line)
  (let ((n (length line)))
    (if (and (plusp n) (char= (elt line (1- n)) #\Return))
        (subseq line 0 (1- n))
        line)))
;; not exported

(defun read-file-to-lines (filename &key (encoding :utf-8))
  "Reads a text file into a list of lines (without end-of-line characters)."
  (with-open-file (f filename :external-format encoding)
    (loop for line = (read-line f nil nil) while line collect (remove-trailing-carriage-return line))))
(export 'read-file-to-lines)

(defun write-file-from-lines (filename line-seq &key (if-exists :supersede) (if-does-not-exist :create) (encoding :utf-8) (eol-style :native))
  "Writes a sequence of lines into a text file."
  (with-open-file (file filename :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist :external-format encoding)
    (loop for i below (length line-seq) do
      (write-string (elt line-seq i) file)
      (case eol-style
        (:LF (write-char #\Newline file))
        (:unix (write-char #\Newline file))
        (:CRLF (write-char #\Return file) (write-char #\Newline file))
        (:windows (write-char #\Return file) (write-char #\Newline file))
        (:native #+WIN32 (write-char #\Return file) (write-char #\Newline file))))))
(export 'write-file-from-lines)

;;; File <-> Vector (binary data)

(defun read-file-to-vector (filename)
  "Reads binary data from a file into a vector."
  (with-open-file (f filename :element-type '(unsigned-byte 8))
    (let ((content (make-array (list (file-length f)))))
      (read-sequence content f)
      content)))
(export 'read-file-to-vector)

(defun write-file-from-vector (filename content &key (if-exists :supersede) (if-does-not-exist :create))
  "Writes binary data from a vector of bytes into a file."
  (with-open-file (f filename :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist :element-type '(unsigned-byte 8))
      (write-sequence content f)))
(export 'write-file-from-vector)

;;; Iterators

(defmacro for-line-in-stream ((var stream) &body body)
  "Iterates over lines in a text stream."
  (let ((handle (gensym)))
    `(let ((,handle ,stream))
       (do (,var) ((not (setf ,var (remove-trailing-carriage-return (read-line ,handle nil)))))
         ,@body))))
(export 'for-line-in-stream)

(defmacro for-line-in-file ((var filename &key (encoding :utf-8)) &body body)
  "Iterates over lines in a text file."
  (let ((handle (gensym)))
    `(with-open-file (,handle ,filename :external-format ,encoding)
       (for-line-in-stream (,var ,handle)
         ,@body))))
(export 'for-line-in-file)

;;; Filenames

(defun file-name (pathspec)
  (let ((name (file-namestring pathspec)))
    (if (string= name "") nil name)))
(export 'file-name)

(defun file-basename (pathspec)
  (pathname-name pathspec))
(export 'file-basename)

(defun file-suffix (pathspec)
  (pathname-type pathspec))
(export 'file-suffix)

(defun file-directory (pathspec)
  (let ((dir (directory-namestring pathspec)))
    (if (string= dir "") "./" dir)))
(export 'file-directory)

(defun filepath-split (pathspec)
  (values (file-directory pathspec) (file-basename pathspec) (file-suffix pathspec)))
(export 'filepath-split)

(defmacro with-filepath ((dir-var file-var &optional suffix-var) pathspec &body body)
  (let ((path (gensym)))
    (if suffix-var
        `(let* ((,path ,pathspec) (,dir-var (file-directory ,path)) (,file-var (file-basename ,path)) (,suffix-var (file-suffix ,path))) ,@body)
        `(let* ((,path ,pathspec) (,dir-var (file-directory ,path)) (,file-var (file-name ,path))) ,@body))))
(export 'with-filepath)

(defun file-exists-p (pathspec)
  (not (null (probe-file pathspec))))
(export 'file-exists-p)

(defun random-temporary-filename (&key (name-length 8) (dir "/tmp") (prefix ""))
  (loop with name while (or (not name) (probe-file name)) do
       (setf name (format nil "~a/~a~a~{~a~}" dir prefix (if (plusp (length prefix)) "-" "") (loop for i below name-length collect (code-char (+ (random 26) 97)))))
       finally (return name)))
(export 'random-temporary-filename)

;;; Directories

(defun directory-recurse (directory function)
  (let ((content (sort (directory (concatenate 'string directory "/" "*.*")) #'string< :key #'namestring)))
    (loop for file in content
       when (zerop (length (pathname-name file))) do
         (directory-recurse (namestring file) function)
       else do
         (funcall function file))))
(export 'directory-recurse)

(defmacro with-directory-recursion ((path-variable directory) &body body)
  `(directory-recurse ,directory #'(lambda (,path-variable) ,@body)))
(export 'with-directory-recursion)
