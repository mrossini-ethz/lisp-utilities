(in-package :utils)

(defun read-file-to-string (filename)
  ;; Reads an entire file into a string
  (with-open-file (f filename)
    (let ((content (make-string (file-length f))))
      (read-sequence content f)
      content)))
(export 'read-file-to-string)

(defun read-lines-to-vector (filename &optional (trim-eol t))
  ;; Reads the lines of a file into a vector
  (with-open-file (f filename)
    (apply #'vector (loop for line = (read-line f nil nil) while line collect (if trim-eol (string-right-trim '(#\Newline #\Return) line) line)))))
(export 'read-lines-to-linevector)

(defun write-file-from-string (filename content &key if-exists if-does-not-exist)
  "Writes content into a file."
  (with-open-file (file filename :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (write-sequence content file)))
(export 'write-file-from-string)

(defun write-file-from-lines (filename line-seq &key (if-exists :error) (if-does-not-exist :create))
  "Writes the sequence of lines into a file."
  (with-open-file (file filename :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (loop for i below (length line-seq) do
      (write-line (elt line-seq i) file))))
(export 'write-file-from-lines)

(defmacro iterate-lines ((var stream &optional (trim-eol t)) &body body)
  (let ((handle (gensym)))
    `(let ((,handle ,stream))
       (do (,var) ((not (setf ,var (read-line ,handle nil))))
         ,@(if trim-eol `((setf ,var (string-right-trim '(#\Newline #\Return) ,var))))
         ,@body))))
(export 'iterate-lines)

(defmacro iterate-file ((var filename &optional (trim-eol t)) &body body)
  (let ((handle (gensym)))
    `(with-open-file (,handle ,filename)
       (iterate-lines (,var ,handle ,trim-eol)
         ,@body))))
(export 'iterate-file)

(defun update-file-from-string (path content &key (if-does-not-exist :create))
  "Updates a file only if the new content is different."
  (let (previous-content)
    ;; Read possibly existing file for comparison with new content
    (setf previous-content (read-file-to-string path)) ;; FIXME: problem when file does not exits
    ;; Write new content if it is not identical
    (unless (string= previous-content content)
      (write-file-from-string path content :if-exists :overwrite :if-does-not-exist if-does-not-exist))))
(export 'update-file-from-string)

(defun random-temporary-filename (&key (name-length 8) (dir "/tmp") (prefix ""))
  (loop with name while (or (not name) (probe-file name)) do
       (setf name (format nil "~a/~a~a~{~a~}" dir prefix (if (plusp (length prefix)) "-" "") (loop for i below name-length collect (code-char (+ (random 26) 97)))))
       finally (return name)))
(export 'random-temporary-filename)

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
