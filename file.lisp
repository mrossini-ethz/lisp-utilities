(in-package :utils)

(defun read-file-to-string (filename)
  ;; Reads an entire file into a string
  (with-open-file (f filename)
    (let ((content (make-string (file-length f))))
      (read-sequence content f)
      content)))
(export 'read-file-to-string)

(defun read-lines-to-linevector (filename)
  ;; Reads the lines of a file into a vector
  (with-open-file (f filename)
    (apply #'vector (loop for line = (read-line f nil nil) while line collect line))))
(export 'read-lines-to-linevector)

(defun write-file-from-string (filename content &key if-exists if-does-not-exist)
  "Writes content into a file."
  (with-open-file (file filename :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (write-sequence content file)))
(export 'write-file-from-string)

(defun update-file-from-string (path content &key (if-does-not-exist :create))
  "Updates a file only if the new content is different."
  (let (previous-content)
    ;; Read possibly existing file for comparison with new content
    (setf previous-content (read-file path)) ;; FIXME: problem when file does not exits
    ;; Write new content if it is not identical
    (unless (string= previous-content content)
      (write-file path content :if-exists :overwrite :if-does-not-exist if-does-not-exist))))
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
