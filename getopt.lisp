(in-package :utils)

;; Option syntax and features:
;; - Short switches: -a
;; - Multiple short switches: -abc (equivalent to -a -b -c)
;; - Long switches: --all
;; - Short options with arguments: -a 3
;; - Long options with arguments: --length 3 or --length=3
;; - Stop processing options: --
;; - Non-option arguments
;; - Synonyms for options
;;
;; Example:
;;
;;   (with-command-line-options ((all ("a" "all") :switch)
;;                               (full ("f" "full") :switch)
;;                               (count ("c" "count") :argument)) arguments
;;     (do-stuff all full count))

(defun match-option (option-str bindings)
  (cond
    ;; Single short option
    ((and (= (length option-str) 2) (char= (elt option-str 0) #\-) (char/= (elt option-str 1) #\-))
     (let* ((option (subseq option-str 1)) (binding (position-if (lambda (x) (some (lambda (y) (string= y option)) (second x))) bindings)))
       (when binding
         (values binding option nil ""))))
    ;; Multiple short options (process only one of them)
    ((and (> (length option-str) 2) (char= (elt option-str 0) #\-) (char/= (elt option-str 1) #\-))
     (let* ((option (subseq option-str 1 2)) (binding (position-if (lambda (x) (some (lambda (y) (string= y option)) (second x))) bindings)))
       (when binding
         (values binding option nil (concatenate 'string "-" (subseq option-str 2))))))
    ;; Long option (possibly with argument)
    ((and (> (length option-str) 2) (char= (elt option-str 0) #\-) (char= (elt option-str 1) #\-))
     (let* ((pos (position #\= option-str :test #'char=)) (option (subseq option-str 2 pos)) (binding (position-if (lambda (x) (some (lambda (y) (string= y option)) (second x))) bindings)))
       (when binding
         (values binding option (if pos (subseq option-str (1+ pos)) nil) ""))))))
;; not exported

(defun parse-options (argv bindings)
  (let ((argv (copy-seq argv)) (i 0) (n (length argv)) (result (make-list (list-length bindings) :initial-element nil)))
    (loop while (< i n) for arg = (nth i argv) do
      ;; Identify the next option
      (multiple-value-bind (index option argument rest) (match-option arg bindings)
        ;; No option found. Abort!
        (unless index
          (when (and (>= (length arg) 1) (char= (elt arg 0) #\-) (not (string= arg "--")))
            (error "Unrecognized option ~a" arg))
          (when (string= arg "--")
            (incf i))
          (return))
        ;; Check which type of option it is (switch or argument)
        (case (third (nth index bindings))
          ;; Turn on the switch
          (:switch (setf (nth index result) t))
          (:argument (if argument
                         ;; The argument is already known
                         (setf (nth index result) argument)
                         ;; The argument is unknown
                         (progn
                           ;; Check argument availability
                           (when (or (plusp (length rest)) (= (1+ i) n) (match-option (nth (1+ i) argv) bindings))
                             (error "Missing argument to option ~:[--~;-~]~a" (= (length option) 1) option))
                           ;; Set the argument
                           (setf (nth index result) (nth (1+ i) argv))
                           ;; Advance the argv index
                           (incf i 1)))))
        (if (zerop (length rest))
            (incf i 1)
            (setf (nth i argv) rest))))
    (list result (subseq argv i))))
;; not exported

(define-symbol-macro *argv* #+:SBCL sb-ext:*posix-argv*)
(export '*argv*)

(defmacro with-command-line-options ((&rest option-bindings) argument-list &body body)
  "Binds the command line options described in OPTION-BINDINGS to variables for use in the body."
  ;; Check option bindings format
  (loop for o in option-bindings do
    (unless (and (listp o) (= (list-length o) 3) (symbolp (first o)) (every #'stringp (second o)) (or (eql (third o) :switch) (eql (third o) :argument)))
      (error "Malformed option binding ~s." o)))
  `(destructuring-bind (,(mapcar #'first option-bindings) ,argument-list) (parse-options (rest *argv*) ',option-bindings)
     (declare (ignorable ,@(mapcar #'first option-bindings) ,argument-list))
     ,@body))
(export 'with-command-line-options)
