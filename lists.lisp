(in-package :utils)

;(proclaim '(inline last-1 append-1 mklist))

(defmacro appendf (list &rest items)
  "Appends the given item to the list using setf."
  `(setf ,list (append ,list (list ,@items))))
(export 'appendf)

;; Graham
(defun append-1 (obj list)
  (append list (list obj)))
(export 'append-1)

(defun prepend-1 (obj list)
  (append (list obj) list))
(export 'prepend-1)

;; Graham
(defun last-1 (list)
  "Retrieves the last item from the given list."
  (car (last list)))
(export 'last-1)

;; Graham
(defun mklist (obj)
  "Makes a list out of the given object unless it already is a list."
  (if (listp obj) obj (list obj)))
(export 'mklist)

;; Family of functions that test the length of a list without iterating through all members

;; Helper macro
(defmacro l=</>-traverser (list length)
  "Traverses the list until either the list or the length are exhausted. Return the state of each upon reaching"
  (once-only (list length)
    `(progn
       (check-type ,list list)
       (check-type ,length fixnum)
       (locally
           (declare (fixnum ,length) (optimize (speed 3) (safety 0)))
         (do () ((or (not (listp ,list)) (null ,list) (<= ,length 0)) (values (null ,list) (<= ,length 0)))
           (setf ,list (cdr ,list) ,length (1- ,length)))))))
;; not exported (helper macro)

(declaim (inline l=))
(defun l= (list length)
  "Tests efficiently whether the length of the list is equal to the given length."
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list length)
    (and list-exhausted length-exhausted)))
(export 'l=)

(declaim (inline l/=))
(defun l/= (list length)
  "Tests efficiently whether the length of the list is different from the given length."
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list length)
    (not (and list-exhausted length-exhausted))))
(export 'l/=)

(declaim (inline l>))
(defun l> (list length)
  "Tests efficiently whether the length of the list is greater than the given length."
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list length)
    (and (not list-exhausted) length-exhausted)))
(export 'l>)

(declaim (inline l<=))
(defun l<= (list length)
  "Tests efficiently whether the length of the list is smaller or equal to the given length."
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list length)
    (declare (ignore length-exhausted))
    list-exhausted))
(export 'l<=)

(declaim (inline l<))
(defun l< (list length)
  "Tests efficiently whether the length of the list is smaller than the given length."
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list length)
    (and list-exhausted (not length-exhausted))))
(export 'l<)

(declaim (inline l>=))
(defun l>= (list length)
  "Tests efficiently whether the length of the list is greater or equal to the given length."
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list length)
    (declare (ignore list-exhausted))
    length-exhausted))
(export 'l>=)

(declaim (inline l=0))
(defun l=0 (list)
  (check-type list list)
  (null list))
(export 'l=0)

(declaim (inline l=1))
(defun l=1 (list)
  (check-type list list)
  (and (consp list) (null (cdr list))))
(export 'l=1)

(declaim (inline l=2))
(defun l=2 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 2)
    (and list-exhausted length-exhausted)))
(export 'l=2)

(declaim (inline l=3))
(defun l=3 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 3)
    (and list-exhausted length-exhausted)))
(export 'l=3)

(declaim (inline l=4))
(defun l=4 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 4)
    (and list-exhausted length-exhausted)))
(export 'l=4)

(declaim (inline l>0))
(defun l>0 (list)
  (check-type list list)
  (and (consp list) (listp (cdr list))))
(export 'l>0)

(declaim (inline l>1))
(defun l>1 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 1)
    (and (not list-exhausted) length-exhausted)))
(export 'l>1)

(declaim (inline l>2))
(defun l>2 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 2)
    (and (not list-exhausted) length-exhausted)))
(export 'l>2)

(declaim (inline l>3))
(defun l>3 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 3)
    (and (not list-exhausted) length-exhausted)))
(export 'l>3)

(declaim (inline l>4))
(defun l>4 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 4)
    (and (not list-exhausted) length-exhausted)))
(export 'l>4)

(declaim (inline l<1))
(defun l<1 (list)
  (check-type list list)
  (null list))
(export 'l<1)

(declaim (inline l<2))
(defun l<2 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 2)
    (and list-exhausted (not length-exhausted))))
(export 'l<2)

(declaim (inline l<3))
(defun l<3 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 3)
    (and list-exhausted (not length-exhausted))))
(export 'l<3)

(declaim (inline l<4))
(defun l<4 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 4)
    (and list-exhausted (not length-exhausted))))
(export 'l<4)

(declaim (inline l<5))
(defun l<5 (list)
  (multiple-value-bind (list-exhausted length-exhausted) (l=</>-traverser list 5)
    (and list-exhausted (not length-exhausted))))
(export 'l<5)

;; Family of functions that test the lengths of two lists against each other without iterating through all members

;; Helper macro
(defmacro ll=</>-traverser (list-a list-b)
  "Traverses the list until either the list or the length are exhausted. Return the state of each upon reaching"
  (once-only (list-a list-b)
    `(progn
       (check-type ,list-a list)
       (check-type ,list-b list)
       (locally
           (declare (optimize (speed 3) (safety 0)))
         (do () ((or (not (listp ,list-a)) (not (listp ,list-b)) (null ,list-a) (null ,list-b)) (values (null ,list-a) (null ,list-b)))
           (setf ,list-a (cdr ,list-a) ,list-b (cdr ,list-b)))))))
;; not exported (helper macro)

(declaim (inline ll=))
(defun ll= (list-a list-b)
  "Tests efficiently whether the length of list-a is equal to the length of list-b."
  (multiple-value-bind (a-exhausted b-exhausted) (ll=</>-traverser list-a list-b)
    (and a-exhausted b-exhausted)))
(export 'll=)

(declaim (inline ll/=))
(defun ll/= (list-a list-b)
  "Tests efficiently whether the length of list-a is different from the length of list-b."
  (multiple-value-bind (a-exhausted b-exhausted) (ll=</>-traverser list-a list-b)
    (not (and a-exhausted b-exhausted))))
(export 'll/=)

(declaim (inline ll>))
(defun ll> (list-a list-b)
  "Tests efficiently whether the length of list-a is greater than the length of list-b."
  (multiple-value-bind (a-exhausted b-exhausted) (ll=</>-traverser list-a list-b)
    (and (not a-exhausted) b-exhausted)))
(export 'll>)

(declaim (inline ll<=))
(defun ll<= (list-a list-b)
  "Tests efficiently whether the length of list-a is smaller or equal to the length of list-b."
  (multiple-value-bind (a-exhausted b-exhausted) (ll=</>-traverser list-a list-b)
    (declare (ignore b-exhausted))
    a-exhausted))
(export 'll<=)

(declaim (inline ll<))
(defun ll< (list-a list-b)
  "Tests efficiently whether the length of list-a is smaller than the length of list-b."
  (multiple-value-bind (a-exhausted b-exhausted) (ll=</>-traverser list-a list-b)
    (and a-exhausted (not b-exhausted))))
(export 'll<)

(declaim (inline ll>=))
(defun ll>= (list-a list-b)
  "Tests efficiently whether the length of list-a is greater or equal to the length of list-b."
  (multiple-value-bind (a-exhausted b-exhausted) (ll=</>-traverser list-a list-b)
    (declare (ignore a-exhausted))
    b-exhausted))
(export 'll>=)

(defun list-index-valid-p (index list)
  (and (>= index 0) (l> list index)))
(export 'list-index-valid-p)

(defun list-flat-p (list)
  (or (null list) (and (not (consp (car list))) (list-flat-p (cdr list)))))
(export 'list-flat-p)

;; Graham
(defun group (source n)
  "Groups the source list into sublists of length n."
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
(export 'group)

;; Graham
(defun flatten (x)
  "Flattens the given tree, x, into a list."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
(export 'flatten)

;; Graham
(defun prune (test tree)
  "Removes items from the given tree if they satisfy the test."
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))
(export 'prune)

(defmacro list-comp (var list &body body)
  `(mapcar (lambda (,var) ,@body) ,list))
(export 'list-comp)
