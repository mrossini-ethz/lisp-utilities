(in-package :utils)

;(proclaim '(inline last-1 append-1 mklist))

(defmacro appendf (list &rest items)
  "Appends the given item to the list using setf."
  `(setf ,list (append ,list (list ,@items))))
(export 'appendf)

;; Graham
(defun append-1 (list obj)
  (append list (list obj)))
(export 'append-1)

(defun prepend-1 (list obj)
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
(eval-when (:compile-toplevel)
  (proclaim '(inline l/= l<= l>= single ll/= ll< ll<= ll>=)))
(defun l= (list length)
  "Tests efficiently whether the length of the list is equal to the given length."
  (cond
    ((null list) (= 0 length))
    ((zerop length) (null list))
    (t (l= (rest list) (- length 1)))))
(export 'l=)

(defun l/= (list length)
  "Tests efficiently whether the length of the list is different from the given length."
  (not (l= list length)))
(export 'l/=)

(defun l> (list length)
  "Tests efficiently whether the length of the list is greater than the given length."
  (cond
    ((minusp length) (listp list))
    ((null list) nil)
    ((zerop length) (consp list))
    (t (l> (rest list) (- length 1)))))
(export 'l>)

(defun l<= (list length)
  "Tests efficiently whether the length of the list is smaller or equal to the given length."
  (not (l> list length)))
(export 'l<=)

(defun l< (list length)
  "Tests efficiently whether the length of the list is smaller than the given length."
  (cond
    ((null list) (plusp length))
    ((zerop length) nil)
    (t (l< (rest list) (- length 1)))))
(export 'l<)

(defun l>= (list length)
  "Tests efficiently whether the length of the list is greater or equal to the given length."
  (not (l< list length)))
(export 'l>=)

(defun single (list)
  (l= list 1))
(export 'single)

;; Family of functions that test the lengths of two lists against each other without iterating through all members
(defun ll= (list-a list-b)
  "Tests efficiently whether the length of list-a is equal to the length of list-b."
  (if (nand list-a list-b)
      (xnor list-a list-b)
      (ll= (rest list-a) (rest list-b))))
(export 'll=)

(defun ll/= (list-a list-b)
  "Tests efficiently whether the length of list-a is different from the length of list-b."
  (not (ll= list-a list-b)))
(export 'll/=)

(defun ll> (list-a list-b)
  "Tests efficiently whether the length of list-a is greater than the length of list-b."
  (cond
    ((null list-a) nil)
    ((null list-b) t)
    (t (ll> (rest list-a) (rest list-b)))))
(export 'll>)

(defun ll<= (list-a list-b)
  "Tests efficiently whether the length of list-a is smaller or equal to the length of list-b."
  (not (ll> list-a list-b)))
(export 'll<=)

(defun ll< (list-a list-b)
  "Tests efficiently whether the length of list-a is smaller than the length of list-b."
  (ll> list-b list-a))
(export 'll<)

(defun ll>= (list-a list-b)
  "Tests efficiently whether the length of list-a is greater or equal to the length of list-b."
  (not (ll< list-a list-b)))
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
