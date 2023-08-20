(in-package :utils)

;(proclaim '(inline last-1 append-1 mklist))

(with-export
  (defmacro appendf (list &rest items)
    "Appends the given item to the list using setf."
    `(setf ,list (append ,list (list ,@items))))

  ;; Graham
  (defun append-1 (list obj)
    (append list (list obj)))

  ;; Graham
  (defun last-1 (list)
    "Retrieves the last item from the given list."
    (car (last list)))

  ;; Graham
  (defun mklist (obj)
    "Makes a list out of the given object unless it already is a list."
    (if (listp obj) obj (list obj)))

  ;; Family of functions that test the length of a list without iterating through all members
  (proclaim '(inline l/= l<= l>= single ll/= ll< ll<= ll>=))
  (defun l= (list length)
    "Tests efficiently whether the length of the list is equal to the given length."
    (cond
      ((null list) (= 0 length))
      ((zerop length) (null list))
      (t (l= (rest list) (- length 1)))))

  (defun l/= (list length)
    "Tests efficiently whether the length of the list is different from the given length."
    (not (l= list length)))

  (defun l> (list length)
    "Tests efficiently whether the length of the list is greater than the given length."
    (cond
      ((null list) nil)
      ((zerop length) (consp list))
      (t (l> (rest list) (- length 1)))))

  (defun l<= (list length)
    "Tests efficiently whether the length of the list is smaller or equal to the given length."
    (not (l> list length)))

  (defun l< (list length)
    "Tests efficiently whether the length of the list is smaller than the given length."
    (cond
      ((null list) (plusp length))
      ((zerop length) nil)
      (t (l< (rest list) (- length 1)))))

  (defun l>= (list length)
    "Tests efficiently whether the length of the list is greater or equal to the given length."
    (not (l< list length)))

  (defun single (list)
    (l= list 1))

  ;; Family of functions that test the lengths of two lists against each other without iterating through all members
  (defun ll= (list-a list-b)
    "Tests efficiently whether the length of list-a is equal to the length of list-b."
    (if (nand list-a list-b)
        (xnor list-a list-b)
        (ll= (rest list-a) (rest list-b))))

  (defun ll/= (list-a list-b)
    "Tests efficiently whether the length of list-a is different from the length of list-b."
    (not (ll= list-a list-b)))

  (defun ll> (list-a list-b)
    "Tests efficiently whether the length of list-a is greater than the length of list-b."
    (cond
      ((null list-a) nil)
      ((null list-b) t)
      (t (ll> (rest list-a) (rest list-b)))))

  (defun ll<= (list-a list-b)
    "Tests efficiently whether the length of list-a is smaller or equal to the length of list-b."
    (not (ll> list-a list-b)))

  (defun ll< (list-a list-b)
    "Tests efficiently whether the length of list-a is smaller than the length of list-b."
    (ll> list-b list-a))

  (defun ll>= (list-a list-b)
    "Tests efficiently whether the length of list-a is greater or equal to the length of list-b."
    (not (ll< list-a list-b)))

  (defun list-index-valid-p (index list)
    (and (>= index 0) (l> list index)))

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

  ;; Graham
  (defun flatten (x)
    "Flattens the given tree, x, into a list."
    (labels ((rec (x acc)
               (cond ((null x) acc)
                     ((atom x) (cons x acc))
                     (t (rec (car x) (rec (cdr x) acc))))))
      (rec x nil)))

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

  (defun same (test sequence &key (key #'identity))
    ;; Checks whether every item in the sequence is the same according to test (using key)
    (every #'(lambda (x) (funcall test (funcall key x) (funcall key (first sequence)))) sequence))

  (defun have (item sequence &key (test #'eql) (key #'identity))
    ;; Checks whether the given item is in the list
    (some #'(lambda (x) (funcall test item (funcall key x))) sequence))

  (defun remove-nth (n list)
    (if (list-index-valid-p n list)
        (append (subseq list 0 n) (if (l> list (1+ n)) (subseq list (1+ n))))
        list))
  )

