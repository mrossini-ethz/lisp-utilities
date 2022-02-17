(defpackage :btree
  (:use :common-lisp))

(in-package :btree)

;; Conditions ------------------------------------------------------------------------

(define-condition binary-tree-collision (error)
  ((value :initarg :value :reader binary-tree-collision-value)))
(export 'binary-tree-collision)

;; Generic Tree ----------------------------------------------------------------------

(defstruct (btree (:conc-name nil)
                  (:constructor make-btree (&key predicate equality balance-mode error-on-collision)))
  (root nil)
  (entries 0)
  (error-on-collision t)
  (predicate #'<)
  (balance-mode nil)
  (equality #'eql))
(export 'btree)
(export 'entries)

(defun node-insert-dispatch (tree node val predicate equality error-on-collision)
  (case (balance-mode tree)
    (:aa (insert-aa-node node val predicate equality error-on-collision))
    (t (insert-btree-node node val predicate equality error-on-collision))))

(defun insert (tree &rest values)
  "Inserts values in the given binary tree."
  (dolist (val values t)
    (setf (root tree) (node-insert-dispatch tree (root tree) val (predicate tree) (equality tree) (error-on-collision tree)))))
(export 'insert)

(defun mklist (tree)
  (mklist% (root tree)))
(export 'mklist)

;; Standard binary tree --------------------------------------------------------------

(defstruct (btree-node (:conc-name "BTREE-"))
  (value nil)
  (left nil)
  (right nil))

(defun insert-btree-node (node val predicate equality error-on-collision)
  "Inserts a value into the tree and returns new (modified) tree or nil if the value already exists."
  (if (null node)
      ;; Create new node
      (make-btree-node :value val)
      ;; Check whether the value matches the current node
      (if (funcall equality val (btree-value node))
          ;; Value collides
          (if error-on-collision
              (error 'binary-tree-collision :value val)
              node)
          ;; Value is not yet in tree
          (progn
            (if (funcall predicate val (btree-value node))
                ;; Insert to the left
                (setf (btree-left node) (insert-btree-node (btree-left node) val predicate equality error-on-collision))
                ;; Insert to the right
                (setf (btree-right node) (insert-btree-node (btree-right node) val predicate equality error-on-collision)))
            ;; Return the new tree
            node))))

(defmethod mklist% ((node btree-node))
  ;; Converts a binary tree into a flat list (which is sorted by design)
  (append (if (btree-left node) (mklist% (btree-left node))) (list (btree-value node)) (if (btree-right node) (mklist% (btree-right node)))))

;; AA tree ---------------------------------------------------------------------------

(defstruct (aa-node (:conc-name "AA-"))
  (value nil)
  (left nil)
  (right nil)
  (level 1))

(defun aa-skew (node)
  (when node
    (if (or (null (aa-left node)) (/= (aa-level node) (aa-level (aa-left node))))
        ;; Do nothing
        node
        ;; Swap the pointer of horizontal left links
        (let ((left (aa-left node)))
          (setf (aa-left node) (aa-right left))
          (setf (aa-right left) node)))))

(defun aa-split (node)
  (when node
    (if (or (null (aa-right node)) (null (aa-right (aa-right node))) (/= (aa-level node) (aa-level (aa-right (aa-right node)))))
        ;; Do nothing
        node
        ;; We have two horizontal right links. Take the middle node, elevate it, and return it.
        (let ((right (aa-right node)))
          (setf (aa-right node) (aa-left right))
          (setf (aa-left right) node)
          (incf (aa-level right))
          right))))

(defun insert-aa-node (node val predicate equality error-on-collision)
  "Inserts a value into the tree and returns new (modified) tree or nil if the value already exists."
  (if (null node)
      ;; Create new node
      (make-aa-node :value val)
      ;; Check whether the value matches the current node
      (if (funcall equality val (aa-value node))
          ;; Value collides
          (if error-on-collision
              (error 'binary-tree-collision :value val)
              node)
          ;; Value is not yet in tree
          (progn
            (if (funcall predicate val (aa-value node))
                ;; Insert to the left
                (setf (aa-left node) (insert-aa-node (aa-left node) val predicate equality error-on-collision))
                ;; Insert to the right
                (setf (aa-right node) (insert-aa-node (aa-right node) val predicate equality error-on-collision)))
            ;; Return the new tree, skewed and split
            (aa-split (aa-skew node))))))

(defmethod mklist% ((node aa-node))
  ;; Converts a binary tree into a flat list (which is sorted by design)
  (append (if (aa-left node) (mklist% (aa-left node))) (list (aa-value node)) (if (aa-right node) (mklist% (aa-right node)))))

;; Test environment ------------------------------------------------------------------

(let ((tree (make-btree :error-on-collision t)))
  (insert tree 5 2 1 0 7 8 6.5 5.5 5.25 5.125)
  (print (mklist tree))
  tree)

(let ((tree (make-btree :error-on-collision nil :balance-mode :aa)))
  (dotimes (x 1023)
    (insert tree x))
  (insert tree 3)
  (print (mklist tree))
  tree)

(let ((root (make-btree :error-on-collision nil)))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (insert root (random 100))
  (print (mklist root))
  (print (entries root)))
  
