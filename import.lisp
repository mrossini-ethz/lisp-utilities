(in-package :utils)

(defun use-package-no-shadowing (package-to-use &optional (package *package*))
  (do-external-symbols (symbol package-to-use)
    (unless (find-symbol (symbol-name symbol) package)
      (import symbol package)))
  t)
(export 'use-package-no-shadowing)

(defun use-package-with-shadowing (package-to-use &optional (package *package*))
  (do-external-symbols (symbol package-to-use)
    (unless (find-symbol (symbol-name symbol) package)
      (shadowing-import symbol package)))
  t)
(export 'use-package-with-shadowing)

(defun alias-package (package-designator new-nickname)
  (rename-package package-designator package-designator (list new-nickname)))
(export 'alias-package)
