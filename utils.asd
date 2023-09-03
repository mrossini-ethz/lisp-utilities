(in-package :asdf-user)

(defsystem "utils"
  :description "Utilities"
  :version "0.1"
  :author "Marco Rossini"
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "file")
               (:file "random")
               (:file "sequence")
               (:file "statistics")
               (:file "string")
               (:file "binding")
               (:file "symbols")
               (:file "debug")
               (:file "logic")
               (:file "integer")
               (:file "lists")
               (:file "iteration")
               (:file "query")
               (:file "lambda")
               (:file "anaphoric")
               (:file "test-framework")
               (:file "arithmetic")
               (:file "io")
               (:file "import")
               (:file "hash-table")
               (:file "getopt")
               (:file "time"))
  :in-order-to ((test-op (test-op :utils/test))))

(defsystem "utils/test"
  :description "Unit Tests for Utilities"
  :version "0.1"
  :author "Marco Rossini"
  :depends-on (:utils :fiveam)
  :serial t
  :components ((:file "test/test")
               (:file "test/test-lists")
               (:file "test/test-sequence")
               (:file "test/test-string")
               (:file "test/test-arithmetic")
               (:file "test/test-getopt")
               (:file "test/test-file")))

(defmethod perform ((operation test-op) (system (eql (find-system :utils/test))))
  (funcall (intern "UTILS-TEST")))
