(in-package :asdf-user)

(defsystem "utils"
  :description "Utilities"
  :version "0.1"
  :author "Marco Rossini"
  :components ((:file "package")
               (:file "file" :depends-on ("package"))
               (:file "random" :depends-on ("package"))
               (:file "sequence" :depends-on ("package" "macros"))
               (:file "statistics" :depends-on ("package"))
               (:file "string" :depends-on ("package"))
               (:file "binding" :depends-on ("package" "lists"))
               (:file "symbols" :depends-on ("package" "string"))
               (:file "macros" :depends-on ("package"))
               (:file "debug" :depends-on ("package" "macros"))
               (:file "logic" :depends-on ("package"))
               (:file "lists" :depends-on ("package" "logic"))
               (:file "iteration" :depends-on ("package" "macros"))
               (:file "query" :depends-on ("package"))
               (:file "lambda" :depends-on ("package"))
               (:file "test-framework" :depends-on ("package"))
               (:file "arithmetic" :depends-on ("package"))
               (:file "io" :depends-on ("package"))
               (:file "import" :depends-on ("package"))
               (:file "hash-table" :depends-on ("package")))
  :in-order-to ((test-op (test-op :utils/test))))

(defsystem "utils/test"
  :description "Unit Tests for Utilities"
  :version "0.1"
  :author "Marco Rossini"
  :depends-on (:utils :fiveam)
  :serial t
  :components ((:file "test/test")
               (:file "test/test-lists")
               (:file "test/test-string")))

(defmethod perform ((operation test-op) (system (eql (find-system :utils/test))))
  (funcall (intern "UTILS-TEST")))
