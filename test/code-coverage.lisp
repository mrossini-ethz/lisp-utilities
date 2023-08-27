;;; Load SB-COVER
(require :sb-cover)

;;; Turn on generation of code coverage instrumentation in the compiler
(declaim (optimize sb-cover:store-coverage-data))

;;; Load some code, ensuring that it's recompiled with the new optimization
;;; policy.
(asdf:test-system :utils :force t)

;;; Produce a coverage report
(sb-cover:report "/tmp/report/utils/")

;;; Turn off instrumentation
(declaim (optimize (sb-cover:store-coverage-data 0)))

