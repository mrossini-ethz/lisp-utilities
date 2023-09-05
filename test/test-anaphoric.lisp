(in-suite utils)

(def-suite* anaphoric :in utils)

(test awhen
  (is (= (utils:awhen 3 it))))
