(in-suite utils)

(def-suite* sequence :in utils)

(test remove-nth
  (is (equal (utils:remove-nth 0 '(1)) nil))
  (is (equal (utils:remove-nth 0 '(1 2)) '(2)))
  (is (equal (utils:remove-nth 1 '(1 2)) '(1))))
