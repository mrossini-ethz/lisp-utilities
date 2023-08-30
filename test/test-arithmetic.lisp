(in-suite utils)

(def-suite* arithmetic :in utils)

(test /+/
  ;; Degenerate calls
  (signals error (utils:/+/))
  (signals error (utils:/+/ 0))
  (signals error (utils:/+/ 0 0))
  (signals error (utils:/+/ 0 0 0))
  (signals error (utils:/+/ 0 1 1))
  (signals error (utils:/+/ 1 0 1))
  (signals error (utils:/+/ 1 1 0))
  ;; Normal calls, one arg
  (is (= (utils:/+/ +5) +5))
  (is (= (utils:/+/ -5) -5))
  ;; Normal calls, two args
  (is (= (utils:/+/ +5 +5) +5/2))
  (is (= (utils:/+/ -5 -5) -5/2))
  (is (= (utils:/+/ +5 +9) +45/14))
  (is (= (utils:/+/ -5 -9) -45/14))
  ;; Normal calls, three args
  (is (= (utils:/+/ +5 +5 +5) +5/3))
  (is (= (utils:/+/ -5 -5 -5) -5/3))
  (is (= (utils:/+/ +2 +5 +9) +90/73))
  (is (= (utils:/+/ -2 -5 -9) -90/73)))

(test /-/
  ;; Degenerate calls
  (signals error (utils:/-/))
  (signals error (utils:/-/ 0))
  (signals error (utils:/-/ 0 0))
  (signals error (utils:/-/ 0 0 0))
  (signals error (utils:/-/ 0 1 1))
  (signals error (utils:/-/ 1 0 1))
  (signals error (utils:/-/ 1 1 0))
  ;; Normal calls, one arg
  (is (= (utils:/-/ +5) +5))
  (is (= (utils:/-/ -5) -5))
  ;; Normal calls, two args
  (is (= (utils:/-/ +5 -5) +5/2))
  (is (= (utils:/-/ -5 +5) -5/2))
  (is (= (utils:/-/ +5 -9) +45/14))
  (is (= (utils:/-/ -5 +9) -45/14))
  ;; Normal calls, three args
  (is (= (utils:/-/ +5 -5 -5) +5/3))
  (is (= (utils:/-/ -5 +5 +5) -5/3))
  (is (= (utils:/-/ +2 -5 -9) +90/73))
  (is (= (utils:/-/ -2 +5 +9) -90/73)))

(test pyt
  ;; Degenerate calls
  (is (eql (utils:pyt) 0d0))
  ;; One number
  (is (eql (utils:pyt 0) 0d0))
  (is (eql (utils:pyt 5) 5d0))
  (is (eql (utils:pyt -5) 5d0))
  ;; Two numbers
  (is (eql (utils:pyt +0 +0) 0d0))
  (is (eql (utils:pyt +1 +1) (sqrt 2d0)))
  (is (eql (utils:pyt +3 +4) 5d0))
  (is (eql (utils:pyt -3 +4) 5d0))
  (is (eql (utils:pyt +3 -4) 5d0))
  (is (eql (utils:pyt -3 -4) 5d0))
  ;; Three numbers
  (is (eql (utils:pyt +0 +0 +0) 0d0))
  (is (eql (utils:pyt +1 +1 +1) (sqrt 3d0)))
  (is (eql (utils:pyt +2 +3 +6) 7d0))
  (is (eql (utils:pyt +2 +3 -6) 7d0))
  (is (eql (utils:pyt +2 -3 +6) 7d0))
  (is (eql (utils:pyt +2 -3 -6) 7d0))
  (is (eql (utils:pyt -2 +3 +6) 7d0))
  (is (eql (utils:pyt -2 +3 -6) 7d0))
  (is (eql (utils:pyt -2 -3 +6) 7d0))
  (is (eql (utils:pyt -2 -3 -6) 7d0)))

(test ipyt
  ;; One number
  (is (eql (utils:ipyt 0) 0d0))
  (is (eql (utils:ipyt 5) 5d0))
  (is (eql (utils:ipyt -5) 5d0))
  ;; Two numbers
  (is (eql (utils:ipyt +0 +0) 0d0))
  (is (< (abs (- (utils:ipyt (sqrt 2d0) +1) 1d0)) 0.00000001d0))
  (is (< (abs (- (utils:ipyt +5 +4) 3d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt -5 +4) 3d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt +5 -4) 3d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt -5 -4) 3d0)) 0.000000001d0))
  ;; Three numbers
  (is (eql (utils:ipyt +0 +0 +0) 0d0))
  (is (< (abs (- (utils:ipyt (sqrt 3d0) +1 +1) 1d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt +7 +3 +6) 2d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt +7 +3 -6) 2d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt +7 -3 +6) 2d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt +7 -3 -6) 2d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt -7 +3 +6) 2d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt -7 +3 -6) 2d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt -7 -3 +6) 2d0)) 0.000000001d0))
  (is (< (abs (- (utils:ipyt -7 -3 -6) 2d0)) 0.000000001d0)))

(test square
  (is (eql (utils:square 0) 0))
  (is (eql (utils:square +1) 1))
  (is (eql (utils:square -1) 1))
  (is (eql (utils:square +1/2) 1/4))
  (is (eql (utils:square -1/2) 1/4))
  (is (eql (utils:square +5) 25))
  (is (eql (utils:square -5) 25)))

(test cube
  (is (eql (utils:cube 0) 0))
  (is (eql (utils:cube +1) +1))
  (is (eql (utils:cube -1) -1))
  (is (eql (utils:cube +1/2) +1/8))
  (is (eql (utils:cube -1/2) -1/8))
  (is (eql (utils:cube +5) +125))
  (is (eql (utils:cube -5) -125)))
