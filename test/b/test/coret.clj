(ns b.test.core
  (:use [b.core])
  (:use [clojure.test]))

(deftest simple-constructs 
  (is (= (ev (intgr 3)) 3) "Simple Integer") 
  (is (= (first ((vrb :x) {:x 3})) 3) "Variable lookup")
  (is (= (ev (bool true)) true) "Simple Boolean")
)

(deftest complex-constructs 
  (is (= (ev (add (intgr 1) (intgr 2))) 3) "adding integers")
  (is (= (ev (add (add (intgr 1) (intgr 2)) (intgr 5))) 8) "adding integers")
  (is (= (first ((add (vrb :a) (vrb :b)) {:a 1, :b 3})) 4) "adding vars")
  (is (= (first ((add (intgr 2) (vrb :b)) {:a 1, :b 3})) 5) "adding var to int")
)

;(deftest set-construction)


