(ns b.test.core
  (:use [b.core])
  (:use [clojure.test]))

(deftest simple-constructs 
  (is (= (ev (intgr 3)) 3) "Simple Integer") 
  (is (= ((vrb :x) {:x 3}) 3) "Variable lookup")
  (is (= (ev (bool true)) true) "Simple Boolean")
)

(deftest complex-constructs 
  (is (= (ev (add (intgr 1) (intgr 2))) 3) "adding 2 integers")
  (is (= (ev (add (add (intgr 1) (intgr 2)) (intgr 5))) 8) "adding 3 integers")
  (is (= ((add (vrb :a) (vrb :b)) {:a 1, :b 3}) 4) "adding vars")
  (is (= ((add (intgr 2) (vrb :b)) {:a 1, :b 3}) 5) "adding var to int")
)

(deftest set-construction
   (is (= (ev (cset (intgr 1) (intgr 2))) #{1,2}) "integer set construction")
   (is (ev (member (intgr 1) (cset (intgr 1) (intgr 2)))) "1 in {1,2}")
   (is (not (ev (member (intgr 4) (cset (intgr 1) (intgr 2))))) "4 not in {1,2}")
)


