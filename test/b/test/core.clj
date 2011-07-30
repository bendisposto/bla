(ns b.test.core
  (:use [b.core])
  (:use [clojure.test]))

(defmacro isnt 
    [tst & desc] `(is (not ~tst) ~@desc))

(deftest simple-constructs 
  (is (= (ev (intgr 3)) 3) "Simple Integer") 
  (is (= ((vrb :x) {:x 3}) 3) "Variable lookup")
  (is (= (ev (bool true)) true) "Simple Boolean")
)

(deftest arithmetic_preds 
  (is (ev (less (intgr 3) (intgr 4)))  "3 < 4") 
  (isnt (ev (less (intgr 4) (intgr 3)))  "not 4 < 3") 
  (is (ev (lesseq (intgr 3) (intgr 4)))  "3 <= 4") 
  (is (ev (lesseq (intgr 3) (intgr 3)))  "3 <= 3") 
  (isnt (ev (lesseq (intgr 4) (intgr 3)))  "not 4 <= 3") 

  (isnt (ev (grt (intgr 3) (intgr 4)))  "not 3 > 4") 
  (is (ev (grt (intgr 4) (intgr 3)))  "4 > 3") 
  (isnt (ev (grteq (intgr 3) (intgr 4)))  "not 3 >= 4") 
  (is (ev (grteq (intgr 3) (intgr 3)))  "3 >= 3")
)

(deftest complex-constructs 
  (is (= (ev (__+ (intgr 1) (intgr 2))) 3) "adding 2 integers")
  (is (= (ev (add (add (intgr 1) (intgr 2)) (intgr 5))) 8) "adding 3 integers")
  (is (= ((add (vrb :a) (vrb :b)) {:a 1, :b 3}) 4) "adding vars")
  (is (= ((add (intgr 2) (vrb :b)) {:a 1, :b 3}) 5) "adding var to int")
)

(deftest set-construction
   (is (= (ev (cset (intgr 1) (intgr 2))) #{1,2}) "integer set construction")
   (is (= (ev (cset (intgr 1) (intgr 2) (intgr 3))) #{1,3,2}) "integer set construction"))

(deftest set-predicates
   (is (ev (member (intgr 1) (cset (intgr 1) (intgr 2)))) "1 in {1,2}")
   (is (not (ev (member (intgr 4) (cset (intgr 1) (intgr 2))))) "4 not in {1,2}"))

(deftest set-expressions
   (is (= (ev (sunion (cset (intgr 1) (intgr 2))
        (cset (intgr 3) (intgr 2)))) #{1,2,3}) "{1,2} U {2,3} = {1,2,3}")
   (is (= (ev (sintersect (cset (intgr 1) (intgr 2))
	        (cset (intgr 3) (intgr 2)))) #{2}) "{1,2} ^ {2,3} = {2}")
   (is (= (ev (ssub (cset (intgr 1) (intgr 2))
			        (cset (intgr 3) (intgr 2)))) #{1}) "{1,2} - {2,3} = {1}")
)

(deftest junctors 
	(isnt (ev (jand (grt (intgr 4) (intgr 3)) (less (intgr 4) (intgr 3)))))
	(is (ev (jand (grt (intgr 4) (intgr 3)) (grt (intgr 4) (intgr 3)))))
	(is (ev (jor (grt (intgr 4) (intgr 3)) (less (intgr 4) (intgr 3)))))
	(isnt (ev (jor (less (intgr 4) (intgr 3)) (less (intgr 4) (intgr 3)))))
)

(deftest tuple-construction
	(is (= (ev (ctuple (intgr 1) (intgr 2))) [1 2]) "integer tuple construction"))
	
(deftest relation-construction
	(is (= (ev (cset (ctuple (intgr 1) (intgr 1)) (ctuple (intgr 2) (intgr 2))))
	{1 1, 2 2})
	"Identity relation on domain 1..2")
	
	;(is (= (ev (applyfun (cset (ctuple (intgr 1) (intgr 1)) (ctuple (intgr 2) (intgr 2))) 1))
	;1)
	;"apply id function on 1 = 1")
	
	)	



