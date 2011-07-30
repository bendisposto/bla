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
  (is (ev (__< (intgr 3) (intgr 4)))  "3 < 4") 
  (isnt (ev (__< (intgr 4) (intgr 3)))  "not 4 < 3") 
  (is (ev (__<= (intgr 3) (intgr 4)))  "3 <= 4") 
  (is (ev (__<= (intgr 3) (intgr 3)))  "3 <= 3") 
  (isnt (ev (__<= (intgr 4) (intgr 3)))  "not 4 <= 3") 

  (isnt (ev (__> (intgr 3) (intgr 4)))  "not 3 > 4") 
  (is (ev (__> (intgr 4) (intgr 3)))  "4 > 3") 
  (isnt (ev (__>= (intgr 3) (intgr 4)))  "not 3 >= 4") 
  (is (ev (__>= (intgr 3) (intgr 3)))  "3 >= 3")
)

(deftest complex-constructs 
  (is (= (ev (__+ (intgr 1) (intgr 2))) 3) "__+ing 2 integers")
  (is (= (ev (__+ (__+ (intgr 1) (intgr 2)) (intgr 5))) 8) "__+ing 3 integers")
  (is (= ((__+ (vrb :a) (vrb :b)) {:a 1, :b 3}) 4) "__+ing vars")
  (is (= ((__+ (intgr 2) (vrb :b)) {:a 1, :b 3}) 5) "__+ing var to int")
)

(deftest set-construction
   (is (= (ev (cset (intgr 1) (intgr 2))) #{1,2}) "integer set construction")
   (is (= (ev (cset (intgr 1) (intgr 2) (intgr 3))) #{1,3,2}) "integer set construction"))

(deftest set-predicates
   (is (ev (__member (intgr 1) (cset (intgr 1) (intgr 2)))) "1 in {1,2}")
   (isnt (ev (__member (intgr 4) (cset (intgr 1) (intgr 2)))) "4 not in {1,2}"))

(deftest set-expressions
   (is (= (ev (__union (cset (intgr 1) (intgr 2))
        (cset (intgr 3) (intgr 2)))) #{1,2,3}) "{1,2} U {2,3} = {1,2,3}")
   (is (= (ev (__intersection (cset (intgr 1) (intgr 2))
	        (cset (intgr 3) (intgr 2)))) #{2}) "{1,2} ^ {2,3} = {2}")
   (is (= (ev (__difference (cset (intgr 1) (intgr 2))
			        (cset (intgr 3) (intgr 2)))) #{1}) "{1,2} - {2,3} = {1}")
)

(deftest junctors 
	(isnt (ev (__andf (__> (intgr 4) (intgr 3)) (__< (intgr 4) (intgr 3)))))
	(is (ev (__andf (__> (intgr 4) (intgr 3)) (__> (intgr 4) (intgr 3)))))
	(is (ev (__orf (__> (intgr 4) (intgr 3)) (__< (intgr 4) (intgr 3)))))
	(isnt (ev (__orf (__< (intgr 4) (intgr 3)) (__< (intgr 4) (intgr 3)))))
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



