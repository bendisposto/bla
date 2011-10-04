(ns b.test.interpreter
  (:use [b.interpreter])
  (:require [b.reader :as reader])
  (:use [clojure.test])
  (:use b.sets)
  (:use [clojure.algo.monads]))

(def empty-env {})

(defmacro exp [x] `(str "#EXPRESSION " ~x))
(defmacro pred [x] `(str "#PREDICATE " ~x))
(defmacro verify 
	([x text expected]     `(is (= (run ~text {}) [~expected {}]) ~x))
	([x text expected env] `(is (= (run ~text ~env) [~expected ~env]) ~x))
	([x text expected in out] `(is (= (run ~text ~in) [~expected ~out]) ~x)))

(defmacro predtest [predicate expect] (let [name (gensym)] `(deftest ~name (verify ~predicate (pred ~predicate) ~expect))))
(defmacro istrue [predicate] `(predtest ~predicate true))
(defmacro isfalse [predicate] `(predtest ~predicate false))	
(defmacro check [expression expect] (let [name (gensym)] `(deftest ~name (verify ~expression (exp ~expression) ~expect))))	

(defmacro checks[_ _])

(deftest test-lookup (verify "lookup a in {:a 3}" (exp "a") 3 {:a 3}))
(deftest test-lookup-and-add (verify "lookup a+1 in {:a 3}" (exp "1+a") 4 {:a 3}))

(istrue "1 = 1")
(istrue "5 = 5")
(isfalse "5 = 1")
(istrue "1 > 0")
(isfalse "0 > 1")
(isfalse "1+1 > 5")
(istrue "1=1 & 2>1")
(isfalse "1=1 & 1+1 > 5")
(istrue "1 < 3")
(isfalse "11 < 5")
(istrue "1 = 2 or 1 < 2")
(istrue "1 = 1 or 4 < 2")
(isfalse "1 > 1 or 4 < 2")
(istrue "1 = 1 => 1 < 2")
(istrue "1 > 1 => 1 < 2")
(istrue "1 > 1 => 1 = 1")
(isfalse "1 = 1 => 1 = 2")
(istrue "not (3 < 2)")
(isfalse "not (1 < 2)")

(istrue  "(1 = 1) <=> (2 = 2)")
(istrue  "(1 < 1) <=> (2 > 2)")
(isfalse "(1 > 1) <=> (2 = 2)")
(isfalse "(1 = 1) <=> (2 < 2)")
(istrue "1 /= 2")
(isfalse "1 /= 1")
;(istrue "!(x).((x = 1) => (x = 1))") ; Universal quantification
; Existential quantification 

(istrue "card({1,2,3} * {4,8,3,1,21}) = card({1,2,3}) * card({4,8,3,1,21})")
(istrue "3 * 5 = 5 * 3")
(istrue "3 + 5 = 5 + 3")
(istrue "1 + (2 + 3) = (1 + 2) + 3")
(istrue "4 * (2 * 3) = (4 * 2) * 3")
(istrue "5 : {4,5,6,7,1}")
(isfalse "15 : {4,5,6,7,1}")
(isfalse "15 : {}")

(isfalse "5 /: {4,5,6,7,1}")
(istrue "15 /: {4,5,6,7,1}")
(istrue "15 /: {}")

;(istrue "{1,2} <: {1,2}")
;(isfalse "{1,2} <: {1}")
;(istrue "{1,2} <: {1,2,3}")
;(istrue "{2} <: {1,2}")
;(istrue "{} <: {1,2}")
;
;(isfalse "{} /<: {1,2}")
;(istrue "{3} /<: {1,2}")
;
;(istrue "{1,2} <<: {1,2,3}")
;(isfalse "{1,2} <<: {1,2}")
;
;(isfalse "{1,2} /<<: {1,2,3}")
;(istrue "{1,2} /<<: {1,2}")

(istrue "0 <= 4")
(istrue "0 <= 0")
(isfalse "55 <= 4")

(istrue "0 >= -2")
(istrue "4 >= 4")
(isfalse "3 >= 4")

(check "4" 4)
(check "1+1" 2)
(check "4+6" 10)
(check "4+6+1" 11)

(check "1 / 1" 1)
(check "2 / 1" 2)
(check "1 / 2" 0)
(check "1 / 0" nil)
(check "-4 / 2" nil)
(check "4 / -2" nil)

(check "10 mod 5" 0)
(check "10 mod 4" 2)
(check "1 mod 4" 1)
(check "1 mod 2" 1)
(check "1 mod 0" nil)
(check "4 mod -2" nil)
(check "-4 mod 2" nil)

(check "(-4 mod 2) + 14" nil)
(check "-4 mod 2 + 14" nil)
(check "4-2", 2) ; minus
(check "4*2", 8) ; multiplication


(check "{}" #{})
(check "{1}" #{1})
(check "{1,2}" #{1,2})
(check "{1,2,3}" #{1,2,3})



(checks "{1} \\/ {2,3}" #{1,2,3})
(checks "{1} \\/ {}" #{1})
(checks "{} \\/ {1,2}" #{2,1})
(checks "{1} \\/ {1}" #{1})
(checks "{1} \\/ {1,2}" #{1,2})

(checks "{1,3,4} /\\ {1,2,3}" #{1,3})
(checks "{4} /\\ {1,2,3}" #{})
(checks "{} /\\ {1,2,3}" #{})
(checks "{1,2,3,4,5} - {1,3}" #{2,4,5}) ; Set difference


(comment
(check "1 |-> 2", [1,2])
(checks "{1 |-> 2, 2 |-> 3}", #{[1,2] [2,3]})
(check "{1,2}*{3,4}",#{[1 3] [1 4] [2 3] [2 4]})
)

;(checks "POW({})",#{#{}})
;(checks "POW({1})",#{#{}, #{1}})
;(checks "POW({1,2})",#{#{}, #{1}, #{2}, #{1,2}})
;(checks "POW({1}) - {{}}" #{#{1}})

;(check "POW1({})",#{})
;(check "POW1({1})",#{#{1}})
;(check "POW1({1,2})",#{#{1}, #{2}, #{1,2}})
;
;(check "FIN({})",#{#{}})
;(check "FIN({1})",#{#{}, #{1}})
;(check "FIN({1,2})",#{#{}, #{1}, #{2}, #{1,2}})
;(check "FIN({1}) - {{}}" #{#{1}})
;
;(check "FIN1({})",#{})
;(check "FIN1({1})",#{#{1}})
;(check "FIN1({1,2})",#{#{1}, #{2}, #{1,2}})

(check "card({})" 0)
(check "card({1})" 1)
(check "card({1,2})" 2)
(check "card({1,2,3} * {4,8,3,1,21})" (* 3 5))

(check "min({1,2,3,4})", 1)
(check "min({3,4})", 3)
(check "min({})" nil)

(check "max({1,2,3,4})", 4)
(check "max({3,4})", 4)
(check "max({})" nil)

(check "1..4" #{1,2,3,4})
(check "0..2" #{0,1,2})


(deftest experimente 
	(is (= (set? #{1,2}) true))
	(is (= (number? 6) true))
;	(is (= (powerset #{}) #{#{}}))
;	(is (= (powerset #{1}) #{ #{}, #{1}}))
;	(is (= (powerset #{1,2}) #{#{}, #{1}, #{2}, #{1,2}}))
	)

