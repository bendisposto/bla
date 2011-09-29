(ns b.test.interpreter
  (:use [b.interpreter])
  (:require [b.reader :as reader])
  (:use [clojure.test])
   (:use [clojure.algo.monads]))

(def empty-env {})

(defmacro exp [x] `(str "#EXPRESSION" ~x))
(defmacro pred [x] `(str "#PREDICATE" ~x))
(defmacro verify 
	([text expected]     `(is (= (run ~text {}) [~expected {}])))
	([text expected env] `(is (= (run ~text ~env) [~expected ~env])))
	([text expected in out] `(is (= (run ~text ~in) [~expected ~out]))))

(defmacro predtest [predicate expect] (let [name (gensym)] `(deftest ~name (verify (pred ~predicate) ~expect))))
(defmacro istrue [predicate] `(predtest ~predicate true))
(defmacro isfalse [predicate] `(predtest ~predicate false))	
(defmacro check [expression expect] (let [name (gensym)] `(deftest ~name (verify (exp ~expression) ~expect))))	
	
(deftest test-lookup (verify (exp "a") 3 {:a 3}))
(deftest test-lookup-and-add (verify (exp "1+a") 4 {:a 3}))

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

(check "4" 4)
(check "1+1" 2)
(check "4+6" 10)
(check "4+6+1" 11)
(check "10 mod 5" 0)
(check "10 mod 4" 2)
(check "1 mod 4" 1)
(check "1 / 1" 1)
(check "2 / 1" 2)
(check "1 / 2" 0)



