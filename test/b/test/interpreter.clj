(ns b.test.interpreter
  (:use [b.interpreter])
  (:require [b.reader :as reader])
  (:use [clojure.test])
   (:use [clojure.algo.monads]))

(def empty-env {})
(defmacro isnt [p] `(is (not ~p)))

(deftest test-integer
		 (is (= (run "#EXPRESSION 3" empty-env) [3 empty-env])))

(deftest test-addition
		 (is (= (run "#EXPRESSION 3 + 1" empty-env) [4 empty-env])))
;
;(deftest test-lookup
;		 (is (= (run "#EXPRESSION a" {:a 12}) 12)))
;		
;(deftest test-lookup_and-add
;		 (is (= (run "#EXPRESSION a + 7" {:a 12}) 19)))		