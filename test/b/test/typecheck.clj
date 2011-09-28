(ns b.test.typecheck
  (:use [b.typecheck])
  (:use [b.interpreter])
  (:use [clojure.test]))


(deftest simple-typecheck 
	(let [ast  '(AIdentifierExpression x)]
       (is (= (typecheck ast) [ast {:x :unknown}]) "Simple Identifier")
))

