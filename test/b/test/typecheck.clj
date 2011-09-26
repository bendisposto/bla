(ns b.test.typecheck
  (:use [b.typecheck])
  (:use b.core)
  (:use [clojure.test]))


(deftest simple-typecheck 
  (is (= (typecheck {} (AIdentifierExpression :x)) {:x :unknown}) "Simple Identifier")
)

