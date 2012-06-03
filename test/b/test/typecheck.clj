(ns b.test.typecheck
  (:use [clojure.test])
  (:use [b.typecheck])
  (:use [midje.sweet]))

(fact
  (typecheck (AAddExpression (AIntegerExpression 4) (AIdentifierExpression :x))) => {:x [:int]}
  (:x (typecheck (AMemberPredicate (AIdentifierExpression :x) (ANatSetExpression)))) => [:int]
  (:x (typecheck (ASubsetPredicate (AIdentifierExpression :x) (ANatSetExpression)))) => [:set [:int]]
  (:x (typecheck (AEqualPredicate (AIdentifierExpression :x) (ANatSetExpression)))) => [:set [:int]]
  (:x (typecheck (AEqualPredicate (AIdentifierExpression :x) (AIntegerExpression 1)))) => [:int]
  (:x (typecheck (AConjunctPredicate (AEqualPredicate (AIdentifierExpression :x) (AIdentifierExpression :y)) (AEqualPredicate (AIdentifierExpression :y) (AIntegerExpression 1))))) => [:int]
  (:x (typecheck (AMultOrCartExpression (AIdentifierExpression :x) (AIntegerExpression 1)))) => [:int]
  (first (:y (typecheck (AEqualPredicate (AIdentifierExpression :y) (AMultOrCartExpression (AIdentifierExpression :x) (ANatSetExpression)))))) => :cart
  )

