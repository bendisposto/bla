(ns b.test.sets
  (:use [b.sets])
  (:import b.sets.BSet)
  (:import b.sets.PredSet)
  (:use midje.sweet))

(tabular "member? tests"
 (fact (member? ?s ?e) => ?tf)
 ?s    |    ?e     | ?tf
 nat   |    1      | true
 nat   |    -1     | false
 nat   |   0       | true)


(fact "elements of nat (bounded)"
      (bounded-elements nat #(< -2 % 4)) => (just [0 1 2 3] :in-any-order))

(fact "union"
      (bounded-elements (union #{1,2,3} #{8,9}) #(< -2 % 40))  => (just [1,2,3,8,9] :in-any-order))


