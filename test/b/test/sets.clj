(ns b.test.sets
  (:use b.sets)
;  (:import b.sets.B-Set)
  (:import b.sets.PredicateSet)
  (:use midje.sweet))



(tabular "Members of Natural"
         (fact (member? natural ?e) => ?tf)
         ?e    | ?tf    
         1     | true  
         2    |  true  
         -1    | false
         0     | true)

(fact "Explicit sets are always finite"
  (finite? #{1}) => true
  (finite? #{}) => true)

(fact
  (let [foo (as-predicate-set #{1,9,10})]
    (member? foo 1) = > truthy
    (member? foo 2) => falsey
    (member? foo 9) => truthy
    (member? foo 10) => truthy))

(fact "Combination of finite, explicit sets"
  (union #{1,2,3} #{8,9}) => (just [1,2,3,8,9] :in-any-order)
  (intersection #{1,2,3} #{8,9}) => (just [])
  (intersection #{1,2,3,4,5,6} #{3,2,8,9}) => (just [2,3] :in-any-order)
  (difference #{1,2,3,4,5,6,7} #{3,4,5,6}) => (just [1,2,7] :in-any-order)
  (difference #{1,2,3,4,5,6,7} #{3,4,5,6,9,10,11}) => (just [1,2,7] :in-any-order))

(fact "Card of finite, explicit sets"
  (card #{1,2,3,4,5,6}) => 6
  (card #{}) => 0
  (card #{1,2}) => 2)

(fact "Card and Union"
  (card (union #{1,2} #{3,4})) => 4
  (card (union #{1} #{1,2})) => 2)

(fact "Union/Intersection/Difference of Predicate Sets"
  (let [s1 (PredicateSet. int-type #(< 2 % 6))
        s2 (PredicateSet. int-type #(< 4 % 10))
        u (union s1 s2)
        i (intersection s1 s2)
        s3 (PredicateSet. int-type #(#{2,3,6,7} %))
        d (difference u s3)]
    (map (partial member? u) [2,3,4,5,6,7,8,9,10]) => (seq [false true true true true true true true false])
    (map (partial member? i) [3,4,5,6]) => (seq [false false true false])
    (map (partial member? d) [3,4,5,6,7,8,9,10]) => (seq [false true true false false true true false])))

(fact "Mixing explicit and predicate Sets"
  (bounded-enumerate (union #{-1,-2} natural) (int-range -2 2)  1000) => #{-2,-1,0,1,2}
  (bounded-enumerate (union natural #{-1,-2}) (int-range -2 2)  1000) => #{-2,-1,0,1,2}
  (bounded-enumerate (intersection #{-1,-2} natural) (constantly true) 1000) => #{}
  (bounded-enumerate (intersection natural #{-1,-2}) (constantly true) 1000) => #{}
  (bounded-enumerate (intersection natural #{17,4}) (constantly true) 1000) => #{4,17})

(fact "Enumerator of an intersection with a finite set ist always finite"
  (let [s1 natural s2 #{1,2,3,4} e (enum-type (intersection s1 s2))]
    (finite? e) => true))

(comment (fact "Powerset"
   (powerset #{}) => #{#{}}
   (powerset #{1}) => #{#{}, #{1}}))


(defmacro px
  ([v] `(fn [foo#] ~v))
  ([v h & t] `(fn [e#] (for [~h e#] ((px ~v ~@t) (range 0 ~h))))))

(defn mk_enum [n v] (if (> n 0)
                      (let [s (gensym) body ((mk_enum (dec n) (conj v s)) (range 0 s))] (fn [e] (for [s e] body)))
                      (constantly v)))

(macroexpand '((let [a (symbol "a") b (symbol "b")] (px [a b] a b)) (range 0 10)))


