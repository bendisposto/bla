(ns b.test.interpreter
  (:use [b.interpreter])
  (:require [b.reader :as reader])
  (:use [clojure.test])
  (:use b.sets)
  (:use midje.sweet)
  (:use [clojure.algo.monads]))

(def empty-env {})

(defn exp [x] (str "#EXPRESSION " x))
(defn pred [x] (str "#PREDICATE " x))
(defn ass [x] (str "#SUBSTITUTION" x))

(defn- compute [f t] (-> t f (run {}) first))
(defn- compute-env [e t] (-> (ass t) (run e) second))

(fact "lookup"
  (run (exp "a") {:a 3}) => [3 {:a 3}])

(fact "lookup and add"
  (run (exp "1+a") {:a 3}) => [4 {:a 3}])

(tabular "Arithmetic Assignments"
         (fact (compute-env ?in ?a) => ?out)
         ?in | ?a | ?out
         {:x 0} |  "x := 1" | {:x 1})

(tabular "True statements" 
         (fact (compute pred ?p) => truthy)
         ?p
         "1=1"
         "5 = 5"
         "1 > 0"
         "1=1 & 2>1"
         "1 < 3"
         "1 = 2 or 1 < 2"
         "1 = 1 or 4 < 2"
         "1 = 1 => 1 < 2"
         "1 > 1 => 1 < 2"
         "1 > 1 => 1 = 1"
         "not (3 < 2)"
         "(1 = 1) <=> (2 = 2)"
         "(1 < 1) <=> (2 > 2)"
         "1 /= 2"
         "card({1,2,3} * {4,8,3,1,21}) = card({1,2,3}) * card({4,8,3,1,21})"
         "3 * 5 = 5 * 3"
         "3 + 5 = 5 + 3"
         "1 + (2 + 3) = (1 + 2) + 3"
         "4 * (2 * 3) = (4 * 2) * 3"
         "5 : {4,5,6,7,1}"
         "15 /: {4,5,6,7,1}"
         "15 /: {}"
         "0 >= -2"
         "4 >= 4"
         "0 <= 4"
         "0 <= 0"
         "{1,2} <: {1,2}"
         "TRUE = TRUE"
         "FALSE = FALSE"
         "bool( 1 = 1) = TRUE"
         "bool(1=2) = FALSE"
         )

(tabular "False statements" 
         (fact (compute pred ?p) => falsey)
         ?p
         "1 = 5"
         "5 = 1"
         "0 > 1"
         "1+1 > 5"
         "1=1 & 1+1 > 5"
         "11 < 5"
         "1 > 1 or 4 < 2"
         "1 = 1 => 1 = 2"
         "not (1 < 2)"
         "(1 > 1) <=> (2 = 2)"
         "(1 = 1) <=> (2 < 2)"
         "1 /= 1"
         "15 : {4,5,6,7,1}"
         "15 : {}"
         "5 /: {4,5,6,7,1}"
         "55 <= 4"
         "3 >= 4"
         "TRUE = FALSE"
         "FALSE = TRUE"
         )

(tabular "Not yet working Predicates"
         (fact (compute pred ?p) =future=> ?r)
         ?p                            | ?r
         "!(x).((x = 1) => (x = 1))"   | truthy
         "!(x).((x:INT) => (x > 0))"   | falsey
         "#(x).((x : NAT1) & (x < 1))" | falsey
         "#(x).((x:INT) & (x > 0))"    | truthy
         "{1,2} <: {1,2}"              | truthy
         "{1,2} <: {1}"                | falsey
         "{1,2} <: {1,2,3}"            | truthy
         "{2} <: {1,2}"                | truthy
         "{} <: {1,2}"                 | truthy
         "{} /<: {1,2}"                | falsey
         "{3} /<: {1,2}"               | truthy
         "{1,2} <<: {1,2,3}"           | truthy
         "{1,2} <<: {1,2}"             | falsey
         "{1,2} /<<: {1,2,3}"          | falsey
         "{1,2} /<<: {1,2}"            | truthy) 



(tabular "Expressions"
         (fact (compute exp ?e) => ?r)
        ?e                                                       |      ?r
        "4" 							 |  	4
        "1+1" 							 |  	2
        "4+6" 							 |  	10
        "4+6+1" 						 |  	11
        "1 / 1" 						 |  	1
        "2 / 1" 						 |  	2
        "1 / 2"							 |      0
        "1 / 0" 						 |  	nil
        "-4 / 2"						 |  	nil
        "4 / -2" 						 |  	nil
        "10 mod 5"						 |  	0
        "10 mod 4" 						 |  	2
        "1 mod 4" 						 |  	1
        "1 mod 2" 						 |  	1
        "1 mod 0" 						 |  	nil
        "4 mod -2" 						 |  	nil
        "-4 mod 2" 						 |  	nil
        "(-4 mod 2) + 14" 				         |  	nil
        "-4 mod 2 + 14" 			         	 |  	nil
        "4-2"							 |  	2 
        "4*2"							 |  	8 
        "{}" 							 |  	#{}
        "{1}" 							 |  	#{1}
        "{1,2}" 						 |  	#{1,2}
        "{1,2,3}" 						 |  	#{1,2,3}
        "1 |-> 2" 						 |  	[1,2]
        "{1,2}*{3,4}"					         |  	#{[1 3] [1 4] [2 3] [2 4]}
        "card({})" 						 |  	0
        "card({1})" 					         |  	1
        "card({1,2})" 					         |  	2
        "min({1,2,3,4})"				         |  	1
        "min({3,4})" 					         |  	3
        "min({})" 						 |  	nil
        "max({1,2,3,4})"				         |  	4
        "max({3,4})"					         |  	4
        "max({})" 						 |  	nil
        "1..4" 							 |  	#{1,2,3,4}
        "0..2" 							 |  	#{0,1,2}
        "card({1,2,3} * {4,8,3,1,21})"                           |      (* 3 5)
        "POW({})"                                                |      #{#{}}
        "POW({1})"                                               |      #{#{}, #{1}}
        "POW({1,2})"                                             |      #{#{}, #{1}, #{2}, #{1,2}}
        "POW({1}) - {{}}"                                        |      #{#{1}}
        "POW1({})"                                               |      #{}
        "POW1({1})"                                              |      #{#{1}}
        "POW1({1,2})"                                            |      #{#{1}, #{2}, #{1,2}}
        "FIN({})"                                                |      #{#{}}
        "FIN({1})"                                               |      #{#{}, #{1}}
        "FIN({1,2})"                                             |      #{#{}, #{1}, #{2}, #{1,2}}
        "FIN({1}) - {{}}"                                        |      #{#{1}}
        "FIN1({})"                                               |      #{}
        "FIN1({1})"                                              |      #{#{1}}
        "FIN1({1,2})"                                            |      #{#{1}, #{2}, #{1,2}}
        "{1,2,3,4} /\\ {2,4}"                                    |      #{2,4}
        "{1,2} \\/ {3,4}"                                        |      #{1,2,3,4}
        "{1,2,3,4} /\\ {}"                                       |      #{}
        "{1,2,3,4} \\/ {}"                                       |      #{1,2,3,4}
        "{1,2,3,4} - {2,4}"                                      |      #{1,3}
        "{} - {1,2,3}"                                           |      #{}
        "{1,2} - {}"                                             |      #{1,2}
        "TRUE"                                                   |      true
        "FALSE"                                                  |      false) 

(def all (constantly true))

(tabular "Set Enumeration Expressions"
  (fact (bounded-enumerate (compute exp ?e) ?p 1000) => ?r)
  ?e | ?p | ?r
  "NAT"                                        | #(< % 3)          | #{0,1,2}
  "NAT"                                        | #(<= % 3 )        | #{0,1,2,3}
  "NAT /\\ {-1,2,3}"                           | all               | #{2,3}
  "NAT \\/ {-1}"                               | (int-range -10 2) | #{-1,0,1,2}
;  "POW(NAT)"                                   | #(< 3 %)          |
;  #{#{}, #{1}, #{2}, #{1,2}}
  )


(comment (tabular "Set Enumeration Predicates"
          (fact (compute pred ?e) => ?r)
          ?e | ?p | ?r
          "{1,2} <: NAT"                                        | all      | true))


