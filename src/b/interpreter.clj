(ns b.interpreter
  (:gen-class)	
   (:require [b.reader :as reader])
   (:require [b.typecheck :as type])
   (:require [b.sets :as set])
   (:use clojure.algo.monads)
)

(def wd-state-m (maybe-t state-m))
				
(defmacro lift 
	([name op n]        `(def ~name (with-monad wd-state-m (m-lift ~n ~op))))
	([name op n & more] `(do (lift ~name ~op ~n) (lift ~@more))))

(defmacro skip 
	([op] `(defmacro ~op [c#] c#))
	([op & more] `(do (skip ~op) (skip ~@more))))

(skip alpha AExpressionParseUnit APredicateParseUnit Start AConvertBoolExpression)
(declare bmod bdiv bminus bmult powerset notmember bmin bmax brange bcouple)

(def AIdentifierExpression fetch-val)
(def AIntegerExpression (with-monad wd-state-m m-result))
(defn ASetExtensionExpression [& x] (with-monad wd-state-m ((m-lift 1 (partial into #{})) (m-seq x))))
(defn ANatSetExpression [] (with-monad wd-state-m (m-result set/natural)))

(lift                                                                       
	 AAddExpression                          +                               2
	 AModuloExpression                       bmod                            2
	 ADivExpression                          bdiv                            2
	 AConjunctPredicate                      #(and % %2)                     2
	 ADisjunctPredicate                      #(or % %2)                      2
	 ANegationPredicate                      #(not %)                        1
	 AEqualPredicate                         =                               2
	 AUnaryExpression                        (partial * -1)                  1 
	 AEmptySetExpression                     (constantly #{})                0
	 AUnionExpression                        set/union                       2        
	 AIntersectionExpression                 set/intersection                2
	 AMinusOrSetSubtractExpression           bminus                          2
	 AMultOrCartExpression                   bmult                           2
	 APowSubsetExpression                    powerset                        1
	 ACardExpression                         count                           1
         ANotBelongPredicate                     notmember                       2
         AMinExpression                          bmin                            1
         AMaxExpression                          bmax                            1
         AIntervalExpression                     brange                          2
         ALessEqualPredicate                     <=                              2
  	 AGreaterEqualPredicate                  >=                              2
         ACoupleExpression                       bcouple                         2
         ATrueExpression                         (constantly true)               0
         AFalseExpression                        (constantly false)              0         
   	)                             

(defmacro AImplicationPredicate [x y] `(ADisjunctPredicate (ANegationPredicate ~x) ~y))   
(defmacro AEquivalencePredicate [x y] `(AConjunctPredicate (AImplicationPredicate ~x ~y) (AImplicationPredicate ~y ~x)))
(defmacro AUnequalPredicate [x y] `(ANegationPredicate (AEqualPredicate ~x ~y)))
(defmacro APow1SubsetExpression [x] `(AMinusOrSetSubtractExpression (APowSubsetExpression ~x) (APowSubsetExpression (AEmptySetExpression))))
(defmacro AFinSubsetExpression [x] `(APowSubsetExpression ~x))
(defmacro AFin1SubsetExpression [x] `(APow1SubsetExpression ~x))
(defmacro ABelongPredicate [A B] `(ANegationPredicate (ANotBelongPredicate ~A ~B)))
(defmacro ALessPredicate [A B] `(ANegationPredicate (AGreaterEqualPredicate ~A ~B)))
(defmacro AGreaterPredicate [A B] `(ANegationPredicate (ALessEqualPredicate ~A ~B)))
(defmacro AIncludePredicate [A B] `(ABelongPredicate ~A (APowSubsetExpression ~B)))

;(defmacro AIncludeStrictlyPredicate [A B] `(AConjunctPredicate (AIncludePredicate ~A ~B) (AUnequalPredicate ~A ~B)))
;(defmacro ANotIncludePredicate [A B] `(ANegationPredicate (AIncludePredicate ~A ~B)))
;(defmacro ANotIncludeStrictlyPredicate [A B] `(ANegationPredicate (AIncludeStrictlyPredicate ~A ~B)))

; ------------           MAIN           ------------           

(defn evaluate [env [ast types]] ;(println "evaluate " ast "in" env) 
                   ((eval ast) env))
(defn run [text, env]  (->> text reader/parse type/typecheck (evaluate env)))
(defn -main[ arg]
    (println (reader/parse arg))) 

; ------------           HELPER           ------------ 

(defn bcouple [a b] [a b])

(defn bmod [n m] 
	(if (or (< n 0) (<= m 0)) nil (rem n m)))
	
(defn bdiv [n m]
	(if (or (< n 0) (<= m 0)) nil (int (/ n m))))
	
(defn bmin [S] (if (and (seq S) (every? number? S)) (apply min S) nil))
(defn bmax [S] (if (and (seq S) (every? number? S)) (apply max S) nil))	
	
(defn bminus [a b]
	(cond (and (set? a) (set? b)) (set/difference a b)
	      (and (number? a) (number? b)) (- a b)
	      :else nil))		

(defn bmult [a b]
	(cond (and (set? a) (set? b)) (into #{} (for [x a y b] [x y]))
	      (and (number? a) (number? b)) (* a b)
	      :else nil))		

(defn powerset [ls]
    (if (empty? ls) #{#{}}
        (set/union (powerset (next ls))
                    (into #{} (map #(conj % (first ls)) (powerset (next ls)))))))

	
(defn notmember [e S] (nil? (S e)))	
(defn member [e S] (not (notmember e S)))
(defn brange [m n] (into #{} (range m (inc n))))

;(defn pow [k n]
;(if (= k n) (recur 1 (inc n))
;  (do (println k) (recur (inc k) n))))

;(def pow-int (pow [1] [] [] 2))