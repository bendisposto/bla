(ns b.interpreter
   (:gen-class)	
   (:refer-clojure :exclude [compile])
   (:require [b.reader :as reader])
   (:require [b.typecheck :as type])
   (:use clojure.algo.monads)
)

;(defmacro lift_bin 
;	([name op] (let [args (into [] (for [_ (range 0 n)] (gensym)))] 
;           `(defn ~name ~args (fn [s#] (apply ~op ((apply juxt ~args) s#))))))
;	([name op & more] `(do (lift ~name ~op) (lift ~@more))))
	
(defmacro liftex [name op]	
	`(defn ~name [f1# f2#]
		(fn [s0#] (let 
			     [[v1# s1#] (f1# s0#)
				  [v2# s2#] (f2# s1#)]
				  [(~op v1# v2#) s2#]))))	
				
(defmacro liftexm [name op n]
	`(def ~name (with-monad state-m (m-lift ~n ~op))))
	
(defn AIntegerExpression [x] (fn [e] [x e]))
(liftexm AAddExpression + 2)


(defmacro skip 
	([op] `(defmacro ~op [c#] c#))
	([op & more] `(do (skip ~op) (skip ~@more))))

(defn andf [a b] (and a b))

;(lift                             
;	 AAddExpression       +       
;	 AModuloExpression    mod     
;	 ADivExpression       /       
;	 AAndExpression       andf    
;	 AEqualPredicate      =       
;	)                             
	
(skip AExpressionParseUnit APredicateParseUnit Start AConvertBoolExpression)

(def AIdentifierExpression fetch-val)

(def compile eval)
(defn evaluate [env [ast types]] (println "evaluate " ast "in" env) 
         ((compile ast) env))
(defn run [text, env]  (->> text reader/parse type/typecheck (evaluate env)))
(defn -main[ arg]
    (println (reader/parse arg)))