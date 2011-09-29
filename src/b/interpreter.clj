(ns b.interpreter
   (:gen-class)	
   (:refer-clojure :exclude [compile])
   (:require [b.reader :as reader])
   (:require [b.typecheck :as type])
   (:use clojure.algo.monads)
)

				
(defmacro lift 
	([name op n]        `(def ~name (with-monad state-m (m-lift ~n ~op))))
	([name op n & more] `(do (lift ~name ~op ~n) (lift ~@more))))
	
(def AIntegerExpression (with-monad state-m m-result))

(defmacro skip 
	([op] `(defmacro ~op [c#] c#))
	([op & more] `(do (skip ~op) (skip ~@more))))


(lift                             
	 AAddExpression       +             2
	 AModuloExpression    mod           2
	 ADivExpression       (comp int /)  2
	 AConjunctPredicate   #(and % %2)   2
	 AEqualPredicate      =             2
	 AGreaterPredicate    >             2
	 ALessPredicate       <             2 
	)                             
   
(skip AExpressionParseUnit APredicateParseUnit Start AConvertBoolExpression)

(def AIdentifierExpression fetch-val)

(def compile eval)
(defn evaluate [env [ast types]] ;(println "evaluate " ast "in" env) 
         ((compile ast) env))
(defn run [text, env]  (->> text reader/parse type/typecheck (evaluate env)))
(defn -main[ arg]
    (println (reader/parse arg)))