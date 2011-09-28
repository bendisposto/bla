(ns b.interpreter
   (:gen-class)	
   (:refer-clojure :exclude [compile])
   (:require [b.reader :as reader])
   (:require [b.typecheck :as type])
)

(defmacro lift 
	([name op n] (let [args (into [] (for [_ (range 0 n)] (gensym)))] 
           `(defn ~name ~args (fn [s#] (apply ~op ((apply juxt ~args) s#))))))
	([name op n & more] `(do (lift ~name ~op ~n) (lift ~@more))))

(defmacro skip 
	([op] `(defmacro ~op [c#] c#))
	([op & more] `(do (skip ~op) (skip ~@more))))

(defn andf [a b] (and a b))

(lift 
	 AAddExpression       +        2 
	 AModuloExpression    mod      2 
	 ADivExpression       /        2 
	 AAndExpression       andf     2 
	 AEqualPredicate      =        2 
	 AIntegerExpression   identity 1
	)
	
(skip AExpressionParseUnit APredicateParseUnit Start AConvertBoolExpression)

(defn AIdentifierExpression [x] (fn [e] (e x)))
(def compile eval)
(defn evaluate [env [ast types]] ;(println "evaluate " ast "in" env) 
         ((compile ast) env))
(defn run [text, env]  (->> text reader/parse type/typecheck (evaluate env)))
(defn -main[ arg]
    (println (reader/parse arg)))