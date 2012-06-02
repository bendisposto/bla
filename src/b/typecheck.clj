(ns b.typecheck 
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)) 

(defmacro rule [n vars t1 t2 rt]
  `(defn ~n [a# b#]
     (fn [t# in# out#]
       (fresh [btw#]
              (fresh ~vars
                     (a# ~t1 in# btw#)
                     (b# ~t2 btw# out#)
                     (== t# ~rt))))))

(defmacro rules
  ([n v a b c & more] `(do (rule ~n ~v ~a ~b ~c) (rules ~@more)))
  ([n v a b c] `(rule ~n ~v ~a ~b ~c)))


(rules
 ;name              type variables   left type   right type   result type
 ABelongPredicate   [t]              t           [:set t]     [:bool]
 AIncludesPredicate [t]              [:set t]    [:set t]     [:bool]
 AAddExpression     []               [:int]      [:int]       [:int]
 )

(defn AIntegerExpression [_] (fn [type in out] (all (== type [:int]) (== in out))))
(defn AIdentifierExpression [id] (fn [type in out] (appendo in [[id type]] out)))
(defn ANatSetExpression [] (fn [type in out] (all (== type [:set [:int]]) (== in out))))

(defn typecheck [ast] (let [res (first (run* [q] (fresh [t,o] (ast t [] q))))] (into {} res)))

(defn t1 [] (run* [q] (fresh [t,o] ((AAddExpression (AIntegerExpression 4) (AIdentifierExpression :x)) t [] o) (== q {:type t, :env o}))))
(defn t2 [] (typecheck (AAddExpression (AIntegerExpression 4) (AIdentifierExpression :x))))
(defn t3 [] (typecheck (ABelongPredicate (AIdentifierExpression :x) (ANatSetExpression))))
(defn t4 [] (typecheck (ABelongPredicate (AIdentifierExpression :e) (AIdentifierExpression :S))))
















