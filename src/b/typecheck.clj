(ns b.typecheck 
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))


;(defn binopo [btw vars t1 t2 rt]
;  (f)
;)

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

(defmacro junctor
  ([n] `(rule ~n [] [:bool] [:bool] [:bool]))
  ([n & more] `(do (junctor ~n) (junctor ~@more))))

(defmacro arithmetic
  ([n] `(rule ~n [] [:int] [:int] [:int]))
  ([n & more] `(do (arithmetic ~n) (arithmetic ~@more))))

(defmacro arithmetic_predicate
  ([n] `(rule ~n [] [:int] [:int] [:bool]))
  ([n & more] `(do (arithmetic_predicate ~n) (arithmetic_predicate ~@more))))


(junctor AConjunctPredicate ADisjunctPredicate AImplicationPredicate AEquivalencePredicate)
(arithmetic AAddExpression)
(arithmetic_predicate ALessPredicate ALessEqualPredicate AGreaterPredicate AGreaterEqualPredicate)


(rules
 ;name                         type variables   left type   right type   result type
 AMemberPredicate              [t]              t           [:set t]     [:bool]
 ASubsetPredicate              [t]              [:set t]    [:set t]     [:bool]
 AEqualPredicate               [t]              t           t            [:bool]
 AMinusOrSetSubtractExpression [t]              t           t            t
)


;(defn AMultOrCartExpression [a b] (fn [type in out] (conde ())  ))


(def ASubsetStrictPredicate ASubsetPredicate)
(def ANotSubsetStrictPredicate ASubsetPredicate)
(def ANotSubsetPredicate ASubsetPredicate)

(defmacro AForallPredicate [_ p] `(~p))
(defmacro AExistsPredicate [_ p] `(~p))

(def ANotEqualPrdicate AEqualPredicate)
(def ANotMemberPredicate AMemberPredicate)

(defn AFinitePredicate [e] (fn [type in out] (fresh [t] (== type [:set t]) (e type in out))))
(defn ANegationPredicate [_] (fn [type in out] (all (== type [:bool]) (== in out))))
(defn AIntegerExpression [_] (fn [type in out] (all (== type [:int]) (== in out))))
(defn AIdentifierExpression [id]
  (fn [type in out]
    (fresh [t]
           (conda ((membero [id t] in) (== t type) (== in out))
                  ((appendo in [[id type]] out))))))
(defn ANatSetExpression [] (fn [type in out] (all (== type [:set [:int]]) (== in out))))

(defn typecheck [ast] (let [res (first (run* [q] (fresh [t,o] (ast t [] q))))] (into {} res)))


















