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

(defmacro analog
  ([f g] `(def ~g ~f))
  ([f g & more] `(do (analog ~f ~g) (analog ~f ~@more))))


(rules
 ;name                         type variables   left type   right type   result type
 AMemberPredicate              [t]              t           [:set t]     [:bool]
 ASubsetPredicate              [t]              [:set t]    [:set t]     [:bool]
 AEqualPredicate               [t]              t           t            [:bool]
 AMinusOrSetSubtractExpression [t]              t           t            t
 AConjunctPredicate            []               [:bool]     [:bool]      [:bool]
 AAddExpression                []               [:int]      [:int]       [:int]
 ALessPredicate                []               [:int]      [:int]       [:bool]
 AIntervalExpression           []               [:int]      [:int]       [:set [:int]]
 AUnionExpression              [t]              [:set t]    [:set t]     [:set t]
)

(analog AConjunctPredicate ADisjunctPredicate AImplicationPredicate AEquivalencePredicate)
(analog AAddExpression ADivExpression AModExpression APowerOfExpression)
(analog ALessPredicate ALessEqualPredicate AGreaterPredicate AGreaterEqualPredicate)
(analog ASubsetPredicate ASubsetStrictPredicate ANotSubsetStrictPredicate ANotSubsetPredicate)
(analog AEqualPredicate ANotEqualPrdicate)
(analog AMemberPredicate ANotMemberPredicate)
(analog AUnionExpression AIntersectionExpresson ASetSubtractionExpression)


(defn AMultOrCartExpression [a b] 
	(fn [type in out] 
		(fresh [btw s t]
			   (conde ((a [:int] in btw)   (b [:int] btw out) (== type [:int]))
		              ((a [:set s] in btw) (b [:set t] btw out) (== type [:cart s t]))))))



(defmacro AConvertBoolExpression [p] `(~p))
(defmacro AForallPredicate [_ p] `(~p))
(defmacro AExistsPredicate [_ p] `(~p))

(defn APowSubsetExpression [e] (fn [type in out]  (fresh [t] (e t in out) (== type [:set t]))))
(analog APowSubsetExpression  APow1SubsetExpression AFinSubsetExpression AFin1SubsetExpression)

(defn identity [e] (fn [type in out] (fresh [t] (e t in out) (== type [:cart t t]))))

(defn AFinitePredicate [e] (fn [type in out] (fresh [t] (== type [:set t]) (e type in out))))
(defn ANegationPredicate [_] (fn [type in out] (all (== type [:bool]) (== in out))))

(defn AIntegerExpression [_] (fn [type in out] (all (== type [:int]) (== in out))))
(analog AIntegerExpression AUnaryMinusExpression)

(defn ACardExpression [e] (fn [type in out] (fresh [t] (== type [:int]) (e [:set t] in out))))
(defn AMaxExpression [e] (fn [type in out] (all (== type [:int]) (e [:set [:int]] in out))))
(analog AMaxExpression AMinExpression)



(defn AIdentifierExpression [id]
  (fn [type in out]
    (fresh [t]
           (conda ((membero [id t] in) (== t type) (== in out))
                  ((appendo in [[id type]] out))))))

(defn ANatSetExpression [] (fn [type in out] (all (== type [:set [:int]]) (== in out))))
(defn ASuccessorExpression [] (fn [type in out] (all (== type [:cart [:int] [:int]]) (== in out))))
(analog ASuccessorExpression APredecessorExpression)


(defn typecheck [ast] (let [res (first (run* [q] (fresh [t,o] (ast t [] q))))] (into {} res)))


















