(ns b.sets (:gen-class))

(defprotocol Set
	 (member? [this e])
	 (predicate [this]))
	

(deftype PredicateSet [P] 
	Set 
	 (member? [_ e] (P e)))
	
(deftype IntersectionSet [S1 S2] 
	Set 
	 (member? [_ e] (and (member? S1 e) (member? S2 e)))
	 (predicate [_] (and (predicate S1) (predicate S2))))
	
(deftype UnionSet [S1 S2] 
	Set 
	 (member? [_ e] (or (member? S1 e) (member? S2 e)))
 	 (predicate [_] (or (predicate S1) (predicate S2))))
	
(deftype DifferenceSet [S1 S2]  
	Set 
	 (member? [_ e] (and (member? S1 e) (not (member? S2 e))))
	 (predicate [_] (and (predicate S1) (not (predicate S2)))))

(extend-type clojure.lang.PersistentHashSet 
	Set
	 (member? [this e] (this e))
	 (predicate [this] this))

(defn enumerate [S Type] (filter (partial member? S) Type))

(def naturals (PredicateSet. #(and (number? %) (<= 0 %))))
(def integers (PredicateSet. number?))
(def bools #{true, false})
(def strings (PredicateSet. string?))
(defn mk-set [x] (PredicateSet. x))
(defn union [x y] (UnionSet. x y))
(defn intersection [x y] (IntersectionSet. x y))
(defn difference [x y] (DifferenceSet. x y))
