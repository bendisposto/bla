(ns b.sets (:gen-class))

(defprotocol Set
	 (member? [this e])
	 (elements [this]))	

(deftype PredicateSet [P EnumFn] 
	Set 
	 (member? [_ e] (P e))
	 (elements [this] (filter P (EnumFn))))
	
(deftype IntersectionSet [S1 S2 EnumFn] 
	Set 
	 (member? [_ e] (and (member? S1 e) (member? S2 e)))
	 (elements [this] (filter #(and (member? S1 %) (member? S2 %)) (EnumFn))))
	 
	
(deftype UnionSet [S1 S2 EnumFn] 
	Set 
	 (member? [_ e] (or (member? S1 e) (member? S2 e)))
	 (elements [this] (filter #(or (member? S1 %) (member? S2 %)) (EnumFn))))
 	
(deftype DifferenceSet [S1 S2 EnumFn]  
	Set 
	 (member? [_ e] (and (member? S1 e) (not (member? S2 e))))
	 (elements [this] (filter #(and (member? S1 %) (not (member? S2 %))) (EnumFn))))
	 

(extend-type clojure.lang.PersistentHashSet 
	Set
	 (member? [this e] (this e))
	 (elements [this] (seq this))
	 )

(defn enumerate [S Type] (filter (partial member? S) Type))

(def all-ints (fn [] (interleave (iterate dec 0) (iterate inc 1))))

(def naturals (PredicateSet. #(and (number? %) (<= 0 %)) (fn [] (iterate inc 0))))
(def integers (PredicateSet. number? all-ints))
(def bools #{true, false})
(def strings (PredicateSet. string? (fn [] nil)))


(defn mk-set [x] (PredicateSet. x nil))
(defn union [x y] (UnionSet. x y nil))
(defn intersection [x y] (IntersectionSet. x y nil))
(defn difference [x y] (DifferenceSet. x y nil))

