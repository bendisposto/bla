(ns b.sets
  (:require [clojure.set :as set])
  (:gen-class))

(defprotocol B-Type
  (finite? [this])
  (elements [this]))

(def int-type
  (reify B-Type
    (finite? [_] false)
    (elements [_] (interleave (iterate dec 0) (iterate inc 1)))))

(def bool-type #{true, false})

(def printable-chars (map char (range 32 126)))

(defn make-deferred-type [name] (fn [] (for [p (repeat name) n (iterate inc 1)] (str p n))))
(defn make-explicit-type [& elements] (fn [] (into #{} elements)))

(defprotocol B-Set
  (member? [this e])
  (enum-type [this])
  (decider [this]))

(defrecord PredicateSet [T P]
  B-Set
  (member? [this e] (P e))
  (enum-type [_] T)
  (decider [_] P))

(extend-type clojure.lang.PersistentHashSet
  B-Type
  (finite? [_] true)
  (elements [this] this)
  B-Set
  (member? [this e] )
  (enum-type [this] this)
  (decider [this] this))

(def natural (PredicateSet. int-type (partial <= 0)))

(defn combine_types [T1 T2]
  (if (finite? T2)
    T1
    T2))

(defmacro set-operation [operation combine]
  `(fn [S1# S2#]
     (if (and (set? S1#) (set? S2#))
       (~operation S1# S2#)
       (PredicateSet. (combine_types (enum-type S1#) (enum-type S2#)) (~combine S1# S2#)))))

(def union (set-operation set/union (fn [S1 S2] #(or (member? S1 %) (member? S2 %)))))
(def intersection (set-operation set/intersection (fn [S1 S2] #(and (member? S1 %) (member? S2 %)))))
(def difference (set-operation set/difference (fn [S1 S2] #(and (member? S1 %) (not (member? S2 %))))))

(defn enumerate [S] (into #{} (filter (decider S) (elements (enum-type S)))))
(defn bounded-enumerate [S bound] (into #{} (take-while bound (filter (decider S) (elements (enum-type S))))))

(defn type-product [T1 T2] (for [x T1 y T1] [x y]))
                                        ;diagonalization
;(defn type-pow [T] nil)

(defn cartesian-product [S, T] (PredicateSet. (type-product (enum-type S) (enum-type T)) (fn [[x y]] (and (member? S x) (member? T y)))))

(defn card [S]  (count (enumerate S)))

;(defn pow [S]
;(B-Set.
; (type-pow S)
; (fn [e] (every? (partial member? S) (elements e)))))

(defn as-predicate-set [S] (if (set? S) (PredicateSet. S S) S))
(defn as-explicit-set [S bound] (if (set? S) S (into #{} (bounded-enumerate S bound))))

