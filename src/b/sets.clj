(ns b.sets
  (:require [clojure.set :as set])
  (:use clojure.algo.monads)
  (:gen-class))

;; # Sets vs. Types
;; The SETS clause in B defines types rather than sets. A type is a
;; set that is by definition not empty and disjoint from any other
;; type. Therefore it is not possible to define a type that is a
;; subset of another type. For instance, INTEGER is actually a type,
;; while NATURAL is a set. We will use consequently type when we
;; refer to the SETS clause and the built-in types INTEGER and BOOL.
;; Opposed to types a set is a collection of elements of a certain
;; type. For instance, {1,9,-3} is a set of type INTEGER.  


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
  (member? [this e] (this e))
  (enum-type [this] this)
  (decider [this] this))

(def natural (PredicateSet. int-type (partial <= 0)))

(defn combine_types [T1 T2 FC]
  (let [fin1 (finite? T1)
        fin2 (finite? T2)
        finite (FC fin1 fin2)
        etype (if fin1 T2 T1)]
    (if finite
      (reify B-Type
        (finite? [_] (FC fin1 fin2))
        (elements [_] (elements etype)))
      etype)))

(defn int-range [minv,maxv] (fn [x] (<= minv x maxv)))

(defmacro set-operation [operation l_combine f_combine]
  `(fn [S1# S2#]
     (if (and (set? S1#) (set? S2#))
       (~operation S1# S2#)
       (PredicateSet. (combine_types (enum-type S1#) (enum-type S2#) ~f_combine) (~l_combine S1# S2#)))))

(def union (set-operation set/union (fn [S1 S2] #(or (member? S1 %) (member? S2 %))) #(and %1 %2) ))
(def intersection (set-operation set/intersection (fn [S1 S2] #(and (member? S1 %) (member? S2 %))) #(or %1 %2) ))
(def difference (set-operation set/difference (fn [S1 S2] #(and (member? S1 %) (not (member? S2 %)))) (fn [a _] a)))

(defn enumerate [S] (into #{} (filter (decider S) (elements (enum-type S)))))
(defn hard-bounded-enumerate [S hard-bound] (into #{} (filter (decider S) (take hard-bound (elements (enum-type S))))))
(defn bounded-enumerate [S predicate hard-bound] (into #{} (filter predicate (hard-bounded-enumerate S hard-bound))))

(defn type-product [T1 T2] (for [x T1 y T1] [x y]))
                                        ;diagonalization
;(defn type-pow [T] nil)

(defn cartesian-product [S, T] (PredicateSet. (type-product (enum-type S) (enum-type T)) (fn [[x y]] (and (member? S x) (member? T y)))))

(defn card [S]  (count (enumerate S)))

;(defn pow [S]
;(B-Set.
; (type-pow S)
; (fn [e] (every? (partial member? S) (elements e)))))

(defn- rrange [a b] (take (- a b) (iterate dec a)))
(defn- pow-seq [n] (apply concat (map-indexed (fn [i v] (repeat (int (Math/pow 2 i)) v)) (rrange n 0))))
(def card-sequence (mapcat pow-seq (iterate inc 1)))

(defn- mk_set_generator [n e]
  (if (= n 1)
    (map vector e) 
    (for [z e x  (mk_set_generator (dec n) (range (dec n) z))] (conj x z))))

(defn- get_generator [s n e] (if-let [g (s n)] g (mk_set_generator n e)))

(defn- gen [f state] (let [[head new-state] (f state)] (lazy-seq (cons head (gen f new-state)))))

(defn- p [e state] (let [[c & cs] (:cards state) g (get_generator state c e)] [(first g)  (assoc (assoc state c (rest g)) :cards cs)]))

(def nat-pow (cons [] (gen (partial p (iterate inc 0)) {:cards card-sequence})))

(defn as-predicate-set [S] (if (set? S) (PredicateSet. S S) S))
(defn as-explicit-set [S bound] (if (set? S) S (into #{} (hard-bounded-enumerate S bound))))

