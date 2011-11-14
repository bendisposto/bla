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
;; Consequently we use two Protocols, one for types and one for sets.

;; The type protocol has two functions. finite? and elements. finite?
;; returns true if we know for sure that the set is finite and false
;; otherwise, i.e., it might also return false for types that are
;; actually finite, but we don't know yet. The elements function
;; enumerates the elements of the type.
(defprotocol B-Type
  (finite? [this])
  (elements [this]))

;; The built-in type INTEGER reifies the B-Type protocol. The elements
;; function returns a lazy sequence of all integer numbers. The
;; enumeration schema alternates between positive and negative
;; integers: 0, 1, -1, 2, -2, ... 
(def int-type
  (reify B-Type
    (finite? [_] false)
    (elements [_] (interleave (iterate dec 0) (iterate inc 1)))))

(def printable-chars (map char (range 32 126)))

(defn make-deferred-type [name] (fn [] (for [p (repeat name) n (iterate inc 1)] (str p n))))

(defn make-explicit-type [& elements] (fn [] (into #{} elements)))

;; The second protocol consists of three functions: member? returns
;; true if the element is member of the set. The enum-type function
;; returns the type the set is based on and the decider function
;; filters the elements from the type. In the implementation
;; (the PredicateSet record) the member? predicate always agrees with
;; the decider predicate. 
(defprotocol B-Set
  (member? [this e])
  (enum-type [this])
  (decider [this]))

;; PredicateSet is the record that is used to represent symbolic sets,
;; i.e., sets that are defined as a set comprehension. S = { x | P(x) }
(defrecord PredicateSet [T P]
  B-Set
  (member? [this e] (P e))
  (enum-type [_] T)
  (decider [_] P))

;; Clojure sets can be used as both, types and sets. Because clojure
;; sets are actually functions that return truthy values if the
;; argument is element of the set, we can use the set itself as
;; the decider predicate.  
(extend-type clojure.lang.PersistentHashSet
  B-Type
  (finite? [_] true)
  (elements [this] this)
  B-Set
  (member? [this e] (this e))
  (enum-type [this] this)
  (decider [this] this))

;; The natural numbers can be defined as a set comprehension N = {x
;; | x \in Z & 0 <= x}
;; We use partial application of the clojure <= function as the
;; decider, i.e., the decider function is similar to (fn [x] (<= 0 x))
(def natural (PredicateSet. int-type (partial <= 0)))

;; When we do operations on sets (union, interdection, ...) we need to
;; combine the types. 
;; TODO The abstract interpretaion should figure this out and replace
;; this (compliicated) function
(defn combine_types  [T1 T2 FC]
  (let [fin1 (finite? T1)
        fin2 (finite? T2)
        finite (FC fin1 fin2)
        etype (if fin1 T2 T1)]
    (if finite
      (reify B-Type
        (finite? [_] true)
        (elements [_] (elements etype)))
      etype)))

(defn int-range [minv,maxv] (fn [x] (<= minv x maxv)))

;; When we define operations on sets, we want to make sure, that if we
;; have two clojure sets the result is a clojure set. Otherwise we
;; construct the symbolic representation (PredicateSet).  
(defmacro set-operation
  [operation l_combine f_combine]
  `(fn [S1# S2#]
     (if (and (set? S1#) (set? S2#))
       (~operation S1# S2#)
       (PredicateSet. (combine_types (enum-type S1#) (enum-type S2#) ~f_combine) (~l_combine S1# S2#)))))

;; The set operations can be defined using the set-operation marco, we
;; pass a clojure function that deald with the case that both sets are
;; clojure sets, a function that combines the deciders and a function
;; that combines the finiteness of the type enumerators. For instance,
;; the intersection of two sets combines the deciders using logical
;; and, i.e., an eement of the intersection of the sets S and T must
;; ben member of S and member of T. Regarding finiteness, we use
;; logical or as the combining function because the resulting set is
;; finite if one of the original sets was finite. Note again, that
;; we don't always know that a set is finite. For instance the two
;; sets S = {x| x:Z &  x < 4} and T = {x| x:Z & x>2} are both not
;; finite. The intersection \\(S \cap T\\) = {3} is clearly finite but the
;; combiner will say it is not finite. 
(def union (set-operation set/union (fn [S1 S2] #(or (member? S1 %) (member? S2 %))) #(and %1 %2) ))
(def intersection (set-operation set/intersection (fn [S1 S2] #(and (member? S1 %) (member? S2 %))) #(or %1 %2) ))
(def difference (set-operation set/difference (fn [S1 S2] #(and (member? S1 %) (not (member? S2 %)))) (fn [a _] a)))

;;When we have to enumerate (i.e., turn a symbolic set into a cloure
;;set) we need to be very careful. If the type of the set is inifinite
;;we get into an inifinite loop when we use the enumerate
;;function. The hard-bounded-enumerate function can be safely used to
;;enumerate a set (but of course it will restrict sets to finite sets
;;and therefore it might miss cases)  
(defn enumerate [S] (into #{} (filter (decider S) (elements (enum-type S)))))
(defn hard-bounded-enumerate [S hard-bound] (into #{} (filter (decider S) (take hard-bound (elements (enum-type S))))))
(defn bounded-enumerate [S predicate hard-bound] (into #{} (filter predicate (hard-bounded-enumerate S hard-bound))))


;(defn type-product [T1 T2] (for [x T1 y T1] [x y])) ;TODO diagonalization

(defn type-product [T1 T2] (for [x T1 y T2 :when y < x] [x y]))
(defn diagonalize [E] (for [v E f (take (inc v) E)] [f (- v f)]))
(def d-nat-nat (diagonalize (iterate inc 0)))

(defn cartesian-product [S, T] (PredicateSet. (type-product (enum-type S) (enum-type T)) (fn [[x y]] (and (member? S x) (member? T y)))))



(defn card [S]  (count (enumerate S)))

;(defn pow [S]
;(B-Set.
; (type-pow S)
; (fn [e] (every? (partial member? S) (elements e)))))

;; ## Enumerating powersets
;; Cantor has proven that for all sets S the powerset is larger than
;; S. Thus the powerset of the integers is uncountable and we cannot
;; it. However, we can enumerate all finite elements of the powerset
;; as proposed in the paper on eboc (Paulo Matos and Bernd Fischer. A
;; Lazy Unbounded Model Checker for Event-B, ICFEM'09).
;; We use a different enumeration scheme than the one used in the
;; paper. Our schema starts with n counting down to 1 and repeates
;; each number i 2^(n-i+1) times. For instance the sequence for n=3
;; will be 3, 2, 2, 1, 1, 1, 1. The sequence then statrs over with
;; n+1. This yields in a sequence where a number n occurs about twice
;; as often as (n+1).   
(defn- pow-seq [n] (apply concat (map-indexed (fn [i v] (repeat (int (Math/pow 2 i)) v)) (range n 0 -1))))
(def card-sequence (mapcat pow-seq (iterate inc 1)))

(defn- mk_set_generator [n e]
  (if (= n 1)
    (map vector e) 
    (for [z e x  (mk_set_generator (dec n) (range (dec n) z))] (conj x z))))

(defn- get_generator [s n e] (if-let [g (s n)] g (mk_set_generator n e)))

(defn- gen [f state] (let [[head new-state] (f state)] (lazy-seq (cons head (gen f new-state)))))

(defn- p [e state] (let [[c & cs] (:cards state) g (get_generator state c e)] [(first g)  (assoc (assoc state c (rest g)) :cards cs)]))

(def nat-pow (cons [] (gen (partial p (iterate inc 0)) {:cards card-sequence})))

;; TODO We need to find bijections from nat to the actual sets, maybe
;; we could even always use nat as the underlying type and specify a
;; function to map from nat to domain elements.

(defn as-predicate-set [S] (if (set? S) (PredicateSet. S S) S))
(defn as-explicit-set [S bound] (if (set? S) S (into #{} (hard-bounded-enumerate S bound))))

