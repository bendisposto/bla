(ns b.sets (:gen-class))

(defprotocol BSet
  (etype [this])
  (member? [this e])
  (decider [this]))

(defrecord PredSet [_type _decider]
  BSet
  (etype [this] _type)
  (member? [this e] ((decider this) e))
  (decider [this] _decider))

(extend-type clojure.lang.PersistentHashSet 
  BSet
  (etype [this] this)
  (member? [this e] (this e))
  (decider [this]  this))

(def int-type (interleave (iterate dec 0) (iterate inc 1)))
(def bool-type '(false true))
(def printable-chars (map char (range 32 126)))

(defn make-deferred-type [name]  (for [p (repeat name) n (iterate inc 1)] (str p n)))
(defn make-explicit-type [& elements] (into #{} elements))

(def nat (PredSet. (fn [] int-type) #(or (pos? %) (zero? %))))

(defn union [S1 S2]
  (PredSet.
   (type S1)
   #(or (member? S1 %) (member? S2 %))))

;(defn intersection [{type :type decider1 :decider} {decider2 :decider}]
 ;(B-Set.
; type
; #(and (decider1 %) (decider2 %))))

;(defn difference [{type :type decider1 :decider} {decider2 :decider}]
;(B-Set.
; type
; #(and (decider1 %) ((complement decider2) %))))
;(def empty-set (B-Set. #{} #{}))

(defn elements [S] (filter (decider S) ((etype S))))
(defn bounded-elements [S bound] (take-while bound (filter (decider S) ((etype S)))))

(defn type-product [T1 T2] (for [x T1 y T1] [x y]))  ;diagonalization
(defn type-pow [T] nil)

;(defn cartesian-product [{type1 :type decider1 :decider :as S} {type2 :type decider2 :decider :as T}] 
;(Set.
; (type-product type1 type2)
; (fn [[x y]] (and (member? S x) (member? T y)))))

;(defn card [S] (count (elements S)))
;(defn pow [S]
;(B-Set.
; (type-pow S)
; (fn [e] (every? (partial member? S) (elements e)))))

(defn mk-set [x] #{})
(defn union [x y] x)
(defn intersection [x y] x)
(defn difference [x y] x)
