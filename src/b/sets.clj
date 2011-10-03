(ns b.sets (:gen-class))

(def nat-type (iterate inc 0))
(def int-type (interleave (iterate dec 0) (iterate inc 1)))
(def bool-type '(false true))

(def printable-chars (map char (range 32 126)))


(defn make-deferred-type [name]  (for [p (repeat name) n (iterate inc 1)] (str p n)))
(defn make explicit-type [& elements] (into #{} elements))


(defn mk-set [x] #{})
(defn union [x y] x)
(defn intersection [x y] x)
(defn difference [x y] x)

