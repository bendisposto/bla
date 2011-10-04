(ns b.sets 
	(:require [clojure.core.match.core :as match])
	(:gen-class))

(defrecord B-Set [type accept? reject? precise])

(def int-type (interleave (iterate dec 0) (iterate inc 1)))
(def bool-type '(false true))
(def printable-chars (map char (range 32 126)))

(defn make-deferred-type [name]  (for [p (repeat name) n (iterate inc 1)] (str p n)))
(defn make-explicit-type [& elements] (into #{} elements))

(def nat (B-Set. (fn [] int-type) #(or (pos? %) (zero? %)) neg? #{:accept? :reject?}))

(defn- mk-filter [accept? reject? precise]
        (match/match [(boolean (:accept? precise))
                      (boolean (:reject? precise))]
	         [true   true] #(and (accept? %) (not (reject? %)))
	         [true  false] accept?
	         [false  true] (comp not reject?)
	         [false false] #(or (accept? %) (not (reject? %)))))

(defn member? [e {:keys [type accept? reject? precise]}]
;	(println "member?" e " in type" type "acc:" accept? (accept? e) "rej:" reject? (reject? e))
	(cond (accept? e) true
	      (reject? e) false
	       :else (some #(= e %) 
	                   (do ;(println "enumerate") 
		                   (filter (mk-filter accept? reject? precise) (type))))))

(defn mk-set [x] #{})
(defn union [x y] x)
(defn intersection [x y] x)
(defn difference [x y] x)
