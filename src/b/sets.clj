(ns b.sets (:gen-class))

(defrecord B-Set [type accept? reject? precise])


(def nat-type (iterate inc 0))
(def int-type (interleave (iterate dec 0) (iterate inc 1)))
(def bool-type '(false true))
(def printable-chars (map char (range 32 126)))

(defn make-deferred-type [name]  (for [p (repeat name) n (iterate inc 1)] (str p n)))
(defn make-explicit-type [& elements] (into #{} elements))

(def nat (B-Set. (fn [] nat-type) (or pos? zero?) neg? #{:accept? :reject?}))
(def snat (B-Set. (fn [] nat-type) (constantly false) neg? #{:reject?}))

(defn member? [e {:keys [type accept? reject? precise]}]
	(cond (accept? e) true
	      (reject? e) false
	       :else 
	         (let [enumerator (type) 
		           ap (:accept? precise)
	               rp (:reject? precise)
	               filterp (cond (and ap rp) #(and accept? (comp not reject?)) 
	                              ap accept?
	                              rp (comp not reject?))]
	                   (some #(= e %) (filter filterp enumerator)))))

(defn mk-set [x] #{})
(defn union [x y] x)
(defn intersection [x y] x)
(defn difference [x y] x)
