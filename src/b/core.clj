(ns b.core
	(:use [clojure.set]))

(declare mk_set mk_tuple mk_rel  andf orf notf member)

(defmacro lift [name op] `(defn ~name [a# b#] (lift$ ~op a# b#)))

(defmacro autolift
  ([op] `(defn ~(symbol (str "__" op)) [a# b#] (lift$ ~op a# b#)))
  ([op & ops] `(do (defn ~(symbol (str "__" op)) [a# b#] (lift$ ~op a# b#)) (autolift ~@ops))))

(defn lift$ [op & p] (fn [e] (apply op ((apply juxt p) e)))) ;((juxt f g) a) = [(f a) (g a)]

; Construction
(defn vrb [x] (fn [e] (e x)))
(defn intgr [x] (fn [e] x))
(defn bool [x] (fn [e] x))

(lift ctuple mk_tuple)

;(defn cset [& elements] 
;	(apply lift$ mk_set elements))
	
(defmacro cset [& elements]
  (cond 
	(= 'ctuple (first (first elements))) `(lift$ mk_rel ~@elements)
	:else `(lift$ mk_set ~@elements)))	


;Expressions
(autolift + - * > >= < <= union intersection difference = andf orf notf mod member)
(lift __div /)

(defn applyfun [fun arg] identity)


;;;;;;;;;
(defn ev [x] (x {}))
(defn mk_set [& e] (set e))
;(defn mk_rel [& e] (let [e2 (flatten e)] (hash-map e2))) 

(defn mk_rel [& e] (println "e:" (flatten e))) 

(defn mk_tuple [a b] [a b])
(defn contains [S e] (not (nil? (S e))))

(defn andf [a b] (and a b))
(defn orf [a b] (or a b))
(defn notf [a] (not a))
(defn member [a b] (contains b a))

; generic version of lift: can lift any number of args
;
