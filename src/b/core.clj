(ns b.core
	(:use clojure.set b.visitor)
	(:import de.be4.classicalb.core.parser.BParser)
	(:gen-class)
	)

(declare mk_set tuple mk_rel  andf orf notf member  applyfun)

(defmacro lift [name op] `(defn ~name [a# b#] (lift$ ~op a# b#)))

(defmacro autolift
  ([op] `(defn ~(symbol (str "__" op)) [a# b#] (lift$ ~op a# b#)))
  ([op & ops] `(do (defn ~(symbol (str "__" op)) [a# b#] (lift$ ~op a# b#)) (autolift ~@ops))))

(defn lift$ [op & p] (fn [e] (apply op ((apply juxt p) e)))) ;((juxt f g) a) = [(f a) (g a)]

(defn AIdentifierExpression [x] (fn [e] (e x)))
(defn AIntegerExpression [x] (fn [e] x))
(defn AFalseExpression [] (fn [e] false))
(defn ATrueExpression [] (fn [e] true))
(defmacro pass [name] `(defn ~name [x#] (fn [e#] (x# e#))))

(pass AExpressionParseUnit)
(pass Start)

(defmacro __set [& elements]
  (cond 
	(= '__tuple (first (first elements))) `(lift$ mk_rel ~@elements)
	:else `(lift$ mk_set ~@elements)))	

(lift AAddExpression +)

;(autolift + - * > >= < <= union intersection difference = andf orf notf mod member tuple quot applyfun)

;;;;;;;;;
(defn ev [x] (x {}))
(defn mk_set [& e] (set e))
(defn mk_rel [& e] (apply hash-map (flatten e)))
(defn tuple [a b] [a b])
(defn contains [S e] (not (nil? (S e))))
(defn andf [a b] (and a b))
(defn orf [a b] (or a b))
(defn notf [a] (not a))
(defn member [a b] (contains b a))
(defn applyfun [a b] (get a b))

(defn parse [l] 
	(let [input (apply str (interleave l (cycle [" "])))]
       (BParser/parse input)))

(defn -main[ & arg]
  (let [x (read-string (create (BParser/parse (first arg))))] 
    (println (ev (eval x))) 
  )
)
	