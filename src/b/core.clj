(ns b.core
	(:use [clojure.set]))

(declare mk_set mk_tuple contains l_and l_or l_not)

;((juxt f g) a) = [(f a) (g a)]
(defn lift [op a b] (fn [e] (apply op ((juxt a b) e)))) 


(defmacro binlift [name op] `(defn ~name [a# b#] (lift ~op a# b#)))

; Construction
(defn vrb [x] (fn [e] (e x)))
(defn intgr [x] (fn [e] x))
(defn bool [x] (fn [e] x))
(defn cset [& elements] (apply lift mk_set elements))
(defn ctuple [a b] (lift mk_tuple a b))


;Expressions
(defn add [a b] (lift + a b))
(defn sub [a b] (lift - a b))
(defn mult [a b] (lift * a b))
(defn div [a b] (lift / a b))

(defn sunion [s1 s2] (lift union s1 s2))
(defn sintersect [s1 s2] (lift intersection s1 s2))
(defn ssub [s1 s2] (lift difference s1 s2))

(defn applyfun [fun arg] identity)

; Predicates
(defn jand [a b] (lift l_and a b))
(binlift jor l_or)


(defn grt [a b] (lift > a b))
(defn grteq [a b] (lift >= a b))
(defn less [a b] (lift < a b))
(defn lesseq [a b] (lift <= a b))

(defn member [e S] (lift contains S e))

(defn eq [a b] (lift = a b))
;;;;;;;;;
(defn ev [x] (x {}))
(defn mk_set [& e] (set e))
(defn mk_tuple [a b] [a b])
(defn contains [S e] (not (nil? (S e))))

(defn l_and [a b] (and a b))
(defn l_or [a b] (or a b))
(defn l_not [a] (not a))


; generic version of lift: can lift any number of args
;(defn lift [op & p] (fn [e] (apply op ((apply juxt p) e))))
