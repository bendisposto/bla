(ns b.core
	(:use [clojure.set]))

(declare mk_set contains)

(defn lift [op & p] (fn [e] (apply op ((apply juxt p) e))))

(defn vrb [x] (fn [e] (e x)))
(defn intgr [x] (fn [e] x))
(defn bool [x] (fn [e] x))
(defn cset [& elements] (apply lift mk_set elements))

(defn member [e S] (lift contains S e))
(defn add [a b] (lift + a b))
(defn sub [a b] (lift - a b))
(defn mult [a b] (lift * a b))
(defn div [a b] (lift / a b))
(defn sunion [s1 s2] (lift union s1 s2))
(defn sintersect [s1 s2] (lift intersection s1 s2))

;;;;;;;;;
(defn ev [x] (x {}))
(defn mk_set [& e] (set e))
(defn contains [S e] (not (nil? (S e))))