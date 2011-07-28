(ns b.core)

(def exp1 '(add (intgr 2) (intgr 3)))

(defn ev [x] (first (x {})))
(defn lift
  ([op a]   (fn [e] [(op a),e]))
  ([op a b] (fn [e] [(op (first (a e)) (first (b e))), e])))

(defn vrb [x] (fn [e] [(e x),e]))
(defn intgr [x] (lift identity x))
(defn add [a b] (lift + a b))
(defn sub [a b] (lift - a b))
(defn mult [a b] (lift * a b))
