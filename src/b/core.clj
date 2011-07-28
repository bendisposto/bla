(ns b.core)

(def exp1 '(add (intgr 2) (intgr 3)))

(defn ev [x] (first (x {})))
;(defn lift
;  ([op a]   (fn [e] [(op a),e]))
;  ([op a b] (fn [e] [(op (first (a e)) (first (b e))), e])))

;(defn lift [op & p] (fn [e] [(apply op (map first (map (fn [f] (f e)) p))) e]))

;(defn lift [op & p] (fn [e] [(apply op (map (comp first (fn [f] (f e))) p)) e]))

(defn mk_set [& e] (set e))

(defn lift [op & p] (fn [e] [(apply op (map first ((apply juxt p) e))) e]))

(defn lift2 [op & p] (println "o:" op " p:" p))

(defn vrb [x] (fn [e] [(e x),e]))
(defn intgr [x] (fn [e] [x,e]))
(defn bool [x] (fn [e] [x,e]))

(defn bset [& elements] (apply lift mk_set elements))

(defn contains [S e] (nil? S e))

(defn member [e S] (apply lift contains S e))
(defn add [a b] (lift + a b))
(defn sub [a b] (lift - a b))
(defn mult [a b] (lift * a b))
(defn div [a b] (lift / a b))
