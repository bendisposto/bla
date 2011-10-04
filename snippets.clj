; Diagonalisierung für Nat x Nat:
(def d-nat-nat (for [v (iterate inc 0) f (range 0 (inc v))] [f (- v f)]))


(defn diagonalize [E] (for [v E f (take (inc v) E)] [f (- v f)]))
(def d-nat-nat (diagonalize (iterate inc 0)))
(def d-int-int (diagonalize (interleave (interate dec 0) (iterate inc 1))))