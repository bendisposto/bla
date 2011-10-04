(ns b.test.sets
  (:use [b.sets])
  (:import b.sets.B-Set)
  (:use [clojure.test]))

(defmacro ignore [_ _])

(def semi-reject-nat (B-Set. (fn [] int-type) (constantly false) neg? #{:reject?}))
(def semi-accept-nat (B-Set. (fn [] int-type) 
#(or (pos? %) (zero? %)) 
(constantly false) 
#{:accept?}))
(def weird-nat (B-Set. (fn [] int-type) 
                       #(and (even? %) (or (pos? %) (zero? %))) 
                       #(or (not (odd? %)) (neg? %)) 
                       #{}))

(deftest membertest
	(is (member? 1 nat))
	(is (member? 0 nat))
	(is (not (member? -1 nat)))
	(is (member? 1 semi-reject-nat))
	(is (member? 0 semi-reject-nat))
	(is (not (member? -1 semi-reject-nat)))
	(is (member? 1 semi-accept-nat))
	(is (member? 0 semi-accept-nat))
	(ignore "infinite loop because of semi-decision" (is (not (member? -1 semi-accept-nat))))
	(is (member? 1 weird-nat))
	(is (member? 0 weird-nat))
	(is (not (member? -1 weird-nat))))

