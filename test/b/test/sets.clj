(ns b.test.sets
  (:use [b.sets])
  (:use [clojure.test]))

(defmacro check [exp act]
	(let [name (gensym)]
	 `(deftest ~name (is (= ~act ~exp)))))

(defn checkset [e a] (let [s (into #{} a)] (check e s)))

(defmacro istrue [p] (let [name (gensym)] `(deftest ~name (is ~p))))		
(defmacro isfalse [p] (let [name (gensym)] `(deftest ~name (is (not ~p)))))		
				
(check 1 1)
(istrue true)

(istrue (member? naturals 17))
(istrue (member? naturals 0))
(isfalse (member? naturals -17))
(istrue (member? integers -17))
(isfalse (member? integers true))
(istrue (member? bools true))
(isfalse (member? bools 3))
(istrue (member? strings "foo"))
(isfalse (member? strings 3))

(check '(0 1 2 3 4) (enumerate naturals (range -5 5)))
(check '(-5 -4 -3 -2 -1 0 1 2 3 4) (enumerate integers (range -5 5)))

(defn expand 
	([s a b] (enumerate s (range a b)))
	([s] (enumerate s (range -100 100))))

(checkset #{1,2,8,9} (expand (union #{1,2} #{8,9}) -10 10))
(checkset #{1,9} (expand (intersection #{1,2,3,4,5,6,7,8,9,12} #{1,9,10,11})))
(checkset #{} (expand (intersection #{1,2,3} #{9,10,11})))
(checkset #{2,3,4,5} (expand (difference naturals #{1,0}) -10 6))
(checkset #{0,2,4} (expand (mk-set even?) 0 6))
(checkset #{1,5} (expand (difference (mk-set odd?) (mk-set #(= % 3))) 0 6))
