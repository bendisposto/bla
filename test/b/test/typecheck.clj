(ns b.test.typecheck
  (:require [b.reader :as reader])
  (:use [clojure.test])
  (:use [b.typecheck])
  (:use [midje.sweet]))

(defn formulize [text] (str "#FORMULA " text))
(defn tc [text] (->> text formulize reader/parse typecheck))
(defn dg [f] (->> f formulize reader/bparser reader/mk_clojure_ast))

;.;. The highest reward for a man's toil is not what he gets for it but
;.;. what he becomes by it. -- Ruskin
(tabular "basic-rules"
         (fact (tc ?text) => (just ?out))
         ?text | ?out
         "c : STRING" | {:c (bstring)} 
         "1 : NAT" | {}
         "x <: NAT" | {:x (bset (bint))}
         "x <: BOOL" | {:x (bset (bbool))}         
         "3+x" | {:x (bint)}
         "x + y" | {:y (bint), :x (bint)}
         "1 < y" | {:y (bint)}
         "a = b" | {:a '_.0 :b '_.0}
         "x : a..b" | {:x (bint) :a (bint) :b (bint)}
         "c = a - b & a = 5" | {:a (bint) :b (bint) :c (bint)}
         "c = a - b & c = 5" | {:a (bint) :b  (bint) :c  (bint)}
         "a - b = c & d:c" | {:a (bset '_.0) :b (bset '_.0) :c (bset '_.0) :d '_.0}
         "S \\/ {1}" | {:S (bset (bint))}
         "x = POW(NAT)" | {:x (bset (bset (bint)))}
         "x : POW(NAT)" | {:x (bset (bint))}
         "x = id(NAT)" | {:x (brel (bint) (bint))}
         "x = id(y)" | {:x (brel '_.0 '_.0) :y (bset '_.0)}
         "not(x=1)" | {:x (bint)}
         "x = card({1,2})" | {:x (bint)}
         "x = max({1,2,3,4})" | {:x (bint)}
         "x : dom(y) & x = FALSE" | {:x (bbool) :y (brel (bbool) '_.0)}
         "x : ran(y) & x = FALSE" | {:x (bbool) :y (brel '_.0 (bbool))}
         "x = {1,2} <| NAT * NAT" | {:x (brel (bint) (bint))}
         "x =  NAT * NAT |> {4}" | {:x (brel (bint) (bint))}
         "{1|->2}" | {}
         "x : {1|->2, 3|->5}" | {:x (bpair (bint) (bint))}
         "x = {1|->1, 2|->3} <+ {a|->b}" | {:x (brel (bint) (bint)) :a (bint) :b (bint)}
         "x = BOOL <-> INT" | {:x (brel (bbool) (bint))}
         "x = id(y)[z]" | {:z (bset '_.0) :x  (bset '_.0) :y (bset '_.0)}
         "x = closure({1|->2,2|->4})"  | {:x (brel (bint) (bint))}
         "x = iterate({1|->2,2|->4},12)"  | {:x (brel (bint) (bint))}
         
         )

(tabular "composed-rules"
         (fact (tc ?text) => (just ?out))
         ?text | ?out
         "x = y & y = z & z : BOOL" | {:x (bbool) :y (bbool) :z (bbool)}
         "1=1 => y<19" | {:y (bint)}
         "t = bool(!x.(x:NAT => x:INT))" | {:t (bbool) :x (bint)}
         "!x.(x:NAT => x:INT)" | {:x (bint)}
         "x : y & y = POW(STRING)" | {:x (bset (bstring))  :y (bset (bset (bstring)))}
         "x = y & y + 2 = 5" | {:x (bint) :y (bint)}
         
         
         "d = bool(1<2) => a - b = c & d : c" | {:a (bset (bbool)) :b (bset (bbool)) :c (bset (bbool)) :d (bbool)}

         "a = b \\/ c & d:c & d=12" | {:d (bint) :a (bset (bint)) :b (bset (bint)) :c (bset (bint))}
         "a = b /\\ c & d:c & d=12" | {:d (bint) :a (bset (bint)) :b
                                       (bset (bint)) :c (bset (bint))}
         "s /: NATURAL" | {:s (bint)}
         "x /= 5" | {:x (bint)}
         "x = bool(not(1<b))" | {:x (bbool) :b (bint)}

         "x = {1,2}" | {:x (bset (bint))}
         

         "x = min(y)" | {:x (bint) :y (bset (bint))}
       
         "x = NAT * NAT" | {:x (brel (bint) (bint))}
         "1" | {})





