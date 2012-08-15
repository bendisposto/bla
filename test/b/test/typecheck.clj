(ns b.test.typecheck
  (:require [b.reader :as reader])
  (:use [clojure.test])
  (:use [b.typecheck])
  (:use [midje.sweet]))

(defn formulize [text] (str "#FORMULA " text))
(defn tc [text] (->> text formulize reader/parse typecheck))
(defn dg [f] (->> f formulize reader/bparser reader/mk_clojure_ast))


(tabular "basic-rules"
         (fact (tc ?text) => (just ?out))
         ?text | ?out
         "5" | {}
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
         "x : {1|->2, 3|->5}" | {:x (btuple (bint) (bint))}
         "x = {1|->1, 2|->3} <+ {a|->b}" | {:x (brel (bint) (bint)) :a (bint) :b (bint)}
         "x = BOOL <-> INT" | {:x (brel (bbool) (bint))}
         "x = id(y)[z]" | {:z (bset '_.0) :x  (bset '_.0) :y (bset '_.0)}
         "x = closure({1|->2,2|->4})"  | {:x (brel (bint) (bint))}
         "x = iterate({1|->2,2|->4},12)"  | {:x (brel (bint) (bint))}
         "a = (b;c) & dom(b) = BOOL & ran(c) = INT" | {:a (brel (bbool) (bint)) :b (brel (bbool) '_.0) :c (brel '_.0 (bint))}
         "x : seq(y)" | {:y (bset '_.0) :x (bseq '_.0)}
         "x = succ(4)" | {:x (bint)}
         "y = first(x)" | {:x (bseq '_.0) :y '_.0}
         "y = front(x)" | {:x (bseq '_.0) :y (bseq '_.0)}
         "x = y ^ y" | {:x (bseq '_.0) :y (bseq '_.0)}
         "x = (NAT * BOOL)~" | {:x (brel (bbool) (bint))}
         "x = TRUE" | {:x (bbool)}
         "y = 4 -> x  & x : seq(NAT)" | {:x (bseq (bint)) :y (bseq (bint))}
         "y = x <- 2  & x : seq(NAT)" | {:x (bseq (bint)) :y (bseq (bint))}
         "a = r \\|/ n" | {:a (bseq '_.0)  :r (bseq '_.0) :n (bint) }
         "e=[]" | {:e (bseq '_.0)} 
         "e={}" | {:e (bset '_.0)}
         "e = {1|->3} & f = e~" {:e (brel (bint) (bint)) :f (brel (bint) (bint))}
         
         )


;.;. FAIL at (NO_SOURCE_FILE:1)
;.;. With table substitutions: {?text "a = {x,y,z|x:NAT & y:NAT & z:NAT & x = y + z}", ?out {:a (btuple (bint) (bint) (bint)), :x (bint), :y (bint), :z (bint)}}
;.;. Actual result did not agree with the checking function.
;.;.         Actual result: java.lang.IllegalArgumentException: Don't know how to create ISeq from: clojure.core.logic.LVar
;.;.               b.typecheck$RComprehensionSetExpression$fn__26562$fn__26565$_inc__26566.invoke(typecheck.clj:181)
;.;.               b.typecheck$typecheck.invoke(typecheck.clj:195)
;.;.               b.test.typecheck$tc.invoke(typecheck.clj:8)
;.;.               b.test.typecheck$eval26614$fn__26615$fn__26616.invoke(NO_SOURCE_FILE:1)
;.;.               b.test.typecheck$eval26614$fn__26615.invoke(NO_SOURCE_FILE:1)
;.;.               b.test.typecheck$eval26614.invoke(NO_SOURCE_FILE:1)
;.;.               b.test.typecheck$eval26610.invoke(NO_SOURCE_FILE)
;.;.     Checking function: (just {:a (btuple (bint) (bint) (bint)), :x (bint), :y (bint), :z (bint)})
;.;.     The checker said this about the reason:
;.;.         Expected four elements. There was one.
(tabular "comprehension sets"
         (fact (tc ?text) =future=> (just ?out))
         ?text | ?out
         "a = {x,y,z|x:NAT & y:NAT & z:NAT & x = y + z}" | {:a (btuple (bint) (bint) (bint)) :x (bint) :y (bint) :z (bint) })

(tabular "analog"
         (fact (tc ?a) => (just (tc ?b)))
         ?a | ?b
         "a = NAT" | "a=INT"
         "a=INT" | "a=NAT1"
         "a=NAT" | "a=NATURAL"
         "a=INT" | "a=INTEGER"
         "e = 6" | "e=MAXINT"
         "e=MAXINT" | "e=MININT"
         "f:a --> b" | "f:a <-> b"
         "f:a +-> b" | "f:a --> b"
         "e=TRUE" | "e=FALSE"
         "r /|\\ 4" | "r \\|/ 7"
         "12" | "-4"
         "{1,2,3} <| r" | "{4} <<| r"
         "r <| s" | "r <<|s"
         "1<c & a<b" | "1<c or a<b"
         "1<c & a<b" | "1<c => a<b"
         "1<c & a<b" | "1<c <=> a<b"
         "a + 1" | "a - 1"
         "a + b" | "a / b"
         "a + b" | "a mod b"
         "a + b" | "a ** b"
         "a < b" | "a <= b"
         "a < b" | "a > b"
         "a < b" | "a >= b"
         "a <: b" | "a <<: b"
         "a <: b" | "a /<: b"
         "a <: b" | "a /<<: b"
         "a = b" | "a /= b"
         "a : S" | "a /: S"
         "a \\/ b" | "a /\\ b"
         "a \\/ {1,2}" | "a - {1,2}"
         "a \\/ b" | "a \\ b"
         "e :POW(S)" | "e : POW1(S)"
         "e :POW(S)" | "e : FIN1(S)"
         "e :POW(S)" | "e : FIN(S)"
         "x = max(S)" | "x = min(S)"
         "f : A <-> B" | "f : A +-> B"
         "f : A <-> B" | "f : A --> B"
         "f : A <-> B" | "f : A +->> B"
         "f : A <-> B" | "f : A -->> B"
         "f : A <-> B" | "f : A >-> B"
         "f : A <-> B" | "f : A >+> B"
         "f : A <-> B" | "f : A >->> B"
         "x : closure(S)" | "x : closure1(S)"
         "s : seq(T)" | "s: seq1(T)"
         "s : seq(T)" | "s: iseq(T)"
         "s : seq(T)" | "s: iseq1(T)"
         "s : seq(T)" | "s: perm(T)"
         "x = succ(9)" | "x = pred(3)"
         "m = card(T)" | "m = size(T)"
         "e = first(S)" | "e = last(S)"
         "x = front(S)" | "x = tail(S)"
         "x = front(S)" | "x = rev(S)"
         )

(fact    (tc "a = [1,2,3,4]") =future=> (just {:a (bseq (bint))}))

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
         "a = r /|\\ n" | {:a (bseq '_.0)  :r (bseq '_.0) :n (bint) }       
         "x = NAT * NAT" | {:x (brel (bint) (bint))}
         "1" | {})
	
(fact (tc "x*y : INTEGER") => (just {:x (bint) :y (bint) })) 	
(fact (tc "x*y : S*T") => (just {:x '_.0 :y '_.1 :S (bset '_.0) :T (bset '_.1) })) 

(tabular "tuple"
         (fact (apply btuple ?t) => ?r)
         ?t | ?r
         [1 2] | [:pair 1 2]
         [1 2 3]  | [:pair 1 [:pair 2 3]]
         [1 2 3 4] | [:pair 1 [:pair 2 [:pair 3 4]]])



