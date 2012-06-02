(ns b.test.typecheck
  (:use [clojure.test])
  (:use [b.typecheck])
  (:use [midje.sweet]))

(fact "Storing in the environment"
  (store :x [:int] {}) => {:x [:int]}
  (store :x [:int] {:x [:int]})  => {:x [:int]}
  (store :x [:int] {:x [:bool]})  => nil
  (store :x [:int] {:y [:int]})  => {:x [:int] :y [:int]}
  (store :x [:set [:int]] {:x [:set ['?x]]}) => {:x [:set [:int]]})

(defn consistent-type? [id] (fn [[x e]] (= x (e id))))

(fact "Atomic types"
 ((AIntegerExpression 17) {}) => [[:int] {}]
 ((AIdentifierExpression :x) {:x [:foo]}) => [[:foo] {:x [:foo]}]
)

(fact "constent checker"
  ((consistent-type? :x) [:foo {:x :bar}]) => falsey
  ((consistent-type? :x) [:bar {:x :bar}]) => truthy
)

(fact
  [:int {:x :int}] => (consistent-type? :x)
  ((AIdentifierExpression :x) {:y [:foo]}) => (consistent-type? :x)
  ((AIdentifierExpression :x) {}) => (consistent-type? :x))

;.;. FAIL at (NO_SOURCE_FILE:1)
;.;.     Expected: [[:int] {:x [:int]}]
;.;.       Actual: clojure.lang.ArityException: Wrong number of args (0) passed to: typecheck$AIntegerExpression$fn
;.;.               b.typecheck$AAddExpression$fn__5638.invoke(typecheck.clj:21)
;.;.               b.test.typecheck$eval5743$fn__5744$fn__5745.invoke(NO_SOURCE_FILE:1)
;.;.               b.test.typecheck$eval5743$fn__5744.invoke(NO_SOURCE_FILE:1)
;.;.               b.test.typecheck$eval5743.invoke(NO_SOURCE_FILE:1)
;.;.               b.test.typecheck$eval5739.invoke(NO_SOURCE_FILE)
(fact
  ((AAddExpression (AIntegerExpression 1) (AIdentifierExpression :x)) {}) => [[:int] {:x [:int]}])

