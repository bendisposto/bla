(ns b.typecheck 
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))


(defmacro binary_rule [n vars [t1 t2] rt]
  `(defn ~n [a# b#]
     (fn [t# in# out#]
       (fresh [btw#]
              (fresh ~vars
                     (a# ~t1 in# btw#)
                     (b# ~t2 btw# out#)
                     (== t# ~rt))))))


(defmacro unary_rule [n vars t rt]
  `(defn ~n [a#]
     (fn [t# in# out#]
            (fresh ~vars
                     (a# ~t in# out#)
                     (== t# ~rt)))))

(defmacro constant_rule [n rt]
   `(defn ~n []
        (fn [t# in# out#]
               (all
                        (== in# out#)
                        (== t# ~rt)))))

(defmacro rules
  ([n ar v at rt & more] `(do (rules ~n ~ar ~v ~at ~rt) (rules ~@more)))
  ([n ar v at rt] (cond 
	               (= ar 0) `(constant_rule ~n ~rt)
	               (= ar 1) `(unary_rule ~n ~v ~(first at) ~rt)
	               (= ar 2) `(binary_rule ~n ~v ~at ~rt))))

(defmacro analog
  ([f g] `(def ~g ~f))
  ([f g & more] `(do (analog ~f ~g) (analog ~f ~@more))))


(defmacro skip 
	([op] `(defmacro ~op [c#] c#))
	([op & more] `(do (skip ~op) (skip ~@more))))


(skip AExpressionParseUnit APredicateParseUnit Start AConvertBoolExpression)

(defmacro bint [] `:integer)
(defmacro bbool [] `:boolean)
(defmacro bstring [] `:string)
(defmacro bset [t] `[:set ~t])
(defmacro bpair [s t] `[:pair ~s ~t])

(defmacro brel [s t] `(bset (bpair ~s ~t)))
(defmacro bseq [t] `(brel (bint) ~t))




(rules
 ;name                           arity     type variables   argument types                      result type
 ;                                                            first         second       
 AMemberPredicate                2         [t]              [ t             (bset t)      ]     (bbool)
 ASubsetPredicate                2         [t]              [ (bset t)      (bset t)      ]     (bbool)
 AEqualPredicate                 2         [t]              [ t             t             ]     (bbool)
 AMinusOrSetSubtractExpression   2         [t]              [ t             t             ]     t
 AConjunctPredicate              2         []               [ (bbool)       (bbool)       ]     (bbool)
 AAddExpression                  2         []               [ (bint)        (bint)        ]     (bint)
 ALessPredicate                  2         []               [ (bint)        (bint)        ]     (bbool)
 AIntervalExpression             2         []               [ (bint)        (bint)        ]     (bset (bint))
 AUnionExpression                2         [t]              [ (bset t)      (bset t)      ]     (bset t)
 APowSubsetExpression            1         [t]              [ (bset t)                    ]     (bset (bset t))
 AIdentityExpression             1         [t]              [ (bset t)                    ]     (brel t t)
 ANegationPredicate              1         []               [(bbool)                      ]     (bbool)
 ACardExpression                 1         [t]              [(bset t)                     ]     (bint)
 AMaxExpression                  1         []               [(bset (bint))                ]     (bint)
 ADomainExpression               1         [s,t]            [(brel s t)                   ]     (bset s)
 ARangeExpression                1         [s,t]            [(brel s t)                   ]     (bset t)
 ADomainRestrictionExpression    2         [s,t]            [(bset s)       (brel s t)    ]     (brel s t)
 ARangeRestrictionExpression     2         [s,t]            [(brel s t)     (bset t)       ]     (brel s t) 
 AOverwriteExpression            2         [s,t]            [(brel s t)    (brel s t)   ]     (brel s t)
 ARelationsExpression            2         [s,t]            [(bset s)       (bset t)      ]     (brel s t)
 AImageExpression                2         [s,t]            [(brel s t)     (bset s)     ]     (bset t)
 AClosureExpression              1         [t]              [(brel t t)                   ]     (brel t t)
 AIterationExpression              2         [t]              [(brel t t)      (bint)       ]     (brel t t)
 ACompositionExpression          2         [r,s,t]          [(brel r s)      (brel s t)   ]     (brel r t)
 ASeqExpression                  1         [t]              [(bset t)                     ]     (bseq t)
 ANatSetExpression               0         []               [                             ]     (bset (bint))
 ASuccessorExpression            0         []               [                             ]     (bseq (bint))
 AFirstExpression                1         [t]              [(bseq t)                     ]     t
 AFrontExpression                1         [t]              [(bseq t)                     ]     (bseq t)
 AConcatExpression               2         [t]              [(bseq t)       (bseq t)      ]     (bseq t)
 AReverseExpression              1         [s,t]            [(brel s t)                    ]     (brel t s)
 ABooleanTrueExpression          0         []               [                             ]     (bbool)
 ABoolSetExpression              0         []               [                             ]     (bset (bbool))
 AStringSetExpression            0         []               [                             ]     (bset (bstring))
 AInsertFrontExpression          2         [t]              [t              (bseq t)      ]     (bseq t)
 AInsertTailExpression           2         [t]              [(bseq t)       t             ]     (bseq t)
 ARestrictFrontExpression        2         [t]              [(bseq t)       (bint)        ]     (bseq t)
 singleton_set                   1         [t]              [t                            ]     (bset t)
 ACoupleExpression               2         [s t]            [s              t             ]     (bpair s t)
)                                                                                       
(defmacro leaf 
  ([n t] `(defn ~n [value#] (fn [type# in# out#] (all (== type# ~t) (== in# out#)))))
  ([n t & more] `(do (leaf ~n ~t) (leaf ~@more))))

(leaf AIntegerExpression (bint) AStringExpression (bstring))

(analog ANatSetExpression ANat1SetExpression ANaturalSetExpression ANatural1SetExpression AIntSetExpression AIntegerSetExpression)

(analog AIntegerExpression AMaxIntExpression AMinIntExpression )
(analog ABooleanTrueExpression ABooleanFalseExpression)

(defn AEmptySequenceExpression [] (fn [type in out] (fresh [t] (== in out) (== type (bseq t)))))
(defn AEmptySetExpression [] (fn [type in out] (fresh [t] (== in out) (== type (bset t)))))


(analog ARestrictFrontExpression ARestrictTailExpression)

(analog AIntegerExpression AUnaryMinusExpression)
(analog ADomainRestriction ADomainSubtraction)
(analog ARangeRestriction ARangeSubtraction)
(analog AConjunctPredicate ADisjunctPredicate AImplicationPredicate AEquivalencePredicate)
(analog AAddExpression ADivExpression AModExpression APowerOfExpression)
(analog ALessPredicate ALessEqualPredicate AGreaterPredicate AGreaterEqualPredicate)
(analog ASubsetPredicate ASubsetStrictPredicate ANotSubsetStrictPredicate ANotSubsetPredicate)
(analog AEqualPredicate ANotEqualPredicate)
(analog AMemberPredicate ANotMemberPredicate)
(analog AUnionExpression AIntersectionExpression ASetSubtractionExpression)
(analog APowSubsetExpression  APow1SubsetExpression AFinSubsetExpression AFin1SubsetExpression)
(analog AMaxExpression AMinExpression)
(analog ARelationsExpression APartialFunctionExpression ATotalFunctionExpression APartialInjectionExpression
        ATotalInjectionExpression APartialSurjectionExpression ATotalSurjectionExpression
        APartialBijectionExpression ATotalBijectionExpression)
(analog AClosureExpression AReflexiveClosureExpression)
(analog ASeqExpression ASeq1Expression AISeqExpression AISeq1Expression APermExpression) 
(analog ASuccessorExpression APredecessorExpression)
(analog ACardExpression ASizeExpression)
(analog AFirstExpression ALastExpression)
(analog AFrontExpression ATailExpression ARevExpression)



(defmacro ASetExtensionExpression
  ([] `(AEmptySetExpression))
  ([e & es] `(AUnionExpression (singleton_set ~e)  (ASetExtensionExpression ~@es))))


(defn AMultOrCartExpression [a b] 
  (fn [type in out] 
    (fresh [btw s t]
           (conde ((a (bint) in btw)   (b (bint) btw out) (== type (bint)))
                  ((a (bset s) in btw) (b (bset t) btw out) (== type (brel s t)))))))


(defmacro AForallPredicate [_ p] `~p)
(defmacro AExistsPredicate [_ p] `~p)


(defn RLambdaExpression [ex pred & vars] (let [vs (reverse vars)] (fn [type in out]    )))
(defn ALambdaExpression [& args] (apply RLambdaExpression (reverse args)))

(defn AIdentifierExpression [id]
  (fn [type in out]
    (fresh [t]
           (conda ((membero [id t] in) (== t type) (== in out))
                  ((appendo in [[id type]] out))))))

(defn typecheck [ast] (let [res (first (run* [q] (fresh [t,o] (ast t [] q))))] (into {} res)))


