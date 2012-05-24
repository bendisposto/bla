(ns b.typecheck 
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)) 

;; ## The typechecker
;; In Prolog we would use something like 
;; store(Var,Type,EIn,EIn) :- member(type(Var,Type),EIn), !.
;; store(Var,Type,EIn,type_error) :- member(type(Var,Type2),EIn),Type \= Type2,!.
;; store(Var,Type,E,[type(Var,Type)|E]).
;; 
;; tc(int(X),int,E,E) :- number(X),!.
;; tc(id(A),T,I,O) :- store(A,T,I,O),!.
;; 
;; tc(plus(L,R),int,In,Out) :- tc(L,int,In,Out1),tc(R,int,Out1,Out),!.
;; tc(elementof(E,S),bool,In,Out) :- tc(E,TE,In,Out1), tc(S,set(TE),Out1,Out),!.
;; tc(equals(A,B),bool,In,Out) :- tc(A,T,In,Out1), tc(B,T,Out1,Out),!.
;; tc(and(A,B),bool,In,Out) :- tc(A,bool,In,Out1), tc(B,bool,Out1,Out),!.
;; 
;; test1(X) :- tc(elementof(plus(int(1),id(x)),id(s)),_,[],X).
;; test2(X) :- tc(elementof(id(x),id(s)),_,[],X).
;; test3(X) :- tc(and(equals(id(x),id(s)), equals(id(s),int(5))),_,[],X). 





























