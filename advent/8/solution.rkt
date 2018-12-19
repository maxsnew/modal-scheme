#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

;; A StackOn A B is one of
;;   F (cl-cons A (U Stackon A B))
;;   CoList B
;; A Tree is a (Node (StackOn Tree Nat))
;; 

;; Tokenize
(def-thunk (! nums)
  (! <<n
     list<-colist 'o cl-map parse-num 'o sep-by #\space 'o read-all-chars))

;; Nat ... -> F Nat
(def-thunk (! parse)
  (letrec
   ;; (U(Nat -> 'stk -> Nat ... -> F Nat) -> Nat -> 'stk -> Nat -> Nat -> ... -> F Nat
   ([tree
     (~ (copat [(k tot (= 'stk) children# metadata#)
                [k = (~ (! metadata k metadata#))]
                (! forest k children# tot 'stk)]))]
    [metadata
     (~ (copat
         [(k     (= 0) tot (= 'stk)) (! k tot 'stk)]
         [(k metadata# tot (= 'stk) n)
          [metadata# <- (! - metadata# 1)] [tot <- (! + tot n)]
          (! metadata k metadata# tot 'stk)]))]
    [forest
     (~ (copat
         [(k (= 0) tot (= 'stk)) (! k tot 'stk)]
         [(k children# tot (= 'stk))
          [children# <- (! - children# 1)]
          [k = (~ (! forest k children#))]
          (! tree k tot 'stk)]))])
    (! tree abort 0 'stk)))

(def-thunk (! main-a)
  [ns <- (! nums)]
  (! apply parse ns))

(def-thunk (! main-b)
  (ret 'not-done-yet))
