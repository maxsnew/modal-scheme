#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")
(require "../Stream.rkt")

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

;; Listof Nat -> Stream Nat
(def-thunk (! stream<-totals tots)
  (! stream-cons
     0
     (~ (! push-list tots (~ (! stream-const 0))))))

(def-thunk (! parse-b)
  (letrec
   ;; (U(Nat -> 'stk -> Nat ... -> F Nat) -> Nat -> 'stk -> Nat -> Nat -> ... -> F Nat
   ([tree ;; I only need to know my destination and the total so far
     (~ (copat [(k tot (= 'stk) children# metadata#)
                (cond [(! equal? 0 children#)
                       (! add-metadata k metadata# tot 'stk)]
                      [else
                       [k = (~ (! deref-metadata k metadata#))]
                       (! forest k children# '() tot 'stk)])]))]
    [add-metadata
     (~ (copat
         [(k     (= 0) tot (= 'stk)) (! k tot 'stk)]
         [(k metadata# tot (= 'stk) n)
          [metadata# <- (! - metadata# 1)] [tot <- (! + tot n)]
          (! add-metadata k metadata# tot 'stk)]))]
    [forest
     (~ (copat
         [(k (= 0) tots tot (= 'stk))
          [tots <- (! reverse tots)]
          (! k (~ (! stream<-totals tots)) tot 'stk)]
         [(k children# tots tot (= 'stk))
          [children# <- (! - children# 1)]
          [k = (~
                (copat
                 [(sub-tot (= 'stk))
                  (! forest k children# (cons sub-tot tots) tot 'stk)]))]
          (! tree k 0 'stk)]))]
    [deref-metadata
     (~ (copat
         [(k     (= 0) tots tot (= 'stk)) (! k tot 'stk)]
         [(k metadata# tots tot (= 'stk) ix)
          [metadata# <- (! - metadata# 1)]
          (! stream-ref tots ix)
          [tot <- (! <<v + tot 'o stream-ref tots ix)]
          (! deref-metadata k metadata# tots tot 'stk)]))])
    (! tree abort 0 'stk)))

(def-thunk (! main-b)
  [ns <- (! nums)]
  (! apply parse-b ns))
