#lang sbpv

(require "../stdlib.rkt")
(require "CoList.rkt")
(provide mutable-flexvec<-list)

;; A FlexVec A is a codata type implementing
;; codata FlexVec A where
;;   'remove  |- Nat -> F (List A (U FlexVec A))
;;   'set     |- Nat -> A -> F (U FlexVec A)
;;   'get     |- Nat -> F A
;;   'update  |- Nat -> U(A -> F A) -> F (U FlexVec A)
;; TODO, we should also implement
;;   'size    |- F Nat (constant time)
;; and maybe
;;   'to-colist |- CoList A

(def-thunk (! mutable-flexvec v)
  (copat
   [((= 'get) ix #:bind)
    (! vector-ref v ix)]
   [((= 'set) ix a #:bind)
    (! vector-set! v ix a)
    (ret (~ (! mutable-flexvec v)))]
   [((= 'update) ix up #:bind)
    (! <<v vector-set! v ix 'o up 'o vector-ref v ix)
    (ret (~ (! mutable-flexvec v)))]
   [((= 'to-colist) #:bind)
    [len <- (! vector-length v)]
    (! <<n cl-map (~ (! vector-ref v)) 'o range 0 len)]))

(def-thunk (! mutable-flexvec<-list l #:bind)
  [v <- (! list->vector l)]
  (ret (~ (! mutable-flexvec v))))

;; U (FlexVec A) -> CoList A
(def-thunk (! colist<-flexvec v)
  (! cl-unfold
     (~ (λ (i)
          (do [a <- (! v 'get i)]
              [i+1 <- (! + i 1)]
            (! Cons a i+1))))
     0))