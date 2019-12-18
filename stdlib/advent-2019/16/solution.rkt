#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")

(require "../../FlexVec.rkt")

(provide main-a main-b)

(def-thunk (! fft-filter n)
  (! <<n
     (~ (! <<v $ 'o clv-tl 'o $))'o
     cycle 'o
     cl-append* (~ (! repeat 0 n)) (~ (! repeat 1 n)) (~ (! repeat 0 n)) (~ (! repeat -1 n)) '$
     ))

;; U(CoList Num) -> Filter -> Num
(def-thunk (! apply-filter nums filter)
  ;; (! displayall 'apply-filter nums filter)
  (! <<n
     cl-foldl^ + 0 'o
     cl-map * nums (~ (! colist<-list filter)) '$))

(def-thunk (! digitize) (! <<v swap modulo 10 'o abs))

(def-thunk (! phase filters nums)
  (! <<n
     list<-colist 'o
     cl-map digitize 'o
     cl-map (~ (! apply-filter (~ (! colist<-list nums)))) 'o
     colist<-list filters '$))

(def-thunk (! generate-filters len)
  [len+1 <- (! + 1 len)]
  (! <<n
     list<-colist 'o
     cl-map (~ (! <<n (~ (! <<v first 'o split-at len)) 'o fft-filter)) 'o
     range 1 len+1))

(def-thunk (! main-a (rest args))
  [l-of-num <- (! <<v map parse-num 'o map List 'o string->list 'o first 'o apply slurp-lines! args)]
  [len <- (! length l-of-num)]
  [filters <- (! generate-filters len)]
  (! displayall 'start)
  (! <<n cl-foreach displayall 'o cl-zipwith (~ (! range 0 2)) (~ (! iterate (~ (! phase filters)) l-of-num))))


;; For B, the vector is too big to attempt the same thing as
;; before. We need a different strategy.  The trick is that when you
;; are calculating the nth element of a phase where n is more than
;; half the size of the vector, the formula becomes much simpler.
;;

;; l is a (backwards) CoList Number representing a tail of the input
;; that doesn't include the front half
;
;; then the formula is just a bunch of partial sums
(def-thunk (! reversed-phase l)
  (! cl-foldr l
     (~ (λ (n rest partial)
          (do [partial <- (! <<v swap modulo 10 'o + n partial)]
              (! cl-cons partial (~ (! rest partial))))))
     (~ (λ (n) (! cl-nil)))
     0))

;; To make a U(UB -> B) into a U(UB -> FUB),  use
;; (~ (<<n Ret 'o f))

;; Number -> U(CoList A) -> F A
(def-thunk (! nth n cl)
  (! <<v clv-hd 'o $ 'o second 'o split-at n cl '$))

(def-thunk (! foo rounds n)
  (! <<n (~ (! <<v nth n 'o nth rounds)) 'o iterate (~ (! <<n Ret 'o reversed-phase)) 'o range 0 +inf.0)
  )

;; The correct instructions:
;; L,10,R,8,R,8,L,10,R,8,R,8,L,10,L,12,R,8,R,10,R,10,L,12,R,10,L,10,L,12,R,8,R,10,R,10,L,12,R,10,L,10,L,12,R,8,R,10,R,10,L,12,R,10,R,10,L,12,R,10,L,10,R,8,R,8

;; Main Routine: A,A,B,C,B,C,B,C,C,A
;; Subroutine A: L,10,R,8,R,8
;; Subroutine B: L,10,L,12,R,8,R,10
;; Subroutine C: R,10,L,12,R,10

(def-thunk (! main-b)
  (ret 'not-done-yet))

