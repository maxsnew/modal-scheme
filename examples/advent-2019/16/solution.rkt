#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/IO)
(require sbpv/stdlib/CoList)
(require "../../Parse.rkt")

(require sbpv/stdlib/FlexVec)

(provide main-a main-b)

;; 3 maps to
;; 0 0 0 1 1 1 0 0 0 -1 -1 -1
;; 4 5 6
;;
(def-thunk (! digitize) (! <<v swap modulo 10 'o abs))

(def-thunk (! range+ n x)
  [x+n <- (! + x n)]
  (! range x x+n))

(def-thunk (! fft-+-filter n max)
  (! <<n
     take-while (~ (! > max))'o 
     cl-bind^ (~ (! range+ n)) 'o
     cl-map  (~ (! <<v + -1 n 'o * 4 n)) 'o 
     range 0 +inf.0 '$))

(def-thunk (! fft---filter n max)
  [3n <- (! * 3 n)]
  (! <<n
     take-while (~ (! > max))'o 
     cl-bind^ (~ (! range+ n)) 'o
     cl-map  (~ (! <<v + -1 3n 'o * 4 n)) 'o 
     range 0 +inf.0 '$))

(def-thunk (! apply-fft-filters src out n)
  ;; (! displayall src out n)
  [max <- (! src 'size)]
  [n+1 <- (! + n 1)]
  [pluses  = (~ (! <<n cl-map (~ (! <<v src 'get)) 'o fft-+-filter n+1 max))]
  [minuses = (~ (! <<n cl-map (~ (! <<v * -1 'o src 'get)) 'o fft---filter n+1 max))]
  (! <<v out 'set n 'o digitize 'o
     cl-foldl^ + 0
     (~ (! cl-append pluses minuses))))

(def-thunk (! test)
  [v <- (! mutable-flexvec<-list '(0 1 2 3 4 5 6 7 8 9))]
  [o <- (! mk-mutable-flexvec 20 0)]
  ;; (! apply-fft-filters v o 0)
  (! <<n list<-colist 'o o 'to-colist)
  )

(def-thunk (! phase-v src)
  [sz <- (! src 'size)]
  [out <- (! mk-mutable-flexvec sz 0)]
  (! cl-foreach
     (~ (! apply-fft-filters src out))
     (~ (! range 0 sz)))
  (ret out))

(def-thunk (! fft-filter n)
  (! <<n
     (~ (! <<v $ 'o clv-tl 'o $))'o
     cl-cycle 'o
     cl-append* (~ (! repeat 0 n)) (~ (! repeat 1 n)) (~ (! repeat 0 n)) (~ (! repeat -1 n)) '$
     ))

(def-thunk (! apply-filter nums filter)
  ;; (! displayall 'apply-filter nums filter)
  (! <<n
     cl-foldl^ + 0 'o
     cl-map * nums (~ (! colist<-list filter)) '$))


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
  [v <- (! <<v mutable-flexvec<-list 'o map parse-num 'o map List 'o string->list 'o first 'o apply slurp-lines! args)]
  (! displayall 'start v)
  (! <<n cl-foreach displayall 'o
     cl-zipwith
     (~ (! range 0 101))
     (~ (! <<n cl-map (~ (! <<n list<-colist 'o @> 'to-colist)) 'o iterate phase-v v))))

(def-thunk (! b-phase src)
  [sz <- (! src 'size)]
  [out <- (! mk-mutable-flexvec sz 0)]
  (! <<n
     cl-foldl^
     (~ (λ (acc ix)
          (do [val <- (! <<v swap modulo 10 'o + acc 'o src 'get ix)]
              (! out 'set ix val)
            (ret val))))
     0 'o
     cl-map (~ (! - sz 1))'o
     range 0 sz '$)
  (ret out)
  )

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

(def-thunk (! main-b)
  
  (ret 'not-done-yet))

