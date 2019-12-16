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
  (! <<n cl-foreach displayall 'o cl-zipwith (~ (! range 0 101)) (~ (! iterate (~ (! phase filters)) l-of-num))))

(def-thunk (! main-b)
  (ret 'not-done-yet))
