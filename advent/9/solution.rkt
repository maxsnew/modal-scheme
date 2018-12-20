#lang sbpv

(require "../../stdlib.rkt")
(require "../CoList.rkt")
(require "../Stream.rkt")
(require "../Finger.rkt")

(provide main-a main-b)

;; A Circle A is an ADT supporting the following operations
;; codata Circle A where
;;   'current-value |- F A
;;   'remove-current-value |- FU Circle A
;;   'add |- A -> FU Circle A
;;   'move          |- Int -> FU Circle A
;; such that move and current-value are consistent
;; and the current-value after removing should be the one to the right of it

;; FlexVec A -> Nat -> Nat -> Circle A
(def-thunk (! zippy-circle flex size cursor)
  (! <<v displayln 'o flex 'debug)
  (copat
   [((= 'current-value)) (! displayln 'cur)
    (! flex 'get cursor)]
   [((= 'remove-current-value))
    (! displayln 'rem)
    (! displayln size)
    (! displayln cursor)
    [flex <- (! <<v second 'o flex 'remove cursor)]
    [size <- (! - size 1)]
    [cursor <- (! modulo cursor size)]
    (ret (~ (! zippy-circle flex size cursor)))]
   [((= 'add) x)
    (cond
      [(! >= cursor size) (error "too big")]
      [else
       (! displayln 'add)
       (! displayln cursor)
       (! displayln x)
       [flex <- (! flex 'insert cursor x)]
       (! displayln 'thisdoesnthappen)
       [size <- (! + size 1)]
       (ret (~ (! zippy-circle flex size cursor)))])]
   [((= 'move) n) (! displayln 'move)
    [cursor <- (! <<v swap modulo size 'o + cursor n)]
                  (! displayln size)
    (! displayln 'new-cursor)  (! displayln cursor)
    (ret (~ (! zippy-circle flex size cursor)))]))

(def-thunk (! single-circle x)
  [fv <- (! mt-flexvec 'cons x)]
  (! zippy-circle fv 1 0))

(define NUM-PLAYERS 10)
(define NUM-MARBLES 1618) ;; answer: 8317

(def-thunk (! next-player cur-player)
  (! <<v swap modulo NUM-PLAYERS 'o + 1 cur-player))
(def-thunk (! operate circle scores cur-player turns)
  [turns-v <- (! turns)]
  (cond [(! clv-nil? turns-v) (! cl-nil)]
        [else
         [marble <- (! clv-hd turns-v)] [turns <- (! clv-tl turns-v)]
         (! displayln 'turn) (! displayln marble)
         (cond [(! <<v equal? 0 'o modulo marble 23)
                [circle <- (! circle 'move -7)]
                [marble2 <- (! circle 'current-value)]
                [circle <- (! circle 'remove-current-value)]
                [scores <- (! scores 'update cur-player (~ (! + marble marble2)))]
                [cur-player <- (! next-player cur-player)]
                (! cl-cons scores
                           (~ (! operate circle scores cur-player turns)))]
               [else
                [circle <- (! circle 'move 2)]
                [circle <- (! circle 'add marble)]
                [cur-player <- (! next-player cur-player)]
                (! cl-cons scores
                           (~ (! operate circle scores cur-player turns)))])]))

(def-thunk (! main-a)
  [circle = (~ (! single-circle 0))]
  [zeros <- (! <<n list<-colist 'o take NUM-PLAYERS 'o stream-const 0)]
  [init-scores <- (! flexvec<-list zeros)]
  (! <<n cl-foreach displayln 'o
     operate circle init-scores 0 'o
     range 1 NUM-MARBLES))

(def-thunk (! main-b)
  (ret 'not-done-yet))
