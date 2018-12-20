#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

;; A Circle A is an ADT supporting the following operations
;; codata Circle A where
;;   'current-value |- F A
;;   'remove-current-value |- FU Circle A
;;   'add |- A -> FU Circle A
;;   'move          |- Int -> FU Circle A
;; such that move and current-value are consistent
;; and the current-value after removing should be the one to the right of it


;; Listof A -> A -> NEListof A -> Circle A
(def-thunk (! zippy-circle sz )
  (copat
   [((= 'current-value) #:bind) (ret c)]
   [((= 'remove-current-value) #:bind)
    [c <- (! car r)] [r <- (! cdr r)]
    (ret (~ (! zippy-circle l c r)))]
   [((= 'move i)
     (cond [(! equal? i 0) (ret (~ (! zippy-circle l c r)))]
           [(! positive? 0)
            [i <- (! - i 1)]
            (cond [])
            ]))]))

(def-thunk (! single-circle x)
  (copat
   [((= 'current-value) #:bind) (ret x)]
   [((= 'move i) (ret (~ (! single-circle x))))]))



(def-thunk (! main-a)
  (ret 'not-done-yet))

(def-thunk (! main-b)
  (ret 'not-done-yet))
