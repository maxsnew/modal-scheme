#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Stream.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)

(def-thunk (! slurp-input)
  [chars <- (! <<v string->list 'o first 'o slurp-lines! '$)]
  (ret (~ (! <<n cl-map digit<-char 'o colist<-list chars '$))))

(def-thunk (! main-a)
  [nums <- (! slurp-input)]
  
  )

(def-thunk (! main-b)
  (ret 'not-done-yet))
