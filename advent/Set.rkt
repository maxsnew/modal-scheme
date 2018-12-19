#lang sbpv

(require "../stdlib.rkt")

;; codata Set A where
;;   'member? |- A -> F bool
;;   'add     |- A -> FU (Set A)
;;   'remove  |- A -> FU (Set A)

(def-thunk (! set<-hash h)
  (copat
   [((= 'member?) x #:bind)
    (! hash-has-key? h x)]
   [((= 'add) x #:bind)
    [h <- (! hash-set h x #t)]
    (ret (thunk (! set<-hash h)))]
   [((= 'remove) x #:bind)
    [h <- (! hash-remove h x)]
    (ret (thunk (! set<-hash h)))]
   [((= 'debug) #:bind) (! .v displayln hash-count h)]))
