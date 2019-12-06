#lang sbpv

(require "stdlib.rkt")
(require "stdlib/CoList.rkt")
(require "stdlib/Stream.rkt")


(def-thunk (! evens)
  (! CBN
     (~ (! range 0))
     '>> (~ (! cl-map (~ (! * 2))))
     '>> stream<-colist
     ;; '>> (~ (! take 10))
     ;; '>> list<-colist
     '!))
