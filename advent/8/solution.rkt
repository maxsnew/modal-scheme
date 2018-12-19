#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

;; A Tree is a (Node ,(U (CoList Node)) ,(U (CoList Nat))
;; 

;; Tokenize
(def-thunk (! nums)
  (! <<n
     list<-colist 'o cl-map parse-num 'o sep-by #\space 'o read-all-chars))

(def-thunk (! main-a)
  (! nums))

(def-thunk (! main-b)
  (ret 'not-done-yet))
