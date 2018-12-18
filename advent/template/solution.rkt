#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

(define-thunk (! main-a)
  (ret 'not-done-yet))

(define-thunk (! main-b)
  (ret 'not-done-yet))
