#lang fiddle

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

(def-thunk (! main-a)
  (ret 'not-done-yet))

(def-thunk (! main-b)
  (ret 'not-done-yet))
