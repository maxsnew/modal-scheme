#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")
(require "../../FlexVec.rkt")
(require "../Intcode.rkt")

(provide main-a main-b)

(def-thunk (! main-a)
  (! <<v swap run-intcode-program '(1) 'o debug 'parsed 'o parse-intcode-program '$))

(def-thunk (! main-b)
  (! <<v swap run-intcode-program '(5) 'o debug 'parsed 'o parse-intcode-program '$))
