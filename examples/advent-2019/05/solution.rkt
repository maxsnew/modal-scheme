#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/IO)
(require sbpv/stdlib/CoList)
(require "../../Parse.rkt")
(require sbpv/stdlib/FlexVec)
(require "../Intcode.rkt")

(provide main-a main-b)

(def-thunk (! run-with-input x)
  [syntax <- (! parse-intcode-program)]
  [inps <- (! List x)]
  [driver = (~ (! static-input-return-last-output inps #f))]
  (! interp-intcode-program syntax driver))

(def-thunk (! main-a) (! run-with-input 1))

(def-thunk (! main-b) (! run-with-input 5))
