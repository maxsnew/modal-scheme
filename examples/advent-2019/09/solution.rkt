#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/IO)
(require sbpv/stdlib/CoList)
(require "../../Parse.rkt")
(require "../Intcode.rkt")

(provide main-a main-b)

(def/copat (! print-outputs-driver inp)
  [((= 'input) k) (! k inp (~ (! print-outputs-driver inp)))]
  [((= 'output) o k) (! displayall o) (! k (~ (! print-outputs-driver inp)))]
  [((= 'halt)) (ret 'done)])

(def-thunk (! main-a)
  [syntax <- (! parse-intcode-program)]
  (! interp-intcode-program syntax (~ (! print-outputs-driver 1))))

(def-thunk (! main-b)
  [syntax <- (! parse-intcode-program)]
  (! interp-intcode-program syntax (~ (! print-outputs-driver 2))))
