#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")
(require "../Intcode.rkt")

(provide main-a main-b)

(def/copat (! print-outputs-driver)
  [((= 'input) k) (! k 1 (~ (! print-outputs-driver)))]
  [((= 'output) o k) (! displayall o) (! k (~ (! print-outputs-driver)))]
  [((= 'halt)) (ret 'done)])

(def-thunk (! main-a)
  [syntax <- (! parse-intcode-program)]
  (! interp-intcode-program syntax print-outputs-driver))

(def-thunk (! main-b)
  (ret 'not-done-yet))
