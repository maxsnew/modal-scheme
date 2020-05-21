#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require "../../Parse.rkt")

(require "../Intcode.rkt")
(require "../Coordinates.rkt")

(provide main-a main-b)

(def/copat (! output-collector driver)
  [((= 'output) o0 k0)
   (! k0 (~ (copat
   [((= 'output) o1 k1)
   (! k1 (~ (copat
   [((= 'output) o2 k2)
   (! driver 'output o0 o1 o2 (~ (copat
   [(driver)
   (! k2 (~ (! output-collector driver)))])))])))])))]
  [((= 'input) iK)
   (! driver 'input (~ (λ (i driver) (! iK i (~ (! output-collector driver))))))]
  [((= 'halt)) (! driver 'halt)]
  )

(def/copat (! count-block-driver num)
  [((= 'output) x y id k)
   (! displayall 'pt x y)
   [num <- (ifc (! = 2 id) (! + num 1) (ret num))]
   (! k (~ (! count-block-driver num)))]
  [((= 'halt)) (ret num)]
  )

(def-thunk (! driver-a)
  (! output-collector (~ (! count-block-driver 0))))

(def-thunk (! main-a)
  [syntax <- (! parse-intcode-program "input")]
  (! interp-intcode-program syntax driver-a))

(def/copat (! paint-tile)
  [((= 0)) (ret #\space)] ;; empty
  [((= 1)) (ret #\#)] ;; indestructible
  [((= 2)) (ret #\%)] ;; destructible
  [((= 3)) (ret #\^)] ;; paddle
  [((= 4)) (ret #\*)] ;; ball
  )

(def-thunk (! joystick)
  [c <- (! <<v string->list 'o read-line)]
  (! apply
     (~ (copat
         [((= #\u001B) (= #\[) (= #\D) (rest who-cares));; left
          (ret -1)]
         [((= #\u001B) (= #\[) (= #\C) (rest who-cares)) ;; right
          (ret 1)]
         [((= #\u001B) (= #\[) (= #\A) (rest who-cares)) ;; up
          (ret 0)]
         [((rest invalid-input))
          (! displayall "invalid input")
          (! joystick)]))
     c))

(def-thunk (! game-ai block player)
  (cond [(! = block player) (ret 0)]
        [(! < block player) (ret -1)]
        [(! > block player) (ret 1)]))

(def-thunk (! pause)
  (! read-char))

(def/copat (! core-game-driver canvas block-x player-x inp-trace)
  [((= 'output) (= -1) (= 0) score k)
   (! cl-foreach displayall (~ (! canvas 'paint paint-tile)))
   (! displayall 'score-update score)
   (! k (~ (! core-game-driver canvas block-x player-x inp-trace)))]
  [((= 'output) x y id k)
   [pt <- (! mk-coord x y)]
   [block-x <- (ifc (! = id 4)  (ret x) (ret block-x))]
   [player-x <- (ifc (! = id 3) (ret x) (ret player-x))]
   (! canvas 'write pt id)
   (! k (~ (! core-game-driver canvas block-x player-x inp-trace)))]
  [((= 'input) k)
   ;; (! cl-foreach displayall (~ (! canvas 'paint paint-tile)))
   ;; (! displayall 'block-x:  block-x)
   ;; (! displayall 'player-x: player-x)
   ;; (! displayall 'input-demanded)
   ;; [i <- (! joystick)]
   [i <- (! game-ai block-x player-x)]
   ;; (! pause)
   [inp-trace <- (! Cons i inp-trace)]
   (! k i (~ (! core-game-driver canvas block-x player-x inp-trace)))]
  [((= 'halt))
   ;; (! cl-foreach displayall (~ (! canvas 'paint paint-tile)))
   (ret inp-trace)])

(def-thunk (! game-driver)
  [canvas <- (! mk-canvas 45 26 0)]
  (! output-collector (~ (! core-game-driver canvas #f #f '()))))

(def-thunk (! main-b)
  [tail-syn <- (! <<v rest 'o parse-intcode-program "input" '$)]
  [syntax <- (! Cons 2 tail-syn)]
  (! interp-intcode-program syntax game-driver))
