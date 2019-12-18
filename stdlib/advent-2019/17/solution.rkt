#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")

(require "../Intcode.rkt")
(require "../Coordinates.rkt")

(provide main-a main-b)

(def/copat (! driver-a canvas x y)
  [((= 'output) n oK)
   [c <- (! integer->char n)]
   (cond [(! equal? c #\newline)
          [x = 0] [y <- (! + y 1)]
          (! oK (~ (! driver-a canvas x y)))]
         [else
          [pt <- (! mk-coord x y)]
          (! canvas 'write pt c)
          [x <- (! + x 1)]
          (! oK (~ (! driver-a canvas x y)))])]
  [((= 'halt)) (ret canvas)])

(define WIDTH 49)
(define HEIGHT 41)

(def-thunk (! initialize-driver-a)
  [c <- (! mk-canvas WIDTH HEIGHT #\space)]
  (ret (~ (! driver-a c 0 0))))

(def-thunk (! intersection? canvas x y)
  [pt <- (! mk-coord x y)]
  [cross <- (! idiom^ List (~ (! mk-coord 0 0)) (~ (! mk-coord 0 1)) (~ (! mk-coord 0 -1)) (~ (! mk-coord 1 0)) (~ (! mk-coord -1 0)))]
  (! <<n cl-foldr^ and (~ (ret #t)) 'o
     cl-map Thunk 'o
     cl-map (~ (! <<v equal? #\# 'o canvas 'read)) 'o
     cl-map (~ (! coord-add pt)) 'o
     colist<-list cross))

(def-thunk (! main-a)
  [syn <- (! parse-intcode-program "input")]
  [driver <- (! initialize-driver-a)]
  [canvas <- (! interp-intcode-program syn driver)]
  (! <<n cl-foreach displayall 'o canvas 'paint Ret)
  [w-2 <- (! - WIDTH 2)]
  [h-2 <- (! - HEIGHT 2)]
  (! <<n cl-foldl^ + 0 'o
     cl-map (~ (! apply *)) 'o
     cl-filter (~ (! apply (~ (! intersection? canvas)))) 'o
     cartesian-product (~ (! range 1 w-2)) (~ (! range 1 h-2))))

(def-thunk (! main-b)
  (ret 'not-done-yet))
