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

;; The correct instructions:
;; L,10,R,8,R,8,L,10,R,8,R,8,L,10,L,12,R,8,R,10,R,10,L,12,R,10,L,10,L,12,R,8,R,10,R,10,L,12,R,10,L,10,L,12,R,8,R,10,R,10,L,12,R,10,R,10,L,12,R,10,L,10,R,8,R,8

;; Main Routine: A,A,B,C,B,C,B,C,C,A
(define MAIN "A,A,B,C,B,C,B,C,C,A")
;; Subroutine A: L,10,R,8,R,8
(define A "L,10,R,8,R,8" )
;; Subroutine B: L,10,L,12,R,8,R,10
(define B "L,10,L,12,R,8,R,10")
;; Subroutine C: R,10,L,12,R,10
(define C "R,10,L,12,R,10")

(def/copat (! enter-string chars k)
  [((= 'input) iK)
  (! displayall 'letsgo)
   (cond [(! empty? chars) (! iK #\newline k)]
         [else
          [c <- (! first chars)] [chars <- (! rest chars)]
          (! iK c (~ (! enter-string chars k)))])])

(def-thunk (! input-list strings k)
  (! displayall 'letsgo)
  [fst <- (! first strings)]
  [strings <- (! rest strings)]
  [k <- (cond [(! empty? strings) (ret k)]
              [else (ret (~ (! input-list strings k)))])]
  (! enter-string fst k))

(def-thunk (! driver-b)
  [inputs <- (! <<v map string->list 'o List MAIN A B C "n")]
  (! input-list inputs
     (~ (copat
         [((= 'output) o oK) (ret o)]))))

(def-thunk (! main-b)
  [syn <- (! parse-intcode-program "input")]
  [syn-tail <- (! rest syn)]
  [syn <- (! Cons 2 syn-tail)]
  (! interp-intcode-program syn (~ (! driver-b))))

