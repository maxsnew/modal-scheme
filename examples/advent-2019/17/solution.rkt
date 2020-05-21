#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require "../../Parse.rkt")

(require "../Intcode.rkt")
(require "../Coordinates.rkt")

(provide main-a main-b)

(def/copat (! driver-a canvas x y k)
  [((= 'output) n oK)
   [c <- (! integer->char n)]
   (cond [(! equal? c #\newline)
          [x = 0] [y <- (! + y 1)]
          (! oK (~ (! driver-a canvas x y k)))]
         [else
          [pt <- (! mk-coord x y)]
          (! canvas 'write pt c)
          [x <- (! + x 1)]
          (! oK (~ (! driver-a canvas x y k)))])]
  [() (! k canvas)])

(define WIDTH 49)
(define HEIGHT 41)

(def-thunk (! initialize-driver-a)
  [c <- (! mk-canvas WIDTH HEIGHT #\space)]
  (ret (~ (! driver-a c 0 0 abort))))

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
   (cond [(! empty? chars)
          [newline-int <- (! char->integer #\newline)]
          (! iK newline-int k)]
         [else
          [c <- (! first chars)] [chars <- (! rest chars)]
          (! iK c (~ (! enter-string chars k)))])])

(def/copat (! write-loop canvas x y k)
  [((= 'output) n oK)
   [c <- (! integer->char n)]
   (cond [(! equal? c #\newline)
          [x = 0] [y <- (! + y 1)]
          (! oK (~ (! write-loop canvas x y k)))]
         [else
          [pt <- (! mk-coord x y)]
          (! canvas 'write pt c)
          [x <- (! + x 1)]
          (! oK (~ (! write-loop canvas x y k)))])]
  [() (! k)])

;; (def-thunk (! input-list c strings k)
;;   (! displayall 'letsgo strings k)
;;   (! <<n cl-foreach displayall 'o c 'paint Ret)
;;   [fst <- (! first strings)]
;;   [strings <- (! rest strings)]
;;   [k <- (cond [(! empty? strings) (ret k)]
;;               [else (ret (~ (! input-list c strings k)))])]
;;   (! write-loop c (~ (! enter-string fst (~ (! input-list c strings k))))))

(def-thunk (! driver-b c inps last-out)
  (copat
   [((= 'input))
    [inp <- (! <<v map char->integer 'o string->list 'o first inps)]
    [inps <- (! rest inps)]
    (! displayall 'inputting inp)
    (! enter-string inp (~ (! driver-b c inps last-out)) 'input)]
   [((= 'output) o oK) (! oK (~ (! driver-b c inps o)))]
   [((= 'halt)) (ret last-out)]))

(def-thunk (! main-b)
  [HEIGHT <- (! + 20 HEIGHT)]
  [syn <- (! parse-intcode-program "input")]
  [syn-tail <- (! rest syn)]
  [syn <- (! Cons 2 syn-tail)]
  [c <- (! mk-canvas WIDTH HEIGHT #\space)]
  [inps <- (! List MAIN A B C "n")]
  (! interp-intcode-program syn (~ (! driver-b c inps #f))))

