#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

(define SERIAL-NUMBER 18)
;; (define SERIAL-NUMBER 1718)

(def-thunk (! hundreds-place n)
  [n <- (! quotient n 100)]
  (! modulo n 10))

(def-thunk (! power x y)
  [rack-ID <- (! + x 10)]
  [pl0 <- (! * rack-ID y)]
  [pl1 <- (! + pl0 SERIAL-NUMBER)]
  [pl2 <- (! * pl1 rack-ID)]
  [pl3 <- (! hundreds-place pl2)]
  (! - pl3 5))

(def-thunk (! region<-vec v min-x x-size min-y y-size size)
  [ix<-pt
   = (~ (copat [(x y) [x-off <- (! - x min-x)]
                      [y-off <- (! - y min-y)]
                      (! <<v + x-off 'o * y-off x-size)]))]
  (copat
   [((= 'set) x y c)
    
    [ix <- (! ix<-pt x y)]
    (cond [(! or (~ (! < ix 0)) (~ (! >= ix size))) (ret '())]
          [else (! vector-set! v ix c) (ret '())])]
   [((= 'get) x y) [ix <- (! ix<-pt x y)] (! vector-ref v ix)]
   [((= 'rows))
    [max-x <- (! + min-x x-size)]
    [max-y <- (! + min-y y-size)]
    (! <<n
       cl-map (~ (λ (y)
                   (ret (~
                         (! <<n cl-map (~ (λ (x) (! <<v vector-ref v 'o ix<-pt x y))) 'o range min-x max-x))))) 'o
       range min-y max-y)]))

(def-thunk (! mk-region min-x max-x min-y max-y)
  [x-size <- (! - max-x min-x)]
  [y-size <- (! - max-y min-y)]
  [vec-size <- (! * x-size y-size)]
  [v <- (! make-vector vec-size #f)]
  (ret (~ (! region<-vec v min-x x-size min-y y-size vec-size))))



(def-thunk (! main-a)
  [r <- (! mk-region 1 301 1 301)]
  [set-power = (~ (λ (xy)
                    (do [x <- (! first xy)] [y <- (! second xy)]
                      [xy-power <- (! power x y)]
                      (! r 'set x y xy-power))))]
  (! <<n cl-foreach set-power 'o cartesian-product (~ (! range 1 301)) (~ (! range 1 301)))
  [read-power
   = (~ (! r 'get))]
  [3x3-power
   = (~ (λ (xy)
          (do [x <- (! first xy)] [y <- (! second xy)]
            (cond
              [(! and (~ (! <<v >= 300 'o + x 2)) (~ (! <<v >= 300 'o + y 2)))
               [3x3-power <- (! <<n cl-foldl^ + 0 'o cl-map (~ (! apply read-power)) 'o
                    cartesian-product (~ (! <<v range x 'o + x 3)) (~ (! <<v range y 'o + y 3)))]
               (! cl-cons (list 3x3-power x y) cl-nil)]
              [else (! cl-nil)]))))]
  [display-row
   = (~ (λ (row)
          (do (! cl-foreach (~ (λ (x) (do (! display x) (! display " ")))) row)
              (! display "\n"))))]
  [3x3-powers = (~ (! cl-bind 3x3-power 'o cartesian-product (~ (! range 1 301)) (~ (! range 1 301))))]
  (! <<n cl-foreach displayln 3x3-powers))

(def-thunk (! main-b)
  (ret 'not-done-yet))
