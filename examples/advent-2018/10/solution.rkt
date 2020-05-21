#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require "../../Parse.rkt")

(provide main-a main-b)

(def-thunk (! assert th msg)
  (cond [(! th) (ret '())]
        [else (! error msg)]))

;; A Position is a (List 'position (List Int Int))
(def-thunk (! pos-x) (! <<v first  'o second))
(def-thunk (! pos-y) (! <<v second 'o second))
(def-thunk (! mk-pos x y) (! List 'position (list x y)))
;; A Velocity is a (List 'velocity Int Int)
(def-thunk (! vel-x) (! second))
(def-thunk (! vel-y) (! third))
(def-thunk (! mk-vel dx/dt dy/dt) (! List 'velocity dx/dt dy/dt))

;; A Light is a (List 'light Position Velocity)
(def-thunk (! light-pos) (! second))
(def-thunk (! light-vel) (! third))
(def-thunk (! mk-light p v) (! List 'light p v))

(def/copat (! parse-space-num)
  [((= #\space)) (! dot-args parse-num)]
  [() (! dot-args parse-num)])
(def/copat (! parse-light)
  [(p o s i t i o n = < (upto     px #\,) (= #\space) (upto     py #\>) (= #\space)
    v e l o c i t y = < (upto dx/dt #\,) (= #\space) (upto dy/dt #\>))
   [xydxdy <- (! map (~ (! apply parse-space-num)) (list px py dx/dt dy/dt))]
   (! @>> xydxdy
      (~ (copat
          [(x y dx/dt dy/dt)
           [p <- (! mk-pos x y)] [v <- (! mk-vel dx/dt dy/dt)]
           (! mk-light p v)])))])

(def-thunk (! lights)
  (! <<n cl-map (~ (! apply parse-light)) 'o cl-map string->list 'o slurp-lines~))

;; (define MIN-X -10)
;; (define MAX-X 20)
;; (define MIN-Y -10)
;; (define MAX-Y 20)
(define MIN-X 100)
(define MAX-X 300)
(define MIN-Y 100)
(define MAX-Y 150)
;;   MIN-X: -150
;;   MAX-X: 550
;;   MIN-Y: -200
;;   MAX-Y: 500


;; A region represents a function from [MIN-X,MAX-X) * [MIN-Y,MAX-Y) -> Char
;; mutable
;; codata Region A where
;;   'set   |- X-coord -> Y-coord -> Char -> F 1
;;   'get   |- X-coord -> Y-coord -> F Char
;;   'clear |- F 1
;;   'rows  |- CoList (U CoList Char)
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
   [((= 'clear)) (! vector-fill! v #\.) (ret '())]
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
  [v <- (! make-vector vec-size #\space)]
  (ret (~ (! region<-vec v min-x x-size min-y y-size vec-size))))

;; A Skyline is a codata type:
;; codata Skyline where
;;   'set-time |- 

(def-thunk (! update-position initial velocity dt)
  [x <- (! pos-x initial)] [y <- (! pos-y initial)]
  [dx/dt <- (! vel-x velocity)] [dy/dt <- (! vel-y velocity)]
  [x <- (! <<v + x 'o * dx/dt dt)]
  [y <- (! <<v + y 'o * dy/dt dt)]
  (ret (list x y))
  )

(def-thunk (! display@time r lights t)
  (! displayall 'time t)
  (! cl-foreach
     (~ (λ (l)
          (do [pos <- (! light-pos l)] [vel <- (! light-vel l)]
            [xy <- (! update-position pos vel t)]
            [x <- (! first xy)] [y <- (! second xy)]
            (! r 'set x y #\#))))
     lights)
  (! <<n cl-foreach (~ (! <<v displayln 'o list->string 'o list<-colist)) 'o r 'rows)
  (! r 'clear))

;; t = (- 2 (x1 - x2) (dx1 - dx2) - 2 (y1 - y2) (dy1 - dy2)) / (2 (dx1 - dx2)^2 + 2 (dy - dy2)^2)

(def-thunk (! closest-point l1 l2)
  [p1 <- (! light-pos l1)] [v1 <- (! light-vel l1)]
  [x1 <- (! pos-x p1)] [y1 <- (! pos-y p1)]
  [dx1 <- (! vel-x v1)] [dy1 <- (! vel-y v1)]
  [p2 <- (! light-pos l2)] [v2 <- (! light-vel l2)]
  [x2 <- (! pos-x p2)] [y2 <- (! pos-y p2)]
  [dx2 <- (! vel-x v2)] [dy2 <- (! vel-y v2)]
  ;;
  [x1-2 <- (! - x1 x2)] [dx1-2 <- (! - dx1 dx2)]
  [y1-2 <- (! - y1 y2)] [dy1-2 <- (! - dy1 dy2)]
  [dx-sq <- (! * dx1-2 dx1-2)]
  [dy-sq <- (! * dy1-2 dy1-2)]
  [x-dx- <- (! * x1-2 dx1-2)]
  [y-dy- <- (! * y1-2 dy1-2)]
  [numerator <- (! <<v * -2 'o + x-dx- y-dy-)]
  [denominator <- (! <<v * 2 'o + dx-sq dy-sq)]
  (! <<v truncate 'o / numerator denominator))

(def-thunk (! main-a)
  (! displayln 'started)
  [l <- (! list<-colist lights)] [l-iter = (~ (! colist<-list l))]
  (! displayln 'parsed)
  [r <- (! mk-region MIN-X MAX-X MIN-Y MAX-Y)]
  (! displayln 'initialized)
  (! <<n cl-foreach (~ (! display@time r l-iter)) 'o
     range 10136 10137))

(def-thunk (! main-b)
  (ret 'not-done-yet))

;; two points from the input
#;
(do [l1 <- (! <<v apply parse-light 'o string->list "position=< -9951, -50547> velocity=< 1,  5>")]
    [l2 <- (! <<v apply parse-light 'o string->list "position=< 40761, -20137> velocity=<-4,  2>")]
  (! displayall l1 l2)
  [t <- (! closest-point l1 l2)]
  [p1 <- (! light-pos l1)] [v1 <- (! light-vel l1)]
  [p2 <- (! light-pos l2)] [v2 <- (! light-vel l2)]
  (! displayall 'time t)
  (! <<v displayall l1 'o update-position p1 v1 t)
  (! <<v displayall l2 'o update-position p2 v2 t))
;; returns
;; at time t: 10140
;; (189 153)
;; and
;; (201 143)
;; the input is 344 total points, so let's give it 350 in each direction
;; Origin: 200 150
;;   MIN-X: -150
;;   MAX-X: 550
;;   MIN-Y: -200
;;   MAX-Y: 500

