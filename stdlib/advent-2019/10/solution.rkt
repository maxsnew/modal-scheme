#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)

;; data:
;;
;; 1. the width and height of the map
;; 2. the starmap, a function (n <= width) x (m <= height) -> Bool

;; A coordinate n m is a List {<= n} {<= m}

(def/copat (! mk-coord) [(x y #:bind) (! List x y)])
(def/copat (! x-coord) [(c #:bind) (! first c)])
(def/copat (! y-coord) [(c #:bind) (! second c)])
(def/copat (! scale pt n)
  [(#:bind)
   (! idiom (~ (ret mk-coord)) (~ (! <<v * n 'o x-coord pt '$))
                               (~ (! <<v * n 'o y-coord pt '$)))])
(def-thunk (! coord-add c1 c2)
  (! idiom^ mk-coord (~ (! idiom^ + (~ (! x-coord c1)) (~ (! x-coord c2))))
                     (~ (! idiom^ + (~ (! y-coord c1)) (~ (! y-coord c2))))))

(def-thunk (! change-of-origin new-origin c)
  (! idiom^ (~ (! coord-add c)) (~ (! scale new-origin -1))))

(def-thunk (! unchange-of-origin new-origin c)
  (! coord-add new-origin c))

(def-thunk (! magnitude coord)
  [x <- (! x-coord coord)] [y <- (! y-coord coord)]
  (! gcd x y))

(def-thunk (! unitize coord)
  [scaler <- (! magnitude coord)]
  (! <<v scale coord 'o / 1 scaler '$))

;; Coordinate n m -> Colist (Coordinate n m)
(def-thunk (! obstructions coord)
  [scaler <- (! magnitude coord)] [unit <- (! unitize coord)]
  (! <<n cl-map (~ (! scale unit)) 'o range 1 scaler '$))

;; A starmap n m is a thunk supporting
;; 'width : F Nat
;; 'height : F Nat
;; 'asteroid? : Coordinate n m -> F Bool
;; 'vaporize! : Coordinate n m -> F 1

(def-thunk (! pt<-ix w h ix)
  (! idiom^ mk-coord (~ (! modulo ix w)) (~ (! quotient ix w))))

;; x y |-> y * width + x
(def-thunk (! ix<-pt w h pt)
  [x <- (! x-coord pt)] [y <- (! y-coord pt)]
  (! <<v + x 'o * y w '$))

;; Implement a starmap backed by a vector of size width * height
(def/copat (! from-vec width height v)
  [((= 'width)) (ret width)]
  [((= 'height)) (ret height)]
  [((= 'asteroid?) pt)
   (! <<v vector-ref v 'o ix<-pt width height pt '$)]
  [((= 'vaporize!) pt)
   [ix <- (! ix<-pt width height pt)]
   (! vector-set! v ix #f)])

(def-thunk (! in-bounds? smap pt)
  (! and
     (~ (! idiom^ <  (~ (! x-coord pt)) (~ (! smap 'width))))
     (~ (! idiom^ >= (~ (! x-coord pt)) (~ (ret 0))))
     (~ (! idiom^ <  (~ (! y-coord pt)) (~ (! smap 'height))))
     (~ (! idiom^ >= (~ (! y-coord pt)) (~ (ret 0))))))

;; F (Starmap)
(def-thunk (! slurp-map)
  [lines <- (! slurp-lines!)]
  [width <- (! <<v length 'o string->list 'o first lines '$)]
  [height <- (! length lines)]
  [v <- (! <<n (~ (! .v list->vector list<-colist)) 'o
           cl-map (~ (! equal? #\#)) 'o
           cl-join 'o
           cl-map (~ (λ (xs) (ret (~ (! colist<-list xs))))) 'o
           cl-map string->list 'o
           colist<-list lines '$)]
  (ret (~ (! from-vec width height v))))

;; idea: use relative coordinates, if a candidate's relative
;; coordinates x, y have a common divisor, then check the shit that's
;; closer first to see if it's blocked.

;; To start let's do a slow implementation: just for each square check
;; every other square to see if it sees something there.
(def-thunk (! sees? starmap src tgt)
  [relative-tgt <- (! change-of-origin src tgt)]
  (! <<n
     (~ (! .v not any?)) 'o
     cl-map Thunk 'o
     cl-map (~ (! starmap 'asteroid?)) 'o
     cl-map (~ (! unchange-of-origin src)) 'o
     obstructions relative-tgt '$))

;; all-asteroids : Starmap -> CoList Coordinate
(def-thunk (! all-asteroids smap)
  [w <- (! smap 'width)] [h <- (! smap 'height)]
  [len <- (! * w h)]
  (! <<n
     cl-map (~ (! pt<-ix w h)) 'o
     cl-filter (~ (λ (i)
                    (do [pt <- (! pt<-ix w h i)]
                        (! smap 'asteroid? pt)))) 'o
     range 0 len '$))

(def-thunk (! all-seen smap pt)
  (! <<n
     cl-length 'o
     cl-filter (~ (! sees? smap pt)) 'o
     cl-filter (~ (! <<v not 'o equal? pt)) 'o
     all-asteroids smap '$))

(def-thunk (! main-a)
  [smap <- (! slurp-map)]
  (! <<n
     minimum-by (~ (! <<v * -1 'o second)) '(0 -inf.0) 'o
     cl-map (~ (λ (pt) (! <<v List pt 'o all-seen smap pt '$))) 'o
     all-asteroids smap '$))

(def-thunk (! safe-/ x y)
  (cond [(! zero? y) (! * x +inf.0)]
        [else (! / x y)]))

;; pre-cond not both = 0

;; return an angle from 0 to 2pi
(def-thunk (! better-angle z)
  [bad-angle <- (! angle z)]
  (cond [(! < bad-angle 0) (! idiom^ + (~ (! * 2 pi)) (~ (ret bad-angle)))]
        [else (ret bad-angle)]))
;; polar coordinates as fraction of the way around a circle of unit circumference
;; 0 is at x = 0, y = -max

(def-thunk (! anglify x y)
  [complex <- (! idiom^ (~ (! + x)) (~ (! * y 0+1i)))]
  [rotated <- (! * complex 0+1i)]
  (! better-angle rotated))

(def-thunk (! angle-< c1 c2)
  (! idiom^ < (~ (! apply anglify c1)) (~ (! apply anglify c2))))

(def-thunk (! all-angles smap pt)
  [angles-with-dups <- (! <<n
                          list<-colist 'o
                          cl-map unitize 'o
                          cl-map (~ (! change-of-origin pt)) 'o
                          cl-filter (~ (! <<v not 'o equal? pt)) 'o
                          all-asteroids smap '$)]
  [angles-unsorted <- (! <<v set->list 'o list->set angles-with-dups '$)]
  (! sort angles-unsorted angle-<))

(def-thunk (! attempt-to-vaporize smap pt k)
  (cond [(! smap 'asteroid? pt)
         (! smap 'vaporize! pt)
         (ret pt)]
        [else (! k)]))

;; vaporize-one : Starmap -> Coordinate -> Unit-Coordinate -> F(Union #f Coordinate)
(def-thunk (! vaporize-one smap src angle)
  (! <<n
     cl-foldr^ (~ (! attempt-to-vaporize smap)) (~ (ret #f)) 'o
   take-while (~ (! in-bounds? smap)) 'o
   cl-map (~ (! unchange-of-origin src)) 'o
   cl-map (~ (! scale angle)) 'o
   range 1 +inf.0 '$)
  )

;; vaporize-loop : Starmap -> Coordinate -> U(CoList Coordinate) -> CoList Coordinate
(def-thunk (! vaporize-loop smap src targets)
  [vaporize-step
   = (~ (λ (tgt later-vaporized)
          (do 
              [may-coord <- (! vaporize-one smap src tgt)]
              (if may-coord
                  (! cl-cons may-coord later-vaporized)
                  (! later-vaporized)))))]
  (! cl-foldr targets vaporize-step cl-nil))

;; sample-b start is 8 3
;; sample-4 start is 11 13
;; part a answer  is 8 16
(def-thunk (! main-b)
  [smap <- (! slurp-map)]
  [src <- (! mk-coord 8 16)]
  [angle-list <- (! all-angles smap src)]
  [vaporized-asteroids = (~ (! <<n vaporize-loop smap src 'o cycle (~ (! colist<-list angle-list)) '$))]
  (! <<n cl-foreach displayall 'o cl-zipwith vaporized-asteroids 'o range 1 201 '$))

;; cartesian x y to polar is
;; theta = tan^-1(y / x)
;; so comparing angle(x y) <= angle(x' y') is the same as
;;   y/x <= y'/x' 
