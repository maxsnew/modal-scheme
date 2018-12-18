#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

;; First, we will determine a bounding box of the input. This can
;; easily be done by finding the smallest and largest x and y values
;; in the coordinate list.
;;
;; We can safely ignore any points outside the box: the nearest
;; neighbor of a point outside the box must have infinite associated
;; area anyway.
;;
;; To start, we'll keep it simple: just compare each cell to the
;; points in the input.
;; 

(define-thunk (! mk-pt)
  (copat [(x y) (ret (list 'pt x y))]))
(define-thunk (! pt-x)
  (copat [(pt) (! second pt)]))
(define-thunk (! pt-y) (ret #f))
(def-thunk (! pt-y pt) (! third pt))
(define-thunk (! manhattan)
  (copat
   [(p1 p2)
    (do [x1 <- (! pt-x p1)] [x2 <- (! pt-x p2)]
      [y1 <- (! pt-y p1)] [y2 <- (! pt-y p2)]
      [dx <- (! <<v abs 'o - x1 x2 )]
      [dy <- (! <<v abs 'o - y1 y2 )]
      (! + dx dy))]))

(define-thunk (! parse-pt-stk)
  (copat
   [((upto xs #\,) #\space (rest ys))
    (do [x <- (! parse-num xs)] [y <- (! parse-num ys)]
      (! mk-pt x y))]))
(define-thunk (! parse-line)
  (copat
   [(s #:bind) (! <<v apply parse-pt-stk 'o string->list l)]))

(define-thunk (! parse-input)
  (do [ls <- (! slurp-lines)]
      [id*pts <- (<<n list<-colist 'o
                      cl-zipwith (~ (! range 0 1000000000))
                      (~ (! colist<-list ls)))]
    [id*pts-c <- (ret (~ (! colist<-list id*pts)))]
    (ret id*pts-c)))

(define SMALL-X 40)
(define LARGE-X 354) ; upper bounds are exclusive
(define SMALL-Y 41)
(define LARGE-Y 355) ; upper bounds are exclusive
#;
(define (! mk-grid)
  )
(define-thunk (! main-a)
  (ret 'not))

(define-thunk (! main-b)
  (ret 'not-done-yet))
