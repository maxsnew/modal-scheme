#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)

(def-thunk (! mk-direction dir num) (! List dir num))

(def/copat (! parse-chars)
  [((= 'start-line) wires #:bind)
   (! reverse wires)]
  [((= 'start-line) wires)
   ;; (! displayall 'start wires)
   (! parse-chars 'dir wires '())]
  [((= 'dir) wires dirs dir)
   ;; (! displayall 'dir wires dirs dir)
   (! parse-chars 'length wires dirs dir '())]
  [((= 'length) wires dirs dir digits (= #\newline))
   ;; (! displayall 'len wires dirs dir digits 'newline)
   [direction <- (! <<v mk-direction dir 'o parse-num 'o reverse digits '$)]
   [wires <- (! <<v swap Cons wires 'o reverse 'o Cons direction dirs '$)]
   (! parse-chars 'start-line wires)]
  [((= 'length) wires dirs dir digits (= #\,))
   ;; (! displayall 'len wires dirs dir digits 'comma)
   [direction <- (! <<v mk-direction dir 'o parse-num 'o reverse digits '$)]
   [dirs <- (! Cons direction dirs)]
   (! parse-chars 'dir wires dirs)]
  [((= 'length) wires dirs dir digits digit)
   ;; (! displayall 'len wires dirs dir digits digit)
   [digits <- (! Cons digit digits)]
   (! parse-chars 'length wires dirs dir digits)]
  [()
   ;; (! displayall 'init)
   (! parse-chars 'start-line '())])

;; 
(def-thunk (! parse-input)
  [cs <- (! <<n list<-colist 'o read-all-chars '$)]
  (! apply parse-chars cs))

;; A Line is one of
;; (list 'Vert x lo-y hi-y) where x and lo-y <= hi-y are numbers
;; (list 'Hor  lo-x hi-x y) where lo-x <= hi-x and y are numbers

(def-thunk (! apply-vector start vector)
  [x <- (! first start)]
  [y <- (! second start)]
  [dir <- (! first vector)]
  [n <- (! second vector)]
  ((copat [((= #\U))
           [hi-y <- (! + y n)]
           [line <- (! List 'Vert x y hi-y)]
           [new-pt <- (! List x hi-y)]
           (! List line new-pt)]
          [((= #\D))
           [lo-y <- (! - y n)]
           [line <- (! List 'Vert x lo-y y)]
           [new-pt <- (! List x lo-y)]
           (! List line new-pt)]
          [((= #\L))
           [lo-x <- (! - x n)]
           [line <- (! List 'Hor  lo-x x y)]
           [new-pt <- (! List lo-x y)]
           (! List line new-pt)]
          [((= #\R))
           [hi-x <- (! + x n)]
           [line <- (! List 'Hor x hi-x y)]
           [new-pt <- (! List hi-x y)]
           (! List line new-pt)])
   dir))

;; U (CoList Direction) -> CoList Lines
(def-thunk (! lines<-directions dirs)
  [next-line
   = (~ (copat
         [(dir k start)
          [line*new-pt <- (! apply-vector start dir)]
          (! apply (~ (copat
                       [(line new-pt)
                        (! cl-cons line (~ (! k new-pt)))]))
             line*new-pt)]))]
  (! cl-foldr dirs next-line
     (~ (λ (xy) (! cl-nil)))
     '(0 0)))

(def-thunk (! inside? x lo hi)
  (! and (~ (! <= lo x))
         (~ (! <= x hi))))

(def-thunk (! intersect-orthogonal x lo-y hi-y y lo-x hi-x)
  (cond [(! and (~ (! inside? x lo-x hi-x))
                (~ (! inside? y lo-y hi-y)))
         (! <<v List 'o List x y '$)]
        [else
         (ret '())]))

(def-thunk (! intersection l1 l2)
  [or1 <- (! first l1)]
  [or2 <- (! first l2)]
  ;; (! displayall 'intersecting l1 l2)
  ((copat [((= 'Vert) (= 'Vert))
           ;; TODO: this is wrong but might work
           (ret '())]
          [((= 'Hor) (= 'Hor)) (ret '()) ] ;; same here
          [((= 'Vert) (= 'Hor))
           [x <- (! second l1)]
           [lo-y <- (! third l1)]
           [hi-y <- (! fourth l1)]
           [lo-x <- (! second l2)]
           [hi-x <- (! third l2)]
           [y <- (! fourth l2)]
           (! intersect-orthogonal x lo-y hi-y y lo-x hi-x)]
          [((= 'Hor) (= 'Vert))
           (! intersection l2 l1)])
   or1 or2))

(def-thunk (! manhattan-to-origin x y)
  (! idiom (~ (ret +)) (~ (! abs x)) (~ (! abs y))))

(def-thunk (! main-a)
  [wires <- (! parse-input)]
  (! displayall 'parsed)
  [w1 <- (! first wires)]
  [w2 <- (! second wires)]
  [lines1 = (~ (! lines<-directions (~ (! colist<-list w1))))]
  [lines2 = (~ (! lines<-directions (~ (! colist<-list w2))))]
  [line-pairs = (~ (! cartesian-product lines1 lines2))]
  [not-both-zero =
                 (~ (copat [(x y)
                            (! <<v not 'o and (~ (! zero? x)) (~ (! zero? y)) '$)]))]
  ;; #;(! list<-colist line-pairs)
  ;; (! intersect-orthogonal 3 8 5 6 3 7)
  (! <<n
   minimum-by (~ (! apply manhattan-to-origin)) '(+inf.0 +inf.0) 'o
   cl-filter (~ (! apply not-both-zero)) 'o
   cl-foldr line-pairs
   (~ (λ (line*line tl)
        (do [pts <- (! apply intersection line*line)]
            (! cl-append (~ (! colist<-list pts)) tl))))
   cl-nil
   '$
     ))

(def-thunk (! main-b)
  (ret 'not-done-yet))
