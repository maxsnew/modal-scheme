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

(def-thunk (! apply-vector start vector distance)
  [x <- (! first start)]
  [y <- (! second start)]
  [dir <- (! first vector)]
  [n <- (! second vector)]
  [new-distance <- (! + n distance)]
  ((copat [((= #\U))
           [hi-y <- (! + y n)]
           [line <- (! List 'Vert x y hi-y y distance)]
           [new-pt <- (! List x hi-y)]
           (! List line new-pt new-distance)]
          [((= #\D))
           [lo-y <- (! - y n)]
           [line <- (! List 'Vert x lo-y y y distance)]
           [new-pt <- (! List x lo-y)]
           (! List line new-pt new-distance)]
          [((= #\L))
           [lo-x <- (! - x n)]
           [line <- (! List 'Hor lo-x x y x distance)]
           [new-pt <- (! List lo-x y)]
           (! List line new-pt new-distance)]
          [((= #\R))
           [hi-x <- (! + x n)]
           [line <- (! List 'Hor x hi-x y x distance)]
           [new-pt <- (! List hi-x y)]
           (! List line new-pt new-distance)])
   dir))

;; U (CoList Direction) -> CoList Lines
(def-thunk (! lines<-directions dirs)
  [next-line
   = (~ (copat
         [(dir k start distance-traveled)
          [line*new-pt*new-distance <- (! apply-vector start dir distance-traveled)]
          (! apply (~ (copat
                       [(line new-pt new-distance)
                        (! cl-cons line (~ (! k new-pt new-distance)))]))
             line*new-pt*new-distance)]))]
  (! cl-foldr dirs next-line
     (~ (λ (xy distance) (! cl-nil)))
     '(0 0) 0))

(def-thunk (! inside? x lo hi)
  (! and (~ (! <= lo x))
         (~ (! <= x hi))))

(def-thunk (! distance*intersection-pairs l1 l2)
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
           (cond [(! and (~ (! inside? x lo-x hi-x))
                         (~ (! inside? y lo-y hi-y)))
                  [start-x <- (! fifth l2)]
                  [start-y <- (! fifth l1)]
                  [hor-dist <- (! sixth l2)]
                  [vert-dist <- (! sixth l1)]
                  [tot-dist
                   <- (! idiom (~ (ret +))
                         (~ (! + hor-dist vert-dist))
                         (~ (! <<v abs 'o - start-x x))
                         (~ (! <<v abs 'o - start-y y)))]
                  (! idiom (~ (ret List))
                     (~ (! idiom (~ (ret List)) (~ (ret tot-dist)) (~ (! List x y)))))]
                 [else
                  (ret '())])]
          [((= 'Hor) (= 'Vert))
           (! distance*intersection-pairs l2 l1)])
   or1 or2))

(def-thunk (! intersection l1 l2)
  (! <<v map second 'o distance*intersection-pairs l1 l2 '$))

(def-thunk (! manhattan-to-origin x y)
  (! idiom (~ (ret +)) (~ (! abs x)) (~ (! abs y))))

(def-thunk (! all-line-pairs)
  [wires <- (! parse-input)]
  (! displayall 'parsed)
  [w1 <- (! first wires)]
  [w2 <- (! second wires)]
  [lines1 = (~ (! lines<-directions (~ (! colist<-list w1))))]
  [lines2 = (~ (! lines<-directions (~ (! colist<-list w2))))]
  [line-pairs = (~ (! cartesian-product lines1 lines2))]
  (ret line-pairs)
  )

(def-thunk (! main-a)
  [line-pairs <- (! all-line-pairs)]
  ;; [_ <- (! list<-colist line-pairs)]
  ;; (! displayall 'not-the-line-pairs-fualt)
  [not-both-zero =
                 (~ (copat [(x y)
                            (! <<v not 'o and (~ (! zero? x)) (~ (! zero? y)) '$)]))]
  (! <<n
   minimum-by (~ (! apply manhattan-to-origin)) '(+inf.0 +inf.0) 'o
   cl-filter (~ (! apply not-both-zero)) 'o
   cl-foldr line-pairs
   (~ (λ (line*line tl)
        (do [pts <- (! apply intersection line*line)]
            (! cl-append (~ (! colist<-list pts)) tl))))
   cl-nil
   '$))

(def-thunk (! main-b)
  [line-pairs <- (! all-line-pairs)]
  ;; [_ <- (! list<-colist line-pairs)]
  ;; (! displayall 'not-the-line-pairs-fualt)
  [non-zero-dist = (~ (copat [(x y) (! <<v not 'o zero? x '$)]))]
  (! <<n
   minimum-by first '(+inf.0 something-is-wrong) 'o
   cl-filter (~ (! apply non-zero-dist)) 'o
   cl-foldr line-pairs
   (~ (λ (line*line tl)
        (do [pts <- (! apply distance*intersection-pairs line*line)]
            (! cl-append (~ (! colist<-list pts)) tl))))
   cl-nil
   '$))
