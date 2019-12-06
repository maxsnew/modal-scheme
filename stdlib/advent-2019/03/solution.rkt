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

(def-thunk (! main-a)
  [wires <- (! parse-input)]
  [w1 <- (! first wires)]
  [w2 <- (! second wires)]
  (! list<-colist (~ (! lines<-directions (~ (! colist<-list w1))))))

(def-thunk (! main-b)
  (ret 'not-done-yet))
