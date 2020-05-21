#lang fiddle

(require fiddle/prelude)
(require fiddle/prelude)
(require fiddle/stdlib/CoList)
(require fiddle/stdlib/IO)
(require "../../Parse.rkt")

;; data Rectangle where
;;   '(rect id ,Num ,Coordinates ,Size)
(define-thunk (! r-coordinates)
  (copat [(r) (! <<v car 'o cdr 'o cdr 'o cdr r '$)]))
(define-thunk (! r-size)
  (copat [(r) (! <<v car 'o cdr 'o cdr 'o cdr 'o cdr r '$)]))

;; data Coordinates where
;;   '(coord x ,Nat y ,Nat)
(define-thunk (! c-x)
  (copat [(c) (! <<v car 'o cdr 'o cdr c '$)]))
(define-thunk (! c-y)
  (copat [(c) (! <<v car 'o cdr 'o cdr 'o cdr 'o cdr c '$)]))

;; data Size where
;;   '(size width ,Nat height ,Nat)
(define-thunk (! s-width)
  (copat [(s) (! <<v car 'o cdr 'o cdr s '$)]))
(define-thunk (! s-height)
  (copat [(s) (! <<v car 'o cdr 'o cdr 'o cdr 'o cdr s '$)]))

(define-thunk (! mk-coord)
  (copat [(x y) (ret (list 'coord 'x x 'y y))]))

(define-thunk (! mk-rect id x y w h)
  (do [c <- (! mk-coord x y)]
      (ret
       (list 'rect 'id id
             c
             (list 'size 'width w 'height h)))))

(define r-ex
  (list 'rect 'id 0
         (list 'coord 'x 3 'y 4)
         (list 'size 'width 8 'height 8)))
(define pt-ex
  (list 'coord 'x 3 'y 4))

;; parse-rect : String -> Rectangle
;; assume it always succeeds
;; Format: #(0-9)+ @ (0-9)+,(0-9)+: (0-9)+x(0-9)+
(define-rec-thunk (! parse-loop)
  (copat
   [(_hash
     (upto idcs #\space)
     _at _spc
     (upto xcs #\,)
     (upto ycs #\:)
     (= #\space)
     (upto wcs #\x))
    (! grab-stack
       (thunk
        (copat
         [(hcs)
          (! <<v apply mk-rect 'o map parse-num (list idcs xcs ycs wcs hcs) '$)])))]))

(define-thunk (! parse-rect)
  (copat
   [(s)
    (! <<v apply parse-loop 'o string->list s '$)]))
(define WIDTH  1000)
(define HEIGHT 1000)
(define-thunk (! mk-region)
  (! <<v make-vector 'o * WIDTH HEIGHT '$))
(define-thunk (! region-ref)
  (copat
   [(r pt)
    (do [x <- (! c-x pt)] [y <- (! c-y pt)]
      [i <- (! <<v + x 'o * WIDTH y '$)]
      (! vector-ref r i))]))

(define-thunk (! inc-pt)
  (copat
   [(r pt)
    (do [x <- (! c-x pt)] [y <- (! c-y pt)]
      [i <- (! <<v + x 'o * WIDTH y '$)]
      [v <- (! vector-ref r i)]
      (! <<v vector-set! r i 'o + 1 v '$))]))

(define-thunk (! coords<-rect)
  (copat
   [(r)
    (do [x <- (! <<v c-x 'o r-coordinates r '$)]
        [y <- (! <<v c-y 'o r-coordinates r '$)]
      [w <- (! <<v s-width 'o r-size r '$)]
      [h <- (! <<v s-height 'o r-size r '$)]
      [x+ <- (! + x w)]
      [y+ <- (! + y h)]
      (! <<n
         cl-map (thunk (! apply mk-coord)) 'o
         cartesian-product (thunk (! range x x+)) (thunk (! range y y+)) '$))]))

(define-thunk (! colist<-region)
  (copat
   [(r)
    (do [len <- (! * WIDTH HEIGHT)]
        (! <<n
           cl-map (thunk (! vector-ref r))'o 
           range 0 len '$))]))

(define-thunk (! fill-region)
  (copat
   [(reg rect)
    (! <<n
       cl-foreach (thunk (! inc-pt reg)) 'o
       coords<-rect rect '$)]))

(define-thunk (! main3-1)
  (do [ls <- (! slurp-lines!)]
      [arr <- (! mk-region)]
      [_ <-
         (! <<n
            cl-foreach (thunk (! fill-region arr)) 'o
           cl-map parse-rect 'o
           colist<-list ls '$)]
      (! <<n cl-length 'o
         cl-filter (thunk (λ (x) (! >= x 2))) 'o
         colist<-region arr '$)))

(define-thunk (! disjoint)
  (copat
   [(reg rect)
    (! <<n
       (thunk (λ (c) (! cl-foldr c and (thunk (ret #t))))) 'o
     cl-map (thunk (λ (pt) (ret (thunk  (! <<v equal? 1 'o region-ref reg pt '$))))) 'o
     coords<-rect rect '$)]))

(define-thunk (! main3-2)
  (do [ls <- (! slurp-lines!)]
      [arr <- (! mk-region)]
    [rects <- (ret (thunk (! <<n cl-map parse-rect 'o colist<-list ls '$)))]
    [_ <- (! cl-foreach (thunk (! fill-region arr)) rects)]
    (! <<v
       clv-hd 'o 
       cl-filter (thunk (λ (rect) (! disjoint arr rect))) rects '$)))
;; (! main3-2)
