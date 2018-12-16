#lang sbpv

(require "../../stdlib.rkt")
(require "../Parse.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")

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

(define-thunk (! mk-rect id x y w h)
  (ret
   (list 'rect 'id id
         (list 'coord 'x x 'y y)
         (list 'size 'width w 'height h))))

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

;; A Shape is
;; We assume Shapes all lie in 1000x1000
;; codata Shape where
;;   'contains? : Coordinate -> F Bool
(define-thunk (! shape<-rect)
  (copat
   [(r (= 'contains?) pt)
    (do [rx <- (! <<v c-x 'o r-coordinates r '$)]
        [ry <- (! <<v c-y 'o r-coordinates r '$)]
      [rw <- (! <<v s-width 'o r-size r '$)]
      [rh <- (! <<v s-height 'o r-size r '$)]
      [ptx <- (! c-x pt)]
      [pty <- (! c-y pt)]
      (! and
         (thunk (! <= rx ptx))
         (thunk (! <<v <= ptx 'o + rx rw '$))
         (thunk (! <= ry pty))
         (thunk (! <<v <= pty 'o + ry rh '$))))]))

(define-thunk (! sh-union)
  (copat
   [(s1 s2 (= 'contains?) pt)
    (! or
       (thunk (! s1 'contains? pt))
       (thunk (! s2 'contains? pt)))]))
(define-thunk (! sh-intersect)
  (copat
   [(s1 s2 (= 'contains?) pt)
    (! and
       (thunk (! s1 'contains? pt))
       (thunk (! s2 'contains? pt)))]))
(define-thunk (! sh-empty)
  (copat [((= 'contains?) pt) (ret #f)]))

;; Algorithm: State is two shapes:
;;   used: one covering everywhere that's been used
;;   used-2: and one covering everywhere that's been used twice
;; repeatedly get a new rectangle r:
;;   update: now (r \/ used) is used
;;   update: now ((r /\ used) \/ used-2) is used at least twice
;; need to support:
;;   union of two shapes (every round)
;;   union of a shape and a rectangle 
;;   intersection of a shape with a rectangle
;;   how many points are in an area (once at the end)
(define-thunk (! comb-pair)
  (copat
   [(shs sh)
    (do [all-touched <- (! first shs)]
        [touched-2ce <- (! second shs)]
      (ret (list (thunk (! sh-union sh all-touched))
                 (thunk (! sh-union
                           touched-2ce
                           (thunk (! sh-intersect sh all-touched)))))))]))

(define-thunk (! crush)
  (copat [(l) (! cl-foldl l comb-pair (list sh-empty sh-empty))]))

(define-thunk (! main3-1)
  (do [ls <- (! slurp-lines)]
      [touched*twice <-
       (! <<n crush 'o
           cl-map (thunk (copat [(r) (ret (thunk (! shape<-rect r)))])) 'o
           cl-map parse-rect 'o
           colist<-list ls '$)]
    [twice-touched <- (! second touched*twice)]
    
    (! twice-touched 'contains? (list 'coord 'x 5 'y 5))
    ))
(! main3-1)
