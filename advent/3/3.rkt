#lang sbpv

(require "../../stdlib.rkt")
(require "../Parse.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")

;; data Rectangle where
;;   '(rect id ,Num ,Coordinates ,Size)

;; data Coordinates where
;;   '(coord x ,Nat y ,Nat)

;; data Size where
;;   '(size width ,Nat height ,Nat)

(define-thunk (! mk-rect id x y w h)
  (ret
   (list 'rect 'id id
         (list 'coord 'x x 'y y)
         (list 'size 'width w 'height h))))

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

(define-thunk (! main3-1)
  (do [ls <- (! slurp-lines)]
      (! <<n list<-colist 'o cl-map parse-rect 'o colist<-list ls '$)))
(! main3-1)
