#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require "../../Parse.rkt")
(require fiddle/stdlib/Table)

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

(def-thunk (! mk-pt x y) (ret (list 'pt x y)))
(def-thunk (! pt-x pt) (! second pt))
(def-thunk (! pt-y pt) (! third pt))
(def-thunk (! manhattan p1 p2)
  [x1 <- (! pt-x p1)] [x2 <- (! pt-x p2)]
  [y1 <- (! pt-y p1)] [y2 <- (! pt-y p2)]
  [dx <- (! <<v abs 'o - x1 x2 )]
  [dy <- (! <<v abs 'o - y1 y2 )]
  (! + dx dy))

(def/copat (! parse-pt-stk)
  [((upto xs #\,) #\space (rest ys))
   [x <- (! parse-num xs)] [y <- (! parse-num ys)]
   (! mk-pt x y)])
;; String -> F Point
(def/copat (! parse-line)
  [(s #:bind) (! <<v apply parse-pt-stk 'o string->list s)])

;; FU CoList (List ID Pt)
(def-thunk (! parse-input)
  [ls <- (! slurp-lines!)]
  [parsed = (~ (! <<n cl-map parse-line 'o colist<-list ls))]
  [id*pts <- (! <<n list<-colist 'o
                cl-zipwith (~ (! range 0 1000000000)) parsed
                           )]
  [id*pts-c = (~ (! colist<-list id*pts))]
  (ret id*pts-c))

;; INPUT-dependent
;; (define SMALL-X 0)
;; (define LARGE-X 100) ; upper bounds are exclusive
;; (define SMALL-Y 0)
;; (define LARGE-Y 100) ; upper bounds are exclusive
(define SMALL-X 40)
(define LARGE-X 354) ; upper bounds are exclusive
(define SMALL-Y 41)
(define LARGE-Y 355) ; upper bounds are exclusive
(def-thunk (! tie?) (! equal? 'tie))

(def-thunk (! all-points)
  (! <<n cl-map (~ (! apply mk-pt)) 'o
     cartesian-product
     (~ (! range SMALL-X LARGE-X))
     (~ (! range SMALL-Y LARGE-Y))))

;; -> ('all-cells U 'perimiter-cells) -> (Tie U (List ID Nat))
(def-thunk (! mk-region sites)
  [w <- (! - LARGE-X SMALL-X)] [h <- (! - LARGE-Y SMALL-Y)]
  [sz <- (! * w h)] [v <- (! make-vector sz #f)]
  [ix<-pt = (~ (copat [(pt) [x <- (! <<v swap - SMALL-X 'o pt-x pt)]
                            [y <- (! <<v swap - SMALL-Y 'o pt-y pt)]
                            (! <<v + x 'o * w y)]))]
  [find-closest
   = (~ (copat [(pt)
                [distance
                 = (~ (λ (site)
                        (do [id <- (! first site)] [spt <- (! second site)]
                          (! <<v List id 'o manhattan pt spt))))]
                [closer
                 = (~ (copat
                       [(best next)
                        [db <- (! second best)] [dn <- (! second next)]
                        (cond
                          [(! equal? db dn) (ret (list 'tie db))]
                          [(! < db dn) (ret best)]
                          [else (ret next)])]))]
                [ans <-
                     (! <<n cl-foldl1 closer 'o
                        cl-map distance 'o
                        sites)]
                [ix <- (! <<v ix<-pt pt)]
                (! vector-set! v ix ans)
                (ret ans)]))]
  [pts = (~ (! <<n cl-map (~ (! apply mk-pt)) 'o
               cartesian-product
               (~ (! range SMALL-X LARGE-X))
               (~ (! range SMALL-Y LARGE-Y))))]
  [perim-pts
   =
   (~ (! <<n
         cl-map (~ (! apply mk-pt)) 'o
         cl-append*
         (~ (! cartesian-product (~ (! cl-single SMALL-X)) (~ (! range SMALL-Y LARGE-Y))))
         (~ (! cartesian-product (~ (! range SMALL-X LARGE-X)) (~ (! cl-single SMALL-Y))))
         (~ (! cartesian-product (~ (! <<v cl-single 'o - LARGE-X 1)) (~ (! range SMALL-Y LARGE-Y))))
         (~ (! cartesian-product (~ (! range SMALL-X LARGE-X)) (~ (! <<v cl-single 'o - LARGE-Y 1))))
         ))]
  (! cl-foreach find-closest pts)
  (ret
   (~
    (copat
     [((= 'all-cells)) (! cl-map (~ (! <<v vector-ref v 'o ix<-pt)) pts)]
     [((= 'perimiter-cells)) (! cl-map (~ (! <<v vector-ref v 'o ix<-pt)) perim-pts)]))))

;; associated-area : Region -> U CoList (List ID Point) -> F (Table ID Nat)

(def-thunk (! associated-area r #:bind)
  [extract-id
   = (~ (λ (cell) (cond [(! tie? cell) (ret #f)] [else (! first cell)])))]
  [inc-area
   = (~ (λ (id->size id) (! update id->size id 1 (~ (! + 1)))))]
  (! <<n cl-foldl^ inc-area empty-table 'o
     cl-filter number? 'o
     cl-map extract-id 'o
     r 'all-cells))

(def-thunk (! remove-infinities r id->size)
  [extract-ids
   = (~ (λ (cell) (cond [(! tie? cell) (! cl-nil); (! <<v colist<-list 'o cdr cell)
                                       ]
                        [else (! <<v cl-single 'o first cell)])))]
  [remove = (~ (λ (id->size id) (! id->size 'remove id)))]
  (! <<n
     cl-foldl^ remove id->size 'o
     cl-bind^ extract-ids 'o 
     r 'perimiter-cells))

;; I just inspected the output for the smallest value
(def-thunk (! main-a)
  [sites <- (! parse-input)]
  [region <- (! mk-region sites)]
  ;; Next: make a table mapping ids to the number of squares that are nearest them
  [id->size <- (! associated-area region)]
  ;; Then: remove the table entries for points associated to infinite area (check by walking the perimeter)
  [id->size <- (! remove-infinities region id->size)]
  (! id->size 'to-list))

(def-thunk (! num<-bool b) (if b (ret 1) (ret 0)))

;; U(CoList (List ID Point)) -> Point -> F Bool
(def-thunk (! close-enough? sites pt)
  [total-dist
   <- (! <<n cl-foldl^ + 0 'o
         cl-map (~ (! manhattan pt)) 'o
         cl-map second sites)]
  (! < total-dist 10000)) ; INPUT-dependent
(def-thunk (! main-b)
  [sites <- (! parse-input)]
  (! <<n
   cl-foldl^ + 0 'o
   cl-map num<-bool 'o
   cl-map (~ (! close-enough? sites)) all-points))
