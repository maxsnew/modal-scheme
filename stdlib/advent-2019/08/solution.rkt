#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Stream.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)

(define WIDTH 25)
(define HEIGHT 6)

(def-thunk (! slurp-input)
  [chars <- (! <<v string->list 'o first 'o slurp-lines! '$)]
  (ret (~ (! <<n cl-map digit<-char 'o colist<-list chars '$))))

(def-thunk (! summarize xs)
  [step
   = (~ (λ (summary)
          (do [zeros <- (! first summary)]
              [ones <- (! second summary)]
            [twos <- (! third summary)]
            (copat [((= 0)) [zeros <- (! + 1 zeros)]
                            (! List zeros ones twos)]
                   [((= 1)) [ones <- (! + 1 ones)]
                            (! List zeros ones twos)]
                   [((= 2)) [twos <- (! + 1 twos)]
                            (! List zeros ones twos)]))))]
  (! cl-foldl xs step '(0 0 0)))
(def-thunk (! summarize! xs) (! summarize (~ (! colist<-list xs))))

(def-thunk (! main-a)
  [nums <- (! slurp-input)]
  [layer-size <- (! * WIDTH HEIGHT)]
  [zs*ones*twos <- (! <<n minimum-by first '(+inf.0 0 0) 'o cl-map summarize! 'o chunks layer-size nums '$)]
  [ones <- (! second zs*ones*twos)]
  [twos <- (! third zs*ones*twos)]
  (! * ones twos))

(def/copat (! atop-pixel)
  [((= 2) inner) (ret inner)]
  [(outer inner) (ret outer)])

(def-thunk (! apply-layer l1 l2)
  (! <<n list<-colist
     'o cl-map (~ (! apply atop-pixel))
     'o cl-zipwith (~ (! colist<-list l1)) (~ (! colist<-list l2))))

;; print-row : Listof Number -> F 1
(def-thunk (! print-row ns)
  [pixel->string
   = (~ (copat
         [((= 0)) (ret " ")]
         [((= 1)) (ret "#")]))]
  (! <<v displayall 'o apply string-append 'o map pixel->string ns))

(def-thunk (! main-b)
  [nums <- (! slurp-input)]
  [layer-size <- (! * WIDTH HEIGHT)]
  [final-layer <- (! <<n cl-foldl1 apply-layer 'o chunks layer-size nums '$)]
  (! <<n cl-foreach print-row 'o chunks WIDTH (~ (! colist<-list final-layer)))
  )
