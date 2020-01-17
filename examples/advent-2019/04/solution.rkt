#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)

(define BEGIN 387638)
(define END 919124)
(def-thunk (! CANDIDATES)
  (! range BEGIN END))

;; Number -> F (Listof Number)
(def-thunk (! number->digits n)
  (! <<v
     map digit<-char 'o
     string->list 'o
     number->string n '$))

(def-thunk (! check-increasing first-d remaining)
  (cond [(! empty? remaining) (ret #t)]
        [else
         [second-d <- (! first remaining)]
         [remaining <- (! rest remaining)]
         (cond [(! <= first-d second-d) (! check-increasing second-d remaining)]
               [else (ret #f)])]))

;; check-criteria : Number -> Listof Number -> F Bool
(def-thunk (! check-criteria-a first-d remaining)
  (cond [(! empty? remaining) (ret #f)]
        [else
         [second-d <- (! first remaining)]
         [remaining <- (! rest remaining)]
         (cond [(! = first-d second-d) (! check-increasing second-d remaining)]
               [(! < first-d second-d) (! check-criteria-a second-d remaining)]
               [else (ret #f)])]))

;; good-candidate? : Number -> F Bool
(def-thunk (! good-candidate-a? n)
  [digits <- (! number->digits n)]
  [d1 <- (! first digits)]
  [ds <- (! rest digits)]
  (! check-criteria-a d1 ds))

(def-thunk (! main-a)
  (! <<n cl-length 'o
     cl-filter good-candidate-a? 'o
     CANDIDATES '$))

(def-thunk (! check-criteria-b first-d remaining)
  (! displayall)
  (cond [(! empty? remaining)
         (copat [((= 'two-consecutive)) (ret #t)]
                [(arg) (ret #f)])]
        [else
         [second-d <- (! first remaining)]
         [remaining <- (! rest remaining)]
         (cond [(! = first-d second-d)
                (copat [((= 'one-consecutive))
                        (! check-criteria-b second-d remaining 'two-consecutive)]
                       [(arg) (! check-criteria-b second-d remaining 'three-or-more)])]
               [(! < first-d second-d)
                (copat [((= 'two-consecutive))
                        (! check-increasing second-d remaining)]
                       [(arg)
                        (! check-criteria-b second-d remaining 'one-consecutive)])]
               [else (! abort #f)])]))

(def-thunk (! good-candidate-b? n)
  [digits <- (! number->digits n)]
  [d1 <- (! first digits)]
  [ds <- (! rest digits)]
  (! check-criteria-b d1 ds 'one-consecutive))

(def-thunk (! main-b)
  (! <<n cl-length 'o
     cl-filter good-candidate-b? 'o
     CANDIDATES '$))
