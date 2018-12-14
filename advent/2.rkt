#lang sbpv

(require "../stdlib.rkt")
(require "table.rkt")
(require "CoList.rkt")

(define-rec-thunk (! slurp-lines)
  (copat
   [(#:bind) (! slurp-lines '())]
   [(so-far)
    (do [l <- (! read-line)]
        (cond
          [(! .v not string? l) (ret so-far)]
          [#:else (! slurp-lines (cons l so-far))]))]))

(define-thunk (! add1 x) (! + 1 x))
(define-rec-thunk (! count-occs t s)
  (cond
    [(! empty? s) (ret t)]
    [#:else
     (do [hd <- (! car s)]
         [tl <- (! cdr s)]
       [t <- (! update t hd 1 add1)]
       (! count-occs t tl))]))
(define t (thunk
           (do [h <- (! hash 'x 1 'y 2)]
               (! table<-hash h))))

;; pick-23s: turn a table of occurrences into a list of # of chars that occur exactly twice and thrice
;;   Table Char Nat -> F (List Nat Nat)
(define-thunk (! mark-2 two*three)
  (do [threes <- (! second two*three)]
    (ret (list 1 threes))))
(define-thunk (! mark-3 two*three)
  (do [twos <- (! first two*three)]
    (ret (list twos 1))))

(define-rec-thunk (! pick-23s occs)
  (do [l <- (! occs 'to-list)]
      (! foldl l
         (thunk (λ (counts char*occ-count)
                  (do [occ-count <- (! cdr char*occ-count)]
                      (cond
                        [(! equal? occ-count 2) (! mark-2 counts)]
                        [(! equal? occ-count 3) (! mark-3 counts)]
                        [#:else (ret counts)]))))
         (list 0 0))))

;; TODO: make tail recursive
(define-rec-thunk (! zipwith f xs ys)
  (cond
    [(! or (thunk (! empty? xs))
           (thunk (! empty? ys)))
     (ret '())]
    [#:else
     (do [x <- (! car xs)]
         [xs <- (! cdr xs)]
       [y <- (! car ys)]
       [ys <- (! cdr ys)]
       [z <- (! f x y)]
       [zs <- (! zipwith f xs ys)]
       (ret (cons z zs)))]))

;; add-23s: sum up the co-list of 2-3 pairs
(define-thunk (! add-23s l)
  (! cl-foldl l (thunk (! zipwith +)) (list 0 0)))

(define-thunk (! main2-1)
  (do [ls <- (! slurp-lines)]
      [clines <- (ret (thunk (! colist<-list ls)))]
    [cmapped <- (ret (thunk
                      (! cl-map
                         (thunk (λ (x)
                                  (! <<v
                                     pick-23s 'o
                                     count-occs empty-table 'o
                                     string->list x '$
                                     )))
                         clines)))]
    [cts <- (! add-23s cmapped)]
    (! apply * cts )
    #;
    (! <<n
       cl-map Ret
       #;
       (thunk (λ (x)
                (! <<v
                   ;; pick-23s 'o
                   ;; count-occs empty-table 'o
                   string->list x '$
                   ))) 'o
       colist<-list ls '$)
    #;
    (! <<n add-23s 'o
       cl-map (thunk (! <<v pick-23s 'o count-occs 'o string->list '$)) ls '$)
    #;
    [sum-23s
     <-
     (! <<n add-23s 'o
        cl-map (thunk (! <<v pick-23s 'o count-occs 'o string->list '$)) ls '$)]
    #;(! apply * sum-23s)
    ))
(! main2-1)

;
