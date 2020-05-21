#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/Table)
(require fiddle/stdlib/CoList)
(require fiddle/stdlib/IO)

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
;; U (CoList (List Nat Nat)) -> F (List Nat Nat)
;; both cbn and cbv
(define-thunk (! add-23s)
  (copat
   [(l)
    (! cl-foldl l (thunk (! zipwith +)) (list 0 0))]
   ))

(define-thunk (! calc-23s)
  (copat
   [(s) (! <<v pick-23s 'o count-occs empty-table 'o string->list s '$)]))
(define-thunk (! main2-1)
  (do [ls <- (! slurp-lines!)]
      [cts <- (! <<n add-23s 'o cl-map calc-23s 'o colist<-list ls '$)]
    (! apply * cts)))
;(! main2-1)

;; Need to find 2 words in the list that have exactly one letter not in common:
;; State: list of words seen so far
;;   for each word, check to see if it matches any of the previous ones
;;   otherwise push it onto the stack and continue
;; To see if it matches: both should be represented as lists of characters
;;   do a zipwith equal? where #t -> 0 and #f -> 1 and then do an apply +

(define-thunk (! nat-equal? x y)
  (ifc (! equal? x y)
       (ret 0)
       (ret 1)))

;; Listof A -> Listof A -> Nat
(define-thunk (! compare-words xs ys)
  (! <<v apply + 'o zipwith nat-equal? xs ys '$))

;; find-match : (List Char) -> List (List Char) -> #f or (List (List Char) (List Char))
(define-rec-thunk (! find-match cand seen)
  (cond
    [(! empty? seen) (ret #f)]
    [#:else
     (do [hd <- (! car seen)]
         [seen <- (! cdr seen)]
       (cond
         [(! <<v equal? 1 'o compare-words cand hd '$)
          (ret (list cand hd))]
         [#:else
          (! find-match cand seen)]))]))

;; search : CoList (List Char) -> List (List Char) -> (List (List Char) (List Char))
(define-rec-thunk (! search seen clines)
  (do [v <- (! clines)]
      [hd <- (! clv-hd v)]
    [clines <- (! clv-tl v)]
    [match? <- (! find-match hd seen)]
    (cond
      [(ret match?) (ret match?)]
      [#:else (! search (cons hd seen) clines)])))

(define-thunk (! main2-2)
  (do [ls <- (! slurp-lines!)]
      [matches <-
       (! <<n search '() 'o cl-map string->list 'o colist<-list ls '$)]
    (! <<v
       list->string 'o
       map first 'o
       filter (thunk (! apply equal?)) 'o
       apply (thunk (! zipwith List)) matches '$)))

;; (! main2-2)

