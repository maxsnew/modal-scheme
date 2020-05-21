#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/CoList)
(require "../../Stream.rkt")
(require "../../Finger.rkt")

(provide main-a)

;; A Circle A is an ADT supporting the following operations
;; codata Circle A where
;;   'current-value |- F A
;;   'remove-current-value |- FU Circle A
;;   'add |- A -> FU Circle A
;;   'move          |- Int -> FU Circle A
;; such that move and current-value are consistent
;; and the current-value after removing should be the one to the right of it

;; FlexVec A -> Nat -> Nat -> Circle A
(def-thunk (! zippy-circle flex size cursor)
  ;(! <<v displayall 'zippy-circle 'size size 'cursor cursor 'flex 'o flex 'debug)
  (copat
   [((= 'current-value)) ;(! displayln 'current-value)
    (! flex 'get cursor)]
   [((= 'remove-current-value))
    ;(! displayln 'remove-current-value)
    [flex <- (! <<v second 'o flex 'remove cursor)]
    [size <- (! - size 1)]
    [cursor <- (! modulo cursor size)]
    (ret (~ (! zippy-circle flex size cursor)))]
   [((= 'add) x)
    (cond
      [(! >= cursor size) (! error "too big")]
      [else
       ;(! displayln 'add)
       [flex <- (! flex 'insert cursor x)]
       [size <- (! + size 1)]
       (ret (~ (! zippy-circle flex size cursor)))])]
   [((= 'move) n) ;(! displayln 'move)
    [cursor <- (! <<v swap modulo size 'o + cursor n)]
    (ret (~ (! zippy-circle flex size cursor)))]))

(def-thunk (! single-circle x)
  [fv <- (! mt-flexvec 'cons x)]
  (! zippy-circle fv 1 0))

;; (define NUM-PLAYERS 10)
;; (define NUM-MARBLES 1618) ;; answer: 8317
;; (define NUM-PLAYERS 30)
;; (define NUM-MARBLES 5807) ;; 37305
(define NUM-PLAYERS 470)
;; (define NUM-MARBLES 72170) ;; 388024
(define NUM-MARBLES 7217000) ;; part b

(def-thunk (! next-player cur-player)
  (! <<v swap modulo NUM-PLAYERS 'o + 1 cur-player))
(def-thunk (! operate circle scores cur-player turns)
  [turns-v <- (! turns)]
  (cond [(! clv-nil? turns-v) (! cl-nil)]
        [else
         [marble <- (! clv-hd turns-v)] [turns <- (! clv-tl turns-v)]
         (cond [(! <<v equal? 0 'o modulo marble 23)
                ;(! displayln marble)
                [circle <- (! circle 'move -7)]
                [marble2 <- (! circle 'current-value)]
                [circle <- (! circle 'remove-current-value)]
                
                [scores <- (! scores 'update cur-player (~ (! + marble marble2)))]
                [cur-player <- (! next-player cur-player)]
                (! cl-cons scores
                           (~ (! operate circle scores cur-player turns)))]
               [else
                [circle <- (! circle 'move 2)]
                [circle <- (! circle 'add marble)]
                [cur-player <- (! next-player cur-player)]
                (! operate circle scores cur-player turns)])]))

(def-thunk (! display-fv fv)
  [l <- (! <<n list<-colist 'o fv 'to-colist)]
  (! displayln l))

(def-thunk (! move-right l r n)
  (cond [(! empty? r)
         [r <- (! reverse l)]
         (! move-right '() r n)]
        [else
         [x <- (! car r)] [r <- (! cdr r)]
         (cond [(! zero? n) (! List l x r)]
               [else [n <- (! - n 1)]
                     (! move-right (cons x l) r n)])]))

;; Listof A -> A -> Listof A -> Circle
(def-thunk (! simple-circle l x r)
  (copat
   [((= 'debug)) (! List l x r)]
   [((= 'current-value)) (ret x)]
   [((= 'remove-current-value))
    [st <- (! move-right l r 0)]
    (ret (~ (! apply simple-circle st)))]
   [((= 'add) y) [r = (cons x r)]
                 (ret (~ (! simple-circle l y r)))]
   [((= 'move) n)
    [st <-
        (cond [(! < n 0)
               [n <- (! - n)]
               (! <<v reverse 'o move-right r (cons x l) n)]
              [else
               (! move-right l (cons x r) n)])]
    (ret (~ (! apply simple-circle st)))]))

(def-thunk (! mutable-flexvec v)
  (copat
   [((= 'update) ix up #:bind)
    (! <<v vector-set! v ix 'o up 'o vector-ref v ix)
    (ret (~ (! mutable-flexvec v)))]
   [((= 'to-colist) #:bind)
    [len <- (! vector-length v)]
    (! <<n cl-map (~ (! vector-ref v)) 'o range 0 len)]))

(def-thunk (! mutable-flexvec<-list l #:bind)
  [v <- (! list->vector l)]
  (ret (~ (! mutable-flexvec v))))

(def-thunk (! main-a)
  ;; [circle = (~ (! single-circle 0))]
  [circle = (~ (! simple-circle '() 0 '()))]
  [zeros <- (! <<n list<-colist 'o take NUM-PLAYERS 'o stream-const 0)]
  [init-scores <- (! mutable-flexvec<-list zeros)]
  [final-scores <- (! <<n cl-last 'o
                     operate circle init-scores 0 'o
                     range 1 NUM-MARBLES)]
  [max-monoid <- (! minimum-monoid >= -inf.0)]
  (! <<n monoid-cl-foldl max-monoid 'o final-scores 'to-colist))
