#lang sbpv

(require "../stdlib.rkt")

;; data Elt A where
;;   (Elt A)
(def-thunk (! mk-elt x) (! List 'elt x))
(def-thunk (! elt? x) (! and (~ (! cons? x)) (~ (! <<v equal? 'elt 'o first x))))
(def-thunk (! elt-val x) (! second x))

;; data Node A where
;;   Two Size A A
;;   Three Size A A A
(def-thunk (! node-size x) (! second x))
(def-thunk (! node? x)
  (! and (~ (! cons? x))
     (~ (! or (~ (! <<v equal? 'two   'o first x))
           (~ (! <<v equal? 'three 'o first x))))))
(def-thunk (! list<-node x) (! <<v cdr 'o cdr x))

(def-thunk (! ft-single-val x) (! second x))
(def-thunk (! ft-mt? x) (! <<v equal? 'empty x))
(def-thunk (! ft-single? x)
  (! and (~ (! cons? x)) (~ (! <<v equal? 'single 'o first x))))
(def-thunk (! ft-deep? x)
  (! and (~ (! cons? x)) (~ (! <<v equal? 'deep 'o first x))))
(def-thunk (! deep-size x) (! second x))
(def-thunk (! size x)
  (cond [(! elt? x) (ret 1)]
        [(! node? x) (! node-size x)]
        [(! ft-mt? x) (ret 0)]
        [(! ft-single? x) (! <<v size 'o ft-single-val x)]
        [(! ft-deep?   x) (! deep-size x)]
        [else (error x)]))

(def/copat (! mk-node)
  [(x y z) [sz <- (! <<v foldl^ + 0 'o map size (list x y z))]
   (ret (list 'three sz x y z))]
  [(x y) [sz <- (! <<v foldl^ + 0 'o map size (list x y))]
         (ret (list 'two sz x y))])

;; data FingerTree A where
;;   Empty
;;   Single A
;;   Deep Size (NEList<=4 A) (FingerTree (Node A)) (NEList<=4 A)
(define empty 'empty)
(def-thunk (! mk-single x) [v <- (! size x)] (ret (list 'single x)))
(def-thunk (! mk-deep lefts middles rights)
  [lsz <- (! <<v apply + 'o map size lefts)]
  [rsz <- (! <<v apply + 'o map size rights)]
  [msz <- (! size middles)]
  [sz <- (! + lsz msz rsz)]
  (ret (list 'deep sz lefts middles rights)))
(def-thunk (! deep-lefts x)
  (! apply (~ (copat [((= 'deep) sz lefts middles rights) (ret lefts)])) x))
(def-thunk (! deep-middles x)
  (! apply (~ (copat [((= 'deep) sz lefts middles rights) (ret middles)])) x))
(def-thunk (! deep-rights x)
  (! apply (~ (copat [((= 'deep) sz lefts middles rights) (ret rights)])) x))

;; where A supports the size operation, meaning it is one of
;;   Elt B
;;   Node A' for A' supporting size

;; A NEList<=4 A is a (List A) with length 1,2,3 or 4

;; operations we actually need: insertAt, deleteAt, get
;;   to implement these we need to implement split and append, and cons and snoc

(def-thunk (! full? l) (! <<v equal? 4 'o length l))
(def-thunk (! ft-cons x t)
  (cond [(! ft-mt? t) (! mk-single x)]
        [(! ft-single? t)
         [y <- (! ft-single-val t)]
         (! mk-deep (list x) empty (list y))]
        [else ;; deep
         [lefts <- (! deep-lefts t)]
         [middles <- (! deep-middles t)]
         [rights <- (! deep-rights t)]
         (cond [(! full? lefts) ;; lefts has 4 elements
                [y <- (! first lefts)]
                [node <- (! <<v apply mk-node 'o rest lefts)]
                [middles <- (! ft-cons node middles)]
                (! mk-deep (list x y) middles rights)]
               [else
                (! mk-deep (cons x lefts) middles rights)])]))
(def-thunk (! ft-snoc t y)
  (cond [(! ft-mt? t) (! mk-single y)]
        [(! ft-single? t)
         [x <- (! ft-single-val t)]
         (! mk-deep (list x) empty (list y))]
        [else ;; deep
         [lefts <- (! deep-lefts t)]
         [middles <- (! deep-middles t)]
         [rights <- (! deep-rights t)]
         (cond [(! full? rights)
                [x <- (! <<v first 'o reverse rights)]
                [node <- (! <<v apply mk-node 'o reverse 'o rest 'o reverse rights)]
                [middles <- (! ft-snoc middles node)]
                (! mk-deep lefts middles (list x y))]
               [else
                [rights <- (! <<v reverse 'o Cons y 'o reverse rights)]
                (! mk-deep lefts middles rights)])]))

(do [e1 <- (! mk-elt 'x)]
    [e2 <- (! mk-elt 'y)]
  [e3 <- (! mk-elt 'z)]
    ;(! mk-deep (list e1) empty (list e2))
  (! <<v
     swap ft-snoc e2 'o
     ft-cons e3 'o
     swap ft-snoc e2 'o
     swap ft-snoc e2 'o
     swap ft-snoc e2 'o
     ft-cons e3 'o
     ft-cons e1 'o
     swap ft-snoc e2 'o
     ft-cons e1 'o
     swap ft-snoc e2 'o
     ft-cons e3 'o
     Ret empty))

;; concatenation
;; multi-cons : Listof A -> FingerTree A -> F (FingerTree A)
(def-thunk (! multi-cons xs t) (! foldr xs ft-cons t))
;; multi-snoc : FingerTree A -> Listof A -> F (FingerTree A)
(def-thunk (! multi-snoc t xs) (! foldl xs ft-snoc t))

;; warning: this is the bad order, but we've got very short lists here
(def/copat (! append)
  [(xs ys) [xys <- (! foldr xs Cons ys)] (! append xs)]
  [(xs) (ret xs)]
  [() (ret '())])

;; A -> A -> ... -> Listof (Node A)
(def/copat (! mk-nodes)
  [(a b #:bind) (! mk-node a b)]
  [(a b c #:bind) (! mk-node a b c)]
  [(a b c d #:bind)
   [n1 <- (! mk-node a b)] [n2 <- (! mk-node c d)]
   (! List n1 n2)]
  [(a b c (rest ds))
   [n <- (! mk-node a b c)]
   [ns <- (! apply mk-nodes ds)]
   (! Cons n ns)])
;; app3 : FingerTree A -> Listof A -> FingerTree A -> FingerTree A
(def-thunk (! app3 fl xs fr)
  (cond [(! ft-mt? fl) (! multi-cons xs fr)]
        [(! ft-mt? fr) (! multi-snoc fl xs)]
        [(! ft-single? fl)
         [xl <- (! ft-single-val fl)]
         (! <<v ft-cons xl 'o multi-cons xs fr)]
        [(! ft-single? fr)
         [xr <- (! ft-single-val fr)]
         (! <<v swap ft-snoc xr 'o swap multi-snoc xs fl)]
        [else
         [ll <- (! deep-lefts fl)] [ml <- (! deep-middles fl)] [rl <- (! deep-rights fl)]
         [lr <- (! deep-lefts fr)] [mr <- (! deep-middles fr)] [rr <- (! deep-rights fr)]
         [nodes <- (! <<v apply mk-nodes 'o append rl xs lr)]
         [mm <- (! app3 ml nodes mr)]
         (! mk-deep ll mm rr)])
  )

(def-thunk (! app3^ t1 t2 xs) (! app3 t1 xs t2))

(def-thunk (! fingertree<-list xs) (! app3 empty xs empty))

;; We are going to
;; unzip-at : FingerTree A -> Nat -> F (List (FingerTree A) A (FingerTree A))
;; precondition: the index has to be < the size of the tree
;; TODO: wrap this with something that guarantees the precondition


;; unsafe-unzip-at-list : Listof A -> Nat -> F (List (List A) A (List A))
;; where A is sizable
;; the postcondition is that the sum of sizes of the left list is <= ix, but when combined with the middle A, is > ix
(def-thunk (! unsafe-unzip-at-list l ix)
  [hd <- (! car l)] [tl <- (! cdr l)]
  [hd-sz <- (! size hd)]
  (cond [(! > hd-sz ix) (! List '() hd tl)]
        [(! <= hd-sz ix)
         [unzipped <- (! <<v unsafe-unzip-at-list tl 'o - ix hd-sz)]
         [left <- (! first unzipped)] [middle <- (! second unzipped)] [right <- (! third unzipped)]
         (! List (cons hd left) middle right)]))

;; A version of mk-deep that works when the left list is possibly empty
(def-thunk (! mk-deep-l)
  (letrec
      ([view-l
        (~ (λ (t)
             (do (! displayln 'view-l)
                 (! displayln t)
               (cond
                 [(! ft-mt? t) (ret '())]
                 [(! ft-single? t) [x <- (! ft-single-val t)] (! List x empty)]
                 [else
                  [l <- (! deep-lefts t)] [m <- (! deep-middles t)] [r <- (! deep-rights t)]
                  [x <- (! car l)] [small <- (! cdr l)]
                  [rest <- (! mk-deep-l small m r)]
                  (! List x rest)]))))]
       [mk-deep-l
        (~ (λ (small m r)
             (cond
               [(! empty? small)
                [view <- (! view-l m)]
                (cond [(! empty? view) (! fingertree<-list r)]
                      [else [x <- (! first view)] [m-rest <- (! second view)]
                            (! mk-deep (list x) m-rest r)])]
               [else (! mk-deep small m r)])))])
    (! mk-deep-l)))

;; A version of mk-deep that works when the right list is possibly empty
(def-thunk (! mk-deep-r)
  (letrec
      ([view-r
        (~ (λ (t)
             (cond
               [(! ft-mt? t) (ret '())]
               [(! ft-single? t) [x <- (! ft-single-val t)] (! List empty x)]
               [else
                [l <- (! deep-lefts t)] [m <- (! deep-middles t)] [r <- (! deep-rights t)]
                [x <- (! <<v car 'o reverse r)] [small <- (! <<v reverse 'o cdr 'o reverse r)]
                [rest <- (! mk-deep-r l m small)]
                (! List rest x)])))]
       [mk-deep-r
        (~ (λ (l m small)
             (cond
               [(! empty? small)
                [view <- (! view-r m)]
                (cond [(! empty? view) (! fingertree<-list l)]
                      [else [m-rest <- (! first view)] [x <- (! second view)] 
                            (! mk-deep l m-rest (list x))])]
               [else (! mk-deep l m small)])))])
    (! mk-deep-r)))

(def-thunk (! unsafe-unzip-at t ix)
  (cond
    ;; impossible for it to be empty
    [(! ft-single? t)
     [x <- (! ft-single-val t)]
     (! List empty x empty)]
    [else ;; deep
     [sz <- (! deep-size t)]
     [l <- (! deep-lefts t)] [m <- (! deep-middles t)] [r <- (! deep-rights t)]
     [lsize <- (! <<v apply + 'o map size l)]
     [msize <- (! size m)]
     (cond
       [(! < ix lsize);; it's in l
        [unzipped <- (! unsafe-unzip-at-list l ix)]
        [ll-list <- (! first unzipped)] [x <- (! second unzipped)] [rl-list <- (! third unzipped)]
        ;; want ll x, (rl ++ m ++ r)
        [ll <- (! fingertree<-list ll-list)]
        [rr <- (! mk-deep-l rl-list m r)]
        (! List ll x rr)]
       [(! <<v < ix 'o + lsize msize) ;; it's in m
        [ix <- (! - ix lsize)]
        (! displayln "should be 0")
        (! displayln ix)
        ;; find the Node that contains it in the deep tree
        (! displayln m)
        [unzipped <- (! unsafe-unzip-at m ix)]
        (! displayln 'unzipped1)
        (! displayln unzipped)
        [lm-tree <- (! first unzipped)] [x-node <- (! second unzipped)] [rm-tree <- (! third unzipped)]
        [ix <- (! <<v - ix 'o size lm-tree)]
        [x-list <- (! list<-node x-node)]
        (! displayln x-list)
        [unzipped <- (! unsafe-unzip-at-list x-list ix)]
        (! displayln unzipped)
        [x-nodel <- (! first unzipped)] [x <- (! second unzipped)] [x-noder <- (! third unzipped)]
        (! displayln 'mk-deep-r)
        [l <- (! mk-deep-r l lm-tree x-nodel)]
        (! displayln 'mk-deep-l)
        (! displayln x-noder)
        (! displayln rm-tree)
        (! displayln r)
        [r <- (! mk-deep-l x-noder rm-tree r)]
        (! displayln 'beep)
        (! List l x r)]
       [else ;; it's in r
        [ix <- (! - ix lsize msize)]
        [unzipped <- (! unsafe-unzip-at-list r ix)]
        [lr-list <- (! first unzipped)] [x <- (! second unzipped)] [rr-list <- (! third unzipped)]
        ;; want (l ++ m ++ lr) x rr
        [rr <- (! fingertree<-list rr-list)]
        [ll <- (! mk-deep-r l m lr-list)]
        (! List ll x rr)])]))

