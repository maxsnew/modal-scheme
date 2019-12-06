#lang sbpv

(require "../stdlib.rkt")
(require "CoList.rkt")

(provide mt-flexvec flexvec<-list)

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
  ;; (! displayln 'mk-deep)
  ;; (! displayln lefts)
  ;; (! displayln middles)
  ;; (! displayln rights)
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
#;
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
  [(xs ys) [xys <- (! foldr xs Cons ys)] (! append xys)]
  [(xs) (ret xs)]
  [() (ret '())])

;; A -> A -> ... -> Listof (Node A)
(def/copat (! mk-nodes)
  [(a b #:bind) (! <<v List 'o mk-node a b)]
  [(a b c #:bind) (! <<v List 'o mk-node a b c)]
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
        [else ;; here
         [ll <- (! deep-lefts fl)] [ml <- (! deep-middles fl)] [rl <- (! deep-rights fl)]
         [lr <- (! deep-lefts fr)] [mr <- (! deep-middles fr)] [rr <- (! deep-rights fr)]
         (! <<v append rl xs lr)
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
  (cond [(! > hd-sz ix)
         ;(! displayln 'here-now)
         ;(! displayln l)
         ;(! displayln ix)
         (! List '() hd tl)]
        [(! <= hd-sz ix)
         ;(! displayln 'sndcase)
         ;(! displayln tl)
         [unzipped <- (! <<v unsafe-unzip-at-list tl 'o - ix hd-sz)]
         [left <- (! first unzipped)] [middle <- (! second unzipped)] [right <- (! third unzipped)]
         ;(! displayln 'didiimakeit)
         (! List (cons hd left) middle right)]))

;; A version of mk-deep that works when the left list is possibly empty
(def-thunk (! mk-deep-l)
  (letrec
      ([view-l
        (~ (λ (t)
             (do ;(! displayln 'view-l)
                 ;(! displayln t)
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
                      [else [l <- (! <<v list<-node 'o first view)]
                            [m-rest <- (! second view)]
                            (! mk-deep l m-rest r)])]
               [else (! mk-deep small m r)])))])
    (! mk-deep-l)))

;; A version of mk-deep that works when the right list is possibly empty
(def-thunk (! mk-deep-r)
  (letrec
      ;; view-r : FingerTree A -> F(List FTree A)
      ([view-r
        (~ (λ (t)
             (do ;(! displayall 'view-r t)
                 (cond
                   [(! ft-mt? t) (ret '())]
                   [(! ft-single? t) [x <- (! ft-single-val t)] (! List empty x)]
                   [else
                    [l <- (! deep-lefts t)] [m <- (! deep-middles t)] [r <- (! deep-rights t)]
                    [x <- (! <<v car 'o reverse r)]
                    [small <- (! <<v reverse 'o cdr 'o reverse r)]
                    [rest <- (! mk-deep-r l m small)]
                    (! List rest x)]))))]
       [mk-deep-r
        (~ (λ (l m small)
             (do ;(! displayall 'mk-deep-r l m small)
              (cond
                [(! empty? small)
                 [view <- (! view-r m)]
                 ;(! displayall 'view-r-return view)
                 (cond [(! empty? view) (! fingertree<-list l)]
                       [else [m-rest <- (! first view)] [r <- (! <<v list<-node 'o second view)]
                             (! mk-deep l m-rest r)])]
                [else (! mk-deep l m small)]))))])
    (! mk-deep-r)))


(def-thunk (! unsafe-unzip-at t ix)
  ;; (! displayln 'unsafe-unzip-at)
  ;; (! displayln t)
  ;; (! displayln ix)
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
        ;(! displayln 'looking-at-the-middle)
        ;(! displayln r)
        ;(! displayln ix)
        [ix <- (! - ix lsize)]
        ;; find the Node that contains it in the deep tree
        [unzipped <- (! unsafe-unzip-at m ix)]
        [lm-tree <- (! first unzipped)] [x-node <- (! second unzipped)] [rm-tree <- (! third unzipped)]
        [ix <- (! <<v - ix 'o size lm-tree)]
        [x-list <- (! list<-node x-node)]
        [unzipped <- (! unsafe-unzip-at-list x-list ix)]
        [x-nodel <- (! first unzipped)] [x <- (! second unzipped)] [x-noder <- (! third unzipped)]
        ;(! displayall 'mk-deep-r l lm-tree x-nodel)
        [l <- (! mk-deep-r l lm-tree x-nodel)]
        ;(! displayln 'but-not-me)
        [r <- (! mk-deep-l x-noder rm-tree r)]
        (! List l x r)]
       [else ;; it's in r
        [ix <- (! - ix lsize msize)]
        [unzipped <- (! unsafe-unzip-at-list r ix)]
        [lr-list <- (! first unzipped)] [x <- (! second unzipped)] [rr-list <- (! third unzipped)]
        ;; want (l ++ m ++ lr) x rr
        [rr <- (! fingertree<-list rr-list)]
        [ll <- (! mk-deep-r l m lr-list)]
        (! List ll x rr)])]))

(def-thunk (! get t ix)
  [view <- (! unsafe-unzip-at t ix)]
  (! second view))

(def-thunk (! insert-at t ix x)
  [view <- (! unsafe-unzip-at t ix)]
  [l <- (! first view)] [y <- (! second view)] [r <- (! third view)]
  ;; (! displayln 'app3)
  ;; (! displayln l)
  ;; (! displayln (list x y))
  ;; (! displayln r)
  (! app3 l (list x y) r))

;; delete-at : FingerTree A -> F (List A (FingerTree A))
(def-thunk (! delete-at t ix)
  [view <- (! unsafe-unzip-at t ix)]
  [l <- (! first view)] [deleted <- (! second view)] [r <- (! third view)]
  [t <- (! app3 l '() r)]
  (! List deleted t))

(def-thunk (! update-at t ix up)
  [view <- (! unsafe-unzip-at t ix)]
  [l <- (! first view)] [old-val <- (! second view)] [r <- (! third view)]
  [new-val <- (! up old-val)]
  (! app3 l (list new-val) r))

(def-thunk (! colist<-fingertree t)
  (cond [(! ft-mt? t) (! cl-nil)]
        [else
         [view <- (! unsafe-unzip-at t 0)]
         [hd <- (! second view)] [tl <- (! third view)]
         (! cl-cons hd (~ (! colist<-fingertree tl)))]))

;; FingerTree (Elt A) -> FlexVec A
(def-thunk (! flexvec<-fingertree tree)
  (copat
   [((= 'debug)) (ret tree)]
   [((= 'get) ix) (! <<v elt-val 'o get tree ix)]
   [((= 'remove) ix)
    [x*tree <- (! delete-at tree ix)]
    [x <- (! <<v elt-val 'o first x*tree)]
    [tree <- (! second x*tree)]
    (! List x (~ (! flexvec<-fingertree tree)))]
   [((= 'cons) x)
    [tree <- (! <<v swap ft-cons tree 'o mk-elt x)]
    (ret (~ (! flexvec<-fingertree tree)))]
   [((= 'insert) ix x)
    [x-elt <- (! mk-elt x)]
    [tree <- (! insert-at tree ix x-elt)]
    (ret (~ (! flexvec<-fingertree tree)))]
   [((= 'update) ix up)
    [up = (~ (! <<v mk-elt 'o up 'o elt-val))]
    ;(! displayall 'update tree ix)
    [tree <- (! update-at tree ix up)]
    (ret (~ (! flexvec<-fingertree tree)))]
   [((= 'to-colist))
    (! <<n cl-map elt-val 'o colist<-fingertree tree)]))

(def-thunk (! mt-flexvec) (! flexvec<-fingertree empty))
(def-thunk (! flexvec<-list xs)
  [xs <- (! map mk-elt xs)]
  [t <- (! app3 empty xs empty)]
  (ret (~ (! flexvec<-fingertree t))))

(define ex '(deep 5 ((elt 4) (elt 2) (elt 1) (elt 3)) empty ((elt 0))))
(define ex2
  '(deep 23 ((elt 16)) (deep 20 ((two 4 (two 3 (two 2 (elt 8) (elt 17)) (elt 18)) (elt 19))) empty ((two 6 (two 5 (two 4 (two 3 (two 2 (elt 4) (elt 9)) (elt 10)) (elt 20)) (elt 21)) (elt 22)) (two 10 (two 5 (two 4 (two 3 (two 2 (elt 2) (elt 5)) (elt 11)) (elt 12)) (elt 13)) (three 5 (three 3 (elt 1) (elt 6) (elt 3)) (elt 14) (elt 7))))) ((elt 15) (elt 0))))

(define ex5
  '(deep 9
         ((elt 8) (elt 4) (two 2 (elt 2) (elt 5))) ;; bad!
         (single (three 3 (elt 1) (elt 6) (elt 3)))
         ((elt 7) (elt 0))))
(define ex6
  '(deep 8
         ((elt 4))
         (deep 5
               ((two 2 (elt 2) (elt 5)))
               empty
               ((three 3 (elt 1) (elt 6) (elt 3))))
         ((elt 7) (elt 0))))
;; mk-deep-l bug:
;;   args:
;; '()
;; '(deep 5 ((two 2 (elt 2) (elt 5))) empty ((three 3 (elt 1) (elt 6) (elt 3))))
;; '((elt 7) (elt 0))
;;   result, which has a left subtree with the wrong type:
;; '(deep 7
;;        ((two 2 (elt 2) (elt 5)))
;;        (single (three 3 (elt 1) (elt 6) (elt 3)))
;;        ((elt 7) (elt 0)))


#;
'(empty
  (elt 4)
  (deep 7
        ((two 2 (elt 2) (elt 5)))
        (single (three 3 (elt 1) (elt 6) (elt 3)))
        ((elt 7) (elt 0))))

;; problem case: (! insert-at ex6 0 '(elt 8)) == (ret ex5) which is bad
(define exx
 '(deep 10
        ((elt 8))
        (deep 7
              ((two 2 (elt 4) (elt 9)) (two 2 (elt 2) (elt 5)))
              empty
              ((three 3 (elt 1) (elt 6) (elt 3))))
        ((elt 7) (elt 0))))
; (! insert-at exx 4 '(elt x)) errors but it shouldn't

(define exl '(deep 4 ((elt 8)) (single (two 2 (elt 4) (elt 9))) ((elt 2))))
(define exm '((elt x) (elt 5)))
(define exr '(deep 5 ((elt 1) (elt 6) (elt 3)) empty ((elt 7) (elt 0))))
; problem: (! app3 exl exm exr)

;; '((deep 4
;;         ((elt 8))
;;         (single (two 2 (elt 4) (elt 9)))
;;         ((elt 2)))
;;   (elt 5)
;;   (deep 5
;;         ((elt 1) (elt 6) (elt 3))
;;         empty
;;         ((elt 7) (elt 0))))

(define exa ;; looks valid
  '(deep 11
         ((elt 8))
         (deep 8
               ((two 2 (elt 4) (elt 9)))
               empty
               ((three 3 (elt 2) (elt 10) (elt 5)) (three 3 (elt 1) (elt 6) (elt 3))))
         ((elt 7) (elt 0))))
; (! insert-at exa 6 '(elt x))

;; bug:
;; (! mk-deep-r
;;    '((elt 8))
;;    '(deep 5 ((two 2 (elt 4) (elt 9))) empty ((three 3 (elt 2) (elt 10) (elt 5))))
;;    '())

