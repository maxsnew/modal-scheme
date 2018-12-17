#lang sbpv

(require "../stdlib.rkt")
(provide clv-nil? clv-cons? clv-hd clv-tl
         colist<-list
         cl-map bind cl-foldr cl-filter
         cl-foldl cl-length list<-colist
         range cartesian-product)
;; CoList A = F (CoListVert A)
;; data CoListVert A where
;;   '(nil)
;;   '(cons ,A ,(U(CoList A))

(define-thunk (! clv-nil? v)
  (! <<v equal? 'nil 'o first v '$))
(define-thunk (! clv-cons? v)
  (! <<v equal? 'cons 'o first v '$))
(define clv-hd second)
(define clv-tl third)

;; clv-cons : A -> U CoList A -> CoList A
(define-thunk (! clv-cons)
  (copat [(hd tl) (ret (list 'cons hd tl))]))

;; CBN function:
;; U(A -> F B) -> U(CoList A) -> CoList A
(define-rec-thunk (! cl-map f l)
  (do [vert <- (! l)]
      (cond
        [(! clv-nil? vert) (ret '(nil))]
        [#:else
         (do [hd <- (! clv-hd vert)]
             [tl <- (! clv-tl vert)]
           [hd^ <- (! f hd)]
           (ret (list 'cons hd^ (thunk (! cl-map f tl)))))])))

;; cl-foldl : U(CoList A) -> (Acc -> A -> F Acc) -> Acc -> F Acc
(define-rec-thunk (! cl-foldl l step acc)
  (do [vert <- (! l)]
      (cond
        [(! clv-nil? vert) (ret acc)]
        [#:else
         (do [hd <- (! clv-hd vert)]
             [tl <- (! clv-tl vert)]
           [acc <- (! step acc hd)]
           (! cl-foldl tl step acc))])))

(define-thunk (! cl-length)
  (copat [(l) (! cl-foldl l (thunk (copat [(acc x) (! + 1 acc)])) 0)]))

;; foldr : U (CoList A) -> (A -> U B -> B) -> UB -> B
(define-rec-thunk (! cl-foldr)
  (copat
   [(l cons nil)
    (do [v <- (! l)]
        (cond
          [(! clv-nil? v) (! nil)]
          [#:else
           (do [hd <- (! clv-hd v)]
               [tl <- (! clv-tl v)]
             (! cons hd (thunk (! cl-foldr tl cons nil))))]))]))

(define-rec-thunk (! cl-filter)
  (copat
   [(p? l)
    (! cl-foldr
       l
       (thunk
        (copat
         [(x tl)
          (cond
            [(! p? x) (! clv-cons x tl)]
            [#:else (! tl)])]))
       (thunk (ret '(nil))))]))

(define-rec-thunk (! colist<-list xs)
  (cond
    [(! empty? xs) (ret (list 'nil))]
    [#:else
     (do [x <- (! car xs)]
         [xs <- (! cdr xs)]
       (ret (list 'cons x (thunk (! colist<-list xs)))))]))

(define-rec-thunk (! list<-colist c)
  (! <<v reverse 'o cl-foldl c (thunk (! swap Cons)) '() '$))

#;
(do [c <- (ret (thunk (! colist<-list (list 1 2))))]
    [c^ <- (ret (thunk (! cl-map Ret c)))]
    (! list<-colist c^))

;; Num -> Num -> CoList Num
;; [lo, hi)
;; if hi <= lo: empty
(define-rec-thunk (! range)
  (copat
   [(lo hi)
    (cond
      [(! <= hi lo) (ret '(nil))]
      [#:else
       (do [lo+1 <- (! + lo 1)]
           (ret (list 'cons lo (thunk (! range lo+1 hi)))))])]))

;; cl-append : U CoList A -> U CoList A -> CoList A
(define-rec-thunk (! cl-append)
  (copat [(l1 l2) (! cl-foldr l1 clv-cons l2)]))


;; cl-bind : CoList A -> (A -> CoList A') -> CoList A'
(define-thunk (! cl-bind)
  (copat
   [(l k)
    (! cl-foldr
     l
     ;; A -> U(CoList A') -> CoList A'
     (thunk
      (copat
       [(x l)
        (! cl-append (thunk (! k x)) l)]))
     (thunk (ret '(nil))))]))

;; cartesian-product : CoList A -> CoList B -> CoList (List A B)
(define-thunk (! cartesian-product)
  (copat
   [(l1 l2)
    (! cl-bind
       l1
       (thunk
        (copat
         [(x)
          (! cl-map
             (thunk
              (copat [(y) (ret (list x y))]))
             l2)])))]))

;(! list<-colist (thunk (! cartesian-product (thunk (! range 0 3)) (thunk (! range 5 7)))))

