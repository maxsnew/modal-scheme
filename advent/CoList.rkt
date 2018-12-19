#lang sbpv

(require "../stdlib.rkt")
(provide clv-nil? clv-cons? clv-hd clv-tl clv-nil cl-nil clv-cons cl-cons
         cl-single
         colist<-list
         cl-map cl-bind cl-bind^ cl-foldr cl-foldr^ cl-filter any?
         cl-append cl-append*
         cl-foldl cl-foldl^ cl-foldl1 cl-length list<-colist cl-foreach
         range cartesian-product
         cl-zipwith)
;; CoList A = F (CoListVert A)
;; data CoListVert A where
;;   '(nil)
;;   '(cons ,A ,(U(CoList A))

(define clv-nil (list 'nil))
(def-thunk (! cl-nil) (ret clv-nil))
(define-thunk (! clv-nil? v)
  (! <<v equal? 'nil 'o first v '$))
(define-thunk (! clv-cons? v)
  (! <<v equal? 'cons 'o first v '$))
(define clv-hd second)
(define clv-tl third)

;; clv-cons : A -> U CoList A -> CoList A
(define-thunk (! clv-cons)
  (copat [(hd tl) (ret (list 'cons hd tl))]))
(define cl-cons clv-cons)


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
           (ret (list 'cons hd^ (~ (! cl-map f tl)))))])))

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

(define-thunk (! cl-foldl^ )
  (copat [(step acc l) (! cl-foldl l step acc)]))

(def-thunk (! cl-foldl1 step l)
  [v <- (! l)]  [hd <- (! clv-hd v)] [tl <- (! clv-tl v)]
  (! cl-foldl tl step hd))


(define-thunk (! cl-length)
  (copat [(l) (! cl-foldl l (~ (copat [(acc x) (! + 1 acc)])) 0)]))

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
             (! cons hd (~ (! cl-foldr tl cons nil))))]))]))
(def-thunk (! cl-foldr^ cons nil l) (! cl-foldr l cons nil))

(define-rec-thunk (! cl-filter)
  (copat
   [(p? l)
    (! cl-foldr
       l
       (~
        (copat
         [(x tl)
          (cond
            [(! p? x) (! clv-cons x tl)]
            [#:else (! tl)])]))
       (~ (ret '(nil))))]))

(define-rec-thunk (! colist<-list xs)
  (cond
    [(! empty? xs) (ret (list 'nil))]
    [#:else
     (do [x <- (! car xs)]
         [xs <- (! cdr xs)]
       (ret (list 'cons x (~ (! colist<-list xs)))))]))

(define-thunk (! list<-colist c)
  (! <<v reverse 'o cl-foldl c (~ (! swap Cons)) '() '$))

;; cl-foreach : (A -> F 1) -> U CoList A -> F 1
(define-thunk (! cl-foreach)
  (copat [(f c)
          (! cl-foldl
             c
             (~ (copat [(acc x) (do [_ <- (! f x)] (ret acc)) ]))
             '())]))

#;
(do [c <- (ret (thunk (! colist<-list (list 1 2))))]
    [c^ <- (ret (thunk (! cl-map Ret c)))]
    (! list<-colist c^))

;; Num -> Num -> CoList Num
;; [lo, hi)
;; if hi <= lo: empty
(define-rec-thunk (! range-lo-hi lo hi)
  (cond
    [(! <= hi lo) (ret '(nil))]
    [#:else
     [lo+1 <- (! + lo 1)]
     (! clv-cons lo (~ (! range-lo-hi lo+1 hi)))]))
(define-rec-thunk (! range-lo lo)
  (do [lo+1 <- (! + lo 1)] (! clv-cons lo (~ (! range-lo lo+1)))))
(define-rec-thunk (! range)
  (copat [(lo hi) (! range-lo-hi lo hi)]
         [(lo)    (! range-lo lo)]))

;; cl-append : U CoList A -> U CoList A -> CoList A
(define-rec-thunk (! cl-append)
  (copat [(l1 l2) (! cl-foldr l1 clv-cons l2)]))

(define-thunk (! cl-single x) (! clv-cons x cl-nil))

(def/copat (! cl-append*)
  [(#:bind) (ret clv-nil)]
  [(l) (! <<n cl-append l 'o cl-append*)])

;; cl-bind : CoList A -> (A -> CoList A') -> CoList A'
(def-thunk (! cl-bind l k)
  (! cl-foldr
     l
     ;; A -> U(CoList A') -> CoList A'
     (~ (copat [(x l) (! cl-append (~ (! k x)) l)]))
     cl-nil))

(def-thunk (! cl-bind^) (! swap cl-bind))


;; cartesian-product : CoList A -> CoList B -> CoList (List A B)
(define-thunk (! cartesian-product)
  (copat
   [(l1 l2)
    (! cl-bind
       l1
       (~
        (copat
         [(x)
          (! cl-map
             (~
              (copat [(y) (ret (list x y))]))
             l2)])))]))

;(! list<-colist (thunk (! cartesian-product (thunk (! range 0 3)) (thunk (! range 5 7)))))

; monoid-foldl : (* : A -> A -> F A) -> e : A -> U CoList A -> F A
#;
(define-thunk (! monoid-foldl)
  (copat [(* e c) (! cl-foldl c )]))

(define-thunk (! any?)
  (copat [(c) (! cl-foldr c or (~ (ret #f)))]))

(def-thunk (! cl-zipwith c1 c2)
  [v1 <- (! c1)] [v2 <- (! c2)]
  (cond
    [(! or (~ (! clv-nil? v1))
           (~ (! clv-nil? v2)))
     (ret clv-nil)]
    [else
     [h1 <- (! clv-hd v1)] [h2 <- (! clv-hd v2)]
     [t1 <- (! clv-tl v1)] [t2 <- (! clv-tl v2)]
     (! clv-cons (list h1 h2) (~ (! cl-zipwith t1 t2)))]))

(def-thunk (! ex) (! range 0 10))
(def-thunk (! z-ex) (! cl-zipwith ex ex))
