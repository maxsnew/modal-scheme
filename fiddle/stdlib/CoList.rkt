#lang fiddle

(require fiddle/prelude)
(provide clv-nil? clv-cons? view clv-hd clv-tl tl clv-nil cl-nil clv-cons cl-cons
         cl-single
         cl-unfold colist<-list colist<-string
         cl-map cl-bind cl-bind^ cl-join cl-foldr cl-foldr^ cl-filter any? all?
         cl-append cl-append*
         cl-foldl cl-foldl^ cl-foldl1 cl-length list<-colist cl-foreach
         range cartesian-product sep-by split-when split-at chunks
         cl-zipwith cl-last

         repeat forever iterate
         tails

         monoid-cl-foldl
         minimum-monoid
         minimum-by
         maximum

         take-while
         cl-cycle
         first-such-that

         member?
         )
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

(def-thunk (! tl t)
  [v <- (! t)]
  (cond [(! clv-cons? v)
         [back <- (! clv-tl v)]
         (! back)]
        [else (! error "tried to take the tail of an empty colist")]))

(def/copat (! view)
  [((= clv-nil)) (ret '())]
  [((list 'cons hd tl)) (! Cons hd tl)])
; (def-thunk (! cl-hd t) (! <<v clv-hd 'o t))

;; clv-cons : A -> U CoList A -> CoList A
(define-thunk (! clv-cons)
  (copat [(hd tl) (ret (list 'cons hd tl))]))
(define cl-cons clv-cons)

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


;; U(Seed -> F(U nil (Cons A Seed))) -> Seed -> CoList A
(def-thunk (! cl-unfold iter seed)
  [res <- (! iter seed)]
  (ifc (! clv-nil? res)
       (! cl-nil)
       (do [hd <- (! car res)]
           [seed <- (! cdr res)]
         (! cl-cons hd (~ (! cl-unfold iter seed))))))

(def-thunk (! colist<-list)
  [iter = (~ (λ (xs)
               (cond
                 [(! empty? xs) (ret clv-nil)]
                 [#:else
                  (do [x <- (! car xs)]
                      [xs <- (! cdr xs)]
                    (! Cons x xs))])))]
  (! cl-unfold iter))


;; U(CoList A1) -> U(CoList A2) -> CoList (Cons A1 A2)
(def-thunk (! cl-zip-cons c1 c2)
  [v1 <- (! c1)] [v2 <- (! c2)]
  (cond
    [(! or (~ (! clv-nil? v1))
           (~ (! clv-nil? v2)))
     (ret clv-nil)]
    [else
     [h1 <- (! clv-hd v1)] [h2 <- (! clv-hd v2)]
     [t1 <- (! clv-tl v1)] [t2 <- (! clv-tl v2)]
     (! clv-cons (cons h1 h2) (~ (! cl-zip-cons t1 t2)))]))

(def-thunk (! repeat x n)
  (cond [(! = 0 n) (! cl-nil)]
        [else
         [n-1 <- (! - n 1)]
         (! cl-cons x (~ (! repeat x n-1)))]))

(def-thunk (! forever x)
  (! cl-cons x (~ (! forever x))))

;; zip-with : U(CoList A) -> ... -> CoList (List A ...)
(def/copat (! cl-zipwith)
  [((rest ls))

   (! cl-foldr (~ (! colist<-list ls)) ;; U(CoList (U(CoList A)))
      cl-zip-cons ;; U(CoList A) -> 
      (~ (! forever '())))])

;; CBN function:
;; U(A -> F B) -> U(CoList A) -> CoList A
(define-rec-thunk (! cl-map-one f l)
  (do [vert <- (! l)]
      (cond
        [(! clv-nil? vert) (ret '(nil))]
        [#:else
         (do [hd <- (! clv-hd vert)]
             [tl <- (! clv-tl vert)]
           [hd^ <- (! f hd)]
           (ret (list 'cons hd^ (~ (! cl-map-one f tl)))))])))

(def-thunk (! cl-map f (rest args))
  (! cl-map-one (~ (! apply f)) (~ (! apply cl-zipwith args))))


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

(define-thunk (! list<-colist c)
  (! <<v reverse 'o cl-foldl c (~ (! swap Cons)) '() '$))

(def-thunk (! cl-foldl1 step l)
  [v <- (! l)]  [hd <- (! clv-hd v)] [tl <- (! clv-tl v)]
  (! cl-foldl tl step hd))


(define-thunk (! cl-length)
  (copat [(l) (! cl-foldl l (~ (copat [(acc x) (! + 1 acc)])) 0)]))

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
;; i.e., inclusive of lo, exclusive of hi
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

;; cycle
(def-thunk (! cl-cycle cl)
  (! cl-append cl (~ (! cl-cycle cl))))


;; cl-bind : CoList A -> (A -> CoList A') -> CoList A'
(def-thunk (! cl-bind l k)
  (! cl-foldr
     l
     ;; A -> U(CoList A') -> CoList A'
     (~ (copat [(x l) (! cl-append (~ (! k x)) l)]))
     cl-nil))

(def-thunk (! cl-bind^) (! swap cl-bind))

;; cl-join : U (CoList (U (CoList A))) -> CoList A
(def-thunk (! cl-join cl-of-cl) (! cl-bind cl-of-cl $))

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

(def-thunk (! orv b t) (! or (~! Ret b) t))
(define-thunk (! any?)
  (copat [(c) (! cl-foldr c orv (~ (ret #f)))]))
(define-thunk (! all? c)
  (! idiom^ not (~! any? (~! cl-map not c))))

(def-thunk (! ex) (! range 0 10))
(def-thunk (! z-ex) (! cl-zipwith ex ex))

;; split-when : U(A -> F Bool) -> U(CoList A) -> F(List (Listof A) U(CoList A))
(def-thunk (! split-when-loop stop? c acc)
  (patc (! c)
    [(= clv-nil)
     [l <- (! reverse acc)]
     (! List l cl-nil)]
    [(list 'cons hd tl)
     (cond [(! stop? hd)
            [l <- (! reverse acc)]
            (! List l (~! cl-cons hd tl))]
           [else
            (! split-when-loop stop? tl (cons hd acc))])]))

(def-thunk (! split-when stop? c) (! split-when-loop stop? c '()))

; A -> U(CoList A) -> CoList (Listof A)
(def-thunk (! sep-by sep c)
  (letrec
      ([loop
        (~ (copat [(acc c)
                   [v <- (! c)]
                   (cond [(! and (~ (! clv-nil? v)) (~ (! empty? acc)))
                          (! cl-nil)]
                         [(! clv-nil? v)
                          (! <<v swap cl-cons cl-nil 'o reverse acc)]
                         [else
                          [hd <- (! clv-hd v)] [tl <- (! clv-tl v)]
                          (cond
                            [(! equal? hd sep)
                             (! <<v swap cl-cons (~ (! loop '() tl)) 'o reverse acc)]
                            [else (! loop (cons hd acc) tl)])])]))])
    (! loop '() c)))

(def-thunk (! cl-last-loop x c)
  [v <- (! c)]
  (cond [(! clv-nil? v) (ret x)]
        [else [hd <- (! clv-hd v)] [tl <- (! clv-tl v)]
              (! cl-last-loop hd tl)]))
(def-thunk (! cl-last c #:bind)
  [v <- (! c)]
  (cond [(! clv-nil? v) (! error "called cl-last with an empty colist!")]
        [else [hd <- (! clv-hd v)] [tl <- (! clv-tl v)]
              (! cl-last-loop hd tl)]))

(define-thunk (! monoid-cl-foldl)
  (copat
   [(m)
    (do [* <- (! first m)]
        [e <- (! second m)]
      (! cl-foldl^ * e))]))

;; A Monoid for a type A is a (list (A -> A -> F A) A)
(define-thunk (! minimum-monoid <= top)
  (ret (list
        (thunk
         (copat
          [(x y)
           (cond [(! <= x y) (ret x)] [#:else (ret y)])]))
        top)))


;; A Comparater A is a A -> A -> F bool

(define-thunk (! pb f comparator x y)
  (do [x <- (! f x)] [y <- (! f y)]
    (! comparator x y)))

;; U (A -> F Num) -> A -> U(CoList A) -> F A
(define-thunk (! minimum-by f f*+inf)
  (do [m <- (! minimum-monoid (thunk (! pb f <=)) f*+inf)]
   (! monoid-cl-foldl m)))

(def-thunk (! maximum)
  [m <- (! minimum-monoid >= -inf.0)]
  (! monoid-cl-foldl m))

;; Nat -> U(CoList A) -> F(List (Listof A) (U(CoList A)))
(def/copat (! split-at)
  [((= 0) l) (! List '() l)]
  [(n     l)
   [spine <- (! l)]
   (cond [(! clv-nil? spine) (! List '() cl-nil)]
         [else
          [hd <- (! clv-hd spine)] [tl <- (! clv-tl spine)]
          [n-1 <- (! - n 1)]
          [split-at-n-1 <- (! split-at n-1 tl)]
          [front <- (! first split-at-n-1)] [back <- (! second split-at-n-1)]
          [front <- (! Cons hd front)]
          (! List front back)])])

(def-thunk (! take-while p? cl)
  [v <- (! cl)]
  (cond [(! clv-nil? v) (! cl-nil)]
        [else
         [hd <- (! clv-hd v)] [tl <- (! clv-tl v)]
         (cond [(! p? hd)
                (! cl-cons hd (~ (! take-while p? tl)))]
               [else (! cl-nil)])]))

(def-thunk (! take n cl)
  (cond [(! <= n 0) (ret (list '() cl))]
        [else
         [v <- (! cl)]
         ((copat
           [((= clv-nil)) (! List '() cl)]
           [((cons (= 'cons) (cons hd (cons tl _))))
            [n-1 <- (! - n 1)]
            [ft*back <- (! take n-1 tl)]
            [ft <- (! first ft*back)] [back <- (! second ft*back)]
            (! List (cons hd ft) back)])
          v)])
  )

;; chunks : Nat -> U(CoList A) -> CoList (Listof A)
(def-thunk (! chunks size l)
  [front*back <- (! split-at size l)]
  [front <- (! first front*back)] [back <- (! second front*back)]
  (cond [(! empty? front) (! cl-nil)]
        [else (! cl-cons front (~ (! chunks size back)))]))


;; U(A -> F Bool) -> U(CoList A) -> F A
(def-thunk (! first-such-that p? xs)
  (! cl-foldr xs
     (~ (copat [(x k)
                (cond [(! p? x) (ret x)]
                      [else (! k)])]))
     (~ (! error 'didnt-find-it))))


(def-thunk (! iterate f seed)
  (! cl-unfold
     (~ (λ (cur~)
          (do [cur <- (! cur~)]
              (! Cons cur (~ (! f cur))))))
     (~ (ret seed))))

(def-thunk (! member? x cl) (! <<n any? 'o cl-map (~ (! equal? x)) cl))

(def-thunk (! tails l)
  [unwrap = (~ (copat
                [('()) (! cl-nil)]
                [((cons _ xs)) (! Cons xs xs)]))]
  (! cl-cons l (~! cl-unfold unwrap l)))

;
(def-thunk (! colist<-string s)
  [l <- (! string-length s)]
  (! cl-map (~! string-ref s) (~! range 0 l)))
