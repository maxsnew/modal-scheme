#lang sbpv

(require "../stdlib.rkt")
(provide clv-nil? clv-cons? clv-hd clv-tl
         cl-map cl-foldl
         colist<-list list<-colist)
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

(define-rec-thunk (! cl-foldl l step acc)
  (do [vert <- (! l)]
      (cond
        [(! clv-nil? vert) (ret acc)]
        [#:else
         (do [hd <- (! clv-hd vert)]
             [tl <- (! clv-tl vert)]
           [acc <- (! step acc hd)]
           (! cl-foldl tl step acc))])))

(define-rec-thunk (! colist<-list xs)
  (cond
    [(! empty? xs) (ret (list 'nil))]
    [#:else
     (do [x <- (! car xs)]
         [xs <- (! cdr xs)]
       (ret (list 'cons x (thunk (! colist<-list xs)))))]))

(define-rec-thunk (! list<-colist c)
  (! <<v reverse 'o cl-foldl c (thunk (! swap Cons)) '() '$))

(do [c <- (ret (thunk (! colist<-list (list 1 2))))]
    [c^ <- (ret (thunk (! cl-map Ret c)))]
    (! list<-colist c^))
