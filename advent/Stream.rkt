#lang sbpv

(require "../stdlib.rkt")
(provide cycle stream-ref stream<-list)

;; codata Stream A where
;;   'hd |- F A
;;   'tl |- F (Cons A (U Colist))
(define-rec-thunk (! cycle^ og-hd og-tl cur nexts)
  (copat
   [((= 'hd) #:bind) (ret cur)]
   [((= 'tl) #:bind)
    (cond
      [(! null? nexts)
       (ret (thunk (! cycle^ og-hd og-tl og-hd og-tl)))]
      [#:else
       (do [next <- (! car nexts)]
           [nexts <- (! cdr nexts)]
         (ret (thunk (! cycle^ og-hd og-tl next nexts))))])]))

(define-thunk (! cycle lst)
  (do [hd <- (! car lst)]
      [tl <- (! cdr lst)]
    (ret (thunk (! cycle^ hd tl hd tl)))))

(define-rec-thunk (! stream-ref s n)
  (cond [(! equal? n 0) (! s 'hd)]
        [#:else
         (do [tl <- (! s 'tl)]
             [n-1 <- (! - n 1)]
           (! stream-ref tl n-1))]))

(define-rec-thunk (! stream<-list l)
  (copat
   [((= 'hd)) (! car l)]
   [((= 'tl))
    (do [tl <- (! cdr l)] (ret (thunk (! stream<-list tl))))]))

(define-rec-thunk (! stream<-colist c)
  (copat
   [((= 'hd)) (! <<v car 'o c)]
   [((= 'tl))
    (do [tl <- (! <<v cdr 'o c)] (ret (thunk (! stream<-list tl))))]))

; Nat -> U Stream A -> CoList A
(define-rec-thunk (! take n s)
  (cond
    [(! zero? n) (ret '())]
    [#:else
     (do [n-1 <- (! - n 1)]
         [hd <- (! s 'hd)]
       [tl <- (! s 'tl)]
       (ret (list 'cons hd (thunk (! take n-1 tl)))))]))
