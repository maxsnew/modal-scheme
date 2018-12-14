#lang sbpv

(require "../stdlib.rkt")
(provide cycle)

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
