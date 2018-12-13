#lang sbpv

(require "../stdlib.rkt")

(define DIGITS (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define-rec-thunk (! digit<-char-loop c n ds)
  (do [d <- (! car ds)]
      [ds <- (! cdr ds)]
    (ifc (! equal? c d)
         (ret n)
         (do [n <- (! + 1 n)]
             (! digit<-char-loop c n ds)))))

(define-thunk (! digit<-char c)
  (! digit<-char-loop c 0 DIGITS))

;; ds should be a list of digits: characters \0 to \9
(define-rec-thunk (! parse-num-loop acc ds)
  (ifc (! null? ds)
       (ret acc)
       (do [d  <- (! car ds)]
           [ds <- (! cdr ds)]
         [n <- (! digit<-char d)]
         [acc <- (! .v
                    (thunk (! + n))
                    (thunk (! * 10))
                    acc)]
         [(! parse-num-loop acc ds)])))
(define-thunk (! parse-num ds)
  (! parse-num-loop 0 ds))

(define-thunk (! num<-sgn sgn)
  (ifc (! equal? #\+ sgn)
       (ret 1)
       (ret -1)))

(define-rec-thunk (! parse s)
  (do [l <- (! string->list s)]
      [sgn <- (! car l)]
    [n <- (! .v parse-num cdr l)]
    (! .v (thunk (! * n)) num<-sgn sgn)
    ))

; solution for part 1
(define-rec-thunk (! loop acc)
  (do [x <- (! read-line)]
    (ifc (! .v not string? x)
         (ret acc)
         (do [n <- (! parse x)]
;             [_ <- (! displayln n)]
             [acc <- (! + acc n)]
           (! loop acc)))))
; (! loop 0)

(define-rec-thunk (! read-nums)
  (copat
   [(#:bind) (! read-nums '())]
   [(acc)
    (do [x <- (! read-line)]
    (ifc (! .v not string? x)
         (! reverse acc)
         (do [n <- (! parse x)]
             (! read-nums (cons n acc)))))]))

;; A colist is a codata
;; codata Colist A where
;;   'hd |- F A
;;   'tl |- F (Cons A (U Colist))
(define-rec-thunk (! cycle^ og-hd og-tl cur nexts)
  (copat
   [((= 'hd) #:bind) (ret cur)]
   [((= 'tl) #:bind)
    (ifc (! null? nexts)
         (ret (thunk (! cycle^ og-hd og-tl og-hd og-tl)))
         (do [next <- (! car nexts)]
             [nexts <- (! cdr nexts)]
           (ret (thunk (! cycle^ og-hd og-tl next nexts)))))]))

(define-thunk (! cycle lst)
  (do [hd <- (! car lst)]
      [tl <- (! cdr lst)]
    (ret (thunk (! cycle^ hd tl hd tl)))))

;; codata Set A where
;;   'member? |- A -> F bool
;;   'add     |- A -> FU (Set A)

(define-rec-thunk (! set<-hash h)
  (copat
   [((= 'member?) x #:bind)
    (! hash-has-key? h x)]
   [((= 'add) x #:bind)
    (do [h <- (! hash-set h x #t)]
        (ret (thunk (! set<-hash h))))]
   [((= 'debug) #:bind) (! .v displayln hash-count h)]))

(define-rec-thunk (! search nums running seen)
  (do ;[_ <- (! seen 'debug)]
      [cur <- (! nums 'hd)]
      [new-running <- (! + cur running)]
    (ifc (! seen 'member? new-running)
         (ret new-running)
         (do [nexts <- (! nums 'tl)] ;[_ <- (! displayln new-running)]
             [seen  <- (! seen 'add new-running)]
             (! search nexts new-running seen)))))
(define-thunk (! singleton x)
  (do [h <- (! hash x #t)]
      (ret (thunk (! set<-hash h)))))

(define-rec-thunk (! part2)
  (do [nums <- (! read-nums)]
      [knotted <- (! cycle nums)]
    [seen <- (! singleton 0)]
    (! search knotted 0 seen)
    ))
; (! part2)

