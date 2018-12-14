#lang sbpv

(require "../stdlib.rkt")
(require "Stream.rkt")

(define DIGITS (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define-rec-thunk (! digit<-char-loop c n ds)
  (do [d <- (! car ds)]
      [ds <- (! cdr ds)]
    (cond
      [(! equal? c d) (ret n)]
      [#:else
       (do [n ~ (! + 1 n)]
           (! digit<-char-loop c n ds))])))

(define-thunk (! digit<-char c)
  (! digit<-char-loop c 0 DIGITS))

;; ds should be a list of digits: characters \0 to \9
(define-rec-thunk (! parse-num-loop acc ds)
  (cond
    [(! null? ds) (ret acc)]
    [#:else
     (do [d  <- (! car ds)]
         [ds <- (! cdr ds)]
       [n <- (! digit<-char d)]
       [acc <- (! <<v + n 'o * 10 acc '$)]
       [(! parse-num-loop acc ds)])]))
(define-thunk (! parse-num ds)
  (! parse-num-loop 0 ds))

(define-thunk (! num<-sgn sgn)
  (ifc (! equal? #\+ sgn)
       (ret 1)
       (ret -1)))

(define-rec-thunk (! parse s)
  (do [l <- (! string->list s)]
      [sgn <- (! car l)]
    [n <- (! <<v parse-num 'o cdr l '$)]
    (! <<v * n 'o num<-sgn sgn '$)))

; solution for part 1
(define-rec-thunk (! loop acc)
  (do [x <- (! read-line)]
      (cond
        [(! .v not string? x) (ret acc)]
        [#:else
         (do [n <- (! parse x)]
             [acc <- (! + acc n)]
           (! loop acc))])))
; (! loop 0)

(define-rec-thunk (! read-nums)
  (copat
   [(#:bind) (! read-nums '())]
   [(acc)
    (do [x <- (! read-line)]
        (cond
          [(! .v not string? x) (! reverse acc)]
          [#:else
           (do [n <- (! parse x)]
               (! read-nums (cons n acc)))]))]))

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
    (cond
      [(! seen 'member? new-running)
       (ret new-running)]
      [#:else
       (do [nexts <- (! nums 'tl)]
           ;[_ <- (! displayln new-running)]
           [seen  <- (! seen 'add new-running)]
         (! search nexts new-running seen))])))
(define-thunk (! singleton x)
  (do [h <- (! hash x #t)]
      (ret (thunk (! set<-hash h)))))

(define-rec-thunk (! part2)
  (do [nums <- (! read-nums)]
      [knotted <- (! cycle nums)]
    [seen <- (! singleton 0)]
    (! search knotted 0 seen)
    ))
(! part2)
