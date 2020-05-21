#lang fiddle

(require fiddle/prelude)
(require "../../Set.rkt")
(require "../../Stream.rkt")
(require "../../Parse.rkt")

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
;; (! part2)
