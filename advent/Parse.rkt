#lang sbpv

(require "../stdlib.rkt")
(provide parse-num)

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

;; Parses a list of characters into a number
(define-thunk (! parse-num ds)
  (! parse-num-loop 0 ds))
