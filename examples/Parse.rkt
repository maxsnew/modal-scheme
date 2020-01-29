#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/CoList)
(provide parse-num upper-case? lower-case? letter? UPPERS digit<-char
         slurp-satisfying)

(define UPPERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define DOWNERS "abcdefghijklmnopqrstuvwxyz")
(define LETTERS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define DIGITS (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))


(def-thunk (! upper-case? c)
  [n <- (! char->integer c)]
  (! and (~ (! <= 65 n)) (~ (! <= n 90))))

(def-thunk (! lower-case? c)
  [n <- (! char->integer c)]
  (! and (~ (! <= 97 n)) (~ (! <= n 122))))

(def-thunk (! letter? c) (! or (~ (! upper-case? c)) (~ (! lower-case? c))))

(define-rec-thunk (! digit<-char-loop c n ds)
  (do [d <- (! car ds)]
      [ds <- (! cdr ds)]
    (cond
      [(! equal? c d) (ret n)]
      [#:else
       (do [n <- (! + 1 n)]
           (! digit<-char-loop c n ds))])))

(def-thunk (! digit<-char c)
  (! digit<-char-loop c 0 DIGITS))

;; ds should be a list of digits: characters \0 to \9
(def-thunk (! parse-num-loop acc)
  (copat
   [(#:bind) (ret acc)]
   [(d) [n <- (! digit<-char d)]
        [acc <- (! <<v + n 'o * 10 acc '$)]
        (! parse-num-loop acc)]))
#;
(define-rec-thunk (! parse-num-loop acc ds)
  (cond
    [(! null? ds) (ret acc)]
    [#:else
     (do [d  <- (! car ds)]
         [ds <- (! cdr ds)]
       [n <- (! digit<-char d)]
       [acc <- (! <<v + n 'o * 10 acc '$)]
       [(! parse-num-loop acc ds)])]))

(def/copat (! parse-num^)
  [((= #\+)) (! parse-num-loop 0)]
  [((= #\-)) (! <<v - 'o parse-num-loop 0)]
  [() (! parse-num-loop 0)])

;; Parses a list of characters into a natural number
;; Listof Char -> F Nat
(define-thunk (! parse-num ds)
  (! apply parse-num^ ds))

(def/copat (! slurp-sat-loop k p? acc)
  [(#:bind)
   [out <- (! reverse acc)]
   (! k out)]
  [(x)
   (cond [(! p? x)
          [acc <- (! Cons x acc)]
          (! slurp-sat-loop k p? acc)]
         [else
          [out <- (! reverse acc)]
          (! k out x)])])

(def-thunk (! slurp-satisfying p? k) (! slurp-sat-loop k p? '()))
