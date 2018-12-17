#lang sbpv

(require "../stdlib.rkt")
(provide slurp-lines read-all-chars)

;; CoList Char
;; lazily read in stdin as a CoList
(define-rec-thunk (! read-all-chars)
  (do [c <- (! read-char)]
      (cond
        [(! eof-object? c) (ret '(nil))]
        [#:else (ret (list 'cons c (thunk (! read-all-chars))))])))

(define-rec-thunk (! slurp-lines)
  (copat
   [(#:bind) (! slurp-lines '())]
   [(so-far)
    (do [l <- (! read-line)]
        (cond
          [(! eof-object? l) (ret so-far)]
          [#:else (! slurp-lines (cons l so-far))]))]))
