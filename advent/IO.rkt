#lang sbpv

(require "../stdlib.rkt")
(provide slurp-lines)

(define-rec-thunk (! slurp-lines)
  (copat
   [(#:bind) (! slurp-lines '())]
   [(so-far)
    (do [l <- (! read-line)]
        (cond
          [(! .v not string? l) (ret so-far)]
          [#:else (! slurp-lines (cons l so-far))]))]))
