#lang sbpv

(require "../stdlib.rkt")
(require "CoList.rkt")
(provide slurp-lines! slurp-lines~ read-all-chars)

;; CoList Char
;; lazily read in stdin as a CoList
(define-rec-thunk (! read-all-chars)
  (do [c <- (! read-char)]
      (cond
        [(! eof-object? c) (ret '(nil))]
        [#:else (ret (list 'cons c (thunk (! read-all-chars))))])))

;; CoList String
(def-thunk (! slurp-lines~)
  (do [l <- (! read-line)]
      (cond [(! eof-object? l) (! cl-nil)]
            [else (! cl-cons l (~ (! slurp-lines~)))])))

;; F List String
(def-thunk (! slurp-lines!)
  (! list<-colist slurp-lines~))
