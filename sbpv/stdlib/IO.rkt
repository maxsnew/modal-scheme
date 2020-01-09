#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/CoList)
(provide slurp-lines! slurp-lines~ read-all-chars)

;; CoList Char
;; lazily read stdin
(define-rec-thunk (! read-all-chars-port p)
  (do [c <- (! read-char p)]
      (cond
        [(! eof-object? c)
         (! close-input-port p)
         (ret '(nil))]
        [#:else (ret (list 'cons c (thunk (! read-all-chars-port p))))])))

(def/copat (! read-all-chars)
  [(#:bind)
   [p <- (! open-input-file "/dev/stdin")]
   (! read-all-chars-port p)]
  [(name #:bind)
   [p <- (! open-input-file name)]
   (! read-all-chars-port p)])

(def-thunk (! slurp-lines-port~ p)
  (do [l <- (! read-line p)]
      (cond [(! eof-object? l)
             (! close-input-port p)
             (! cl-nil)]
            [else (! cl-cons l (~ (! slurp-lines-port~ p)))])))

;; CoList String
(def-thunk (! slurp-lines~)
  [slurp-file
   = (~ (λ (name)
          (do [p <- (! open-input-file name)]
              (! slurp-lines-port~ p))))]
  (copat
   [(#:bind) (! slurp-file "/dev/stdin")]
   [(name #:bind) (! slurp-file name)]))


;; F List String
(def-thunk (! slurp-lines! (rest args))
  (! list<-colist (~ (! apply slurp-lines~ args))))
