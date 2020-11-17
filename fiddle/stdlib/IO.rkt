#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/CoList)
(provide slurp-lines!
         slurp-lines~
         read-all-chars
         display-all-to-file
         apply/cmd-line)

;; CoList Char
;; lazily read stdin
(define-rec-thunk (! read-all-chars-port p)
  (do [c <- (! read-char p)]
      (cond
        [(! eof-object? c)
         (! close-input-port p)
         (ret '(nil))]
        [#:else (ret (list 'cons c (thunk (! read-all-chars-port p))))])))

;; A ->? F B := (A -> F B) /*\ F B

;; Path ->? CoList Char
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

(def-thunk (! display-loop p)
  (! cl-foreach (~ (! swap display p))))

;; U(CoList String) -> Path -> F 1
(def-thunk (! display-all-to-file strs path)
  [p <- (! open-output-file path #:exists 'replace)]
  (! display-loop p strs)
  (! close-output-port p))

(def-thunk (! apply/cmd-line k)
  [args <- (! current-command-line-arguments)]
  (! apply/vector k args))
