#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../Parse.rkt")
(require "../CoList.rkt")
(provide main-a main-b)

;; data FreeGroup A = List (Polar A) where there are no adjacent terms (+ x) (- x) or (- x) (+ x)
;; data Polar A = `(,Sign ,A)
;; data Sign = '+ or '-

; Letter -> FreeGroup UpperLetter
(define-thunk (! parse-atom)
  (copat
   [(c)
    (cond
      [(! upper-case? c) (ret (list '+ c))]
      [#:else (do [C <- (! char-upcase c)] (ret (list '- C)))])]))

(define-thunk (! starts-with? l x)
  (! and
     (~ (! <<v not 'o empty? l '$))
     (~ (! <<v equal? x 'o car l '$))))

(define-thunk (! polar-op p)
  (do [sgn <- (! first p)]
      [sgn-op <- (ifc (! equal? '+ sgn) (ret '-) (ret '+))]
    (! <<v List sgn-op 'o second p '$)))

; FreeGroup A -> Polar A -> FreeGroup A
(define-thunk (! free-act s x)
  (cond
    [(! <<v starts-with? s 'o polar-op x '$) (! cdr s)]
    [#:else (ret (cons x s))]))

(define-thunk (! main-a)
  (do [reduced <- (! <<n
                     cl-foldl^ free-act '() 'o
                     cl-map parse-atom 'o
                     cl-filter letter? 'o
                     read-all-chars '$)]
      (! length reduced)))
(define-thunk (! main-b) (ret 'not-done-yet))
