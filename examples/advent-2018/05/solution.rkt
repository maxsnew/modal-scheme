#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/IO)
(require "../../Parse.rkt")
(require sbpv/stdlib/CoList)
(provide main-a main-b)

(define-thunk (! log x)
  (do [_ <- (! displayln x)] (ret x)))

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

(define-thunk (! remove-and-reduce)
  (copat
   [(un-reduced c)
    (do [reduced <- (! <<n
                       cl-foldl^ free-act '() 'o
                       cl-filter (~ (λ (x) (! <<v not 'o equal? c 'o second x '$))) 'o
                       colist<-list un-reduced '$)]
        (! <<v log 'o List c 'o length reduced '$))]))

;; just did a visual inspection: the smallest answer is much smaller
;; so it's obvious

;; since removing a letter is a homomorphism (just deciding to send an
;; element and its inverse to the identity), you can reuse the reduced
;; word from part a.
(define-thunk (! main-b)
  (do [reduced <- (! <<n
                     cl-foldl^ free-act '() 'o
                     cl-map parse-atom 'o
                     cl-filter letter? 'o
                     read-all-chars '$)]
      [cs*counts
       <-
       (! <<n
          list<-colist 'o
          cl-map (~ (! remove-and-reduce reduced)) 'o
          (~ (! <<v colist<-list 'o string->list UPPERS '$)) '$)]
    (ret 'done)))
