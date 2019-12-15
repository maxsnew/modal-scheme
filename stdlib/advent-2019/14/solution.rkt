#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)


;; solution outline:
;
;; think of a reaction as a term in a free symmetric semicartesian
;; monoidal category generated on objects by the chemicals and on
;; terms by the generators we are given
;
;; The puzzle is then, given a presentation of a free symmetric
;; monoidal category, to find the minimal n such that there is a
;; morphism Ore^n -> FUEL
;
;; It is semi-cartesian because we allow weakening: our final reaction
;; can have discard unused by-products.
;
;; There are some simplifying assumptions that make this easier than
;; the general case. First, the generators always output 1 sort of
;; chemical (though possibly many times), and each chemical has
;; exactly one corresponding generator
;
;; I think a strategy that then works is to topologically sort the
;; chemicals where the ordering is the least such that A <= B if every
;; morphism Ore^m => B^n uses the generator for A as a subterm.
;
;; Then once they're sorted we can construct the optimal term by
;; starting with the generator for FUEL: Gamma |- 1 Fuel, we
;; iteratively pick the input type that is highest in the topological
;; ordering, substitute in its generator (using weakening if needed)
;; and repeat until all inputs are ORE.
;
;; the topological ordering is the key to letting us get away with
;; this easy use of weakening without sacrificing minimality. If we
;; picked an input A that was <= a B in the ordering, and we weakened
;; away the extra As, then we might end up with a non-minimal solution
;; because those excess As might have contributed to constructing a
;; B. With the topological ordering, we know that we can discard any
;; excess Bs because they won't help in constructing the other inputs

;; A Sequent is a tuple
;; (Context Number Chemical)

(def-thunk (! mk-sequent ctx num outp) (! List ctx num outp))
(define sequent-ctx first)
(define sequent-num second)
(define sequent-outp third)

;; A Chemical is a string
;; A Context is a Listof (List Number Chemical)

;; STEP 1: parsing
;; The input is a Listof Sequents

(def/copat (! parse-num*chemical k)
  [((upto digits #\space))
   (! slurp-satisfying letter? (~ (copat
   [(letters)
    [num <- (! parse-num digits)]
    [chem <- (! list->string letters)]
    (! k num chem)])))])

(def-thunk (! parse-ctx chems k)
  (! parse-num*chemical (~ (copat
  [(num chem (= #\,) (= #\space))
   [chems <- (! <<v swap Cons chems 'o List num chem '$)]
   (! parse-ctx chems k)]
  [(num chem (= #\space) (= #\=) (= #\>) (= #\space))
   [chems <- (! <<v swap Cons chems 'o List num chem '$)]
   (! k chems)]
  [() (! displayall 'parse-error)]))))

;; Char ->* F Sequent
(def-thunk (! parse-sequent)
  (! parse-ctx '() (~ (λ (ctx)
  (! parse-num*chemical (~ (λ (num chem)
  (do
      (! <<v mk-sequent ctx num chem)))))))))

;; F (Listof Sequent)
(def-thunk (! parse-generators file)
  (! <<n
     list<-colist 'o
     cl-map (~ (! <<v apply parse-sequent 'o string->list)) 'o
     slurp-lines~ file '$)
  )

;; STEP 2: sorting

;; We need to develop a sorted list of all the chemicals so we can
;; quickly check if one is less than another

;; STEP 3: substituting

(def-thunk (! main-a)
  (! parse-generators "/dev/stdin"))

(def-thunk (! main-b)
  (ret 'not-done-yet))
