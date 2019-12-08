#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")
(require "../Intcode.rkt")

(provide main-a main-b)

(def-thunk (! remove-loop sigil prev xs)
  [x <- (! first xs)] [xs <- (! rest xs)]
  (cond [(! equal? x sigil) (! append prev xs)]
        [else [prev <- (! Cons x prev)]
              (! remove-loop sigil prev xs)]))

(def-thunk (! remove x xs) (! remove-loop x '() xs))

;; Listof A -> CoList (List A (Listof A))
;; lazily generate all pairs of an element of a list and the list with that element removed
(def-thunk (! plucks xs)
  [with-removed
    = (~ (λ (x) (! <<v List x 'o remove x xs '$)))]
  (! cl-map with-removed (~ (! colist<-list xs))))

;; Listof A -> CoList (Listof A)
;; lazily generate all of the different permutations of a list
(def/copat (! permutations)
  [((= '())) (! cl-cons '() cl-nil)]
  [(xs)
   (! cl-bind (~ (! plucks xs))
      (~ (λ (x*xs)
           (do [x <- (! first x*xs)] [xs <- (! second x*xs)]
             (! cl-map (~ (! Cons x)) (~ (! permutations xs)))))))])

(def/copat (! run-intcode-flow prog input)
  [((= '()) #:bind) (ret input)]
  [(phase-settings #:bind)
   [cur-setting <- (! first phase-settings)]
   [phase-settings <- (! rest phase-settings)]
   [cur-inputs <- (! List cur-setting input)]
   [output <- (! run-intcode-program prog cur-inputs)]
   (! run-intcode-flow prog output phase-settings)])

(def-thunk (! main-a)
  [prog <- (! parse-intcode-program)]
  [combos = (~ (! permutations '(0 1 2 3 4)))]
  (! <<n maximum 'o cl-map (~ (! run-intcode-flow prog 0)) combos '$))

(def-thunk (! main-b)
  (ret 'not-done-yet))
