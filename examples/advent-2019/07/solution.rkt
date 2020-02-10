#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/IO)
(require sbpv/stdlib/CoList)
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

(def/copat (! driver-a inps)
  [((= 'input) k)
   [input <- (! first inps)] [inps <- (! rest inps)]
   (! k input (~ (! driver-a inps)))]
  [((= 'output) o _k) (ret o)])

(def/copat (! run-intcode-flow prog input)
  [((= '()) #:bind) (ret input)]
  [(phase-settings #:bind)
   [cur-setting <- (! first phase-settings)]
   [phase-settings <- (! rest phase-settings)]
   [cur-inputs <- (! List cur-setting input)]
   [output <- (! interp-intcode-program prog (~ (! driver-a cur-inputs)))]
   (! run-intcode-flow prog output phase-settings)])

(def-thunk (! main-a)
  [prog <- (! parse-intcode-program)]
  [combos = (~ (! permutations '(0 1 2 3 4)))]
  (! <<n maximum 'o cl-map (~ (! run-intcode-flow prog 0)) combos '$))

(def/copat (! two-inputs-then-output-then-save-input-req in1 in2 thenK)
  [((= 'input) inK)
   (! inK in1
      (~ (copat
  [((= 'input) inK)
   (! inK in2
      (~ (copat
  [((= 'output) o resumeK)
   (! resumeK
      (~ (copat
  [((= 'input) inK)
   (! thenK o inK)])))])))])))])

(def/copat (! feedback-loop)
  [((= 'init) inp threads)
   [thread <- (! first threads)] [threads <- (! rest threads)]
   (! thread inp (~ (copat
   [((= 'output) o resumeK)
   (! resumeK (~ (copat
   [((= 'input) inK)
    [old-threads <- (! List inK)]
    (! feedback-loop 'input-loop o old-threads threads)]
   [((= 'halt))
    (! feedback-loop 'halt-loop o threads)])))])))]
  [((= 'input-loop) inp old-threads (= '()))
   [old-threads <- (! reverse old-threads)]
   (! feedback-loop 'init inp old-threads)]
  [((= 'input-loop) inp old-threads threads)
   [thread <- (! first threads)] [threads <- (! rest threads)]
   (! thread inp (~ (copat
   [((= 'output) o resumeK) (! resumeK (~ (copat
   [((= 'input) inK)
    [old-threads <- (! Cons inK old-threads)]
    (! feedback-loop 'input-loop o old-threads threads)])))])))]
  [((= 'halt-loop) inp (= '())) (ret inp)]
  [((= 'halt-loop) inp threads)
   [thread <- (! first threads)] [threads <- (! rest threads)]
   (! thread inp (~ (copat
   [((= 'output) o resumeK) (! resumeK (~ (copat
   [((= 'halt))
    (! feedback-loop 'halt-loop o threads)])))])))])

(def/copat (! run-intcode-loop syntax threads)
  [((= '()))
   [threads <- (! reverse threads)]
   (! feedback-loop 'init 0 threads)]
  [(phase-settings)
   [cur-setting <- (! first phase-settings)] [phase-settings <- (! rest phase-settings)]
   (! interp-intcode-program syntax (~ (copat
    [((= 'input) resumeK)
     (! resumeK cur-setting (~ (copat
    [((= 'input) thread)
     [threads <- (! Cons thread threads)]
     (! run-intcode-loop syntax threads phase-settings)])))])))])

(def-thunk (! main-b)
  [prog <- (! parse-intcode-program)]
  [combos = (~ (! permutations '(5 6 7 8 9)))]
  (! <<n maximum 'o cl-map (~ (λ (phases)
                                (do (! displayall 'launching phases)
                                    (! run-intcode-loop prog '() phases)))) combos '$))
