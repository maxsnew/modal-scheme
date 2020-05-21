#lang fiddle

(require "../../stdlib.rkt")
(require "../CoList.rkt")

;; If the integers from 1 to 999,999,999 are written as words, sorted
;; alphabetically, and concatenated, what is the 51 billionth letter?

;; The code by Shan & Thurston makes heavy use of Haskell
;; typeclasses. We'll monomorphise instead.

;; Also, we'll use CBName, whereas Haskell is lazy and CBNeed, so
;; we'll see what issues.

;; We'll also use Scheme-style var-arg functions a lot.

;; This time we're just laying the groundwork by constructing some
;; lazy lists using Seminearring structure.
;; word-list : CoList String

;; + : U(CoList A) -> ... -> CoList A
;; appends all of them
(def-thunk (! ++) (! cl-append*))

;; Concatenate
;; ! ** xss yss = [ xs ++ ys | xs <- xss, ys <- yss ]
;; U(CoList String) -> U(CoList String) -> CoList String
(def-thunk (! **2 xss yss)
  (! <<n cl-map string-append 'o cartesian-product xss yss))
;; CoList String
(def-thunk (! **0) (! cl-single ""))

;; U(CoList String) -> ... -> CoList String
(def/copat (! ** (rest args))
  (! <<n cl-foldr **2 **0 'o colist<-list args))

(def-thunk (! ten1)
  (! colist<-list '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
(def-thunk (! prefixes) (! colist<-list '("fif six seven eigh nine")))
(def-thunk (! ten2)
  (! ++
     ten1
     (~ (! colist<-list '("ten eleven twelve")))
     (~ (! **
           (~ (! ++ (! colist<-list '("thir" "four")) prefixes))
           (~ (! cl-single "teen"))))
     (~ (! **
           (...)
           (~ (! ))
           (~ (! ++ **0 ten1))))
     ))
