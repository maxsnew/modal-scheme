#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")
(require "../FlexVec.rkt")

(provide parse-intcode-program)

;; Char ->* F (Listof Number)
(def/copat (! parse-chars)
  [((= 'loop) nums digits (= #\newline))
   [num <- (! <<v parse-num 'o reverse digits '$)]
   (! <<v reverse 'o Cons num nums '$)]
  [((= 'loop) nums digits (= #\,))
   [num <- (! <<v parse-num 'o reverse digits '$)]
   [nums <- (! Cons num nums)]
   (! parse-chars 'loop nums '())]
  [((= 'loop) nums digits c)
   [digits <- (! Cons c digits)]
   (! parse-chars 'loop nums digits)]
  )

;; Parses Intcode program 
(def-thunk (! parse-intcode-program)
  [chars <- (! <<n list<-colist 'o read-all-chars '$)]
  (! apply (~ (! parse-chars 'loop '() '())) chars))
