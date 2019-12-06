#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)

(def-thunk (! mk-direction dir num) (! List dir num))

(def/copat (! parse-chars)
  [((= 'start-line) wires #:bind)
   (! reverse wires)]
  [((= 'start-line) wires)
   ;; (! displayall 'start wires)
   (! parse-chars 'dir wires '())]
  [((= 'dir) wires dirs dir)
   ;; (! displayall 'dir wires dirs dir)
   (! parse-chars 'length wires dirs dir '())]
  [((= 'length) wires dirs dir digits (= #\newline))
   ;; (! displayall 'len wires dirs dir digits 'newline)
   [direction <- (! <<v mk-direction dir 'o parse-num 'o reverse digits '$)]
   [wires <- (! <<v swap Cons wires 'o reverse 'o Cons direction dirs '$)]
   (! parse-chars 'start-line wires)]
  [((= 'length) wires dirs dir digits (= #\,))
   ;; (! displayall 'len wires dirs dir digits 'comma)
   [direction <- (! <<v mk-direction dir 'o parse-num 'o reverse digits '$)]
   [dirs <- (! Cons direction dirs)]
   (! parse-chars 'dir wires dirs)]
  [((= 'length) wires dirs dir digits digit)
   ;; (! displayall 'len wires dirs dir digits digit)
   [digits <- (! Cons digit digits)]
   (! parse-chars 'length wires dirs dir digits)]
  [()
   ;; (! displayall 'init)
   (! parse-chars 'start-line '())])

;; 
(def-thunk (! parse-input)
  [cs <- (! <<n list<-colist 'o read-all-chars '$)]
  (! apply parse-chars cs))

(def-thunk (! main-a)
  (! parse-input))

(def-thunk (! main-b)
  (ret 'not-done-yet))
