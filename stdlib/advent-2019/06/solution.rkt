#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../table.rkt")
(require "../../Parse.rkt")

(provide main-a main-b)

;; Invariant: each planet orbits exactly one other

;; An Orbits is a
;;   U (Table Planet (Listof Planet)) where a planet is associated to all the planets that orbit it

;; A Planet is a
;;   String (of 3 characters)

(def-thunk (! parse-edge s)
  (! <<v apply
     (~
      (copat
       [(from1 from2 from3 paren to1 to2 to3)
        [from <- (! <<v list->string 'o List from1 from2 from3 '$)]
        [to <- (! <<v list->string 'o List to1 to2 to3 '$)]
        (! List from to)]))
     'o
     string->list s '$
     ))

(def-thunk (! count-orbits tbl planet dist-to-center)
  [nexts <- (! tbl 'get planet '())]
  [dist+1 <- (! + dist-to-center 1)]
  [loop
   = (~ (λ (next-planet) (! count-orbits tbl next-planet dist+1)))]
  (! <<v apply (~ (! + dist-to-center)) 'o map loop nexts '$))

(def-thunk (! main-a)
  [step = (~ (λ (orbits edge)
               (do [center <- (! first edge)]
                   [orbiter <- (! second edge)]
                 [def <- (! List orbiter)]
                 (! update orbits center def (~ (! Cons orbiter))))))]
  [tbl <- (! <<n cl-foldl^ step empty-table 'o cl-map parse-edge slurp-lines~ '$)]
  (! count-orbits tbl "COM" 0))

(def-thunk (! main-b)
  (ret 'not-done-yet))
