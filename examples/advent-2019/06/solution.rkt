#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/IO)
(require sbpv/stdlib/CoList)
(require sbpv/stdlib/Table)
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

;; An Orbiter->Orbitee Map is a
;;   U (Table Planet Planet)

(def-thunk (! transitive-orbits orbits start so-far)
  (cond [(! equal? start "COM") (! Cons "COM" so-far)]
        [else
         [next <- (! orbits 'get start #f)]
         (! <<v transitive-orbits orbits next 'o Cons start so-far '$)]))

(def/copat (! remove-common-prefix)
  [((= '()) ys) (! List '() ys)]
  [(xs (= '())) (! List xs '())]
  [(xs ys)
   [x <- (! first xs)] [xs^ <- (! rest xs)]
   [y <- (! first ys)] [ys^ <- (! rest ys)]
   (cond [(! equal? x y) (! remove-common-prefix xs^ ys^)]
         [else (! List xs ys)])])

(def-thunk (! main-b)
  [step = (~ (λ (orbits edge)
               (do [center <- (! first edge)]
                   [orbiter <- (! second edge)]
                 (! orbits 'set orbiter center))))]
  [tbl <- (! <<n cl-foldl^ step empty-table 'o cl-map parse-edge slurp-lines~ '$)]
  [san-orbits <- (! transitive-orbits tbl "SAN" '())]
  [you-orbits <- (! transitive-orbits tbl "YOU" '())]
  [paths <- (! remove-common-prefix san-orbits you-orbits)]
  (! idiom (~ (ret (~ (! + -2))))
     (~ (! <<v length 'o first paths '$))
     (~ (! <<v length 'o second paths '$))))
