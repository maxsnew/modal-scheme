#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

(def-thunk (! assert th msg)
  (cond [(! th) (ret '())]
        [else (error msg)]))

;; A Position is a (List 'position (List Int Int))
(def-thunk (! pos-x) (! <<v first  'o second))
(def-thunk (! pos-y) (! <<v second 'o second))
(def-thunk (! mk-pos x y) (! List 'position (list x y)))
;; A Velocity is a (List 'velocity Int Int)
(def-thunk (! vel-x) (! second))
(def-thunk (! vel-y) (! third))
(def-thunk (! mk-vel dx/dt dy/dt) (! List 'velocity dx/dt dy/dt))

;; A Light is a (List 'light Position Velocity)
(def-thunk (! light-pos) (! second))
(def-thunk (! light-vel) (! third))
(def-thunk (! mk-light p v) (! List 'light p v))

(def/copat (! parse-space-num)
  [((= #\space)) (! dot-args parse-num)]
  [() (! dot-args parse-num)])
(def/copat (! parse-light)
  [(p o s i t i o n = < (upto     px #\,) (= #\space) (upto     py #\>) (= #\space)
    v e l o c i t y = < (upto dx/dt #\,) (= #\space) (upto dy/dt #\>))
   [xydxdy <- (! map (~ (! apply parse-space-num)) (list px py dx/dt dy/dt))]
   (! @>> xydxdy
      (~ (copat
          [(x y dx/dt dy/dt)
           [p <- (! mk-pos x y)] [v <- (! mk-vel dx/dt dy/dt)]
           (! mk-light p v)])))])

(def-thunk (! lights)
  (! <<n cl-map (~ (! apply parse-light)) 'o cl-map string->list 'o slurp-lines~))

(def-thunk (! main-a)
  (! <<n cl-foreach displayln 'o lights))

(def-thunk (! main-b)
  (ret 'not-done-yet))
