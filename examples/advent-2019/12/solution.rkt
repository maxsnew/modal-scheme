#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")
(require "../../Set.rkt")

(provide main-a main-b)

;; input is small enough to manually parse
;; <x= -16, y= 15, z= -9>
;; <x= -14, y= 5, z= 4>
;; <x= 2, y= 0, z= 6>
;; <x= -3, y= 18, z= 9>

;; A planet-state is a List Posn Velocity
;; A Posn     is a List Num Num Num
;; A Velocity is a List Num Num Num

(define input
  '(((-16 15 -9) (0 0 0))
    ((-14  5  4) (0 0 0))
    ((  2  0  6) (0 0 0))
    (( -3 18  9) (0 0 0))))

(def-thunk (! map! f (rest ls))
  [cls <- (! map (~ (λ (l) (ret (~ (! colist<-list l))))) ls)]
  (! list<-colist (~ (! apply (~ (! cl-map f)) cls))))

(def-thunk (! pull from to)
  (cond [(! = from to) (ret  0)]
        [(! < from to) (ret  1)]
        [(! > from to) (ret -1)]))

(def-thunk (! apply-gravity system planet)
  [pull-one-planet
   = (~ (λ (planet posn)
          (do
              [planet-posn <- (! first planet)]
              [vel-delta <- (! map! pull planet-posn posn)]
              [planet-vel <- (! second planet)]
              [planet-vel <- (! map! + vel-delta planet-vel)]
              (! List planet-posn planet-vel)
            )))]
  (! <<n
     cl-foldl^ pull-one-planet planet 'o
     cl-map first 'o
     cl-filter (~ (! <<v not 'o equal? planet)) 'o
     colist<-list system '$)
  )

(def-thunk (! apply-velocity planet)
  [posn <- (! first planet)] [vel <- (! second planet)]
  [posn <- (! map! + posn vel)]
  (! List posn vel))

;; System -> System
(def-thunk (! evolve system)
  (! <<n list<-colist 'o
        cl-map apply-velocity 'o 
        cl-map (~ (! apply-gravity system)) 'o
        colist<-list system '$)
  )

(def-thunk (! simulate-system state)
  [step = (~(λ (state) (do [new-state <- (! evolve state)] (! Cons state new-state))))]
  (! cl-unfold step state))

(define sample-0
  '(((-1 0 2) (0 0 0))
    ((2 -10 -7) (0 0 0))
    ((4 -8 8) (0 0 0))
    ((3 5 -1) (0 0 0))))

(define sample-1
  '(((-8 -10 0) (0 0 0))
    ((5 5 10) (0 0 0))
    ((2 -7 3) (0 0 0))
    ((9 -8 -3) (0 0 0))))

(def-thunk (! planet-energy planet)
  [posn <- (! first planet)] [vel <- (! second planet)]
  [potential <- (! <<v apply + 'o map abs posn '$)]
  [kinetic <- (! <<v apply + 'o map abs vel '$)]
  (! * potential kinetic))

(def-thunk (! sys-energy system)
  (! <<n cl-foldl^ + 0 'o
     cl-map planet-energy 'o
     colist<-list system))

(def-thunk (! energies-of-system system)
  (! <<n
     cl-map sys-energy 'o
     simulate-system system '$))

(def-thunk (! main-a)
  (! <<n cl-foreach displayall 'o
     cl-zipwith
     (~ (! range 0 1001))
     (~ (! energies-of-system input)) '$))

(def-thunk (! find-repeat extract xs)
  [step
   = (~ (λ (x k seen)
          (do [y <- (! extract x)]
              (cond [(! seen 'member? y)
                     (ret x)]
                    [else
                     [seen <- (! seen 'add y)]
                     (! k seen)]))
          ))]
  (! cl-foldr xs step (~ (λ (set) (ret #f)))
     empty-set))

(def-thunk (! sub-input extract system)
  [l-of-extract = (~ (! .v List extract))]
  (! map (~ (! map l-of-extract)) system))

(def-thunk (! numbered xs)
  (! cl-zipwith (~ (! range 0 +inf.0)) xs))

(def-thunk (! main-b)
  [x-sys <- (! sub-input first input)]
  [y-sys <- (! sub-input second input)]
  [z-sys <- (! sub-input third input)]
  [x-rep <- 186028 ;; computed already
         ;(! <<n find-repeat second 'o numbered 'o simulate-system x-sys '$)
         ]
  (! displayall x-rep)
  [y-rep <- 56344
         ; (! <<n find-repeat second 'o numbered 'o simulate-system y-sys '$)
         ]
  (! displayall y-rep)
  [z-rep <- (! <<n find-repeat second 'o numbered 'o simulate-system z-sys '$)]
  (! displayall z-rep)
  (! idiom^ lcm (~ (! first x-rep)) (~ (! first y-rep))(~ (! first z-rep))))
