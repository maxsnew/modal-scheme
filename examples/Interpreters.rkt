#lang sbpv

(require sbpv/prelude)

;; Let's write some cbpv interpreters in sbpv. Why interpreters?
;; Because they naturally lead to the use of user-defined effects, and
;; we want to figure out what a good abstraction of effects is in
;; CBPV.

;; First, we'll make a De-Bruijn style interpreter
;; DBIx = Nat
;; data Val  = Var DBIx | 'True | 'False | Thunk Comp
;; data Comp = If Val Comp Comp | Force Val | Lambda Comp | App Comp Val | Bind Comp Comp | Ret Val
;;
;; data SVal = Var DBIx | 'true | 'false | Closure Comp (List SVal)

(def/copat (! db-lookup)
  [((= 0) x) (! abort x)]
  [(n y)
   [n <- (! - n 1)]
   (! db-lookup n)])

;; SVal = Var Ix -- the effect
;;        \+/ 'true \+/ 'false
;;        \+/ Closure[SComp]
;; SComp = Args[Env[SVal]]
;; Args[B] := eos -> B /*\ SVal -> Args[B]
;; note that we have a "reader effect" given by the fact that arguments are passed on the stack
;; Env[A]  := F A /*\ SVal -> Env[A]
;; Closure[B] := UB * List SVal
;;   alternative definition: UB \+/ (A * Closure[B])
;;   (makes it clear the duality with Env
;;  Box Num ->
;;  ('v -> Val  -> Env[SVal]
;;  /*\
;;   'c -> Comp -> SComp)
(define eos 'end-of-object-language-stack)
(def/copat (! db-interp cell)
  ;; value cases
  [((= 'v) (cons (= 'var) (cons x _)))
   ;(! displayall 'case 'var)
   (! db-lookup x)]
  [((= 'v) (cons (= 'thunk) (cons c _)) (rest env))
   ;; (! displayall 'case 'thunk)
   (ret (list 'closure (~ (! db-interp cell 'c c)) env))]
  [((= 'v) v)
   ;; (! displayall 'case 'val)
   (! abort v)]
  ;; comp cases
  [((= 'c) (cons (= 'ret) (cons x _)) (upto _ eos))
   ;; (! displayall 'case 'ret)
   (! db-interp cell 'v x)]
  [((= 'c) (cons (= 'bind) (cons c (cons k _)))
           (upto args eos) (rest env))
   ;; (! displayall 'case 'bind)
   (do [th <- (! apply (~ (! apply (~ (! db-interp cell 'c c)) args eos)) env)]
       (! apply (~  (! apply (~ (! db-interp cell 'c k)) args eos th)) env))]
  [((= 'c) (cons (= 'force) (cons v _)) (upto args eos) (rest env))
   ;; (! displayall 'force v)
   (pm (! apply (~ (! db-interp cell 'v v)) env)
    [(cons (= 'closure) (cons thk (cons env _)))
     (! apply (~ (! apply thk args eos)) env)])]
  [((= 'c) (cons (= 'lambda) (cons c _)))
   (copat
    [(arg (upto args eos)) (! apply (~ (! db-interp cell 'c c)) args eos arg)]
    [() (! error 'db-interp "object lang expected an arg but there wasn't one")])]
  [((= 'c) (cons (= 'app) (cons c (cons v _))) (upto args eos) (rest env))
   [sv <- (! apply (~ (! db-interp cell 'v v)) env)]
   (! apply (~ (! apply (~ (! db-interp cell 'c c sv)) args eos)) env)]

  [((= 'c) (cons (= 'if) (cons v (cons ct (cons cf _))))
           (upto args eos) (rest env))
   [sv <- (! db-interp cell 'v v env)]
   [c <-
    (pm (ret sv)
        [(= 'true) (ret ct)]
        [(= 'false) (ret cf)]
        [_ (! error 'db-interp "called if on ~v" sv)])]
   (! apply (~ (! apply (~ (! db-interp cell 'c c)) args eos)) env)]
  [((= 'c) (cons (= 'beep) (cons c _)))
   (! displayall 'beep)
   (! db-interp cell 'c c)]
  [((= 'c) (cons (= 'inc) (cons c _)))
   (! <<v set-box! cell 'o + 1 'o unbox cell)
   (! db-interp cell 'c c)]
  [((= 'c) (cons (= 'dec) (cons c _)))
   (! <<v set-box! cell 'o swap - 1 'o unbox cell)
   (! db-interp cell 'c c)])

(def-thunk (! interpc c)
  [cell <- (! box 0)]
  [val <- (! db-interp cell 'c c eos)]
  [final-state <- (! unbox cell)]
  (ret (list 'val: val 'state: final-state)))

(define ex0
  (list 'bind (list 'ret 'true) (list 'ret (list 'var 0))))
(define ex1
  (list 'bind (list 'ret 'true)
        (list 'bind (list 'ret 'false)
              (list 'ret (list 'var 0)))))
(define ex2
  (list 'bind (list 'ret 'true)
        (list 'bind (list 'ret 'false)
              (list 'ret (list 'var 1)))))

(define thk-ex0
  '(ret (thunk (ret true))))
(define thk-ex1
  '(bind (bind (ret true) (ret (thunk (ret (var 0)))))
         (ret (var 0))))
(define thk-ex2
  '(bind (bind (ret true) (ret (thunk (ret (var 0)))))
         (force (var 0))))
(def/copat (! get-val)
  [((cons (= 'val:) (cons v (cons (= 'state:) (cons st _)))))
   (ret v)])

;; SVal  = Bool \+/ (St * [Closure[SComp]])
;; SComp = Args[St -> Env[SVal]]

;; SVal  = Bool \+/ Cons SVal SVal \+/ U SComp
;; SComp =           SVal -> SComp /*\ F SVal
;;   [[-]] -> B = eoG -> B
;; [[G,x]] -> B = SVal -> [[G]] -> B
;; db-interp2 'v : (G |- Val)  -> [[G]] -> F SVal
;; db-interp2 'c : (G |- Comp) -> [[G]] -> SComp

;; This one has the object language environment on the stack, using a
;; marker eoG to delimit the environment from the object lang stack.
;;
;; This has the disadvantage of requiring a lot of stack copying,
;; because the environment needs to be duplicated, so there's a lot of
;; "thrashing": the environment is repeatedly removed from the stack
;; and then placed back on top.
;; The advantage that it is sort of "pure lambda calculus", just
;; functions, no datatypes.
(define eoG 'end-of-environment)
(def/copat (! db-interp2)
  ;; value cases
  [((= 'v) (cons (= 'var) (cons x _)))
   ;(! displayall 'case 'var)
   (! db-lookup x)]
  [((= 'v) (cons (= 'thunk) (cons c _)) (rest env))
   ;; (! displayall 'case 'thunk)
   (ret (list 'thunk (~ (! apply (~ (! db-interp2 'c c)) env eoG))))]
  [((= 'v) v)
   ;; (! displayall 'case 'val)
   (! abort v)]
  ;; comp cases
  [((= 'c) (cons (= 'ret) (cons x _)) (upto env eoG))
   ;; (! displayall 'case 'ret)
   (copat
    [(#:bind) (! apply (~ (! db-interp2 'v x)) env)]
    [((rest args)) (! error 'db-interp2 "object lang function tried to return, but the stack was non-empty: ~v" args)])]
  [((= 'c) (cons (= 'bind) (cons c (cons k _)))
           (upto env eoG))
   ;; (! displayall 'case 'bind)
   [v <- (! apply (~ (! db-interp2 'c c)) env eoG)]
   (! apply (~ (! db-interp2 'c k v)) env eoG)]
  [((= 'c) (cons (= 'force) (cons v _)) (upto env eoG))
   ;; (! displayall 'force v)
   (pm (! apply (~ (! db-interp2 'v v)) env)
       [(cons (= 'thunk) (cons th _)) (! th)]
       [v (! error 'db-interp2 "tried to force a non-thunk: ~v" v)])]
  [((= 'c) (cons (= 'lambda) (cons c _)) (upto env eoG))
   (copat
    [(arg) (! apply (~ (! db-interp2 'c c arg)) env eoG)]
    [() (! error 'db-interp2 "object lang expected an arg but there wasn't one")])]
  [((= 'c) (cons (= 'app) (cons c (cons v _))) (upto env eoG))
   [sv <- (! apply (~ (! db-interp2 'v v)) env)]
   (! apply (~ (! db-interp2 'c c)) env eoG sv)]
  [((= 'c) (cons (= 'if) (cons v (cons ct (cons cf _))))
           (upto env eoG))
   [sv <- (! db-interp2 'v v env)]
   [c <- (pm (ret sv)
             [(= 'true) (ret ct)]
             [(= 'false) (ret cf)]
             [_ (! error 'db-interp2 "called if on ~v" sv)])]
   (! apply (~ (! db-interp2 'c c)) env eoG)]
  [((= 'c) (cons (= 'beep) (cons c _)))
   (! displayall 'beep)
   (! db-interp2 'c c)])
(def-thunk (! run2 c) (! db-interp2 'c c eoG) )

;; SVal  = Bool \+/ (St * [Closure[SComp]])
;; SComp = Args[St -> Env[SVal]]

;; SVal  = Bool \+/ Cons SVal SVal \+/ U SComp
;; SComp =           SVal -> SComp /*\ F SVal
;;   [[-]] = 'nil
;; [[G,x]] = (cons SVal [[G]])
;; db-interp3 'v : (G |- Val)  -> [[G]] -> F SVal
;; db-interp3 'c : (G |- Comp) -> [[G]] -> SComp

;; This is a simpler implementation using environments represented as
;; a list. This has the performance advantage that 
(def/copat (! db-interp3)
  ;; value cases
  [((= 'v) (cons (= 'var) (cons x _)))
   ;(! displayall 'case 'var)
   (! apply (~ (! db-lookup x)))]
  [((= 'v) (cons (= 'thunk) (cons c _)) env)
   ;; (! displayall 'case 'thunk)
   (ret (list 'thunk (~ (! db-interp3 'c c env))))]
  [((= 'v) v env) (ret v)]
  ;; comp cases
  [((= 'c) (cons (= 'ret) (cons x _)) env)
   ;; (! displayall 'case 'ret)
   (copat
    [(#:bind) (! db-interp3 'v x env)]
    [((rest args)) (! error 'db-interp3 "object lang function tried to return, but the stack was non-empty: ~v" args)])]
  [((= 'c) (cons (= 'bind) (cons c (cons k _))) env)
   ;; (! displayall 'case 'bind)
   [v <- (! db-interp3 'c c env)]
   (! db-interp3 'c k (cons v env))]
  [((= 'c) (cons (= 'force) (cons v _)) env)
   ;; (! displayall 'force v)
   (pm (! db-interp3 'v v env)
       [(cons (= 'thunk) (cons th _)) (! th)]
       [v (! error 'db-interp3 "tried to force a non-thunk: ~v" v)])]
  [((= 'c) (cons (= 'lambda) (cons c _)) env)
   (copat
    [(arg) (! db-interp3 'c c (cons arg env))]
    [() (! error 'db-interp3 "object lang expected an arg but there wasn't one")])]
  [((= 'c) (cons (= 'app) (cons c (cons v _))) env)
   [sv <- (! db-interp3 'v v env)]
   (! db-interp3 'c c env sv)]
  [((= 'c) (cons (= 'if) (cons v (cons ct (cons cf _)))) env)
   [sv <- (! db-interp3 'v v env)]
   [c <- (pm (ret sv)
             [(= 'true) (ret ct)]
             [(= 'false) (ret cf)]
             [_ (! error 'db-interp3 "called if on ~v" sv)])]
   (! db-interp3 'c c env)]
  [((= 'c) (cons (= 'beep) (cons c _)))
   (! displayall 'beep)
   (! db-interp3 'c c)])

(def-thunk (! run3 c) (! db-interp3 'c c '()))

(do (! test-equal! (~ (! interpc '(inc (inc (dec (ret true)))))) (~ (ret '(val: true state: 1))))
    (! test-equal! (~ (! <<v run2 (list 'ret 'true))) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run2 ex0)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run2 ex1)) (~ (ret 'false)))
  (! test-equal! (~ (! <<v run2 ex2)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v first 'o run2 thk-ex1)) (~ (ret 'thunk)))
  (! test-equal! (~ (! <<v run2 thk-ex2)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run2 '(if true (ret false) (ret true))))
     (~ (ret 'false)))
  (! test-equal! (~ (! <<v run2 '(if false (ret false) (ret true))))
     (~ (ret 'true)))
  (! test-equal! (~ (! run2 '(bind (ret (thunk (lambda (lambda (app (force (var 1)) (var 0))))))
                                   (app (app (force (var 0)) (thunk (lambda (ret (var 0))))) true))))
     (~ (ret 'true)))
  (! test-equal! (~ (! <<v run3 (list 'ret 'true))) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run3 ex0)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run3 ex1)) (~ (ret 'false)))
  (! test-equal! (~ (! <<v run3 ex2)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v first 'o run3 thk-ex1)) (~ (ret 'thunk)))
  (! test-equal! (~ (! <<v run3 thk-ex2)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run3 '(if true (ret false) (ret true))))
     (~ (ret 'false)))
  (! test-equal! (~ (! <<v run3 '(if false (ret false) (ret true))))
     (~ (ret 'true)))
  (! test-equal! (~ (! run3 '(bind (ret (thunk (lambda (ret (var 0)))))
                                   (app (force (var 0)) true))))
     (~ (ret 'true)))
  (! test-equal! (~ (! run3 '(bind (ret (thunk (lambda (lambda (app (force (var 1)) (var 0))))))
                                   (app (app (force (var 0)) (thunk (lambda (ret (var 0))))) true))))
     (~ (ret 'true)))
  (ret 'all-db-interp-tests-pass))

(define ex-app
  '(bind (ret (thunk (lambda (lambda (app (force (var 1)) (var 0))))))
         (app (app (force (var 0)) (thunk (lambda (ret (var 0))))) true)))

(define ex-app1
  '(bind (ret (thunk (lambda (ret (var 0)))))
         (app (force (var 0)) true)))

;; Monads and adjunctions
;;   Error-like effects are given by monads on the *computation* category.
;;
;; For instance:
;;   Err E B = B ⊕ E
;;
;; Teletype Char B = (Char -> B) ⊕ (Char ⊘ (Teletype Char B))
;;   ^^ free monad
