#lang sbpv

(require "../stdlib.rkt")
;; Let's write a sbpv interpreter! We'll include bits and funs

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

(def-thunk (! run c) (! db-interp2 'c c eoG))

(do (! test-equal! (~ (! <<v get-val 'o interpc (list 'ret 'true))) (~ (ret 'true)))
    (! test-equal! (~ (! <<v get-val 'o interpc ex0)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v get-val 'o interpc ex1)) (~ (ret 'false)))
  (! test-equal! (~ (! <<v get-val 'o interpc ex2)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v first 'o get-val 'o interpc thk-ex1)) (~ (ret 'closure)))
  (! test-equal! (~ (! <<v third 'o get-val 'o interpc thk-ex1)) (~ (ret '(true))))
  (! test-equal! (~ (! <<v get-val 'o interpc thk-ex2)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v get-val 'o interpc '(if true (ret false) (ret true))))
     (~ (ret 'false)))
  (! test-equal! (~ (! <<v get-val 'o interpc '(if false (ret false) (ret true))))
     (~ (ret 'true)))
  (! test-equal! (~ (! interpc '(inc (inc (dec (ret true)))))) (~ (ret '(val: true state: 1))))
  (! test-equal! (~ (! <<v run (list 'ret 'true))) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run ex0)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run ex1)) (~ (ret 'false)))
  (! test-equal! (~ (! <<v run ex2)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v first 'o run thk-ex1)) (~ (ret 'thunk)))
  (! test-equal! (~ (! <<v run thk-ex2)) (~ (ret 'true)))
  (! test-equal! (~ (! <<v run '(if true (ret false) (ret true))))
     (~ (ret 'false)))
  (! test-equal! (~ (! <<v run '(if false (ret false) (ret true))))
     (~ (ret 'true)))
  (! test-equal! (~ (! run '(bind (ret (thunk (lambda (lambda (app (force (var 1)) (var 0))))))
                                  (app (app (force (var 0)) (thunk (lambda (ret (var 0))))) true))))
     (~ (ret 'true)))
  (ret 'all-db-interp-tests-pass))


