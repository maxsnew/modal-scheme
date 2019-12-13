#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")
(require "../FlexVec.rkt")

(provide parse-intcode-program

         ;; new interface
         interp-intcode-program
         ;; drivers
         static-input-return-last-output
         )


;; PARSING
;
;; An Intcode-Program is a Listof Number

;; Char ->* F Intcode-Program)
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
   (! parse-chars 'loop nums digits)])

;; Parses Intcode program 
(def-thunk (! parse-intcode-program (rest args))
  [chars <- (! <<n list<-colist 'o apply read-all-chars args '$)]
  (! apply (~ (! parse-chars 'loop '() '())) chars))


;; A Parameter-Mode is one of
;;   - 0 , representing position mode
;;   - 1 , representing immediate mode

(def-thunk (! grab-args mem ptr n prev k)
  (cond [(! zero? n)
         [args <- (! reverse prev)]
         (! k ptr args)]
        [else
         [x <- (! mem 'get ptr)]
         [ptr <- (! + ptr 1)]
         [prev <- (! Cons x prev)]
         [n <- (! - n 1)]
         (! grab-args mem ptr n prev k)]))

;; parse-opcode : Number -> Cons Opcode (Listof Parameter-Modes)
(def-thunk (! parse-opcode n)
  [code <- (! modulo n 100)]
  [modes-num <- (! quotient n 100)]
  [modes <- (! <<v reverse 'o map digit<-char 'o string->list 'o number->string modes-num '$)]
  (! Cons code modes))

;; Nat -> Listof Parameter-Mode -> Parameter-Mode
;; lookup n in the list, if the list is too short, return 0
(def/copat (! nth-mode)
  [(n (= '())) (ret 0)]
  [((= 0) modes) (! first modes)]
  [(n modes) (! idiom (~ (ret nth-mode)) (~ (! - n 1)) (~ (! cdr modes)))])

;; read-parameter
;; Memory -> Nat -> Parameter-Mode -> F Nat
(def/copat (! read-parameter)
  [(mem rbase ptr (= 0)) (! mem 'get ptr)]
  [(mem rbase val (= 1)) (ret val)]
  [(mem rbase ptr (= 2))
   [ptr <- (! + rbase ptr)]
   (! mem 'get ptr)]
  [() (! error "read-parameter got an invalid parameter mode")])

(def/copat (! compute-dest)
  [(rbase ptr (= 0)) (ret ptr)]
  [(rbase ptr (= 2)) (! + ptr rbase)])

(def/copat (! op->num-params)
  [((= 1))  (ret 3)]
  [((= 2))  (ret 3)]
  [((= 3))  (ret 1)]
  [((= 4))  (ret 1)]
  (((= 5))  (ret 2))
  (((= 6))  (ret 2))
  (((= 7))  (ret 3))
  (((= 8))  (ret 3))
  (((= 9))  (ret 1))
  [((= 99)) (ret 0)])

;; An Intcode-Driver I:v O:v R:c is a computation supporting
;;  . 'input  U(I -> U(Intcode-Driver I O R) -> R) -> R
;;  . 'output O -> U(U(Intcode-Driver I O R) -> R) -> R
;;  . 'halt   -> R

(def-thunk (! intcode-operation mem iptr rbase driver op params modes resumeK)
  ((copat
    [((= 99)) (! driver 'halt)]
    [((= 1)) ;; + x1 x2 ~> x3
     [param1 <- (! first params)] [param2 <- (! second params)] [param3 <- (! third params)] 
     [mode1 <- (! nth-mode 0 modes)] [mode2 <- (! nth-mode 1 modes)] [mode3 <- (! nth-mode 2 modes)]
     [val1 <- (! read-parameter mem rbase param1 mode1)]
     [val2 <- (! read-parameter mem rbase param2 mode2)]
     [dest <- (! compute-dest rbase param3 mode3)]
     [result <- (! + val1 val2)]
     (! mem 'set dest result)
     (! resumeK iptr rbase driver)]
    [((= 2)) ;; * x1 x2 ~> x3
     [param1 <- (! first params)] [param2 <- (! second params)] [param3 <- (! third params)] 
     [mode1 <- (! nth-mode 0 modes)] [mode2 <- (! nth-mode 1 modes)] [mode3 <- (! nth-mode 2 modes)]
     [val1 <- (! read-parameter mem rbase param1 mode1)]
     [val2 <- (! read-parameter mem rbase param2 mode2)]
     [dest <- (! compute-dest rbase param3 mode3)]
     [result <- (! * val1 val2)]
     (! mem 'set dest result)
     (! resumeK iptr rbase driver)]
    [((= 3)) ;; input ~> x1
     (! driver 'input
        (~ (λ (inp)
             (do ;; (! displayall 'input-received: inp)
                 [dest <- (! idiom (~ (ret compute-dest)) (~ (ret rbase)) (~ (! first params)) (~ (! nth-mode 0 modes)))]
                 (! mem 'set dest inp)
               (! resumeK iptr rbase)))))]
    [((= 4)) ;; x1 ~> output
     [param <- (! first params)]
     [mode <- (! nth-mode 0 modes)]
     [outp <- (! read-parameter mem rbase param mode)]
     ;; (! displayall 'output: outp)
     (! driver 'output outp (~ (! resumeK iptr rbase)))]
    [((= 5)) ;; jump-not-zero
     [discrim <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase))
                    (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [dest <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase))
                       (~ (! second params)) (~ (! nth-mode 1 modes)))]     
     [iptr <- (ifc (! zero? discrim) (ret iptr) (ret dest))]
     (! resumeK iptr rbase driver)]
    [((= 6)) ;; jump-if-zero
     [discrim <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase))
                    (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [dest <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase))
                       (~ (! second params)) (~ (! nth-mode 1 modes)))]
     [iptr <- (ifc (! zero? discrim) (ret dest) (ret iptr))]
     (! resumeK iptr rbase driver)]
    [((= 7)) ;; <
     [v1 <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase)) (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [v2 <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase)) (~ (! second params)) (~ (! nth-mode 1 modes)))]
     [dest <- (! idiom (~ (ret compute-dest)) (~ (ret rbase)) (~ (! third params)) (~ (! nth-mode 2 modes)))]
     [result <- (ifc (! < v1 v2) (ret 1) (ret 0))]
     (! mem 'set dest result)
     (! resumeK iptr rbase driver)]
    [((= 8)) ;; =
     [v1 <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase)) (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [v2 <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase)) (~ (! second params)) (~ (! nth-mode 1 modes)))]
     [dest <- (! idiom (~ (ret compute-dest)) (~ (ret rbase)) (~ (! third params)) (~ (! nth-mode 2 modes)))]
     [result <- (ifc (! = v1 v2) (ret 1) (ret 0))]
     (! mem 'set dest result)
     (! resumeK iptr rbase driver)]
    [((= 9)) ;; rbase += arg
     [v <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (ret rbase))
              (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [rbase <- (! + rbase v)]
     (! resumeK iptr rbase driver)])
   op))

(def-thunk (! intcode-prog-loop mem iptr rbase driver)
  [code*modes <- (! <<v parse-opcode 'o mem 'get iptr '$)]
  [iptr <- (! + iptr 1)]
  [op <- (! first code*modes)]
  [modes <- (! rest code*modes)]
  [n <- (! op->num-params op)]
  (! grab-args mem iptr n '()
     (~ (λ (iptr params)
          (! intcode-operation mem iptr rbase driver op params modes
             (~ (! intcode-prog-loop mem)))))))

;; A Semantic-Intcode-Prog is a forall R. U(Intcode-Driver I O R) -> R

;; interp-intcode-prog : Intcode-Program -> U(Intcode-Driver Number Number R) -> R
(def-thunk (! interp-intcode-program prog)
  [memory <- (! initialize-memory prog)]
  (! intcode-prog-loop memory 0 0))


;; DRIVERS


;; (Listof Number) -> Number -> Intcode-Driver Number Number (F Number)
;; feeds the list as consecutive inputs
;; returns the last output when it halts, using the second arg as default
(def/copat (! static-input-return-last-output)
  [((= '()) last-output (= 'input)) (! error "ran out of inputs")]
  [(inps last-output (= 'input) k)
   [inp1 <- (! first inps)] [inps <- (! rest inps)]
   (! k inp1 (~ (! static-input-return-last-output inps last-output)))]
  [(inps last-output (= 'output) new-output k)
   (! k (~ (! static-input-return-last-output inps new-output)))]
  [(inps last-output (= 'halt)) (ret last-output)])
