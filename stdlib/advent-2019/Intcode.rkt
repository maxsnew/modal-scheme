#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")
(require "../FlexVec.rkt")

(provide parse-intcode-program
         run-intcode-program

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
(def-thunk (! parse-intcode-program)
  [chars <- (! <<n list<-colist 'o read-all-chars '$)]
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
  [(mem ptr (= 0)) (! mem 'get ptr)]
  [(mem val (= 1)) (ret val)]
  [() (! error "read-parameter got an invalid parameter mode")])

;; interprets the operation, returning new iptr, inputs and output
(def-thunk (! operation memory iptr inp outp op args modes)
  ((copat
    [((= 1))
     [arg1 <- (! first args)] [arg2 <- (! second args)]
     [mode1 <- (! nth-mode 0 modes)] [mode2 <- (! nth-mode 1 modes)]
     [val1 <- (! read-parameter memory arg1 mode1)]
     [val2 <- (! read-parameter memory arg2 mode2)]
     [dest <- (! third args)]
     [result <- (! + val1 val2)]
     (! memory 'set dest result)
     (! List iptr inp outp)]
    [((= 2))
     [arg1 <- (! first args)] [arg2 <- (! second args)]
     [mode1 <- (! nth-mode 0 modes)] [mode2 <- (! nth-mode 1 modes)]
     [val1 <- (! read-parameter memory arg1 mode1)]
     [val2 <- (! read-parameter memory arg2 mode2)]
     [dest <- (! third args)]
     [result <- (! * val1 val2)]
     (! memory 'set dest result)
     (! List iptr inp outp)]
    [((= 3))
     [inp1 <- (! first inp)]
     [inp <- (! rest inp)]
     [dest <- (! first args)]
     (! memory 'set dest inp1)
     (! List iptr inp outp)]
    [((= 4))
     [param <- (! first args)]
     [mode <- (! nth-mode 0 modes)]
     [outp <- (! read-parameter memory param mode)]
     ;; (! displayall 'output: outp)
     (! List iptr inp outp)]
    [((= 5)) ;; jump-not-zero
     [discrim <- (! idiom (~ (ret read-parameter)) (~ (ret memory))
                    (~ (! first args)) (~ (! nth-mode 0 modes)))]
     [dest <- (! idiom (~ (ret read-parameter)) (~ (ret memory))
                       (~ (! second args)) (~ (! nth-mode 1 modes)))]     
     [iptr <- (ifc (! zero? discrim) (ret iptr) (ret dest))]
     (! List iptr inp outp)]
    [((= 6)) ;; jump-if-zero
     [discrim <- (! idiom (~ (ret read-parameter)) (~ (ret memory))
                    (~ (! first args)) (~ (! nth-mode 0 modes)))]
     [dest <- (! idiom (~ (ret read-parameter)) (~ (ret memory))
                       (~ (! second args)) (~ (! nth-mode 1 modes)))]
     [iptr <- (ifc (! zero? discrim) (ret dest) (ret iptr))]
     (! List iptr inp outp)]
    [((= 7)) ;; <
     [v1 <- (! idiom (~ (ret read-parameter)) (~ (ret memory)) (~ (! first args)) (~ (! nth-mode 0 modes)))]
     [v2 <- (! idiom (~ (ret read-parameter)) (~ (ret memory)) (~ (! second args)) (~ (! nth-mode 1 modes)))]
     [dest <- (! third args)]
     [result <- (ifc (! < v1 v2) (ret 1) (ret 0))]
     (! memory 'set dest result)
     (! List iptr inp outp)
     ]
    [((= 8)) ;; =
     [v1 <- (! idiom (~ (ret read-parameter)) (~ (ret memory)) (~ (! first args)) (~ (! nth-mode 0 modes)))]
     [v2 <- (! idiom (~ (ret read-parameter)) (~ (ret memory)) (~ (! second args)) (~ (! nth-mode 1 modes)))]
     [dest <- (! third args)]
     [result <- (ifc (! = v1 v2) (ret 1) (ret 0))]
     (! memory 'set dest result)
     (! List iptr inp outp)])
   op))

(def/copat (! op->num-params)
  [((= 1))  (ret 3)]
  [((= 2))  (ret 3)]
  [((= 3))  (ret 1)]
  [((= 4))  (ret 1)]
  (((= 5))  (ret 2))
  (((= 6))  (ret 2))
  (((= 7))  (ret 3))
  (((= 8))  (ret 3))
  [((= 99)) (ret 0)])

(def/copat (! eval-opcodes)
  [(mem iptr input output)
   [code*modes <- (! <<v parse-opcode 'o mem 'get iptr '$)]
   [code <- (! first code*modes)]
   [modes <- (! rest code*modes)]
   ((copat
     [((= 99))
      (ret output)]
     [(op)
      [n <- (! op->num-params op)]
      [iptr <- (! + iptr 1)]
      (! grab-args mem iptr n '()
         (~ (λ (iptr params)
              (do [new-state <- (! operation mem iptr input output op params modes)]
                  (! apply (~ (! eval-opcodes mem)) new-state)))))])
    code)])

;; Intcode-Program -> (Listof Number) -> F Number
(def-thunk (! run-intcode-program opcodes inputs)
  [memory <- (! mutable-flexvec<-list opcodes)]
  (! eval-opcodes memory 0 inputs #f))

;; An Intcode-Driver I:v O:v R:c is a computation supporting
;;  . 'input  U(I -> U(Intcode-Driver I O R) -> R) -> R
;;  . 'output O -> U(U(Intcode-Driver I O R) -> R) -> R
;;  . 'halt   -> R

(def-thunk (! intcode-operation mem iptr driver op params modes resumeK)
  ((copat
    [((= 99)) (! driver 'halt)]
    [((= 1)) ;; + x1 x2 ~> x3
     [param1 <- (! first params)] [param2 <- (! second params)]
     [mode1 <- (! nth-mode 0 modes)] [mode2 <- (! nth-mode 1 modes)]
     [val1 <- (! read-parameter mem param1 mode1)]
     [val2 <- (! read-parameter mem param2 mode2)]
     [dest <- (! third params)]
     [result <- (! + val1 val2)]
     (! mem 'set dest result)
     (! resumeK iptr driver)]
    [((= 2)) ;; * x1 x2 ~> x3
     [param1 <- (! first params)] [param2 <- (! second params)]
     [mode1 <- (! nth-mode 0 modes)] [mode2 <- (! nth-mode 1 modes)]
     [val1 <- (! read-parameter mem param1 mode1)]
     [val2 <- (! read-parameter mem param2 mode2)]
     [dest <- (! third params)]
     [result <- (! * val1 val2)]
     (! mem 'set dest result)
     (! resumeK iptr driver)]
    [((= 3)) ;; input ~> x1
     (! driver 'input
        (~ (λ (inp)
             (do ;; (! displayall 'input-received: inp)
                 [dest <- (! first params)]
                 (! mem 'set dest inp)
               (! resumeK iptr)))))]
    [((= 4)) ;; x1 ~> output
     [param <- (! first params)]
     [mode <- (! nth-mode 0 modes)]
     [outp <- (! read-parameter mem param mode)]
     ;; (! displayall 'output: outp)
     (! driver 'output outp (~ (! resumeK iptr)))]
    [((= 5)) ;; jump-not-zero
     [discrim <- (! idiom (~ (ret read-parameter)) (~ (ret mem))
                    (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [dest <- (! idiom (~ (ret read-parameter)) (~ (ret mem))
                       (~ (! second params)) (~ (! nth-mode 1 modes)))]     
     [iptr <- (ifc (! zero? discrim) (ret iptr) (ret dest))]
     (! resumeK iptr driver)]
    [((= 6)) ;; jump-if-zero
     [discrim <- (! idiom (~ (ret read-parameter)) (~ (ret mem))
                    (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [dest <- (! idiom (~ (ret read-parameter)) (~ (ret mem))
                       (~ (! second params)) (~ (! nth-mode 1 modes)))]
     [iptr <- (ifc (! zero? discrim) (ret dest) (ret iptr))]
     (! resumeK iptr driver)]
    [((= 7)) ;; <
     [v1 <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [v2 <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (! second params)) (~ (! nth-mode 1 modes)))]
     [dest <- (! third params)]
     [result <- (ifc (! < v1 v2) (ret 1) (ret 0))]
     (! mem 'set dest result)
     (! resumeK iptr driver)]
    [((= 8)) ;; =
     [v1 <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (! first params)) (~ (! nth-mode 0 modes)))]
     [v2 <- (! idiom (~ (ret read-parameter)) (~ (ret mem)) (~ (! second params)) (~ (! nth-mode 1 modes)))]
     [dest <- (! third params)]
     [result <- (ifc (! = v1 v2) (ret 1) (ret 0))]
     (! mem 'set dest result)
     (! resumeK iptr driver)])
   op))

(def-thunk (! intcode-prog-loop mem iptr driver)
  [code*modes <- (! <<v parse-opcode 'o mem 'get iptr '$)]
  [iptr <- (! + iptr 1)]
  [op <- (! first code*modes)]
  [modes <- (! rest code*modes)]
  [n <- (! op->num-params op)]
  (! grab-args mem iptr n '()
     (~ (λ (iptr params)
          (! intcode-operation mem iptr driver op params modes
             (~ (! intcode-prog-loop mem)))))))

;; A Semantic-Intcode-Prog is a forall R. U(Intcode-Driver I O R) -> R

;; interp-intcode-prog : Intcode-Program -> U(Intcode-Driver Number Number R) -> R
(def-thunk (! interp-intcode-program prog)
  [memory <- (! mutable-flexvec<-list prog)]
  (! intcode-prog-loop memory 0))


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


