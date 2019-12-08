#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Parse.rkt")
(require "../FlexVec.rkt")

(provide parse-intcode-program
         run-intcode-program)

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
     (! displayall 'output: outp)
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
     (! List iptr inp outp)
     ])
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

;; 
(def-thunk (! run-intcode-program opcodes inputs)
  [memory <- (! mutable-flexvec<-list opcodes)]
  (! eval-opcodes memory 0 inputs #f))
