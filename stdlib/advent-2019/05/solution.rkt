#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")
(require "../../FlexVec.rkt")

(provide main-a main-b)

;; A Parameter-Mode is one of
;;   - 0 , representing position mode
;;   - 1 , representing immediate mode

(define INPUT 1)

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

;; read-inp
;; Memory -> Nat -> Parameter-Mode -> F Nat
(def/copat (! read-inp)
  [(mem ptr (= 0)) (! mem 'get ptr)]
  [(mem val (= 1)) (ret val)]
  [() (! error "read-inp got an invalid parameter mode")])

(def-thunk (! operation memory iptr inp outp op args modes)
  ((copat
    [((= 1))
     [arg1 <- (! first args)] [arg2 <- (! second args)]
     [mode1 <- (! nth-mode 0 modes)] [mode2 <- (! nth-mode 1 modes)]
     [val1 <- (! read-inp memory arg1 mode1)]
     [val2 <- (! read-inp memory arg2 mode2)]
     [dest <- (! third args)]
     [result <- (! + val1 val2)]
     (! memory 'set dest result)
     (! List iptr outp)]
    [((= 2))
     [arg1 <- (! first args)] [arg2 <- (! second args)]
     [mode1 <- (! nth-mode 0 modes)] [mode2 <- (! nth-mode 1 modes)]
     [val1 <- (! read-inp memory arg1 mode1)]
     [val2 <- (! read-inp memory arg2 mode2)]
     [dest <- (! third args)]
     [result <- (! * val1 val2)]
     (! memory 'set dest result)
     (! List iptr outp)]
    [((= 3))
     [dest <- (! first args)]
     (! memory 'set dest inp)
     (! List iptr outp)]
    [((= 4))
     [param <- (! first args)]
     [mode <- (! nth-mode 0 modes)]
     [outp <- (! read-inp memory param mode)]
     (! displayall 'output: outp)
     (! List iptr outp)]
    [((= 5)) ;; jump-not-zero
     [discrim <- (! idiom (~ (ret read-inp)) (~ (ret memory))
                    (~ (! first args)) (~ (! nth-mode 0 modes)))]
     [dest <- (! idiom (~ (ret read-inp)) (~ (ret memory))
                       (~ (! second args)) (~ (! nth-mode 1 modes)))]     
     [iptr <- (ifc (! zero? discrim) (ret iptr) (ret dest))]
     (! List iptr outp)]
    [((= 6)) ;; jump-if-zero
     [discrim <- (! idiom (~ (ret read-inp)) (~ (ret memory))
                    (~ (! first args)) (~ (! nth-mode 0 modes)))]
     [dest <- (! idiom (~ (ret read-inp)) (~ (ret memory))
                       (~ (! second args)) (~ (! nth-mode 1 modes)))]
     [iptr <- (ifc (! zero? discrim) (ret dest) (ret iptr))]
     (! List iptr outp)]
    [((= 7)) ;; <
     [v1 <- (! idiom (~ (ret read-inp)) (~ (ret memory)) (~ (! first args)) (~ (! nth-mode 0 modes)))]
     [v2 <- (! idiom (~ (ret read-inp)) (~ (ret memory)) (~ (! second args)) (~ (! nth-mode 1 modes)))]
     [dest <- (! third args)]
     [result <- (ifc (! < v1 v2) (ret 1) (ret 0))]
     (! memory 'set dest result)
     (! List iptr outp)
     ]
    [((= 8)) ;; =
     [v1 <- (! idiom (~ (ret read-inp)) (~ (ret memory)) (~ (! first args)) (~ (! nth-mode 0 modes)))]
     [v2 <- (! idiom (~ (ret read-inp)) (~ (ret memory)) (~ (! second args)) (~ (! nth-mode 1 modes)))]
     [dest <- (! third args)]
     [result <- (ifc (! = v1 v2) (ret 1) (ret 0))]
     (! memory 'set dest result)
     (! List iptr outp)
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
              (do [iptr*output <- (! operation mem iptr input output op params modes)]
                  [iptr <- (! first iptr*output)]
                [output <- (! second iptr*output)]
                  (! eval-opcodes mem iptr input output)))))])
    code)])

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
   (! parse-chars 'loop nums digits)]
  )

(def-thunk (! parse-opcodes)
  [chars <- (! <<n list<-colist 'o read-all-chars '$)]
  (! apply (~ (! parse-chars 'loop '() '())) chars))

(def-thunk (! run-opcode-program opcodes input)
  [memory <- (! mutable-flexvec<-list opcodes)]
  (! eval-opcodes memory 0 input #f))

(def-thunk (! main-a)
  (! <<v swap run-opcode-program 1 'o debug 'parsed 'o parse-opcodes '$))

(def-thunk (! main-b)
  (! <<v swap run-opcode-program 5 'o debug 'parsed 'o parse-opcodes '$))
