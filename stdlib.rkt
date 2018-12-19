#lang sbpv

(require (for-syntax syntax/parse
                     (except-in racket/base quote)
                     (only-in "sbpv/main.rkt" quote)))
(provide Y do do^ ifc define-rec-thunk define-thunk def-thunk def/copat
         pop1 Cons List .n .v $ swap const abort
         list first second third fourth empty? rest grab-stack dot-args
         rev-apply apply reverse grab-up-to
         copat length Ret cond
         and or foldl map filter ~ debug @>
         ;; "Calling conventions: call-by-value, call-by-name, and method style"
         <<v <<n oo)

(define-syntax (~ syn)
  (syntax-parse syn [(_ e) #'(thunk e)]))
;; A Y combinator to get us moving
(define Y
  (thunk
   (case-λ
    [(#:bind) (error "Y combinator expects one argument, but got none")]
    [(f)
     (let ([self-app (thunk (λ (x) (! f (thunk (! x x)))))])
       ((! self-app) self-app))])))

(define-syntax (do syn)
  (syntax-parse syn
    [(_ [x:id (~literal <-) m] e es ...)
     #`(bind (x m) (do e es ...))]
    [(_ [x:id (~literal =) m] e es ...)
     #`(let ([x m]) (do e es ...))]
    [(_ m) #`m]
    [(_ m e es ...)
     #`(bind (x m) (do e es ...))]))

(define-syntax (ifc syn)
  (syntax-parse syn
    [(_ c e1 e2) #`(bind (x c) (if x e1 e2))]))
(define-syntax (cond syn)
  (syntax-parse syn
    [(_ [#:else e ...]) #`(do e ...)]
    [(_ [(~literal else) e ...]) #`(do e ...)]
    [(_ [b e1 ...] es ...) #`(ifc b (do e1 ...) (cond es ...))]
    [(_) #`(error 'cond-error "all cond conditions failed: TODO")]))

(define-syntax (define-rec-thunk syn)
  (syntax-parse syn
    [(_ ((~literal !) f:id x:id ...) e)
     #`(define f
         (thunk (letrec ([f (thunk (λ (x ...) e))]) (! f))))]))
(define-syntax (define-thunk syn)
  (syntax-parse syn
    [(_ ((~literal !) f:id x:id ...) e)
     #`(define f (thunk (λ (x ...) e)))]))
;; (define-rec loop (thunk (! loop)))

(define pop1 (thunk (λ (x) (ret x))))
(define Cons (thunk (λ (hd tl) (ret (cons hd tl)))))
#;
(define-thunk (! Y2 f1 f2)
  (let ([f1+2
         (thunk (λ (f b)
                  (ifc (! equal? b 0)
                       (! f1 (thunk (! f 0)) (thunk (! f 1)))
                       (! f2 (thunk (! f 0)) (thunk (! f 1))))))])
    (! Cons (thunk (! f1+2 0)) (thunk (! f1+2 1)))))

;; CBN Composition
(define-thunk (! .n f g x)
  (! f (thunk (! g x))))

;; CBV Composition
(define-thunk (! .v f g x)
  (do [y <- (! g x)]
      (! f y)))

; C combinator
(define-thunk (! swap k x y) (! k y x))

(define-rec-thunk (! const x)
  (case-λ
   [(#:bind) (ret x)]
   [(_) (! const x)]))
(define abort const)
;(! const 3 4 5 6 7 8 9 10) ; 3


(define-syntax (list syn)
  (syntax-parse syn
    [(_) #`'()]
    [(_ e es ...)
     #`(cons e (list es ...))]))
(define first car)
(define empty? null?)
(define rest cdr)
(define-thunk (! second lst)
  (do [tl <- (! cdr lst)]
      (! car tl)))
(define-thunk (! third lst)
  (do [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      (! car lst)))
(define-thunk (! fourth lst)
  (do [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      [lst <- (! cdr lst)]
      (! car lst)))

;; Infinite loop
; (! Y (thunk (λ (loop) (! loop))))

(define-rec-thunk (! sum-loop acc)
  (case-λ
   [(#:bind) (ret acc)]
   [(x) (do [acc^ <- (! + x acc)]
            (! sum-loop acc^))]))

(define-thunk (! sum) (! sum-loop 0))
; (! sum)
; (! sum 1)
; (! sum 1 1)
; (! sum 5 6 7 8) ; 26

(define-rec-thunk (! rev-loop acc lst)
  (ifc (! null? lst)
       (ret acc)
       (do [hd <- (! car lst)]
           [tl <- (! cdr lst)]
         (! rev-loop (cons hd acc) tl))))
(define-thunk (! reverse lst) (! rev-loop '() lst))
; (! reverse (cons 3 (cons 4 (cons 5 null))))

;; stack-loop : forall X. (X -> ?c) -> X -> (U (?v -> X -> F X)) -> ?c
;; aka stack-foldl
(define-rec-thunk (! stack-loop k acc cons)
  (case-λ
   [(#:bind) (! k acc)]
   [(x)
    (do [acc^ <- (! cons x acc)]
        (! stack-loop k acc^ cons))]))

(define-thunk (! grab-rev-stack k)
  (! stack-loop k '() Cons))

; (! grab-rev-stack pop1 0 1 2 3 4 5)

(define-thunk (! grab-stack k)
  (! grab-rev-stack
     (thunk
      (λ (rev-stack)
        (do [stack <- (! reverse rev-stack)]
            (! k stack))))))

; (! grab-stack pop1 0 1 2 3 4 5)

; dot-args : forall Y. (List ?v -> Y) -> Y
(define dot-args grab-stack)
(define-thunk (! List) (! dot-args pop1))

; rev-apply : U(X -> ... -> ?c) -> List X -> ?c
(define-rec-thunk (! rev-apply k xs)
  (ifc (! null? xs)
       (! k)
       (do [hd <- (! car xs)]
           [tl <- (! cdr xs)]
         (! rev-apply k tl hd))))

(define-thunk (! apply f xs)
  (do [sx <- (! reverse xs)]
      (! rev-apply f sx)))

; loop-up-to : forall X. (X -> ?c) -> (X -> ?v -> ?c) -> (?v -> F Bool) -> X -> (U (?v -> X -> F X)) -> ?c
(define-rec-thunk (! loop-up-to ran-out finish finished? acc step)
  (case-λ
   [(#:bind) (! ran-out acc)]
   [(x)
    (ifc (! finished? x)
         (! finish acc x)
         (bind (acc^ (! step acc x))
               (! loop-up-to ran-out finish finished? acc^ step)))]))

#;
(! loop-up-to
   (thunk (λ (xs) (bind (done (! Cons 'no-key xs))
                        (! abort done))))
   (thunk (λ (xs key) (bind (res (! Cons key xs)) (! abort res))))
   (thunk (! equal? 'key))
   '()
   (thunk (! swap Cons))
   0 1 2 'key 2 1 0)

; grab-up-to : forall Y+ Y-. (List Y+ -> Y-) -> (x : ?v) -> (Y+ -o {y : ?v | x = y} -> Y-)
(define-thunk (! grab-up-to k prompt)
  (let ([finish
         (thunk (λ (sx)
                  (do [xs <- (! reverse sx)]
                      (! k xs))))])
    (! loop-up-to
       finish
       (thunk (λ (sx _prompt) (! finish sx)))
       (thunk (! equal? prompt))
       '()
       (thunk (! swap Cons)))))
#;
(! grab-up-to
   (thunk (λ (up-to) (! dot-args
                        (thunk
                         (λ (rest)
                           (ret (cons (cons up-to 'before)
                                      (cons rest 'after))))))))
   'middle
   0 1 2 3 'middle 3 2 1 0)

(define-thunk (! even?)
  (letrec ([even? (thunk (λ (x)
                           (ifc (! zero? x)
                                (ret #t)
                                (do [x-1 <- (! - x 1)]
                                    (! odd? x-1)))))]
           [odd? (thunk (λ (x)
                           (ifc (! zero? x)
                                (ret #f)
                                (do [x-1 <- (! - x 1)]
                                    (! even? x-1)))))])
    (! even?)))

;; Copattern matching

;; A copattern is either
;;   a list of patterns (pat ...)
;;   a list of patterns followed by #:bind (pat ... #:bind)
;;, representing a pattern match on the stack.
;;  If it ends with #:bind, it only matches if pat ... matches and
;;  there are no more arguments.
;;  If it does not end with #:bind, it matches when all of pat
;;  ... matches, and keeps the remainder of the stack as is.

;; A pattern is one of
;;   'end - matches only the empty stack #:bind
;;   'var - matches anything, binds to a variable
;;   'rest - matches the rest of the stack
;;   (list 'lit ?v) - representing a literal to be matched by equal?
;;   (list 'upto ?v) - collect all values on the stack into a list until you see something equal? to ?v
;;
;; #:bind is a kind of pattern that matches the empty stack.
;; TODO: add constructors (cons)
;; TODO: add ...-patterns

(define copat-ex0 '())
(define copat-ex1 '((lit hd) (lit tl) var))
(define copat-ex2 '(var))

;; up-to-lit
(define-rec-thunk (! up-to-lit match-k abort-k lit seen)
  (case-λ
   [(#:bind) (! rev-apply abort-k seen)]
   [(x)
    (ifc (! equal? x lit)
         (do [seen~ <- (! reverse seen)]
             [abort-k <- (ret (thunk (! rev-apply abort-k seen)))]
             (! match-k abort-k seen~))
         (! up-to-lit match-k abort-k lit (cons x seen)))]))

; attempst to match the stack against a copattern
;; copat: the copattern we're matching
;; match-k: a kontinuation for a successful match
;; abort-k: a kontinuation for when the current match fails (for backtracking)
;; popped: all the arguments that have been popped off so far, in reverse order they were popped
;; bound: the arguments that have been popped and are bound by the pattern, in reverse order they were popped
; copattern-match1 : Copattern Kont Kont List List -> ?c
(define-rec-thunk (! and)
  (case-λ
   [(#:bind) (ret #t)]
   [(th)
    (cond
      [(! th) (! and)]
      [#:else (! abort #f)])]))
(define-rec-thunk (! or)
  (case-λ
   [(#:bind) (ret #f)]
   [(th)
    (cond
      [(! th) (! abort #t)]
      [#:else (! or)])]))

(define-thunk (! end-pat? pat) (! equal? pat 'end))
(define-thunk (! var-pat? pat) (! equal? pat 'var))
(define-thunk (! rest-pat? pat) (! equal? pat 'rest))
(define-thunk (! lit-pat? pat)
  (! and
     (thunk (! cons? pat))
     (thunk
      (do [tag <- (! car pat)]
          (! equal? 'lit tag)))))
(define-thunk (! upto-pat? pat)
  (! and
     (thunk (! cons? pat))
     (thunk
      (do [tag <- (! car pat)]
          (! equal? 'upto tag)))))
(define-rec-thunk (! copattern-match1 copat match-k abort-k)
  (cond
    [(! empty? copat) (! match-k)]
    [#:else
     (do [pat <- (! first copat)]
         [copat <- (! rest copat)]
       (cond
         [(! rest-pat? pat) (! dot-args match-k)]
         [(! end-pat? pat) (case-λ [(#:bind) (! match-k)] [(x) (! abort-k x)])]
         [(! var-pat? pat)
          (case-λ
           [(#:bind) (! abort-k)]
           [(x) (! copattern-match1 copat (thunk (! match-k x)) (thunk (! abort-k x)))])]
         [(! lit-pat? pat)
          (do [literal <- (! second pat)]
              (case-λ
               [(#:bind) (! abort-k)]
               [(x)
                (cond
                  [(! equal? x literal)
                   (! copattern-match1 copat match-k (thunk (! abort-k x)))]
                  [#:else (! abort-k x)])]))]
         [(! upto-pat? pat)
          (do [literal <- (! second pat)]
              (! up-to-lit
                 (thunk (λ (abort-k xs)
                          (! copattern-match1 copat (thunk (! match-k xs)) abort-k)))
                 abort-k
                 literal
                 '()))]))]))

(define-thunk (! test-fail) (! abort 'failure))


#;
(! copattern-match1
   copat-ex0
   (thunk (ret 'good-match0))
   (thunk (! abort 'failureeeee)))


#;
(! copattern-match1
   copat-ex2
   (thunk (! Cons 'good-match3-its-5))
   test-fail
   5)

#;
(! copattern-match1
   '((lit 3))
   (thunk (! List 'good-match-none))
   test-fail
   3
   )
#;
(! copattern-match1
   '((upto 3))
   (thunk (! List 'good-match-none))
   test-fail
   3
   )

;; copattern-match : (Listof (List Copattern Kont)) -> Kont -> ?c
(define-rec-thunk (! copattern-match copats abort)
  (ifc (! null? copats)
       (! abort)
       (do [copat*kont <- (! first copats)]
           [rest       <- (! rest copats)]
         (! apply
            copattern-match1
            copat*kont
            (thunk (! copattern-match rest abort))))))
(define-thunk (! cpm-test-fail) (! List 'failure-called-with))
#;
(! copattern-match
   (list)
   (thunk (ret 'cpm-test-pass)))
#;
(! copattern-match
   (list (list '() (thunk (! List 'cpm-test1-pass))))
   test-fail
   )
#;
(! copattern-match
   (list (list '(var) (thunk (! List 'cpm-test2-pass-42)))
         (list '() test-fail)
         )
   (thunk (! List 'failure))
   42)
#;
(! copattern-match
   (list (list '(end) test-fail)
         (list '(var) (thunk (! List 'cpm-test2-pass-42))))
   (thunk (! List 'failure))
   42)
#;
(! copattern-match
   (list (list '(end) test-fail)
         (list '((lit 42)) (thunk (ret (list 'cpm-test2-pass)))))
   (thunk (! List 'failure))
   42)
#;
(! copattern-match
   (list (list '(end) test-fail)
         (list '((lit 43)) test-fail))
   (thunk (! List 'cpm-test-pass-with-42))
   42)
#;
(! copattern-match
   (list (list '(end) test-fail)
         (list '((lit 43)) test-fail)
         (list '(var) (thunk (! Cons 'cpm-pass-with-42))))
   cpm-test-fail
   42)
#;
(! copattern-match
   (list (list '(end) test-fail)
         (list '((lit 43)) test-fail)
         (list '(var 55) cpm-test-fail)
         (list '(var) (thunk (! Cons 'cpm-pass-with-42)))
         )
   cpm-test-fail
   42)

(define-rec-thunk (! map-loop f xs acc)
  (ifc (! empty? xs)
       (! reverse acc)
       (do [x  <- (! first xs)]
           [tl <- (! rest  xs)]
         [y  <- (! f x)]
         (! map-loop f tl (cons y acc)))))
(define-rec-thunk (! map f xs) (! map-loop f xs '()))

(define-thunk (! copattern-match-default-error copats)
  (! copattern-match
     copats
     (thunk
      (!
       dot-args
       (thunk
        (λ (args)
          (do [just-copats <- (! map first copats)]
           (error 'copattern-match-error
                  "Tried to match the arguments ~v\n\tAgainst the copatterns: ~v"
                  args
                  just-copats))))))))

;; what I want:
;; (copat
;;   [(f fargs ... 'o) foo]
;;   [(f fargs ... '$) foo]
(begin-for-syntax
  (define-syntax-class pat
    #:attributes (pattern all-vars)
    (pattern
     x:id
     #:attr pattern #''var
     #:attr all-vars #'(x))
    (pattern
     ((~literal =) e:expr)
     #:attr pattern #`(list 'lit e)
     #:attr all-vars #'())
    (pattern
     ((~literal upto) xs:id e:expr)
     #:attr pattern #`(list 'upto e)
     #:attr all-vars #'(xs))
    (pattern
     ((~literal rest) xs:id)
     #:attr pattern #`'rest
     #:attr all-vars #'(xs))
    (pattern
     e:expr
     #:attr pattern #`(list 'lit e)
     #:attr all-vars #'())
    )
  (define-syntax-class copat
    #:attributes (patterns vars)
    (pattern
     (p:pat ...)
     #:attr patterns #`(list p.pattern ...)
     ;; #:when (displayln (syntax-e #`(p.all-vars ...)))
     #:attr vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...)))))
    (pattern
     (p:pat ... #:bind)
     #:attr patterns #`(list p.pattern ... 'end)
     ;; #:when (displayln (syntax-e #`(p.all-vars ...)))
     #:attr vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...)))))))

(define-syntax (copat syn)
  (syntax-parse syn
    [(_ [cop:copat e ...] ...)
     ;; #:do ((displayln #'(cop.vars ...)))
     ;; #`(list (list cop.patterns (thunk (λ cop.vars e))) ...)
     #`(! copattern-match-default-error (list (list cop.patterns (thunk (λ cop.vars (do e ...)))) ...))
     ]))

(define-syntax (do^ syn)
  (syntax-parse syn
    [(_ [x:id ... <- m] e es ...)
     #`(m (thunk (copat [(x ...) (do^ e es ...)])))]
    [(_ m) #`m]))
#;
((copat [((= 3) x) (ret x)]) 3 #t)

(define-thunk (! $) (copat [(f) (! f)] [() (error 'foobar)]))

; idiom is an implementation of "idiom brackets" ala applicative
; functors.  Expects the stack to consist of a sequence of UF thunks,
; the first of which is a function. Then idiom^ forces the thunks in
; sequence and finally applies the function to the arguments in the
; same order that they were on the stack originally. A simple
; implementation of call-by-value as a macro in cbpv is to translate
; subterms to thunks and translate ! to ! idiom.
(define-rec-thunk (! idiom^ f)
  (copat
   [(th)
    (do [x <- (! th)]
        (! idiom^ (thunk (! f x))))]))
(define-thunk (! idiom) (! idiom^ $))


(define-rec-thunk (! length-loop acc xs)
  (ifc (! null? xs)
       (ret acc)
       (do [acc <- (! + 1 acc)]
           [xs <- (! cdr xs)]
         (! length-loop acc xs))))

(define-thunk (! length) (! length-loop 0))

(define-thunk (! Ret x) (ret x))
(define-rec-thunk (! <<v-impl k)
  (copat
   [(f (upto xs 'o))
    (let ([k (thunk (λ (y)
                      (do [z <- (! apply f xs y)]
                          (! k z))))])
      (! <<v-impl k))]
   [(f (upto xs '$))
    (do [z <- (! apply f xs)]
        (! k z))]
   [(f)
    (! dot-args
       (~ (λ (args)
            (do [z <- (! apply f args)]
                (! k z)))))]))

(define-thunk (! <<v) (! <<v-impl Ret))

(define-rec-thunk (! <<n-impl)
  (copat
   [(k f (upto xs 'o))
    (let ([k (thunk (copat [(y) (! k (thunk (! apply f xs y)))]))])
      (! <<n-impl k))]
   [(k f (upto xs '$))
    (! k (thunk (! apply f xs)))]
   [(k f) (! dot-args
             (~ (λ (args) (! k (thunk (! apply f args))))))]))
(define-thunk (! <<n) (! <<n-impl $))

(define-thunk (! beep) (ret "beep"))
(define-thunk (! fc)
  (copat
   [(th) (! th)]))
;(! <<n fc 'o beep '$)


(define-rec-thunk (! foldl l step acc)
  (cond
    [(! empty? l) (ret acc)]
    [#:else
     (do [x <- (! car l)]
         [xs <- (! cdr l)]
       [acc <- (! step acc x)]
       (! foldl xs step acc))]))

;; (define-rec-thunk (! map f l)
;;   (! <<v reverse 'o
;;      foldl l
;;      (thunk
;;       (copat
;;        [(acc x)
;;         (do [y <- (! f x)]
;;             (ret (cons y acc)))]))
;;      '() '$))

(define-syntax (def/copat syn)
  (syntax-parse syn
    [(_ ((~literal !) f:id) ms ...)
     #`(define-rec-thunk (! f) (copat ms ...))]))
(define-syntax (def-thunk syn)
  (syntax-parse syn
    [(_ ((~literal !) f:id pat ...) es ...)
     #`(define-rec-thunk (! f) (copat [(pat ...) es ...]))]))

(define-rec-thunk (! filter p xs)
  (cond
    [(! empty? xs) (ret '())]
    [#:else
     (do [x <- (! car xs)]
         [xs <- (! <<v filter p 'o cdr xs '$)]
       (ifc (! p x)
            (ret (cons x xs))
            (ret xs)))]))

(def-thunk (! debug x) (! displayln x) (ret x))

(def/copat (! oo)
  [(f (upto xs '@)) [g <- (! f xs)] (! oo g)]
  [(f) (! f)])
(def-thunk (! @> x f) (! f x))
