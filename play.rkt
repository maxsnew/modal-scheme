#lang sbpv

;; Stdlib: todo, move to separate file
;; A Y combinator to get us moving
(define Y
  (thunk
   (case-λ
    [() (error "Y combinator expects one argument, but got none")]
    [(f)
     (let ([self-app (thunk (λ (x) (! f (thunk (! x x)))))])
       ((! self-app) self-app))])))

(require (for-syntax syntax/parse
                     (except-in racket/base quote)
                     (only-in "sbpv/main.rkt" quote)))
(define-syntax (do syn)
  (syntax-parse syn
    [(_ [x:id <- m] e es ...)
     #`(bind (x m) (do e es ...))]
    [(_ m) #`m]
    [(_ m e es ...)
     #`(bind (x m) (do e es ...))]))
(define-syntax (ifc syn)
  (syntax-parse syn
    [(_ c e1 e2) #`(bind (x c) (if x e1 e2))]))
(define-syntax (define-rec-thunk syn)
  (syntax-parse syn
    [(_ (! f:id x:id ...) e)
     #`(define f
         (thunk (let ([loop (thunk (λ (f x ...) e))])
                  (! Y loop))))]))
(define-syntax (define-thunk syn)
  (syntax-parse syn
    [(_ (! f:id x:id ...) e)
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

(define-thunk (! $ f x) (! f x))

; C combinator
(define-thunk (! swap k x y) (! k y x))

(define-rec-thunk (! const^ x)
  (case-λ
   [() (ret x)]
   [(_) (! const^ x)]))
(define const
  (thunk
   (λ (x)
     (let ([loop
            (thunk
             (λ (loop)
               (case-λ [() (ret x)] [(y) (! loop)])))])
       (! Y loop)))))
(define abort const)
(! const 3 4 5 6 7 8 9 10) ; 3
(! const^ 3 4 5 6 7 8 9 10) ; 3


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

;; Infinite loop
; (! Y (thunk (λ (loop) (! loop))))

(define-rec-thunk (! sum-loop acc)
  (case-λ
   [() (ret acc)]
   [(x) (do [acc^ <- (! + x acc)]
            (! sum-loop acc^))]))

(define-thunk (! sum) (! sum-loop 0))
(! sum)
(! sum 1)
(! sum 1 1)
(! sum 5 6 7 8) ; 26

(define-rec-thunk (! rev-loop acc lst)
  (ifc (! null? lst)
       (ret acc)
       (do [hd <- (! car lst)]
           [tl <- (! cdr lst)]
         (! rev-loop (cons hd acc) tl))))
(define-thunk (! reverse lst) (! rev-loop '() lst))
(! reverse (cons 3 (cons 4 (cons 5 null))))

;; stack-loop : forall X. (X -> ?c) -> X -> (U (?v -> X -> F X)) -> ?c
;; aka stack-foldl
(define-rec-thunk (! stack-loop k acc cons)
  (case-λ
   [() (! k acc)]
   [(x)
    (do [acc^ <- (! cons x acc)]
        (! stack-loop k acc^ cons))]))

(define-thunk (! grab-rev-stack k)
  (! stack-loop k '() Cons))

(! grab-rev-stack pop1 0 1 2 3 4 5)

(define-thunk (! grab-stack k)
  (! grab-rev-stack
     (thunk
      (λ (rev-stack)
        (do [stack <- (! reverse rev-stack)]
            (! k stack))))))

(! grab-stack pop1 0 1 2 3 4 5)

; dot-args : forall Y. (List ?v -> Y) -> Y
(define dot-args grab-stack)
(define-thunk (! List) (! dot-args pop1))

; rev-apply : (X -> ... -> ?c) -> (List X ...) -> ?c
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
   [() (! ran-out acc)]
   [(x)
    (ifc (! finished? x)
         (! finish acc x)
         (bind (acc^ (! step acc x))
               (! loop-up-to ran-out finish finished? acc^ step)))]))

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

;; A copattern is a list of patterns, representing a pattern match on
;; the stack.
;; TODO: add ...-patterns
;; A pattern is one of
;;   'var - representing a
;;   (list 'lit ?v) - representing a literal to be matched by equal?
;;   TODO: add constructors (cons)

(define copat-ex0 '())
(define copat-ex1 '((lit hd) (lit tl) var))
(define copat-ex2 '(var))

; copattern-match1 : Copattern Kont Kont List List -> ?c
(define-rec-thunk (! copattern-match1 copat match-k abort-k popped bound)
  (ifc
   (! empty? copat)
   (case-λ
    [() (! rev-apply match-k bound)]
    [(x) (! rev-apply abort-k (cons x popped))])
   (case-λ
    [() (! rev-apply abort-k popped)]
    [(scrutinee)
     (do [pat <- (! first copat)]
         [copat^ <- (! rest copat)]
       [popped^ <- (ret (cons scrutinee popped))]
       (ifc (! equal? pat 'var)
            (! copattern-match1 copat^ match-k abort-k popped^
               (cons scrutinee bound))
            (do [literal <- (! second pat)]
                (ifc (! equal? literal scrutinee)
                     (! copattern-match1 copat^ match-k abort-k popped^ bound)
                     (! rev-apply abort-k popped^)))))])))

(define-thunk (! test-fail) (! abort 'failure))
(! copattern-match1
   copat-ex0
   (thunk (ret 'good-match0))
   (thunk (! abort 'failureeeee))
   '()
   '())
(! copattern-match1
   '(var)
   (thunk (ret 'good-match1))
   (thunk (λ (x) (ret (list 'this-is-3 x))))
   '(3)
   '()
   )
(! copattern-match1
   copat-ex0
   (thunk (! Cons 'good-match2))
   (thunk (! abort 'failure))
   '(3)
   '(3))
(! copattern-match1
   copat-ex2
   (thunk (! Cons 'good-match3-its-5))
   test-fail
   '()
   '()
   5
   )
(! copattern-match1
   '(var)
   (thunk (! List 'good-match3-its-5))
   test-fail
   '(3)
   '()
   5
   )
(! copattern-match1
   '(var)
   (thunk (! List 'good-match3-its-3-5))
   test-fail
   '(3)
   '(3)
   5
   )
(! copattern-match1
   '((lit 3))
   (thunk (! List 'good-match-none))
   test-fail
   '()
   '()
   3
   )
(! copattern-match1
   '((lit 3))
   (thunk (! List 'good-match-5))
   test-fail
   '(5)
   '(5)
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
            (thunk (! copattern-match rest abort))
            '()
            '()))))
(define-thunk (! cpm-test-fail) (! List 'failure-called-with))
(! copattern-match
   (list)
   (thunk (ret 'cpm-test-pass)))

(! copattern-match
   (list (list '() (thunk (! List 'cpm-test1-pass))))
   test-fail
   )
(! copattern-match
   (list (list '(var) (thunk (! List 'cpm-test2-pass-42)))
         (list '() test-fail)
         )
   (thunk (! List 'failure))
   42)
(! copattern-match
   (list (list '() test-fail)
         (list '(var) (thunk (! List 'cpm-test2-pass-42))))
   (thunk (! List 'failure))
   42)
(! copattern-match
   (list (list '() test-fail)
         (list '((lit 42)) (thunk (ret (list 'cpm-test2-pass)))))
   (thunk (! List 'failure))
   42)
(! copattern-match
   (list (list '() test-fail)
         (list '((lit 43)) test-fail))
   (thunk (! List 'cpm-test-pass-with-42))
   42)
(! copattern-match
   (list (list '() test-fail)
         (list '((lit 43)) test-fail)
         (list '(var) (thunk (! Cons 'cpm-pass-with-42))))
   cpm-test-fail
   42)
(! copattern-match
   (list (list '() test-fail)
         (list '((lit 43)) test-fail)
         (list '(var 55) cpm-test-fail)
         (list '(var) (thunk (! Cons 'cpm-pass-with-42)))
         )
   cpm-test-fail
   42)

(define-thunk (! bad-error-msg-copattern-match copats)
  (! copattern-match
     copats
     (thunk (error "copattern match error\n I personally apologize for the bad error message -Max Stewart New"))))

(begin-for-syntax
  (define-syntax-class pat
    #:attributes (pattern all-vars)
    (pattern
     x:id
     #:attr pattern #''var
     #:attr all-vars #'(x))
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
     #:attr vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...)))))))

(define-syntax (copat syn)
  (syntax-parse syn
    [(_ [cop:copat e] ...)
     ;; #:do ((displayln #'(cop.vars ...)))
     ;; #`(list (list cop.patterns (thunk (λ cop.vars e))) ...)
     #`(! bad-error-msg-copattern-match (list (list cop.patterns (thunk (λ cop.vars e))) ...))
     ]))

