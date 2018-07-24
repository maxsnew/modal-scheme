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

(require (for-syntax syntax/parse racket/base))
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
;; A (simple) copattern decision tree (CDT) is represented by a
;;   (list (U ?c) (Listof (list Pat CDT))
;; where the car is the continuation for no arguments left
;; and the cdr list is a list of patterns and the CDT to use if the
;; pattern matches, which are processed *in order*.
;;
;; A pattern (Pat) is represented by one of
;;   | 'var
;;   | (cons 'lit ?v)
;; where 'var represents a variable and 'lit says we check for
;; equality with a datum
;; later on we will add dot-args and dot-args upto

(define-thunk (! second lst)
  (do [tl <- (! cdr lst)]
      (! car tl)))

;; done : U ?c
;; decide : Listof (list Pat CDT)
;; acc  : Listof ?v, the arguments in reverse order to the eventual continuation
;;   TODO: for better error messages, make acc record the decisions made
(define-thunk (! interp-tree cdt)
  (letrec
      ([interp-tree-loop
        (thunk
         (λ (done decide args)
           (case-λ
            [()
             (! rev-apply done args)
             ]
            [(x)
             (! try-alts decide args x)])))]
       [try-alts
        (thunk
         (λ (decide args discrim)
           (ifc (! null? decide)
                (! abort 'copattern-match-error)
                (do [hd <- (! car decide)]
                    [decide^ <- (! cdr decide)]
                  [pat <- (! car hd)]
                  [pat-matched <- (! second hd)]
                  (ifc (! symbol? pat)
                       (! apply interp-tree-loop pat-matched (cons discrim args))
                       (do [lit <- (! second pat)]
                           (ifc (! equal? lit discrim)
                                (! apply interp-tree-loop pat-matched args)
                                (! try-alts decide^ args discrim))))))))])
    (! apply interp-tree-loop cdt '())))

(define-syntax (list syn)
  (syntax-parse syn
    [(_) #`'()]
    [(_ e es ...)
     #`(cons e (list es ...))]))

(define-rec-thunk (! List) (! dot-args pop1))

(define matched0 (list (thunk (ret 'match0)) '()))
(define matched1 (list (thunk (λ (x) (ret x))) '()))
(! interp-tree
   (cons (thunk (ret 'match))
         (cons '() '())))
(! interp-tree
   (list (thunk (ret 'match0))
         (list (list 'var matched1)))
   'match1)
(! interp-tree
   (list (thunk (ret 'wrong))
         (list (list '(lit foo)  matched1)))
   'bar)

(! interp-tree
   (list (thunk (ret 'wrong))
         (list (list '(lit foo) matched0)))
   'foo)
#;
(define-thunk (! interp-tree cdt k)
  (do [done <- (! car cdt)]
      [decide <- (! cdr cdt)]
    (! interp-tree-loop done decide '())))
