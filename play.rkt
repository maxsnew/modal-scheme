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

;; CBN Composition
(define-thunk (! .n f g x)
  (! f (thunk (! g x))))

;; CBV Composition
(define-thunk (! .v f g x)
  (do [y <- (! g x)]
      (! f y)))

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

(define pop1 (thunk (λ (x) (ret x))))
(define Cons (thunk (λ (hd tl) (ret (cons hd tl)))))
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

#;
(define interp-tree
  (thunk
   (let ([loop
          (thunk (λ (loop tree)
                   (bind (done (! car tree))
                         (bind (next (! cdr tree))
                               ))))]))))
