#lang turnstile

(require (rename-in racket/function (thunk thunk-)))
(provide
 let main str-main
 U thunk force
 F bind return
 -> λ (rename-out (^ #%app))
 bool true false if
 beep flush
 )
(define (force- f) (f))
(define (beep- bit)
  (display (if- bit 1 0)))

(define-syntax-category kind)
(define-base-kind vty)
(define-base-kind cty)

(define st (box '()))

(begin-for-syntax
  (current-type? (λ (t) (or (vty? t) (cty? t)))))

(define-internal-type-constructor U)
(define-kinded-syntax (U B) ≫
  (⊢ B ≫ B- ⇐ cty)
  -----------------
  (⊢ (U- B-) ⇒ vty))

(define-typed-syntax (thunk e) ≫
  (⊢ e ≫ e- ⇒ B (⇒ ~cty))
  ----------------
  (⊢ (thunk- e) ⇒ (U B)))

(define-typed-syntax (force e) ≫
  (⊢ e ≫ e- ⇒ (~U B))
  -------------------
  (⊢ (force- e-) ⇒ B))

(define-typed-syntax (let (x e) e1) ≫
  (⊢ e ≫ e- ⇒ A (⇒ ~vty))
  ((x ≫ x- : A) ⊢ e1 ≫ e1- ⇒ B (⇒ ~cty))
  ------------------------
  (⊢ (let- ([x- e-]) e1-) ⇒ B))

(define-base-type bool : vty)
(define-internal-type-constructor F)
(define-kinded-syntax (F A) ≫
  (⊢ A ≫ A- ⇐ vty)
  --------------
  (⊢ (F- A-) ⇒ cty))
(define (return- v) v)
(define-typed-syntax (return v) ≫
  (⊢ v ≫ v- ⇒ A (⇒ ~vty))
  ------------------
  (⊢ (return- v-) ⇒ (F A)))

(define-typed-syntax (bind (x:id e) e^) ≫
  (⊢ e ≫ e- ⇒ (~F A))
  ((x ≫ x- : A) ⊢ e^ ≫ e^- ⇒ B- (⇒ ~cty))
  -----------------
  (⊢
   (let- ([x- e-]) e^-) ⇒ B-))

(define-typed-syntax (beep v e) ≫
  [⊢ v ≫ v- ⇐ bool]
  [⊢ e ≫ e- ⇒ B (⇒ ~cty)]
  --------------------
  [⊢ (begin (beep- v-) e-) ⇒ B])
(define-typed-syntax (flush e) ≫
  [⊢ e ≫ e- ⇒ B (⇒ ~cty)]
  --------------------------
  [⊢ (begin (displayln "") e-) ⇒ B])
(define-typed-syntax)

(define-typed-syntax true
  (_:id ≫
   -----------------
   (⊢ #t ⇒ bool)))

(define-typed-syntax false
  (_:id ≫
   -----------------
   (⊢ #f ⇒ bool)))

(define-typed-syntax (if e e1 e2) ≫
  (⊢ e ≫ e- ⇐ bool)
  (⊢ e1 ≫ e1- ⇒ B2 (⇒ ~cty))
  (⊢ e2 ≫ e2- ⇐ B2)
  ------------------
  (⊢ (if- e- e1- e2-) ⇒ B2))

(define-internal-type-constructor ->)
(define-kinded-syntax (-> A B) ≫
  (⊢ A ≫ A- ⇐ vty)
  (⊢ B ≫ B- ⇐ cty)
  ----------------
  (⊢ (->- A- B-) ⇒ cty))

(define-typed-syntax (λ (x : A) e) ≫
  (⊢ A ≫ A- ⇐ vty)
  ((x ≫ x- : A-) ⊢ e ≫ e- ⇒ B (⇒ ~cty))
  --------------------------------------
  (⊢
   (let- ([x- (car (unbox st))])
     (set-box! st (cdr (unbox st)))
     e-)
   ⇒ (-> A- B)))

(define-typed-syntax (^ e1 e2) ≫
  (⊢ e1 ≫ e1- ⇒ (~-> A B))
  (⊢ e2 ≫ e2- ⇐ A)
  ----------------
  (⊢ (let- ()
           (set-box! st (cons e2- (unbox st)))
           e1-)
     ⇒ B))

(define-base-type ⊥)
(define-typed-syntax (main e) ≫
  (⊢ e ≫ e- ⇒ (~F A))
  ----------------
  (⊢ e- ⇒ ⊥))
(define-typed-syntax (str-main e) ≫
  (⊢ e ≫ e- ⇒ (~F A))
  ----------------
  (⊢ (let* ([s (open-output-string)]
             [x (parameterize ([current-output-port s])
                  e-)])
       (list x (get-output-string s))) ⇒ ⊥))


(main (bind (x (return true)) (return x)))
(module+ test
  (require
    turnstile/rackunit-typechecking
    rackunit)
  
  (check-type (return true) : (F bool))
  (check-type (return false) : (F bool))
  (typecheck-fail (return (return true)) #:with-msg "expected .* `vty")

  (check-type (thunk (return true)) : (U (F bool)))
  (check-type (force (thunk (return true))) : (F bool))
  (check-type (main (force (thunk (return true)))) : ⊥)

  (check-equal? (main (if true (return false) (return true))) #f)
  (check-equal? (main (bind (x (return true)) (return x))) #t)
  (check-equal? (main (return true)) #t)

  (check-type (let (x true) (return x)) : (F bool))
  (check-equal? (main (force (thunk (let (x true) (return x))))) #t)

  (check-type (λ (x : bool) (return x)) : (-> bool (F bool)))
  (typecheck-fail (λ (x : bool) x) #:with-msg "expected .* `cty")
  (typecheck-fail (λ (x : (F bool)) x) #:with-msg "expected vty")
  
  (check-type (^ (λ (x : bool) (return x)) true) : (F bool))
  (check-equal? (main (^ (λ (x : bool) (return x)) true)) #t)
  (typecheck-fail (^ true true) #:with-msg "Expected ->")

  (check-type (beep true (return true)) : (F bool))
  (typecheck-fail (beep (return true) (return false)))
  (check-equal? (str-main (beep true (beep false (beep true (return true)))))
                (list #t "101"))
  (check-equal?
   (str-main
    (let (t (thunk (beep true (return true))))
      (return false)))
   (list #f ""))
  (check-equal?
   (str-main
    (let [x (thunk (beep true (return true)))]
      (beep false (force x))))
   '(#t "01")))
