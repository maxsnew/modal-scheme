#lang turnstile

;; A CBPV Scheme-like
;;
(require (rename-in racket/function (thunk thunk-)))
(provide (all-defined-out)
         (rename-out (many-app #%app)))
(define (force- th) (th))
(define st (box '()))

(struct foreign (payload))

(define-base-type value)
(define-base-type computation)

;; Values
;; 
(define-typed-syntax #%datum
  [(_ . d) ≫
   -----------------
   (⊢ (#%datum- . d) ⇒ value)])

(define-typed-syntax (if e e1 e2) ≫
  (⊢ e ≫ e- ⇐ value)
  (⊢ e1 ≫ e1- ⇐ computation)
  (⊢ e2 ≫ e2- ⇐ computation)
  ------------------
  (⊢ (if- e- e1- e2-) ⇒ computation))

(define-typed-syntax (ret e) ≫
  (⊢ e ≫ e- ⇐ value)
  ----------------
  (⊢
   (let- ([x (unbox st)])
     (if- (null? x)
          e-
          (error- (format "expected a return address on the stack but got stack ~a" x))))
   ⇒ computation))

(define-typed-syntax (bind (x:id e) e^) ≫
  (⊢ e ≫ e- ⇐ computation)
  ((x ≫ x- : value) ⊢ e^ ≫ e^- ⇐ computation)
  -----------------
  (⊢ (let- ()
       (define tmp (unbox st)) ;; Save the current stack
       (set-box! st '())       ;; Hide the stack from e
       (define x- e-)          ;; run e
       (set-box! st tmp)       ;; restore the stack
       e^-)
     ⇒ computation))

(define-typed-syntax (let ([x e] ...) e^) ≫
  (⊢ e ≫ e- ⇐ value) ...
  ((x ≫ x- : value) ... ⊢ e^ ≫ e^- ⇐ computation)
  -----------------
  (⊢ (let- ([x- e-] ...) e^-)
     ⇒ computation))
(define-typed-syntax let*
  [(_ () ebod) ≫
   --------------
   (≻ ebod)]
  [(_ ([x e] rst ...) ebod) ≫
   --------------
   (≻ (let ([x e]) (let* (rst ...) ebod)))])

(define-typed-syntax (unsafe-+ e ...) ≫
  (⊢ e ≫ e- ⇐ value) ...
  -----------------
  (⊢ (+- e- ...) ⇒ value))
(define-typed-syntax +
  [(_ e ...) ≫
   ---------
   (≻ (ret (unsafe-+ e ...)))])
(define-typed-syntax λ
  ([_ () ebod] ≫
   ---------------
   [≻ ebod])
  [(_ (x xs ...) ebod) ≫
   ------------------
   [≻ (case-λ
       [() (error "expected more arguments but didn't get them")]
       [(x) (λ (xs ...) ebod)])]])

(define-typed-syntax (zero? e) ≫
  (⊢ e ≫ e- ⇐ value)
  --------------------------
  (⊢ (zero?- e-) ⇒ value))


(define-typed-syntax (thunk e) ≫
  (⊢ e ≫ e- ⇐ computation)
  ----------------
  (⊢ (thunk- e-) ⇒ value))

(define-typed-syntax (! e) ≫
  (⊢ e ≫ e- ⇐ value)
  ----------------
  (⊢ (force- e-) ⇒ computation))

(define-typed-syntax (case-λ [() e] [(x) ex]) ≫
  (⊢ e ≫ e- ⇐ computation)
  ((x ≫ x- : value) ⊢ ex ≫ ex- ⇐ computation)
  --------------------------------
  (⊢ (let- ()
       (define cur (unbox st))
       (cond
         [(null? cur) e-]
         [else
          (define x- (car cur))
          (set-box! st (cdr cur))
          ex-]))
     ⇒ computation))

(define-typed-syntax (^ e1 e2) ≫
  (⊢ e1 ≫ e1- ⇐ computation)
  (⊢ e2 ≫ e2- ⇐ value)
  ----------------
  (⊢ (let- ()
           (set-box! st (cons e2- (unbox st)))
           e1-)
     ⇒ computation))

(define-typed-syntax many-app
  [(_ e) ≫
   -------
   [≻ e]]
  [(_ e x xs ...) ≫
   -----------------
   [≻ (many-app (^ e x) xs ...)]])

(define-typed-syntax (error e ...) ≫
  (⊢ e ≫ e- ⇐ value) ...
  ---------------------------
  (⊢ (error- e- ...) ⇒ computation))

(define-typed-syntax (main e) ≫
  (⊢ e ≫ e- ⇐ computation)
  ----------------
  (⊢ (let- ([x- e-]) x-) ⇒ computation))

(module+ test
  (require
    turnstile/rackunit-typechecking
    rackunit)
  
  (check-type #t : value)
  (check-type #f : value)
  (check-type (bind (x (ret #t)) (ret x)) : computation)
  (typecheck-fail (if #t #t #f))
  (check-equal? (bind (x (ret #t)) (ret x)) #t)
  (check-type (! 3) : computation)
  (check-type (many-app (! 3) 4) : computation)
  (check-type (many-app (! 3) 4 5 6) : computation)
  )
