#lang turnstile

;; A CBPV Scheme-like
;;
(require (rename-in racket/function (thunk thunk-)))
(provide (all-defined-out))
(define (force- th) (th))
(define st (box '()))

(struct foreign (payload))

(define-base-type v)
(define-base-type c)

;; Values
;; 
(define-typed-syntax #%datum
  [(_ . d) ≫
   -----------------
   (⊢ (#%datum- . d) ⇒ v)])

(define-typed-syntax (if e e1 e2) ≫
  (⊢ e ≫ e- ⇐ v)
  (⊢ e1 ≫ e1- ⇐ c)
  (⊢ e2 ≫ e2- ⇐ c)
  ------------------
  (⊢ (if- e- e1- e2-) ⇒ c))

(define-typed-syntax (ret e) ≫
  (⊢ e ≫ e- ⇐ v)
  ----------------
  (⊢
   (let ([x (unbox st)])
     (if- (null? x)
          e-
          (error (format "expected a return address on the stack but got stack ~a" x))))
   ⇒ c))

(define-typed-syntax (bind (x:id e) e^) ≫
  (⊢ e ≫ e- ⇐ c)
  ((x ≫ x- : v) ⊢ e^ ≫ e^- ⇐ c)
  -----------------
  (⊢ (let ()
       (define tmp (unbox st)) ;; Save the current stack
       (set-box! st '())       ;; Hide the stack from e
       (define x- e-)          ;; run e
       (set-box! st tmp)       ;; restore the stack
       e^-)
     ⇒ c))

(define-typed-syntax (thunk e) ≫
  (⊢ e ≫ e- ⇐ c)
  ----------------
  (⊢ (thunk- e-) ⇒ v))

(define-typed-syntax (! e) ≫
  (⊢ e ≫ e- ⇐ v)
  ----------------
  (⊢ (force- e-) ⇒ c))

(define-typed-syntax (case-λ [() e] [(x) ex]) ≫
  (⊢ e ≫ e- ⇐ c)
  ((x ≫ x- : v) ⊢ ex ≫ ex- ⇐ c)
  --------------------------------
  (⊢ (let ()
       (define cur (unbox st))
       (cond
         [(null? cur) e-]
         [else
          (define x- (car cur))
          (set-box! st (cdr cur))
          ex-]))
     ⇒ c))

(define-typed-syntax (^ e1 e2) ≫
  (⊢ e1 ≫ e1- ⇐ c)
  (⊢ e2 ≫ e2- ⇐ v)
  ----------------
  (⊢ (let- ()
           (set-box! st (cons e2- (unbox st)))
           e1-)
     ⇒ c))

(module+ test
  (require
    turnstile/rackunit-typechecking
    rackunit)
  
  (check-type #t : v)
  (check-type #f : v)
  (check-type (bind (x (ret #t)) (ret x)) : c)
  (typecheck-fail (if #t #t #f))
  (check-equal? (bind (x (ret #t)) (ret x)) #t)
  )
