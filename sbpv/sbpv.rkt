#lang turnstile

;; A CBPV Scheme-like
;;
(require (rename-in racket/function (thunk thunk-))
         "initialize.rkt"
         (for-syntax syntax/parse))
(provide (all-defined-out)
         (rename-out (many-app #%app)))
(define (force- th) (th))

(define-base-type value)
(define-base-type computation)


(define-syntax (require-wrapped-provide stx)
  (syntax-parse stx
    [(_ lib x)
     #:with x-tmp (generate-temporary #'x)
     #:with x-wrapped (generate-temporary #'x)
     #'(begin-
         (require (only-in lib [x x-tmp]))
         (define x-wrapped (rkt->sbpv x-tmp))
         (define-primop x x-wrapped : value)
         (provide x))]))
(define-syntax (require-fo-wrapped-provide stx)
  (syntax-parse stx
    [(_ lib x)
     #:with x-tmp (generate-temporary #'x)
     #:with x-wrapped (generate-temporary #'x)
     #'(begin-
         (require (only-in lib [x x-tmp]))
         (define x-wrapped (fo-rkt->sbpv x-tmp))
         (define-primop x x-wrapped : value)
         (provide x))]))

(define-syntax (require-wrapped stx)
  (syntax-parse stx
    [(_ lib x)
     #:with x-tmp (generate-temporary #'x)
     #:with x-wrapped (generate-temporary #'x)
     #'(begin-
         (require (only-in lib [x x-tmp]))
         (define x-wrapped (rkt->sbpv x-tmp))
         (define-primop x x-wrapped : value))]))
#;
(define-syntax (from-racket stx)
  (syntax-parse stx
    [(_ lib x)
     ]))

(require-fo-wrapped-provide racket/base +)
(require-fo-wrapped-provide racket/base abs)
(require-fo-wrapped-provide racket/base *)
(require-fo-wrapped-provide racket/base modulo)
(require-fo-wrapped-provide racket/base quotient)
(require-fo-wrapped-provide racket/base /)
(require-fo-wrapped-provide racket zero?)
(require-fo-wrapped-provide racket/base -)
(require-fo-wrapped-provide racket/base <)
(require-fo-wrapped-provide racket/base <=)
(require-fo-wrapped-provide racket/base =)
(require-fo-wrapped-provide racket/base >)
(require-fo-wrapped-provide racket/base >=)
(require-fo-wrapped-provide racket/base not)
(require-fo-wrapped-provide racket number?)
(require-fo-wrapped-provide racket cons?)
(require-wrapped-provide racket null)
(require-fo-wrapped-provide racket null?)
(require-fo-wrapped-provide racket/base car)
(require-fo-wrapped-provide racket/base cdr)
(require-fo-wrapped-provide racket/base equal?)
(require-fo-wrapped-provide racket/base symbol?)
(require-fo-wrapped-provide racket/base string<=?)
(require-fo-wrapped-provide racket/base string-append)
(require-fo-wrapped-provide racket/base read-line)
(require-fo-wrapped-provide racket/base read-char)
(require-fo-wrapped-provide racket/base displayln)
(require-fo-wrapped-provide racket/base string->list)
(require-fo-wrapped-provide racket/base list->string)
(require-fo-wrapped-provide racket/base char-upcase)
(require-fo-wrapped-provide racket/base char-downcase)
(require-fo-wrapped-provide racket/base char->integer)
(require-fo-wrapped-provide racket/base integer->char)
(require-fo-wrapped-provide racket/base string?)
(require-fo-wrapped-provide racket/base char?)
(require-fo-wrapped-provide racket/base eof-object?)
(require-fo-wrapped-provide racket/base hash?)
(require-fo-wrapped-provide racket/base hash)
(require-fo-wrapped-provide racket/base hash-set)
(require-fo-wrapped-provide racket/base append)
(require-fo-wrapped-provide racket/base hash-ref)
(require-fo-wrapped-provide racket/base hash-remove)
(require-fo-wrapped-provide racket/base hash-empty?)
(require-fo-wrapped-provide racket/base hash-has-key?)
(require-fo-wrapped-provide racket/base hash-count)
(require-fo-wrapped-provide racket/base hash->list)
;; mutable vector stuff
(require-fo-wrapped-provide racket/base make-vector)
(require-fo-wrapped-provide racket/base vector?)
(require-fo-wrapped-provide racket/base vector-ref)
(require-fo-wrapped-provide racket/base vector-length)
(require-fo-wrapped-provide racket/base vector-set!)
(require-fo-wrapped-provide racket/base list->vector)
(require-wrapped-provide racket/base sort)

;; Values
;;

(define-typed-syntax quoth
  [(_ . e) ≫
   -----------
   (⊢ (quote- . e) ⇒ value)])

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
     (if- (null?- x)
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

(define-typed-syntax λ
  ([_ () ebod] ≫
   ---------------
   [≻ ebod])
  [(_ (x xs ...) ebod) ≫
   ------------------
   [≻ (case-λ
       [(#:bind) (error "expected more arguments but didn't get them")]
       [(x) (λ (xs ...) ebod)])]])

(define-typed-syntax (cons e es) ≫
  (⊢ e ≫ e- ⇐ value)
  (⊢ es ≫ es- ⇐ value)
  ----------------------
  (⊢ (cons- e- es-) ⇒ value))

(define-typed-syntax (thunk e) ≫
  (⊢ e ≫ e- ⇐ computation)
  ----------------
  (⊢ (thunk- e-) ⇒ value))

(define-typed-syntax (basic-! e) ≫
  (⊢ e ≫ e- ⇐ value)
  ----------------
  (⊢ (force- e-) ⇒ computation))

(define-typed-syntax (! e es ...) ≫
  ------------------------
  (≻ (many-app (basic-! e) es ...)))


(define-typed-syntax (case-λ [(#:bind) e] [(x:id) ex]) ≫
  (⊢ e ≫ e- ⇐ computation)
  ((x ≫ x- : value) ⊢ ex ≫ ex- ⇐ computation)
  --------------------------------
  (⊢ (let- ()
       (define- cur (unbox- st))
       (cond-
         [(null?- cur) e-]
         [else
          (define- x- (car- cur))
          (set-box!- st (cdr- cur))
          ex-]))
     ⇒ computation))

(define-typed-syntax (^ e1 e2) ≫
  (⊢ e1 ≫ e1- ⇐ computation)
  (⊢ e2 ≫ e2- ⇐ value)
  ----------------
  (⊢ (let- ()
           (set-box! st (cons- e2- (unbox st)))
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

(define-typed-syntax (typed-define x e) ≫
  (⊢ e ≫ e- ⇐ value)
  #:with x-tmp (generate-temporary #'x)
  --------------------
  (≻
   (begin-
     (define x-tmp e-)
     (define-syntax x (make-variable-like-transformer (assign-type
                                                       #'x-tmp #'value
                                                       #:wrap? #f))))))
(define-typed-syntax (letrec ([x:id ex] ...) e) ≫
  ((x ≫ x- : value) ... ⊢ (ex ≫ ex- ⇐ value) ... (e ≫ e- ⇐ computation))
  ------------------------------------
  (⊢ (letrec- ([x- ex-] ...) e-) ⇒ computation))
#;
(define-typed-syntax mutual-recursive
  [(_ (define-thunk (! x:id y:id ...) e) ...)] ≫
  ((x ≫ x- : value) ... ⊢ (define-thunk (! x y ...) e) ≫ e- : value) ...
  --------------------------------
  [≻ (begin- e- ...)])

(module+ test
  (require
    rackunit/turnstile
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
