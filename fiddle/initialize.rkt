#lang racket/base

(provide (struct-out foreign) (struct-out ctype) (struct-out method)
         stack regs new-method matches-method? invoke-method
         rkt->fiddle fiddle->rkt fo-rkt->fiddle)

;; This is the *stack*, a Box Methods
;; where Methods is one of
;; - '() -- meaning the calling context is expecting a value
;; - '(cons ? Methods) -- an argument pushed on
;; - '(method sym (listof ?) Methods) -- a method with its associated arguments and the remaining methods
(define stack (box '()))

;; This is the *register file*, implemented as a mutable hash table
;; kw -o> val
(define regs (make-hash))

(struct vtype (name
               arity))
(struct ctype (name    ;; a gensym'd symbol
               arity)  ;; a natural number
  #:transparent
  )

(struct method (name ;; a gensym'd symbol
                args ;; a list of arguments
                tl)  ;; the rest of the methods
  #:transparent)

(define (matches-method? methods cty)
  (and (method? methods)
       (equal? (method-name methods)
               (ctype-name  cty))))

;; 
(define (invoke-method meths cty)
  (define (loop meths remaining args)
    (cond [(zero? remaining)
           (method (ctype-name cty) (reverse args) meths)]
          [(pair? meths)
           (define hd (car meths))
           (define meths^ (cdr meths))
           (loop meths^ (sub1 remaining) (cons hd args))]
          [else
           (error "tried to apply a method but didn't get enough arguments")]))
  (unless (ctype? cty)
    (error "tried to apply something that wasn't a method: " cty))
  (loop meths (ctype-arity cty) '()))

(struct foreign (payload))

(define (new-method name arity)
  (cond [(and (symbol? name)
              (exact-nonnegative-integer? arity))
         (ctype (gensym name) arity)]
        [else
         (error "new-method expects a symbol for a name and natural number for arity but got" name arity)]))

(define (fiddle-datum? x)
  (or (boolean? x)
      (number? x)
      (string? x)
      (symbol? x)
      (null? x)
      (char? x)
      (keyword? x)
      (struct? x)))

(define (rkt-stack?! args)
  (unless (list? args)
    (error "Racket FFI error: the fiddle stack uses foreign methods that are incompatible with the racket stack " args)))
;; rkt->fiddle
;; wraps first-order functions from racket to fiddle
(define (fo-rkt->fiddle x)
  (cond
    [(procedure? x)
     (λ ()
       (define args (unbox stack))
       (rkt-stack?! args)
       (set-box! stack '())
       (apply x args))];; if the stack isn't a list, this will "go wrong"
    [else (error 'fo-rkt->fiddle-is-for-fo-funs)]))

;; racket value -> fiddle value
(define (rkt->fiddle x)
  (cond
    [(fiddle-datum? x) x]
    [(pair? x) (cons (rkt->fiddle (car x)) (rkt->fiddle (cdr x)))]
    [(procedure? x)
     (λ ()
       (define args (unbox stack))
       (rkt-stack?! args)
       (set-box! stack '())
       (rkt->fiddle (apply x (map fiddle->rkt args))))]
    [else (foreign x)]))

;; fiddle->rkt
(define (fiddle->rkt x)
  (cond
    [(fiddle-datum? x) x]
    [(pair? x) (cons (fiddle->rkt (car x))
                     (fiddle->rkt (cdr x)))]
    [(foreign? x) (foreign-payload x)]
    [(procedure? x)
     (λ args       
       (set-box! stack (map rkt->fiddle args))
       (fiddle->rkt (x)))]))

(module+ test
  (require rackunit)
  (check-equal? (fiddle->rkt #t) #t)
  (check-equal? (rkt->fiddle #t) #t)
  
  (check-equal? ((fiddle->rkt (rkt->fiddle list)) 1 2 3) '(1 2 3))
  (check-equal? ((fiddle->rkt (rkt->fiddle (λ args (reverse args)))) 1 2 3) '(3 2 1)))

