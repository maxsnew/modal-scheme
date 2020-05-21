#lang racket/base

(provide (struct-out foreign)
         stack regs
         rkt->fiddle fiddle->rkt fo-rkt->fiddle)

;; This is the *stack*, implemented as a mutable pointer to a list
(define stack (box '()))
;; This is the *register file*, implemented as a mutable hash table
;; kw -o> val
(define regs (make-hash))

(struct foreign (payload))

(define (fiddle-datum? x)
  (or (boolean? x)
      (number? x)
      (string? x)
      (symbol? x)
      (null? x)
      (char? x)
      (keyword? x)))
;; rkt->fiddle

;; wraps first-order functions from racket to fiddle
(define (fo-rkt->fiddle x)
  (cond
    [(procedure? x)
     (λ ()
       (define args (unbox stack))
       (set-box! stack '())
       (apply x args))]
    [else (error 'fo-rkt->fiddle-is-for-fo-funs)]))

;; racket value -> fiddle value
(define (rkt->fiddle x)
  (cond
    [(fiddle-datum? x) x]
    [(pair? x) (cons (rkt->fiddle (car x)) (rkt->fiddle (cdr x)))]
    [(procedure? x)
     (λ ()
       (define args (unbox stack))
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

