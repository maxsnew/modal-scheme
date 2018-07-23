#lang racket/base

(require (only-in "sbpv.rkt" st foreign foreign? foreign-payload))

(define (datum? x)
  (or (boolean? x)
      (number? x)
      (string? x)
      (symbol? x)
      (null? x)))
;; rkt->sbpv
(define (rkt->sbpv x)
  (cond
    [(datum? x) x]
    [(pair? x) (cons (rkt->sbpv (car x)) (rkt->sbpv (cdr x)))]
    [(procedure? x)
     (λ ()
       (define args (unbox st))
       (set-box! st '())
       (rkt->sbpv (apply x (map sbpv->rkt args))))]
    [else (foreign x)]))

;; sbpv->rkt
(define (sbpv->rkt x)
  (cond
    [(datum? x) x]
    [(pair? x) (cons (sbpv->rkt (car x))
                     (sbpv->rkt (cdr x)))]
    [(foreign? x) (foreign-payload x)]
    [(procedure? x)
     (λ args       
       (set-box! st (map rkt->sbpv args))
       (sbpv->rkt (x)))]))

(module+ test
  (require rackunit)
  (check-equal? (sbpv->rkt #t) #t)
  (check-equal? (rkt->sbpv #t) #t)
  
  (check-equal? ((sbpv->rkt (rkt->sbpv list)) 1 2 3) '(1 2 3))
  (check-equal? ((sbpv->rkt (rkt->sbpv (λ args (reverse args)))) 1 2 3) '(3 2 1)))

