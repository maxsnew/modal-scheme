#lang racket

(require (for-syntax syntax/parse))
(begin-for-syntax
  (define-syntax-class pat
    #:attributes (pattern all-vars)
    (pattern
     x:id
     #:attr pattern #''var
     #:attr all-vars #'(x))
    (pattern
     e:expr
     #:attr pattern #'e
     #:attr all-vars #'()))
  (define-syntax-class copat
    #:attributes (patterns vars)
    (pattern
     (p:pat ...)
     #:attr patterns #`(list p.pattern ...)
     #:when (displayln (syntax-e #`(p.all-vars ...)))
     #:attr vars #`#,(apply append (map syntax-e (syntax-e #`(p.all-vars ...)))))))
(define-syntax (copat syn)
  (syntax-parse syn
    [(_ [cop:copat e] ...)
     #:do ((displayln #'(cop.vars ...)))
     #`(list (list cop.patterns (λ cop.vars e)) ...)]))

(copat)

((cadar (copat [(72 x 5 y) (list x y)])) 4 5)

#;
(copat
 [() ...]
 [(x) ...]
 [(5) ...]
 [(6 x) ...]
 [(7 x y) e] ; (list (lit 7) var var) (λ (x y) e)
 [('hd) 3]
 [('tl 'hd) 4])
