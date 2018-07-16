#lang racket/base

(require (rename-in "cbpv.rkt"
                    [bind core-bind])
         (for-syntax racket/base
                     syntax/parse
                     racket/match))

(provide (all-from-out "cbpv.rkt")
         bind define
         (rename-out [module-begin #%module-begin])
         #%top-interaction)

(define-syntax (bind syn)
  (syntax-parse syn
    [(_ x:id seq ... k)
     #`(core-bind (x (sequence seq ...))
                  k)]))
(define-syntax (define syn)
  (syntax-parse syn
    [(_ x:id seq ... k)
     #`(let (x (sequence seq ...))
                  k)]))

(begin-for-syntax
  (define-literal-set commands (define bind beep flush))
  (define top-level-command? (literal-set->predicate commands))
  (define-syntax-class top-level-id
    (pattern (c:id _ ...)
             #:fail-unless ((literal-set->predicate commands) #'c)
             "Invalid top-level command"))

  (define (pass-args lst end)
    (match lst
      ['() end]
      [(cons hd tl)
       (syntax-parse hd
         [(x ...)
          #`(x ... #,(pass-args tl end))])])))
(define-syntax (module-begin syn)
  (syntax-case syn ()
    [(_ e ...)
     #`(#%module-begin (main (sequence e ...)))]))
(define-syntax (sequence syn)
  (syntax-parse syn
    [(_ e:top-level-id ... e^)
     (pass-args (attribute e) #'e^)]))
