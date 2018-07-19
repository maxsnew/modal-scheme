#lang racket

(require redex)

(define-language sbpv
  [v ::= boolean string (cons v v) nil (thunk e) (fix x e)]
  [e ::= (! v)
     (symbol=? v v)
     (let [x e] e) (e v)
     (case-lambda [() e] [(x) e])
     (match v
       [#t e]
       [#f e]
       [(cons x y) e]
       [nil e]
       [symbol e]
       [thunk e])
     wrong
     (ret v)]
  [s ::= hole (let [x hole] e) (hole v)]
  [x y ::= variable-not-otherwise-mentioned]
  #:binding-forms
  (let [x e] e #:refers-to x)
  (case-lambda [() e] [(x) e #:refers-to x])
  (match v
    [#t e]
    [#f e]
    [(cons x y) e #:refers-to (shadow x y)]
    [nil e]
    [symbol e]
    [thunk e]))
(define-metafunction sbpv
  sym=? : symbol symbol -> boolean
  [(sym=? string_1 string_2)
   ,(string=? (term string_1) (term string_2))])

(define red
  (reduction-relation
   sbpv
   #:domain e
   (--> (in-hole s (! (thunk e)))
        (in-hole s e)
        "Ubeta")
   (--> (in-hole s (! (fix x e)))
        (in-hole s (substitute e x (fix x e))))
   (--> (in-hole s (symbol=? string_1 string_2))
        (in-hole s (ret ,(string=? (term string_1) (term string_2)))))
   (--> (in-hole s (let [x (ret v)] e))
        (in-hole s (substitute e x v))
        "Fbeta")
   (--> (in-hole s (let [y (case-lambda [() e_1] [(x) e_2])] e_3))
        (in-hole s (let [y e_1] e_3))
        "CompDynNone")
   (--> (in-hole s ((case-lambda [() e_1] [(x) e_2]) v))
        (in-hole s (substitute e_2 v x))
        "CompDynArg")
   (--> (in-hole s
                 (match #t
                   [#t e_t]
                   [#f e_f]
                   [(cons x y) e_c]
                   [nil e_n]
                   [symbol e_s]
                   [thunk e_th]))
        (in-hole s e_t)
        "ValDynTrue")
   (--> (in-hole s
                 (match #f
                   [#t e_t]
                   [#f e_f]
                   [(cons x y) e_c]
                   [nil e_n]
                   [symbol e_s]
                   [thunk e_th]))
        (in-hole s e_f)
        "ValDynFalse")
   (--> (in-hole s
                 (match (cons v_x v_y)
                   [#t e_t]
                   [#f e_f]
                   [(cons x y) e_c]
                   [nil e_n]
                   [symbol e_s]
                   [thunk e_th]))
        (in-hole s (substitute (substitute e_c v_x x) v_y y))
        "ValDynCons")
   (--> (in-hole s
                 (match nil
                   [#t e_t]
                   [#f e_f]
                   [(cons x y) e_c]
                   [nil e_n]
                   [symbol e_s]
                   [thunk e_th]))
        (in-hole s e_n)
        "ValDynNil")
   (--> (in-hole s
                 (match string
                   [#t e_t]
                   [#f e_f]
                   [(cons x y) e_c]
                   [nil e_n]
                   [symbol e_s]
                   [thunk e_th]))
        (in-hole s e_s)
        "ValDynSym")
   (--> (in-hole s
                 (match (thunk e)
                   [#t e_t]
                   [#f e_f]
                   [(cons x y) e_c]
                   [nil e_n]
                   [symbol e_s]
                   [thunk e_th]))
        (in-hole s e_th)
        "ValDynThunk")
   (--> (in-hole s
                 (match (fix x e)
                   [#t e_t]
                   [#f e_f]
                   [(cons x y) e_c]
                   [nil e_n]
                   [symbol e_s]
                   [thunk e_th]))
        (in-hole s e_th)
        "ValDynThunkFix")))
