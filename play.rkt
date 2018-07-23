#lang sbpv

;; Infinite loop
#;
(let ([Y
       (thunk
        (case-λ
         [() (error "Y combinator expects one argument, but got none")]
         [(f)
          (let ([self-app (thunk
                           (case-λ
                            [() (error "self-app should always call itself")]
                            [(x) ((! f) (thunk ((! x) x)))]))])
            ((! self-app) self-app))]))])
  ((! Y) (thunk
          (case-λ [() (error "nope")]
                  [(loop) (! loop)]))))
(define Y
  (thunk
         (case-λ
          [() (error "Y combinator expects one argument, but got none")]
          [(f)
           (let ([self-app (thunk
                            (case-λ
                             [() (error "self-app should always call itself")]
                             [(x) (! f (thunk (! x x)))]))])
             ((! self-app) self-app))])))

(define sum
  {thunk
   (let ([loop
          (thunk
           (λ (loop acc)
             (case-λ
                [() (ret acc)]
                [(x)
                 (bind (acc^ (! + x acc))
                       (! loop acc^))])))])
     (! Y loop 0))})
(! sum)
(! sum 1)
(! sum 1 1)
(! sum 5 6 7 8) ; 26

(define const
  (thunk
   (λ (x)
     (let ([loop
            (thunk
             (λ (loop)
               (case-λ [() (ret x)] [(y) (! loop)])))])
       (! Y loop)))))

(! const 3 4 5 6 7 8 9 10) ; 3
