#lang sbpv

;; A Y combinator to get us moving
(define Y
  (thunk
   (case-λ
    [() (error "Y combinator expects one argument, but got none")]
    [(f)
     (let ([self-app (thunk (λ (x) (! f (thunk (! x x)))))])
       ((! self-app) self-app))])))

;; Infinite loop
; (! Y (thunk (λ (loop) (! loop))))

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

(define reverse
  (thunk
   (let ([loop
          (thunk
           (λ (loop acc rest)
             (bind (mt? (! null? rest))
              (if mt?
                  (ret acc)
                  (bind (hd (! car rest))
                        (bind (tl (! cdr rest))
                              (! loop (cons hd acc) tl)))))))])
     (! Y loop null))))

(! reverse (cons 3 (cons 4 (cons 5 null))))
