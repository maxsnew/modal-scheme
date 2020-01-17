#lang sbpv

;; In Haskell, we are used to simulating call-by-value effects using
;; *monads*

;; I hypothesize that the right way to do (CBV) user-defined effects
;; in CBPV is not through monads but *relative monads*, specifically
;; monads relative to F : v -> c.

;; Let's consider the error monad A + E for fixed E. In CBPV this
;; would be an endofunctor on values, but then bind and return
;; morphisms would be *values*. It seems more natural that erroring
;; computations be written as computations, for instance.
;; F (A + E) or something Church encoded/CPS'd like
;
;;   forall Y. U(A -> Y) -> U(E -> Y) -> Y
;
;; This has the advantages of 1. Allowing ambient CBPV effects and
;; 2. being actually quite similar to the stack layout of low-level
;; implementations of exception handlers.

;; Clearly, this isn't a monad anymore, for the obvious reason that
;; it's not even an endofunctor! But Altenkirch, Chapman and Uustalu
;; famously told us that monads need not be endofunctors. So maybe we
;; can get a nice notion of user-defined effect in CBPV by using
;; relative monads.

;; What would it be?

;; Translating almost directly from category theory, we would want two
;; methods in the typeclass

;; class Eff : v -> c where
;;   ret  : F X -o Eff X
;;   bind : (F X -o Eff X') => Eff X -o Eff X'

;; The UP of F simplifies things a bit:
;
;; class Eff : v -> c where
;;   ret  : X -> Eff X
;;   bind : U(X -> Eff X') => Eff X -o Eff X'

;; But we'll need to make a concession in this last one:

;; class RelMonad (Eff : v -> c) where
;;   ret  : X -> Eff X
;;   bind : U(X -> Eff X') -> U(Eff X) -> Eff X'

;;   side-condition: bind should be linear in the U(Eff X) argument
;;   but this might be implied by the monad laws anyway.
;
;; something a little shocking to me about this definition is that it
;; is the definition of "monad relative to F", but there is no mention
;; of F(!) meaning we don't even need to assume that F is defined(!)

;; the monad laws of course:

;; bind mK (ret x) == ! mk x
;; bind (λ x. ret x) th == ! th (implies linearity)
;; bind^ (bind^ th (λ y. th')) (λ x. th'') == bind^ th (λ y. (bind^ th' (λ x. th'')))
;;
;; ([x <- [y <- th; th']; th'' == y <- th; x<- th'; th''


;; Let's try some examples
;; Church encoded Error monad
;;
;; Err E A := forall Y. U(A -> Y) -> U(E -> Y) -> Y
(def/copat (! retE x kV kE) (! kV x))

;; U(Err E X) -> U(X -> Err E X') -> Err E X'
(def/copat (! bindE th mK kV kE)
  (! th (~ (λ (x) (! mK x kV kE))) kE))


;; (bindE (retE x) mK kV kE)
;; == (! retE x (λ y (! mK y kV kE)) kE)
;; == ((λ y (! mK y kV kE)) x)
;; == (! mK x kV kE)

;; How about a church encoded state monad?
;; St S A := forall Y. S -> U(X -> S -> Y) -> Y
(def/copat (! retSt x s k) (! k x))
(def/copat (! bindSt th mK s k)
  (! th s (~ (λ (x s) (! mK x s k)))))

;; Here's a more interesting one, what about the delimited
;; continuation monad?
;; 
;; In Haskell Cont R X = (X -> R) -> R
;;
;; In CBPV we can do the same thing but with R being a computation type:
;;
;; Cont R A := U(A -> R) -> R
(def/copat (! retK x k) (! k x))
(def/copat (! bindK th mK k)
  (! th (~ (λ (x) (! mK x k)))))

;; Reader?
;; Reader R A := R -> F A
(def/copat (! retR x r) (! ret x))
(def/copat (! bind th mK r)
  [x <- (! th r)]
  (! mK x r))

;; Church encoded List monad?
;; F (Listof A)
;; =~ forall Y. U(Listof A -> Y) -> Y
;; =~ forall Y. U((1 + A * Listof A) -> Y) -> Y
;; =~ forall Y. UY -> U(A * Listof A -> Y) -> Y
;; =~ forall Y. UY -> U(A -> Listof A -> Y) -> Y
;; =~ forall Y. UY -> U(A -> Y) -> U(A -> A -> Y) -> ... -> Y
;; =~ nu L. forall Y. UY -> U(A -> UL -> Y) -> Y

(def/copat (! retR x doneK moreK)
  (! moreK x (~ (λ (doneK moreK) (! doneK)))))

(def/copat (! eff-append xs ys nilK consK)
  (! xs (~ (! ys nilK consK))
     (~ (λ (x xs)
          (! consK x (~ (! append xs ys)))))))

(def/copat (! bind th ;; U(ListEff X)
                   mK ;; X -> U(ListEff X')
                   doneK ;; UY
                   moreK ;; U(X' -> U(ListEff X') -> Y)
                   );; Y
  (! th doneK
     (λ (x xs)
       (! eff-append (~ (! mK x doneK moreK))
                     (~ (! bind xs mK doneK moreK))))))

;; What do algebras of these monads look like?

;; class RelMonad Eff => Algebra Eff (R : c) where
;;   ext : (F X -o R) => (Eff X -o R)

;; which we can also write as bind:
;; bind : U(Eff X) -> U(X -> R) -> R
;; whoa that right hand side is the (relative) continuation monad!

;; is the continuation monad a representing object for algebras? I
;; bet it is!

;; Eff algebra structure on a type R is equivalent to a monad morphism
;; from Eff => Cont R ?
;
;; Cont : v -> Monad
;;
;; /Cont : Monad -> Cat
;; is equivalent to EM?


;; 1. For every monad Eff and val type X, there is a monad morphism
;; Eff - -o Cont (Eff X)
;; because this is equivalent to
;; Algebra Eff (Eff X)
;; so we just use the free algebra structure
(def/copat (! embed return bind) (! bind))

;; 2. Run
;; Cont (Eff X) X -o Eff X
(def/copat (! run return bind th)
  (! th return))


;; Back to algebras
;; First, clearly bind itself exhibits that Eff X' is an algebra for any X'
;; bind : U(Eff X) -> U(X -> Eff X') -> Eff X'
;
;; Next, Algebras are closed under X' -> R
;; impl RelMonad Eff, Algebra Eff R => Algebra Eff (X' -> R) where
(def/copat (! bindReader bindAlg ;; Algebra Eff R
              th ;; U(Eff X)
              readerK ;; X -> X' -> R
              x^) ;; X' -> R
  (! bindAlg th (~ (λ (x) (! readerK x x^)))))

;; This means we get our own model of CBPV within CBPV where
;; value types are value types
;; computation types are algebras of the monad

;; This should automatically get us some kind of composition of
;; relative monads, right?

;; forall Y:c. B ~~> 
(def/copat (! bindForall
              bindB ; forall Y. Algebra Eff Y -> Algebra Eff B
              th ; U(Eff X)
              k ;; U(X -> forall Y. Algebra Eff Y -> B). 
              bindY) ;; Algebra Eff Y -> B
  (! bindB bindY th (~ (λ (x) (! k x bindY)))))


;; what about relative monad transformers?  they don't seem to
;; maintain that efficiency aspect that we want, because they just end
;; up applying value constructors.

;; StateT Eff S A = S -> Eff (A x S)

;; ExnT Eff E A = Eff (E + A)
;; =~ forall Y. (Eff (E + A) -o Y) -> Y

;; typical exn/state monad
;; Eff = forall Y. S -> U(A -> S -> Y) -> U(E -> S -> Y) -> Y
;; rollback state exn monad
;; Eff = forall Y. S -> U(A -> S -> Y) -> U(E -> Y) -> Y

;; What if instead we made them something like an arrow?

;; State X Y = U(X -> S -> Y) -> S -> Y

;; Err X Y = U(X -> Y) -> U(E -> Y) -> Y

;; What about the dual? comonads relative to U?
;; I guess it would look something like
; 
;; class Comonad (W : c -> v) where
;;   force : W Y => U Y
;;   apply : (W Y => U Y') => W Y => W Y'
; again some UP magic simplifies much

;; class Comonad W where
;;   force : W Y -> Y
;;   apply : U(W Y -> Y') -> W Y -> F(W Y')

;; with laws
;; apply force x = ret x
;; ...

;; examples from Haskell

;; CoState? Stream? Tape?
;; Reader R B := R x UB if R is a cbv type
;; or should it be U(R & B) =~ UR x UB? 

(def/copat (! forceReader (cons r th)) (! th))
(def/copat (! applyReader f (cons r th))
  (ret (cons r (~ (! f (cons r th))))))

;; what is the "co-operation" that this allows?
;; read : Reader R B -> R or Reader R B -> F R if R is a val type

(def/copat (! read-coop (cons r th)) (ret r))

;; iStream B := Streamof B := mu L. U(B & F(Streamof B)) =~ mu L. UB x UF(Streamof B)
(def/copat (! force-iStream (cons th more)) (! th))
(def/copat (! apply-iStream f (cons th more))
  (! interleave (~ (! f th)) (~ (! <<v apply-iStream 'o more))))

(def/copat (! interleave (cons x xs) (cons y ys))
  (ret (cons x (~ (ret (cons y (~ (do [xs <- (! xs)] [ys <- (! ys)] (! interleave xs ys)))))))))

;; Stream B := UB x U(Stream B)

(def-thunk (! force-Stream (cons hd tl)) (! hd))
(def-thunk (! apply-Stream f str@(cons hd tl))
  (ret (cons (~ (! f str)) (~ (! apply-Stream f tl)))))

;; A comonad coalgebra would be something that "implements" apply:
;; class Comonad W => Algebra W (S : v) where
;;   apply : U(S -> Y) -> S -> F(W Y)

;; Are coalgebras closed under x?

(def-thunk (! apply-x apply1 ; forall Y. U(S1 -> Y) -> S1 -> F(W Y)
              apply2         ; forall Y. U(S2 -> Y) -> S2 -> F(W Y)
              f ; U(S1 x S2 -> Y)
              (cons x1 x2); S1 x S2
              ) ; F(W Y)
  (! apply2 (~ ()) ;; S2 -> Y
     x2)
  [g <-; W (S2 -> Y)
     (~ (! apply1 (~ (! curry f)) x1))]
  ;; to get a F(W Y), we need a
  ;; U(W (S2 -> Y) -> Y) which we def do not have!
  (! apply .. g))

;; What about 1?
(def-thunk (! apply-1 f ; U(W T -> Y)
              unit ; W T
              ) ; F(W Y)
  (! apply ))


;; What about 0?
(def/copat (! apply-0 f)) ;; 0 -> Y

;; What about +?
(def/copat (! apply-+ apply1 apply2 f)
  [((cons (= 'inl) x1)) (! apply1 (~ f 'o 'inl) x1)]
  [((cons (= 'inr) x2)) (! apply2 (~ f 'o 'inr) x2)])

;; This makes 

;; unlike monads, comonads don't have the same opportunities for
;; benefit from church encoding, but we can optimize a little by using
;; tuples rather than &
