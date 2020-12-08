#lang fiddle

(require fiddle/prelude)

(provide raiseE ;; raising operations

         retE abortE ;; returning
         mapE ;; variable-arity version of return (sort of)

         bindE ;; 
         appE appE^ ;; variable-arity version of bind

         handle re-handle ;; handlers/algebras
         firstReq stateAlg
         )

;; All free monads can be given in the form
;; Eff req rsp v = nu c. F (v + (Sigma r:req. U(rsp r -> c)))
;; or in CPS nuc. U(v -> y) -> U(Pi r:req. U(rsp r -> c) -> y) -> y

;; Then the appropriate universal free monad is to make the request and response type dynamic:
;; Eff ? ? a = nu c. F (a + (? * U(? -> c)))
;; in CPS this is
;; nu c. forall y. U(v -> y) -> U(? -> U(? -> c) -> y) -> y
;; And this is the representation we use.

(define! eff (! new-method 'eff 2))


(def/copat (! retE)
  [(x (% eff (succ op))) (! succ x)]
  [() (! error "retE expected an argument and an effect handler but got: ...")])

(def/copat (! abortE)
  [(x (upto _ (% eff))) (! retE x)]
  [() (! error "abortE expexts at least one argument followed by an effect handler but got:")])

(def/copat (! opE)
  [(req responder (% eff (succ op))) (! op req responder)]
  [() (! error "opE expected a request, a responder and an effect handler but got: ...")])

(def-thunk (! raiseE req) (! opE req retE))

;; handle E[raise req] { req, k -> N } =~ N[(~ (λ (x) handle E[x] { req, k -> N }))]

;; handle : U(Eff v) -> U(v -> B) -> U(Req -> U(Rsp -> B) -> B) -> B
(def-thunk (! handle t retHandler opHandler)
  (! t % eff retHandler (~ (λ (req k) (! opHandler req (~ (λ (rsp) (! handle (~ (! k rsp)) retHandler opHandler))))))))

;; U(Eff v) -> U(v -> Eff v')
(def/copat (! bindE)
  [(t k (% eff (succ op)))
   (! t
      % eff
      (~ (λ (x) (! k x % eff succ op)))
      (~ (λ (req responder)
           (! op req (~ (λ (rsp) (! bindE (~ (! responder rsp)) k)))))))
   ]
  [() (! error "bindE expected a thunk, a kont and an effect handler but got: ...")])

(def-thunk (! bindE^ t k)
  (! handle t )
  )

;; To define an algebra of Eff req rsp, we need a type B and
;; bindH : U(Eff req rsp v) -> U(v -> B) -> B
;; such that
;; bindH (~ (! retE x)) k = (! k x)
;; bindH (~ (! bindE t kE)) kH = (! bindH t (~ (λ (x) (! bindH (~ (! kE x)) kH))))
;
;; One way to do it is with a value type s and a single function
;; s -> req -> F(rsp * s)
;; too linear though, this always returns back to s precisely once(!)
;
;; we would like to factor out the recursion instead

(def/copat (! appEloop)
  [(f '()) (! f)]
  [(f (cons t ts))
   (! bindE t (~ (λ (x) (! appEloop (~ (! f x)) ts))))])

;; Multi-argument bind function
;; f : U(A -> ... -> Eff v) -> U(Eff A) -> ... -> Eff v
(def/copat (! appE^)
  [(f (upto ts (% eff))) (! appEloop f ts)]
  [() (! error "appE^: expected a bunch of thunks and then the handler, but got...")])

(def-thunk (! appE) (! appE^ $))

;; Multi-argument map function
;; Essentially the equivalent of f being an applicative
;; f : U(A -> ... -> F v) -> U(Eff A) -> ... -> Eff v
(def-thunk (! mapE f)
  [ret-o-f = (~ (copat
   [((upto xs (% eff)))
    [f-of-xs <- (! apply f xs)]
    (! retE f-of-xs)]))]
  (! appE^ ret-o-f))

; This pushes a lot of dispatching into userspace code

;; Here's some handlers for convenience
; U(Eff v) -> U(v -> F('ret v^  or 'op req k)) -> F ('ret v^ or 'op req k)
(def-thunk (! firstReq t k)
  (! t % eff k (~ (λ (req responder) (! List 'op req responder)))))

(def-thunk (! stateAlg init step t)
  (! t % eff Ret
     (~ (λ (req responder)
          (patc (! step req init)
            [(list rsp new-state)
             (! stateAlg new-state step (~ (! responder rsp)))])))))

(def-thunk (! re-handle t opHandler)
  (! handle t retE opHandler))

;; ;; Eff v = forall Y. U(v -> Y) -> U(v1 -> ... -> Y) -> U(v2 -> ... -> Y) -> ... -> Y

;; ;; We can use a sigil based design to make a generic effect system so
;; ;; that we can make generic implementations of bind return, and
;; ;; therefore a whole monad library.

;; ;; An effect scheme

;; (define kontinuations 'kontsstarthere)
;; (define hiddenstack 'hiddenstack)

;; (def/copat (! retE)
;;   [(x (= kontinuations) retK (upto ks hiddenstack))
;;    (! retK x hiddenstack)]
;;   [() (! error "retE usage: x konststarthere k ... hiddenstack but got:")])

;; ;; We need to know where the kontinuations are in the effect arguments

;; ;; For example the read effect is
;; ;; Read r v = forall Y. U(v -> Y) -> U(U(r -> Read r v) -> Y) -> Y
;; ;;
;; ;; 'handlers forall Y. U(v -> 'opaque -> Y) -> U(v1 -> ... -> 'resume U(v11 -> ... -> 'unfold Eff v) -> ... -> 'opaque Y) -> ... -> 'opaque Y
;; (def/copat (! bindEKont)
;;   [(f  ;; U(v -> ... -> 'resume U(v -> ... -> 'unfold Eff a^) -> ... -> 'opaque Y)
;;     k  ;; a -> Eff a^
;;     (upto data     resumeMark) ;; v -> ... -> 'resume
;;     (upto resumers hiddenstack)) ;; U(v -> ... -> 'unfold Eff a) -> ... -> 'opaque Y
;;    ]
;;   (! f )
;;   )

;; (def/copat (! bindE)
;;   [(th k (= kontinuations) retK (upto hs hiddentstack))
;;    [hs^ <- (! map bindEKont hs)]
;;    (! apply (~ (! th kontinuations (~ (λ (x) (! apply (~ (! k x)) kontinuations retK hs)))))
;;       hs^)
;;    ]
;;   [() (! error "improper bindE usage:")])


;;
(def-thunk (! inc) (! raiseE 'inc))
(def-thunk (! evalInc t init)
  (! handle t
     abort
     (~ (copat
         [('inc resume x)
          [x+1 <- (! + 1 x)]
          (! resume x x+1)]))
     init))

(def/copat (! last x)
  [(y) (! last y)]
  [(#:bind) (ret x)])
