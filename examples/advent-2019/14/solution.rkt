#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")
(require "../../table.rkt")
(require "../../set.rkt")

(provide main-a main-b)


;; solution outline:
;
;; think of a reaction as a term in a free symmetric semicartesian
;; monoidal category generated on objects by the chemicals and on
;; terms by the generators we are given
;
;; The puzzle is then, given a presentation of a free symmetric
;; monoidal category, to find the minimal n such that there is a
;; morphism Ore^n -> FUEL
;
;; It is semi-cartesian because we allow weakening: our final reaction
;; can have discard unused by-products.
;
;; There are some simplifying assumptions that make this easier than
;; the general case. First, the generators always output 1 sort of
;; chemical (though possibly many times), and each chemical has
;; exactly one corresponding generator
;
;; I think a strategy that then works is to topologically sort the
;; chemicals where the ordering is the least such that A <= B if every
;; morphism Ore^m => B^n uses the generator for A as a subterm.
;
;; Then once they're sorted we can construct the optimal term by
;; starting with the generator for FUEL: Gamma |- 1 Fuel, we
;; iteratively pick the input type that is highest in the topological
;; ordering, substitute in its generator (using weakening if needed)
;; and repeat until all inputs are ORE.
;
;; the topological ordering is the key to letting us get away with
;; this easy use of weakening without sacrificing minimality. If we
;; picked an input A that was <= a B in the ordering, and we weakened
;; away the extra As, then we might end up with a non-minimal solution
;; because those excess As might have contributed to constructing a
;; B. With the topological ordering, we know that we can discard any
;; excess Bs because they won't help in constructing the other inputs

;; A Sequent is a tuple
;; (Context Number Chemical)

(def-thunk (! mk-sequent ctx num outp) (! List ctx num outp))
(define sequent-ctx first)
(define sequent-num second)
(define sequent-outp third)

;; A Chemical is a string
;; A Context is a Listof (List Number Chemical)

;; STEP 1: parsing
;; The input is a Listof Sequents

(def/copat (! parse-num*chemical k)
  [((upto digits #\space))
   (! slurp-satisfying letter? (~ (copat
   [(letters)
    [num <- (! parse-num digits)]
    [chem <- (! list->string letters)]
    (! k num chem)])))])

(def-thunk (! parse-ctx chems k)
  (! parse-num*chemical (~ (copat
  [(num chem (= #\,) (= #\space))
   [chems <- (! <<v swap Cons chems 'o List num chem '$)]
   (! parse-ctx chems k)]
  [(num chem (= #\space) (= #\=) (= #\>) (= #\space))
   [chems <- (! <<v swap Cons chems 'o List num chem '$)]
   (! k chems)]
  [() (! displayall 'parse-error)]))))

;; Char ->* F Sequent
(def-thunk (! parse-sequent)
  (! parse-ctx '() (~ (λ (ctx)
  (! parse-num*chemical (~ (λ (num chem)
  (do
      (! <<v mk-sequent ctx num chem)))))))))

;; F (Listof Sequent)
(def-thunk (! parse-generators file)
  (! <<n
     list<-colist 'o
     cl-map (~ (! <<v apply parse-sequent 'o string->list)) 'o
     slurp-lines~ file '$))

;; STEP 2: sorting

;; produce two tables:
;; 1. Antecedents->Succedents
;; 2. Succedents->Antecedents

(def-thunk (! adjoin xs ys) (! <<v set->list 'o list->set 'o append xs ys '$))

;; List (Ante->Succs) (Succ->Antes) -> Sequent -> List (Ante->Succs) (Succ->Antes)
(def-thunk (! summarize-generator tbls seq)
  [ante->succs <- (! first tbls)] [succ->antes <- (! second tbls)]
  [antes <- (! <<v map second 'o sequent-ctx seq '$)] [succ <- (! sequent-outp seq)]
  [succ->antes <- (! update succ->antes succ antes (~ (! adjoin antes)))]
  [ante->succs
   <- (! cl-foldl^ (~ (λ (ante->succs ante)
                        (! update ante->succs ante (cons succ '()) (~ (! adjoin (cons succ '()))))))
         ante->succs
         (~ (! colist<-list antes)))]

  (! List ante->succs succ->antes)
  )

;; Listof Sequent -> F (List (Table Chem (Listof Chem)) (Table Chem (Listof Chem)))
(def-thunk (! summarize-generators gens)
  (! <<n
     cl-foldl^ summarize-generator (cons empty-table (cons empty-table '())) 'o
     colist<-list gens '$)
  )

;; Then use Kahn's algo to sort the chemicals

(def-thunk (! Kahn sorted frontier ante->succs succ->antes)
  (cond [(! empty? frontier) (ret sorted)]
        [else
         [next <- (! first frontier)] [frontier <- (! rest frontier)]
         [succs <- (! ante->succs 'get next '())]
         [remove-dependency = (~ (λ (acc succ) (do
          [frontier <- (! car acc)]
          [succ->antes <- (! cdr acc)]
          [remove-next = (~ (! filter (~ (! <<v not 'o equal? next))))]
          [succ->antes <- (! update succ->antes succ '() remove-next)]
          [frontier <- (ifc (! <<v empty? 'o succ->antes 'get succ '(0))
                            (! Cons succ frontier)
                            (ret frontier))]
          (! Cons frontier succ->antes))))]
         [updated <-
                  (! cl-foldl^ remove-dependency
                     (cons frontier succ->antes)
                     (~ (! colist<-list succs)))]
         [frontier <- (! car updated)] [succ->antes <- (! cdr updated)]
         [sorted <- (! Cons next sorted)]
         (! Kahn sorted frontier ante->succs succ->antes)
         ]))
(def-thunk (! topo-sort ante->succs succ->antes)
  (! Kahn '() (cons "ORE" '()) ante->succs succ->antes))


;; STEP 3: substituting

;; tbl-ctx
(def-thunk (! biggest-user seq priority)
  [antes <- (! <<v set<-list 'o map first 'o @> 'to-list 'o sequent-ctx seq '$)]
  (! first-such-that (~ (! antes 'member?)) (~ (! colist<-list priority))))

(def-thunk (! find-generator gens chem)
  (! first-such-that
     (~ (! <<v equal? chem 'o sequent-outp))
     (~ (! colist<-list gens))))

(def-thunk (! ceil-quotient num denom)
  [q <- (! quotient num denom)]
  [rem <- (! modulo num denom)]
  (cond [(! < 0 rem) (! + 1 q)]
        [else (ret q)]))

(def-thunk (! add-to-tbl-ctx list-ctx tbl-ctx)
  [insert
   = (~ (λ (tbl-ctx num*chem)
          (do [num <- (! first num*chem)]
              [chem <- (! second num*chem)]
            (! update tbl-ctx chem num (~ (! + num))))))]
  (! foldl list-ctx insert tbl-ctx))

;; ->b is a normal sequent but b->c is a table sequent
(def-thunk (! substitute ->b b->c)
  [reagant <- (! sequent-outp ->b)]
  [b->c-ctx <- (! sequent-ctx b->c)]
  [inp-size <- (! b->c-ctx 'get reagant #f)]
  [->b-outp-size <- (! sequent-num ->b)]
  [num-inputs-needed <- (! ceil-quotient inp-size ->b-outp-size)]

  [other-b->c-inputs <- (! b->c-ctx 'remove reagant)]
  [num*n = (~ (λ (num chem) (! <<v swap List chem 'o * num-inputs-needed num '$)))]
  [->b-ctx <- (! <<v map (~ (! apply num*n)) 'o sequent-ctx ->b)]
  [new-ctx <- (! add-to-tbl-ctx ->b-ctx other-b->c-inputs)]
  
  [num <- (! sequent-num b->c)]
  [outp <- (! sequent-outp b->c)]
  (! mk-sequent new-ctx num outp)
  )

(def-thunk (! only-uses-ore? hash-seq)
  [ctx <- (! sequent-ctx hash-seq)]
  (! and (~ (! <<v ctx 'has-key? "ORE" '$))
         (~ (! <<v = 1 'o length 'o ctx 'to-list '$))))

(def-thunk (! get-to-ore ->fuel gens priority)
  (cond [(! only-uses-ore? ->fuel) (ret ->fuel)]
        [else
         [ante <- (! biggest-user ->fuel priority)]
         [->ante <- (! find-generator gens ante)]
         [->fuel <- (! substitute ->ante ->fuel)]
         (! get-to-ore ->fuel gens priority)]))

(def-thunk (! hashify-ctx list-seq)
  (! displayall 'hashify-ctx list-seq)
  [list-ctx <- (! sequent-ctx list-seq)]
  [num <- (! sequent-num list-seq)]
  [outp <- (! sequent-outp list-seq)]
  [tbl-ctx <- (! add-to-tbl-ctx list-ctx empty-table)]
  (! mk-sequent tbl-ctx num outp))

(def-thunk (! ore->fuel n gens priority)
  [t <- (! empty-table 'set "FUEL" n)]
  [fuel-> <- (! mk-sequent t 1 "")]
  (! <<v cdr 'o first 'o @> 'to-list 'o sequent-ctx 'o get-to-ore fuel-> gens priority))

(def/copat (! main-a)
  [(#:bind) (! main-a "/dev/stdin")]
  [(f)
   [gens <- (! parse-generators f)]
   [priority <- (! <<v apply topo-sort 'o summarize-generators gens)]
   (! ore->fuel 1 gens priority)])

(define TRILLION 1000000000000)

;; invariant:
;;   f lo <= goal
;;   f hi > goal
;; if hi = lo + 1, done
(def-thunk (! bin-search f lo hi)
  (cond [(! <<v = hi 'o + 1 lo)
         (ret lo)]
        [else
         [mid <- (! <<v swap quotient 2 'o + lo hi)]
         [mid-y <- (! f mid)]
         (cond [(! <= mid-y TRILLION) (! bin-search f mid hi)]
               [else (! bin-search f lo mid)])]))

(def/copat (! main-b)
  [(#:bind) (! main-a "/dev/stdin")]
  [(f)
   [gens <- (! parse-generators f)]
   [priority <- (! <<v apply topo-sort 'o summarize-generators gens)]
   (! bin-search (~ (λ (n) (! ore->fuel n gens priority))) 1 TRILLION)])
