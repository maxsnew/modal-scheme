#lang fiddle

;; Let's defunctionalize factorial

;; First, we do the naive recursive definition
;; Int -> F Int
(def/copat (! fac n)
  (cond [(! zero? n) (ret 1)]
        [else
         [n-1 <- (! - n 1)]
         [facn-1 <- (! fac n-1)]
         (! * n facn-1)]))
;; Oh no! it use too much stack!
;; What do?
;
;; The stack growth comes from a use of <- with a recursive call:
;; [facn-1 <- (! fac n-1)]
;; (! * n facn-1)
;; Let's defunctionalize this instead
;
;; codata Factorializer where
;;   .mult : Int -> Factorializer
;;   .done  : F Int

;; multiply-all : Int -> Factorializer
(def/copat (! multiply-all acc)
  [((= 'mult) n)
   [acc <- (! * acc n)]
   (! multiply-all acc)]
  [((= 'done)) (ret acc)])

;; Int -> Factorializer
(def/copat (! factorializer n)
  (cond [(! zero? n) (! multiply-all 1)]
        [else
         [n-1 <- (! - n 1)]
         (! factorializer n-1 'mult n)]))

(def/copat (! fac1 n) (! factorializer n 'done))

;; We still haven't actually fixed anything, space-wise! We are still
;; pushing a number onto the stack at every loop. 
;
;; The final reusable insight is that we know that any factorializer
;; is equivalent to multiply-all n for some n, so maybe we can
;; optimize what methods we use because
;;
;; (! multiply-all n .mult m1 .mult m2)
;; == (! multiply-all n .mult m1*m2)
;
;; (! multiply-all n .done) = (! multiplay-all n .mult 1 .done)
;
;; Recursively this means any method call applied to multiply-all is
;; equivalent to
;;
;; codata Factorializer' where
;;   acc : Int -> F Int
(def/copat (! factorialzer2 n _ acc)
  (cond [(! zero? n) (ret acc)]
        [else
         [n-1 <- (! - n 1)] [acc <- (! * n acc)]
         (! factorialzer2 n acc)]))

(def/copat (! fac2 n)
  (! factorializer2 n 'acc 1))


;; Let's try foldl from cbv foldr

;; the value-type foldr has type
;; U(A -> Acc -> F Acc) -> Acc -> Listof A -> F Acc
;; and is written as
(def/copat (! foldr f init)
  [(nil) (ret init)]
  [((cons x xs))
   [acc <- (! foldr f init xs)]
   (! f x acc)])

;; Now let's look, there's one bind after the recursive call
;; [acc <- *] (! f x acc)
;; so let's turn this into a method that pushes x onto the stack instead

;; codata Foldrizer A Acc where
;; .f-x-of    : A -> Foldrizer A Acc
;; .ret-init  : F Acc

;; original function : Input -> F Res
;; interpreter : Methodizer
;; methodicalized-loop : F Res -o Methodizer 

;; loop    : Input -> Foldrizer A Acc -o Foldrizer A Acc
;; interp  : Foldrizer A Acc
;; cleanup : Foldrizer A Acc -o F Res

(def/copat (! interp f acc)
  [(.done) (ret acc)]
  [(.cons x)
   [acc <- (! f x acc)]
   (! interp f acc)])

;; List A -> U(Foldrizer A Acc) -> Foldrizer A Acc
(def/copat (! loop)
  [(nil th) (! th)]
  [((cons x xs) th)
   (! call-foldrizer xs th .cons x)])

(def/copat (! cleanup th) (! th .ret-init))

(def/copat (! foldr2 f init xs)
  (! cleanup (! loop xs (! interp f acc))))

;; bin-tree-foldr : (Acc -> NodeVal -> Acc -> F Acc) -> (LeafVal -> F Acc) -> BinTree NodeVal LeafVal -> F Acc
(def/copat (! btfoldr nodeF leafF)
  [((leaf x)) (! leafF x)]
  [((node l x r))
   [l-acc <- (! btfoldr nodeF leafF l)]
   [r-acc <- (! btfoldr nodeF leafF r)]
   (! nodeF l-acc x r-acc)])

;; codata BTFoldrReturn NodeVal LeafVal Acc where
;;   .ret   : F Acc
;;   .left  : NodeVal -> BinTree NodeVal LeafVal -> BTFoldReturn NodeVal LeafVal Acc
;;   .right : Acc -> NodeVal -> BTFoldReturn NodeVal LeafVal Acc

(def/copat (! cleanup th) (! th .leafF))
(def/copat (! methodizer k)
  [((leaf x)) (! k .leafF x)]
  [((node l x r)) (! methodizer k l .left x r)])

;; FixedData -> BinTree NV LF -> BTFoldReturn NV LF Acc
(def/copat (! defuncFoldr leafF nodeF)
  [((leaf x))
   [acc <- (! leafF x)]
   (! Return leafF nodeF acc)]
  [((node l y r)
    (! traverse leafF nodeF l .left y r))])

;; FixedData -> F Acc -o FoldReturn NV LF Acc
;; FixedData -> Acc -> FoldReturn NV LF Acc
(def/copat (! Return acc)
  [(.ret) (ret acc)]
  [(.left n r)
   (! traverse leafF nodeF r .right acc n)]
  [(.right acc-l n)
   [acc <- (! nodeF acc-l n acc)]
   (! Return leafF nodeF acc)])

(def/copat (! loop nodeF leafF)
  [(.leafF x .ret) (! leafF x)]
  [(.leafF x .left x r)]
  [(.left x r) (! loop nodeF leafF )]
  )


;; Defunctionalizing Kontinuations in CBPV

;; First, "methodize" by turning F A into DefuncF A.
;; This changes arbitrary continuations (F stacks) into method calls of a codata type
;; This *does not* help the space usage! All the space is still on the stack

;; Then do a Stack->Heap transformation by turning a codata type into
;; a function that takes in a data type

;; data BTFoldrK NV LV Acc where
;;   ret()
;;   left(NV, BTree NV LV, BTFoldrK)
;;   right(Acc, NV, BTFoldrK)

;; BTFoldReturnHeap = BTFoldrK NV LV Acc -> F Acc
