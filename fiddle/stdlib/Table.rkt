#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList)
(provide update update~ update^ empty-table
         map-vals
         table<-hash table<-list table-set<-list table-set-intersect
         universal-table-set

         push-tbl
         split-adjacency-tbl
         )

;; codata Table K V where
;;   has-key? |- K -> F bool
;;   empty?   |- F Bool
;;   set      |- K -> V -> FU (Table K V)
;;   get      |- K -> V -> F V
;;   remove   |- K -> FU (Table K V)
;;   to-list  |- F (Listof (Cons K V))

(define-rec-thunk (! table<-hash h)
  (copat
   [((= 'has-key?) k #:bind) (! hash-has-key? h k)]
   [((= 'empty?) #:bind) (! hash-empty? h)]
   [((= 'set) k v #:bind)
    (do [h <- (! hash-set h k v)]
        (ret (thunk (! table<-hash h))))]
   [((= 'get) k v #:bind)
    (ifc (! hash-has-key? h k)
         (! hash-ref h k)
         (ret v))]
   [((= 'remove) k #:bind)
    [h <- (! hash-remove h k)] (ret (~ (! table<-hash h)))]
   [((= 'to-list) #:bind) (! hash->list h)]
   [((= 'to-hash) #:bind) (ret h)]))

(define-thunk (! table<-hash~ h) (ret (thunk (! table<-hash h))))

;; Listof (Cons K V) -> FU(Table K V)
(def-thunk (! table<-list kvs)
  [flattened <- (! foldr kvs (~ (copat [((cons k v) acc) (ret (cons k (cons v acc)))])) '())]
  [h <- (! apply hash flattened)]
  (ret (~! table<-hash h)))

(def/copat (! universal-table-set)
  [((= 'empty?) #:bind) (ret #f)]
  [((= 'has-key?) k #:bind) (ret #t)]
  [((= 'get) k v #:bind)     (ret #t)])

(def-thunk (! table-set<-list xs)
  (! CBV (~! map (~! swap List #t) xs)
     % v> (~! apply append)
     % v> (~! apply hash)
     % v> table<-hash~
     % v$))

;; U(Table A Bool) -> U(Table A Bool) -> FU (Table A Bool)
(def-thunk (! table-set-intersect t2 t1)
  (! CBV (~! t1 'to-list)
     % v> (~! map car)
     % v> (~! filter (~ (λ (k) (! t2 'has-key? k))))
     % v> table-set<-list
     % v$))

(def-thunk (! update~ tbl k v~ updater)
  (ifc (! tbl 'has-key? k)
       (do [v <- (! tbl 'get k #f)]
           [new-v <- (! updater v)]
         (! tbl 'set k new-v))
       (! <<v tbl 'set k 'o v~)))

(define-thunk (! update tbl k v updater)
  (ifc (! tbl 'has-key? k)
       (do [v <- (! tbl 'get k #f)]
           [new-v <- (! updater v)]
         (! tbl 'set k new-v))
       (! tbl 'set k v)))
(def-thunk (! update^ k v up tbl) (! update tbl k v up))


(define empty-table (thunk
                     (do [h <- (! hash)]
                         (! table<-hash h))))
#;
(do [t <- (! <<v table<-hash~ 'o hash 'x 5 'y 16 '$)]
    (! <<v
       (~ (λ (t) (! t 'to-list))) 'o
       t 'remove 'x))

(def-thunk (! map-vals f t)
  (! CBV (~! t 'to-list)
     % v> (~! map (~ (copat [((cons k v))
                            [v <- (! f v)]
                            (ret (cons k v))])))
     % v> table<-list
     % v$))

;; An AdjTbl A B is a Table (list A B) Bool

;; Table K (Table V Bool) -> K -> V -> Table K (Table V Bool)
(def-thunk (! push-tbl t k v)
  (! update~ t k (~! empty-table 'set v #t) (~! swap apply (list 'set v #t))))

;; Table (list A B) Bool -> List (Table A (Listof B)) (Table B (Listof A))
(def-thunk (! split-adjacency-tbl rel)
  (! cl-foldr (~! <<v colist<-list 'o rel 'to-list)
     (~ (copat [((cons (list l r) _) k l->rs r->ls)
                [l->rs <- (! push-tbl l->rs l r)]
                [r->ls <- (! push-tbl r->ls r l)]
                (! k l->rs r->ls)]))
     (~ (λ (l->rs r->ls) (! map (~! map-vals (~! <<v map car 'o swap apply '(to-list))) (list l->rs r->ls))))
     empty-table
     empty-table))
