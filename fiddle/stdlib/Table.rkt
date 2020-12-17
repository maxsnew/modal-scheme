#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList)
(provide update update~ update^ empty-table
         table<-hash table<-list table-set<-list table-set-intersect
         universal-table-set)

;; codata Table K V where
;;   has-key? |- K -> F bool
;;   empty?   |- F Bool
;;   set      |- K -> V -> FU (Table K V)
;;   get      |- K -> V -> F V
;;   remove   |- K -> FU (Table K V)
;;   to-list  |- F (Listof (Cons K V))

;; Listof (Cons K V) -> FU(Table K V)
(def-thunk (! table<-list kvs)
  [flattened <- (! foldr kvs (~ (copat [((cons k v) acc) (ret (cons k (cons v acc)))])) '())]
  (! apply hash flattened))

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
