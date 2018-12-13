#lang sbpv

;; codata Table K V where
;;   has-key? |- K -> F bool
;;   set      |- K -> V -> FU (Table K V)
;;   get      |- K -> V -> F V
;;   to-list  |- F (Listof (Cons K V))

(define-rec-thunk (! table<-hash h)
  (copat
   [((= 'has-key? k) #:bind) (! hash-has-key? h k)]
   [((= 'set) k v #:bind)
    (do [h <- (hash-set h k v)]
        (ret (thunk (! table<-hash h))))]
   [((= 'get) k v #:bind)
    (ifc (! hash-has-key? h k)
         (! hash-ref h k)
         (ret v))]
   [((= 'to-list) #:bind) (! (hash->list h))]))

(define (! update tbl k v updater)
  (ifc (! tbl 'has-key? k)
       (do [v <- (! tbl 'get k #f)]
           [new-v <- (! updater v)]
           (! tbl 'set k new-v))
       (! tbl 'set k v)))
