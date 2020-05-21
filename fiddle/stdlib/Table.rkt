#lang fiddle

(require fiddle/prelude)
(provide update update^ empty-table table<-hash)

;; codata Table K V where
;;   has-key? |- K -> F bool
;;   set      |- K -> V -> FU (Table K V)
;;   get      |- K -> V -> F V
;;   remove   |- K -> FU (Table K V)
;;   to-list  |- F (Listof (Cons K V))

(define-rec-thunk (! table<-hash h)
  (copat
   [((= 'has-key?) k #:bind) (! hash-has-key? h k)]
   [((= 'set) k v #:bind)
    (do [h <- (! hash-set h k v)]
        (ret (thunk (! table<-hash h))))]
   [((= 'get) k v #:bind)
    (ifc (! hash-has-key? h k)
         (! hash-ref h k)
         (ret v))]
   [((= 'remove) k #:bind)
    [h <- (! hash-remove h k)] (ret (~ (! table<-hash h)))]
   [((= 'to-list) #:bind) (! hash->list h)]))
(define-thunk (! table<-hash~ h) (ret (thunk (! table<-hash h))))

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
