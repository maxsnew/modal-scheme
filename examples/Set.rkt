#lang fiddle

(require fiddle/prelude)

(provide set<-hash set<-list empty-set set-single)

;; codata Set A where
;;   'empty? |- F bool
;;   'member? |- A -> F bool
;;   'add     |- A -> FU (Set A)
;;   'remove  |- A -> FU (Set A)
;;   'to-list |- F (Listof A)
(def-thunk (! set<-hash h)
  (copat
   [((= 'empty?) #:bind) (! hash-empty? h)]
   [((= 'member?) x #:bind)
    (! hash-has-key? h x)]
   [((= 'add) x #:bind)
    [h <- (! hash-set h x #t)]
    (ret (~ (! set<-hash h)))]
   [((= 'remove) x #:bind)
    [h <- (! hash-remove h x)]
    (ret (~ (! set<-hash h)))]
   [((= 'to-list) #:bind) (! <<v map first 'o hash->list h)]
   [((= 'debug) #:bind) (! <<v displayln 'o hash-count h)]))


;; Set A
(def-thunk (! empty-set) [e <- (! hash)] (! set<-hash e))
;; set-single : A -> FU Set A
(def-thunk (! set-single x) [h <- (! hash x #t)]
   (ret (~ (! set<-hash h))))

(def-thunk (! set<-list xs)
  (cond [(! empty? xs) (ret (~ (! empty-set)))]
        [else
         [x <- (! first xs)] [xs-set <- (! <<v set<-list 'o rest xs '$)]
         (! xs-set 'add x)]))
