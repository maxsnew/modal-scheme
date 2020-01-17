#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../Parse.rkt")
(require "../Stream.rkt")
(require "../CoList.rkt")
(require "../table.rkt")
(provide main-a main-b)

(define-thunk (! list-ref l n)
  (! stream-ref (thunk (! stream<-list l)) n))

(define-thunk (! monoid-cl-foldl)
  (copat
   [(m)
    (do [* <- (! first m)]
        [e <- (! second m)]
      (! cl-foldl^ * e))]))

;; A Monoid for a type A is a (list (A -> A -> F A) A)



(define-thunk (! parse-entry)
  (copat
   [(#\[ y y y y #\-
     mth1 mth2 #\-
     d1 d2 #\space
     h1 h2 #\:
     min1 min2 #\] #\space
     )
    (do [month <- (! parse-num (list mth1 mth2))]
        [day   <- (! parse-num (list d1 d2))]
      [hr <- (! parse-num (list h1 h2))]
      [min <- (! parse-num (list min1 min2))]
      [datetime <- (ret (list month day hr min))]
      (copat
       [(#\G u a r d spc #\# (upto digs #\space))
        (do [id <- (! parse-num digs)]
            (! abort (list 'datetime datetime id)))]
       [(#\f) (! abort (list 'datetime datetime 'falls-asleep))]
       [(#\w) (! abort (list 'datetime datetime 'wakes-up))]
       ))]))

(define entry-id third)
(define-thunk  (! guard?)
  (copat [(e) (! <<v number? 'o third e '$)]))

(define-thunk (! rec<)
  (copat
   [(r1 r2)
    (do [dt1 <- (! second r1)] [dt2 <- (! second r2)]
      [m1 <- (! first dt1)] [m2 <- (! first dt2)]
      [d1 <- (! second dt1)] [d2 <- (! second dt2)]
      [h1 <- (! third dt1)] [h2 <- (! third dt2)]
      [min1 <- (! fourth dt1)] [min2 <- (! fourth dt2)]
      (cond
        [(! equal? m1 m2)
         (cond [(! equal? d1 d2)
                (cond [(! equal? h1 h2) (! < min1 min2)]
                      [#:else (! < h1 h2)])]
               [#:else (! < d1 d2)])]
        [#:else (! < m1 m2)]))]))

;; (! <<v apply parse-entry 'o string->list "[1518-09-11 00:02] Guard #863 begins shift" '$)
;; (! <<v apply parse-entry 'o string->list "[1518-09-11 23:59] Guard #863 begins shift" '$)
;; (! <<v apply parse-entry 'o string->list "[1518-11-23 00:14] falls asleep" '$)
;; (! <<v apply parse-entry 'o string->list "[1518-04-29 00:43] wakes up" '$)

(define-thunk (! group-entries)
  (letrec
      ([guards
        (thunk
         (copat
          [(acc es)
           (cond
             [(! empty? es) (ret acc)]
             [#:else
              (do [id <- (! <<v entry-id 'o car es '$)]
                  (! <<v naps acc (list id) 'o cdr es '$))])]))]
       [naps
        (thunk
         (copat
          [(acc g es)
           (cond [(! empty? es)
                  (do [g <- (! reverse g)] (ret (cons g acc)))]
                 [#:else
                  (do [hd <- (! car es)]
                      (cond [(! guard? hd)
                             (do [g <- (! reverse g)]
                                 (! guards (cons g acc) es))]
                            [#:else
                             (do [awakens <- (! second es)]
                                 [tl <- (! <<v cdr 'o cdr es '$)]
                               (! naps acc (cons (list hd awakens) g) tl))]))])]))])
    (! guards '())))

(define-thunk (! fudge-nap asleep awake)
  (let ([get-minute (thunk (λ (x) (! <<v fourth 'o second x '$)))])
    (! map get-minute (list asleep awake))))

;; get shit into the right format
(define-thunk (! fudge e)
  (do [id <- (! first e)]
      [fudged <- (! <<v map (thunk (! apply fudge-nap)) 'o cdr e '$)]
    (ret (list id fudged))))

; U (CoList '(,Num ((,Num ,Num) ...))) -> Hash Num `((,Num ,Num) ...)
(define-thunk (! mk-entry-tbl)
  (copat
   [(es)
    (! cl-foldl
       es
       (thunk (λ (id->naps e)
                (do [key <- (! car e)]
                    [val <- (! second e)]
                  (! update id->naps key val (thunk (! append val))))))
       empty-table)]))

(define-thunk (! sleepier e1 e2)
  (do [t1 <- (! second e1)] [t2 <- (! second e2)]
    (cond [(! <= t1 t2) (ret e2)] [#:else (ret e1)])))

(define greatest-sleep (list -1 -inf.0))

(define-thunk (! total-sleep e)
  (do [hd <- (! first e)]
      [sum <- (! <<v apply + 'o map (thunk (! apply (thunk (! swap -)))) 'o rest e '$)]
    (ret (list hd sum))))

(define-thunk (! inc-times)
  (copat
   [(time-array interval)
    (do [lo <- (! first interval)]
        [hi <- (! second interval)]
      (! <<n
         cl-foreach (thunk (λ (ix)
                             (do [x <- (! vector-ref time-array ix)]
                                 (! <<v vector-set! time-array ix 'o + 1 x '$)))) 'o
         range lo hi '$))]))

(define-thunk (! debug-id x)
  (do [_ <- (! displayln x)]
      (ret x)))

(define-thunk (! best-minute naps)
  (do [times <- (! make-vector 60)]
      [_ <-
         (! <<n cl-foreach (thunk (! inc-times times)) 'o colist<-list naps '$)]
    [bst*time <- (! <<n
                    minimum-by (thunk (λ (x) (! <<v - 'o second x '$))) greatest-sleep 'o
                    cl-map (thunk (λ (i) (! <<v List i 'o vector-ref times i '$)))'o
                    range 0 60 '$)]
    (ret bst*time)))

(define-thunk (! main-a)
  (do [l <- (! slurp-lines)]
      [l <- (! <<v map (thunk (! apply parse-entry)) 'o map string->list l '$)]
    [es <- (! <<v group-entries 'o sort l rec< '$)]
    [id->naps
     <- (! <<n mk-entry-tbl 'o
           cl-map fudge 'o colist<-list es '$)]
    ;; (ret id->naps)
    [id*naps <- (ret (thunk (! <<v colist<-list 'o id->naps 'to-list '$)))]
    [q <- (! <<n list<-colist 'o cl-map total-sleep id*naps '$)]
    [big-sleeper*sleep <- (! <<n
                             cl-foldl^ sleepier greatest-sleep 'o
                             cl-map total-sleep id*naps '$
                             )]
    [big-sleeper <- (! first big-sleeper*sleep)]
    [naps  <- (! id->naps 'get big-sleeper #f)]
    [best <- (! <<v first 'o best-minute naps '$)]
    [chksum <- (! * best big-sleeper)]
    (ret (list 'part 'a ': 'id big-sleeper '* 'minute best '= chksum))))

(define-thunk (! main-b)
  (do [l <- (! slurp-lines)]
      [l <- (! <<v map (thunk (! apply parse-entry)) 'o map string->list l '$)]
    [es <- (! <<v group-entries 'o sort l rec< '$)]
    [id->naps
     <- (! <<n mk-entry-tbl 'o
           cl-map fudge 'o colist<-list es '$)]
    [id*naps <- (! id->naps 'to-list)]
    [id*best <- (! <<n
                   minimum-by (thunk (λ (x) (! <<v - 'o second 'o second x '$))) (list -1 greatest-sleep) 'o 
         cl-map (thunk (λ (x) (do [id <- (! first x)] [naps <- (! rest x)]
                                [best <- (! best-minute naps)]
                                (ret (list id best))))) 'o
         colist<-list id*naps '$)]
    [id <- (! first id*best)] [best <- (! second id*best)]
    [chksum <- (! <<v * id 'o first best '$)]
    (ret (list 'part 'b ': 'id id '* 'minute best '= chksum))))


