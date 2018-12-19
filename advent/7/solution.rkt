#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Set.rkt")
(require "../table.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

;; (define NUM-THREADS 2) ;; small 
;; (define JOB-DELAY 0)   ;; small
(define NUM-THREADS 5) ;; large
(define JOB-DELAY 60)  ;; large

;; Steps: parse the input into a graph
;; Do Kahn's algorithm, breaking ties by alphabetical order

;;   State:
;;
;;   1. an ordered list Cur-Round of next nodes to visit, initially those with
;;   no predecessors sorted alphabetically.
;;   2. an ordered list Next-Round of those to be visited in the *next* round, initially empty
;;   2. the graph, initially the original graph
;;   3. the output list, initially empty
;; 
;;   If Cur-Round is empty, make Next-Round the Cur-Round and Next-Round empty

;;   Otherwise, pop off the first node p, visit its successors s and
;;   remove the edge p -> s. If s then has no predecessors, add it to
;;   next-round
;;
;; The graph will be a Table Vertex (List Predecessors Successors)
;;   where Predecessors and Successors are (Setof Vertex)
;; We assume the input is a DAG

;; Vertex is an opaque type supporting equal?
;; we implement it as strings for easy printing

;; PARSING
;; Char -> ... -> F (List Vertex '-> Vertex)
(def-thunk (! edge<-line)
  (copat [(s t e p _ Pred _ m u s t _ b e _ f i n i s h e d _ b e f o r e _ s t e p _ Succ)
          [pred^ <- (! list->string (list Pred))]
          [succ^ <- (! list->string (list Succ))]
          (! abort (list pred^ '-> succ^))]))

;; A Graph V is a U(Table V (List (Pred V) (Succ V)))
;; that is coherent...
(def-thunk (! add-edge edge graph)
  [pred <- (! first edge)] [succ <- (! third edge)]
  [pset <- (! set-single pred)] [sset <- (! set-single succ)]
  [ins-p = (~ (copat [(ps*ss) [ps <- (! first ps*ss)] [ss <- (! second ps*ss)]
                              [ps <- (! ps 'add pred)]
                              (! List ps ss)]))]
  [ins-s = (~ (copat [(ps*ss) [ps <- (! first ps*ss)] [ss <- (! second ps*ss)]
                              [ss <- (! ss 'add succ)]
                              (! List ps ss)]))]
  (! <<v
     update^ succ (list pset empty-set) ins-p 'o 
     update^ pred (list empty-set sset) ins-s graph))

;; F (Graph Vertex)
(def-thunk (! read-graph)
  (! <<n cl-foldl^ (~ (! swap add-edge)) empty-table 'o
     cl-map (~ (! <<v apply edge<-line 'o string->list)) 'o
     slurp-lines~))

;; using string<=?
;; String -> U CoList String -> CoList String
;; and
;; (A -> A -> F Bool) -> String -> U CoList String -> CoList String
(def-thunk (! insert~ <=? x c #:bind)
  [v <- (! c)]
  (cond [(! clv-nil? v) (! cl-single x)]
        [else [hd <- (! clv-hd v)] [tl <- (! clv-tl v)]
              (cond [(! <=? x hd) (! cl-cons x c)]
                    [else (! cl-cons hd (~ (! insert~ <=? x tl)))])]))

(def-thunk (! successors gr v)
  (! <<v second 'o gr 'get v #f))

;; Removes the out-edges of a vertex from a graph, putting any that
;; have no other predecessors in the provided sorted colist output
;; Vertex -> Graph -> USortedCoList Vertex -> F (List Graph (USorted CoList Vertex)
(def-thunk (! remove-outedges cur gr sink <=?)
  [cur-succs <- (! <<v @> 'to-list 'o successors gr cur)]
  ;; (List Graph (SortedList V)) -> Vertex -> F (List Graph (SortedList V))
  [remove-backedge
   = (~ (copat
         [(gr*sink succ)
          [gr <- (! first gr*sink)] [sink <- (! second gr*sink)]
          [ps*ss <- (! gr 'get succ #f)]
          [succ-ps <- (! first ps*ss)] [succ-ss <- (! second ps*ss)]
          [succ-ps <- (! succ-ps 'remove cur)]
          [gr <- (! gr 'set succ (list succ-ps succ-ss))]
          [sink <- (cond [(! succ-ps 'empty?) (ret (~ (! insert~ <=? succ sink)))]
                         [else (ret sink)])]
          (! List gr sink)]))]
  (! foldl cur-succs remove-backedge (list gr sink)))

;; Graph Vertex ->
;; Sorted-CoList Vertex ->
;; CoList Vertex
(def-thunk (! topo-sort-algo nexts gr)
  [v-nexts <- (! nexts)]
  (cond
    [(! clv-nil? v-nexts) (! cl-nil)]
    [else
     [cur <- (! clv-hd v-nexts)] [nexts <- (! clv-tl v-nexts)]
     [gr*nexts <- (! remove-outedges cur gr nexts string<=?)]
     [gr <- (! first gr*nexts)] [nexts <- (! second gr*nexts)]
     (! cl-cons cur (~ (! topo-sort-algo nexts gr)))]))

(def-thunk (! insertion-sort <=?)
  (! cl-foldr^ (~ (! insert~ <=?)) cl-nil))

;; Graph Vertex -> CoList Vertex
(def-thunk (! topo-sort gr)
  [adjs <- (! gr 'to-list)]
  [no-preds = (~ (! <<n
                    (~ (! insertion-sort string<=?)) 'o
                    cl-map first 'o
                    cl-filter (~ (! <<v @> 'empty? 'o second)) 'o
                    colist<-list adjs))]
  (! topo-sort-algo no-preds gr))

(def-thunk (! main-a)
  [gr <- (! read-graph)]
  [chars <- (! <<n list<-colist 'o cl-bind^ colist<-list 'o cl-map string->list 'o topo-sort gr)]
  (! list->string chars))

;; For part 2 we start out the same: build the graph and find
;; initialize with the sorted list of vertices with no predecessors
;;
;; Now our state is 4 things: the graph g, the vertices that are ready
;; next, the state of our threads st-threads, and the elapsed time
;;
;; We are producing a CoList of Events, i.e., a trace of execution
;; An Event is one of
;;   START-JOB ,Vertex ,Timestamp
;;   FINISH-JOB ,Vertex ,Timestamp
;;
;; The state transitions as follows:
;;   - The trace ends if the threads are inactive and next is empty

;;   - We START-JOB v t with the current time t if there is thread
;;     capacity and the next list is (v :: next). We also assign the
;;     job to a thread with finish time = current-time + job-length v
;;
;;   - WE FINISH-JOB v t if threads are at full capacity: we find the
;;     job that finishes soonest, and advance time to then and remove
;;     it from the active jobs

;; data Job = List Vertex Timestamp
;; codata ThreadPool where
;;   'empty?        |- F Bool
;;   'full?         |- F Bool
;;   'add-job       |- Job -> FU ThreadPool -- precondition 'full? is #f, and afterwards empty? is #f
;;   'wait          |- F (List Job (U ThreadPool)) -- precondition 'empty? is #f and postcondition full? is #f

(def-thunk (! finishes-earlier? j1 j2)
  [t1 <- (! second j1)] [t2 <- (! second j2)]
  (! <= t1 t2))

;; Nat+ -> ThreadPool
(def-thunk (! mk-pool limit)
  (letrec
      ([empty-pool
        (~ (copat
            [((= 'empty?)) (ret #t)]
            [((= 'full?)) (ret #f)]
            [((= 'add-job) job) (ret (~ (! ne-pool 1 job cl-nil)))]))]
       [ne-pool
        (~ (λ (cur-running cur-job rest)
             (copat
              [((= 'empty?) #:bind) (ret #f)]
              [((= 'full?) #:bind) (! equal? limit cur-running)]
              [((= 'wait) #:bind)
               [pool--
                <- (cond
                     [(! equal? 1 cur-running) (ret (~ (! empty-pool)))]
                     [else
                      [rest-v <- (! rest)]
                      [next-job <- (! clv-hd rest-v)] [rest <- (! clv-tl rest-v)]
                      [cur-running <- (! - cur-running 1)]
                      (ret (~ (! ne-pool cur-running next-job rest)))])]
               (! List cur-job pool--)]
              [((= 'add-job) j)
               [jobs-v <- (! insert~ finishes-earlier? j (~ (! cl-cons cur-job rest)))]
               [cur-job <- (! clv-hd jobs-v)] [rest <- (! clv-tl jobs-v)]
               [cur-running <- (! + cur-running 1)]
               (ret (~ (! ne-pool cur-running cur-job rest)))])))])
    (! empty-pool)))

;; TODO: calculate this
(def-thunk (! job-length)
  (! <<v + JOB-DELAY 'o swap - 64 'o char->integer 'o first 'o string->list))

;; Time -> Graph -> U(CoList V) -> U(ThreadPool) -> CoList Event
(def-thunk (! operate time gr nexts threads)
  [nexts-v <- (! nexts)]
  (cond
    ;; End the trace with a TIMESTAMP event if the threads are
    ;; inactive and there's no jobs left
    [(! and (~ (! clv-nil? nexts-v)) (~ (! threads 'empty?)))
     (! cl-cons (list 'THE-END: time) cl-nil)]
    ;; We wait for the next job to finish if the threadpool is full
    ;; or there are no jobs left
    [(! or (~ (! clv-nil? nexts-v)) (~ (! threads 'full?)))
     [job*threads <- (! threads 'wait)]
     [job <- (! first job*threads)] [threads <- (! second job*threads)]
     [name <- (! first job)] [time <- (! second job)]
     [gr*nexts <- (! remove-outedges name gr (~ (ret nexts-v)) string<=?)]
     [gr <- (! first gr*nexts)] [nexts <- (! second gr*nexts)]
     (! cl-cons
        (list 'FINISHED-JOB name time)
        (~ (! operate time gr nexts threads)))]
    ;; Otherwise we add another job to the pool
    [else
     [job-name <- (! clv-hd nexts-v)] [nexts <- (! clv-tl nexts-v)]
     [start-time = time] [finish-time <- (! <<v + start-time 'o job-length job-name)]
     [threads <- (! threads 'add-job (list job-name finish-time))]
     (! cl-cons
        (list 'STARTING-JOB job-name start-time 'ETC: finish-time)
        (~ (! operate time gr nexts threads)))]))

(def-thunk (! main-b)
  [gr <- (! read-graph)]
  [adjs <- (! gr 'to-list)]
  [no-preds = (~ (! <<n
                    (~ (! insertion-sort string<=?)) 'o
                    cl-map first 'o
                    cl-filter (~ (! <<v @> 'empty? 'o second)) 'o
                    colist<-list adjs))]
  (! <<n
     cl-foreach displayln 'o
     operate 0 gr no-preds (~ (! mk-pool NUM-THREADS))))
