#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
(require "../Set.rkt")
(require "../table.rkt")
(require "../Parse.rkt")

(provide main-a main-b)

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
(def-thunk (! insert~ x c)
  [v <- (! c)]
  (cond [(! clv-nil? v) (! cl-single x)]
        [else [hd <- (! clv-hd v)] [tl <- (! clv-tl v)]
              (cond [(! string<=? x hd) (! cl-cons x c)]
                    [else (! cl-cons hd (~ (! insert~ x tl)))])]))

(def-thunk (! successors gr v)
  (! <<v second 'o gr 'get v #f))


;; Graph Vertex ->
;; Sorted-List Vertex ->
;; Sorted-CoList Vertex ->
;; CoList Vertex
(def-thunk (! topo-sort-algo cur-round next-round gr)
  (cond
    [(! empty? cur-round)
     [cur-round <- (! list<-colist next-round)]
     (cond [(! empty? cur-round) (! cl-nil)]
           [else (! topo-sort-algo cur-round cl-nil gr)])]
    [else
     [cur <- (! first cur-round)] [cur-round <- (! rest cur-round)]
     [cur-succs <- (! <<v @> 'to-list 'o successors gr cur)]
     ;; (List Graph (SortedList V)) -> Vertex -> F (List Graph (SortedList V))
     [remove-backedge
      = (~ (copat
            [(gr*nr succ)
             [gr <- (! first gr*nr)] [nr <- (! second gr*nr)]
             [ps*ss <- (! gr 'get succ #f)]
             [succ-ps <- (! first ps*ss)] [succ-ss <- (! second ps*ss)]
             [succ-ps <- (! succ-ps 'remove cur)]
             [gr <- (! gr 'set succ (list succ-ps succ-ss))]
             [nr <- (cond [(! succ-ps 'empty?) (ret (~ (! insert~ succ nr)))]
                          [else (ret nr)])]
             (! List gr nr)]))]
     [gr*next-round <- (! foldl cur-succs remove-backedge (list gr next-round))]
     [gr <- (! first gr*next-round)]
     [next-round <- (! second gr*next-round)]
     (! cl-cons cur (~ (! topo-sort-algo cur-round next-round gr)))]))

(def-thunk (! insertion-sort)
  (! cl-foldr^ insert~ cl-nil))

;; Graph Vertex -> CoList Vertex
(def-thunk (! topo-sort gr)
  [adjs <- (! gr 'to-list)]
  [no-preds <- (! <<n
                  list<-colist 'o
                  insertion-sort 'o
                  cl-map first 'o
                  cl-filter (~ (! <<v @> 'empty? 'o second)) 'o
                  colist<-list adjs)]
  (! topo-sort-algo no-preds cl-nil gr))

(def-thunk (! main-a)
  [gr <- (! read-graph)]
  [chars <- (! <<n list<-colist 'o cl-bind^ colist<-list 'o cl-map string->list 'o topo-sort gr)]
  (! list->string chars))

(def-thunk (! main-b)
  (ret 'not-done-yet))
