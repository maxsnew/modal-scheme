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

;; A Graph V is a Table V (List (Pred V) (Succ V)
;; that is coherent...
(def-thunk (! insert edge graph)
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
  (! <<n cl-foldl^ (~ (! swap insert)) empty-table 'o
     cl-map (~ (! <<v apply edge<-line 'o string->list)) 'o
     slurp-lines~))

;; Graph Vertex -> CoList Vertex
(def-thunk (! topo-sort gr))

(def-thunk (! main-a)
  [g <- (! read-graph)]
  (! g 'to-list))

(def-thunk (! main-b)
  (ret 'not-done-yet))
