#lang sbpv

(require "../../stdlib.rkt")
(require "../IO.rkt")
(require "../CoList.rkt")
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

(define-thunk (! main-a)
  (ret 'not-done-yet))

(define-thunk (! main-b)
  (ret 'not-done-yet))
