#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")

(require "../Coordinates.rkt")
(provide main-a main-b)

#;
(def-thunk (! bfs-loop c goal came_from frontier)
  (cond [(! empty? frontier) (! List 'seen-it-all #f)]
        [else
         [rev_front <- (! reverse frontier)]
         [next <- (! first rev_front)] [frontier <- (! <<v reverse 'o rest rev_front '$)]
         [adjs = '(1 2 3 4)]
         [goals <- (! filter (~ (! <<v equal? goal 'o mv-pos next)) adjs)]
         [unknowns <- (! filter (~ (! <<v equal? #\space 'o c 'read 'o mv-pos next)) adjs)]
         [knowns <- (! <<v
                       filter (~ (! <<v not 'o came_from 'has-key? 'o mv-pos next)) 'o
                       filter (~ (! <<v known-empty-space? 'o c 'read 'o mv-pos next)) adjs)]
         (cond [(! <<v not 'o empty? goals)
                [goal-dir <- (! first goals)]
                (! trace-inputs 'goal next came_from (cons goal-dir '()))]
               [(! <<v not 'o empty? unknowns)
                [final-dir <- (! <<v List 'o first unknowns)]
                (! trace-inputs 'explore next came_from final-dir)]
               [else
                [fron*cf <- 
                         (! cl-foldl (~ (! colist<-list knowns))
                            (~ (copat
                                [(fron*cf dir)
                                 [frontier <- (! car fron*cf)] [came_from <- (! cdr fron*cf)]
                                 [next-pos <- (! mv-pos next dir)]
                                 [frontier <- (! Cons next-pos frontier)]
                                 [came_from <- (! came_from 'set next-pos dir)]
                                 (! Cons frontier came_from)]))
                            (cons frontier came_from))]
                [frontier <- (! car fron*cf)] [came_from <- (! cdr fron*cf)]
                (! bfs-loop c goal came_from frontier)])]))

(def-thunk (! main-a (rest args))
  [lines <- (! <<n list<-colist 'o cl-map string->list 'o apply slurp-lines~ args)]
  [width <- (! <<v length 'o first lines)]
  [height <- (! length lines)]
  [v <- (! <<v list->vector 'o apply append lines)]
  [c = (~ (! canvas<-vec width height v))]
  (! <<n cl-foreach displayall 'o c 'paint Ret)
  (ret #f))

(def-thunk (! main-b)
  (ret 'not-done-yet))
