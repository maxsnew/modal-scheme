#lang sbpv

(require sbpv/prelude)
(require sbpv/stdlib/IO)
(require sbpv/stdlib/CoList)
(require "../../Parse.rkt")

(require "../Coordinates.rkt")
(provide main-a main-b)

;; types of squares
;; .    empty space
;; @    player
;; #    wall
;; a    key
;; A    door

(def-thunk (! unseen-key? held-keys char)
  (! and (~ (! lower-case? char))
         (~ (! <<n (~ (! <<v not 'o any?)) 'o cl-map (~ (! equal? char)) 'o colist<-list held-keys))))

(def-thunk (! downcase c)
  (cond [(! lower-case? c) (ret c)]
        [(! upper-case? c) (! <<v integer->char 'o + 32 'o char->integer c)]))

(def-thunk (! empty-space? held-keys char)
  (! or (~ (! equal? #\. char)) (~ (! equal? #\@ char))
     (~ (! and (~ (! letter? char))
           (~ (! <<v swap member? (~ (! colist<-list held-keys)) 'o downcase char))))))

;; 
(def-thunk (! find-next-key c have-keys distances found-keys frontier)
  (cond [(! empty? frontier) (ret found-keys)]
        [else
         [rev_front <- (! reverse frontier)]
         [next <- (! first rev_front)] [frontier <- (! <<v reverse 'o rest rev_front '$)]
         [cross <- (! idiom^ List (~ (! mk-coord 0 1)) (~ (! mk-coord 0 -1)) (~ (! mk-coord 1 0)) (~ (! mk-coord -1 0)))]
         [adjs <- (! map (~ (! coord-add next)) cross)]

         [new-keys <- (! filter (~ (! <<v unseen-key? have-keys 'o map (~ (! c 'read)) adjs)) )]
         [new-spaces <- (! filter (~ (! <<v empty-space? have-keys 'o map (~ (! c 'read)) adjs)))]
         ;; add them to the distances table with dist of next +1
         ;; add the new keys to the found-keys set
         ;; add the new spaces to the frontier
         ;; loop
         (ret 3)
         ;; [goals <- (! filter (~ (! <<v equal? goal 'o mv-pos next)) adjs)]
         ;; [unknowns <- (! filter (~ (! <<v equal? #\space 'o c 'read 'o mv-pos next)) adjs)]
         ;; [knowns <- (! <<v
         ;;               filter (~ (! <<v not 'o came_from 'has-key? 'o mv-pos next)) 'o
         ;;               filter (~ (! <<v known-empty-space? 'o c 'read 'o mv-pos next)) adjs)]
         ;; (cond [(! <<v not 'o empty? goals)
         ;;        [goal-dir <- (! first goals)]
         ;;        (! trace-inputs 'goal next came_from (cons goal-dir '()))]
         ;;       [(! <<v not 'o empty? unknowns)
         ;;        [final-dir <- (! <<v List 'o first unknowns)]
         ;;        (! trace-inputs 'explore next came_from final-dir)]
         ;;       [else
         ;;        [fron*cf <- 
         ;;                 (! cl-foldl (~ (! colist<-list knowns))
         ;;                    (~ (copat
         ;;                        [(fron*cf dir)
         ;;                         [frontier <- (! car fron*cf)] [came_from <- (! cdr fron*cf)]
         ;;                         [next-pos <- (! mv-pos next dir)]
         ;;                         [frontier <- (! Cons next-pos frontier)]
         ;;                         [came_from <- (! came_from 'set next-pos dir)]
         ;;                         (! Cons frontier came_from)]))
         ;;                    (cons frontier came_from))]
         ;;        [frontier <- (! car fron*cf)] [came_from <- (! cdr fron*cf)]
         ;;        (! bfs-loop c goal came_from frontier)])
         ]
         ))


(def-thunk (! starting-point c w h)
  (! <<n (~ (! <<v clv-hd 'o $)) 'o
     cl-filter (~ (! <<v equal? #\@ 'o c 'read)) 'o
     cl-map (~ (! apply mk-coord)) 'o
     cartesian-product (~ (! range 0 w)) (~ (! range 0 h))))

(def-thunk (! main-a (rest args))
  [lines <- (! <<n list<-colist 'o cl-map string->list 'o apply slurp-lines~ args)]
  [width <- (! <<v length 'o first lines)]
  [height <- (! length lines)]
  [v <- (! <<v list->vector 'o apply append lines)]
  [c = (~ (! canvas<-vec width height v))]
  (! <<n cl-foreach displayall 'o c 'paint Ret)
  [start <- (! starting-point c width height)]
  (ret 'fuckoff)
  )

(def-thunk (! main-b)
  (ret 'not-done-yet))
