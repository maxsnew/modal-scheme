#lang sbpv

(require "../../../stdlib.rkt")
(require "../../IO.rkt")
(require "../../CoList.rkt")
(require "../../Parse.rkt")
(require "../../table.rkt")

(require "../Intcode.rkt")
(require "../Coordinates.rkt")

(provide main-a main-b)

(def/copat (! msg-parser driver)
  [((= 'input) iK)
   (! driver (~ (copat
    [(inp driver)
     (! iK inp (~ (copat
     [((= 'output) outp oK)
      (! driver outp (~ (λ (driver)
      (! oK (~ (! msg-parser driver))))))]
     [() (! displayall 'protocol-error:)])))])))]
  [() (! displayall 'protocol-error:)])

;; 
(def-thunk (! arrow-input)
  [c <- (! <<v string->list 'o read-line)]
  (! apply
     (~ (copat
         [((= #\u001B) (= #\[) (= #\A) (rest who-cares)) ;; up
          (ret 1)]
         [((= #\u001B) (= #\[) (= #\B) (rest who-cares)) ;; down
          (ret 2)]
         [((= #\u001B) (= #\[) (= #\D) (rest who-cares));; left
          (ret 3)]
         [((= #\u001B) (= #\[) (= #\C) (rest who-cares)) ;; right
          (ret 4)]
         [((rest invalid-input))
          (! displayall "invalid input")
          (! arrow-input)]))
     c))

(def-thunk (! display-canvas c)
  (! cl-foreach displayall (~ (! c 'paint Ret))))

(def/copat (! coord<-arrow)
  [((= 1)) ;; North
   (! mk-coord 0 -1)]
  [((= 2)) ;; South
   (! mk-coord 0 1)]
  [((= 3));; West
   (! mk-coord -1 0)]
  [((= 4));; East
   (! mk-coord 1 0)])

(def/copat (! reverse-dir)
  [((= 1)) (ret 2)]
  [((= 2)) (ret 1)]
  [((= 3)) (ret 4)]
  [((= 4)) (ret 3)])

(def-thunk (! mv-pos posn dir)
  (! <<v coord-add posn 'o coord<-arrow dir))

(def-thunk (! trace-inputs key pos came_from dirs)
  ;; (! <<v displayall 'trace-inputs pos)
  ;; (! <<v displayall 'o came_from 'to-list)
  [next-dir <- (! came_from 'get pos #f)]
  (cond [(ret next-dir)
         [pos <- (! <<v mv-pos pos 'o reverse-dir next-dir)]
         (! trace-inputs key pos came_from (cons next-dir dirs))]
        [else (! List key dirs)]))

(def/copat (! known-empty-space?)
  [((= #\.)) (ret #t)]
  [((= #\O)) (ret #t)]
  [((= #\D)) (ret #t)]
  [(arg) (ret #f)])

;; c : canvas
;; ptr: Posn
;; came_from : Table Posn Posn
;; frontier : Queue Posn
;; Returns a Listof Dir
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

(def-thunk (! bfs c start goal)
  [came_from <- (! empty-table 'set start #f)]
  [frontier <- (! List start)]
  (! bfs-loop c goal came_from frontier))

(def-thunk (! next-move c ptr) (! <<v second 'o bfs c ptr #f))

(def-thunk (! shortest-path c start goal)
  [outp <- (! bfs c start goal)]
  [key <- (! first outp)]
  ((copat
    [((= 'goal)) (! <<v length 'o second outp)]
    [((= 'explore)) (ret #f)])
   key))

(def-thunk (! repair-driver c ptr start goal inputs count iK)
  [count <- (cond [(! zero? count) (! display-canvas c) (ret 200)]
                  [else (! - count 1)])]
  (cond [(! empty? inputs)
         [found-goal? <- (if goal (! shortest-path c start goal) (ret #f))]
         (cond [(ret found-goal?) (ret found-goal?)]
               [else
                [inputs <- (! next-move c ptr)]
                (! repair-driver c ptr start goal inputs count iK)])]
        [else
         (do
  [inp <- (! first inputs)] [inputs <- (! rest inputs)]
  (! iK inp (~ (copat
   [(outp oK)
    [next-pos <- (! <<v coord-add ptr 'o coord<-arrow inp '$)]
    ((copat
      [((= 0))
       (! c 'write next-pos #\#)
       (! oK (~ (! repair-driver c ptr start goal inputs count)))]
      [((= 1))
       (cond [(! equal? ptr goal) (! c 'write ptr #\O)] [else (! c 'write ptr #\.)])
       (! c 'write next-pos #\D)
       (! oK (~ (! repair-driver c next-pos start goal inputs count)))]
      [((= 2))
       (! c 'write ptr #\.)
       (! c 'write next-pos #\O)
       (! display-canvas c)
       (! oK (~ (! repair-driver c next-pos start next-pos inputs count)))])
     outp)]))))]))

(define sz/2 25)

(def-thunk (! initialize-driver-a)
  [sz <- (! * 2 sz/2)]
  [c <- (! mk-canvas sz sz #\space)]
  [ptr <- (! mk-coord sz/2 sz/2)]
  (! c 'write ptr #\D)
  (ret (~ (! msg-parser (~ (! repair-driver c ptr ptr #f '() 100))))))

(def-thunk (! main-a)
  [syn <- (! parse-intcode-program "input")]
  [driver <- (! initialize-driver-a )]
  (! interp-intcode-program syn driver))

(def-thunk (! maximum x y)
  (cond [(! < x y) (ret y)]
        [else (ret x)]))

;; cur-max: number
;; distances: Table Posn Number
;; frontier: Queue Posn
(def-thunk (! greatest-distance c cur-max distances frontier)
  (cond [(! empty? frontier) (ret cur-max)]
        [else
         [rev_front <- (! reverse frontier)]
         [next <- (! first rev_front)] [frontier <- (! <<v reverse 'o rest rev_front '$)]
         [next-dist+1 <- (! <<v + 1 'o distances 'get next #f)]
         [adjs = '(1 2 3 4)]
         [knowns <- (! <<v
                       filter (~ (! <<v not 'o distances 'has-key? 'o mv-pos next)) 'o
                       filter (~ (! <<v known-empty-space? 'o c 'read 'o mv-pos next)) adjs)]
         [max*fron*dist <- 
                  (! cl-foldl (~ (! colist<-list knowns))
                     (~ (copat
                         [(st dir)
                          [cur-max <- (! first st)] [frontier <- (! second st)] [distances <- (! third st)]
                          [next-pos <- (! mv-pos next dir)]
                          [frontier <- (! Cons next-pos frontier)]
                          [distances <- (! distances 'set next-pos next-dist+1)]
                          [cur-max <- (! maximum cur-max next-dist+1)]
                          (! List cur-max frontier distances)]))
                     (cons cur-max (cons frontier (cons distances '()))))]
         [cur-max <- (! first max*fron*dist)] [frontier <- (! second max*fron*dist)] [distances <- (! third max*fron*dist)]
         (! greatest-distance c cur-max distances frontier)]))

(def-thunk (! oxy-fill-time c oxy-posn)
  [distance-tbl <- (! empty-table 'set oxy-posn 0)]
  [frontier <- (! List oxy-posn)]
  (! greatest-distance c 0 distance-tbl frontier))

;; first, explore until bfs finds no new spots
;; then do a bfs to find the maximal distance from oxy to something else
(def-thunk (! oxy-fill-driver c ptr oxy-posn inputs count iK)
  [count <- (cond [(! zero? count) (! display-canvas c) (ret 300)]
                  [else (! - count 1)])]
  (cond [(! empty? inputs)
         [may-inputs <- (! next-move c ptr)]
         (cond [(ret may-inputs) (! oxy-fill-driver c ptr oxy-posn may-inputs count iK)]
               [else
                (! display-canvas c)
                (! displayall 'fill-time)
                (! oxy-fill-time c oxy-posn)])]
        [else
         (do
  [inp <- (! first inputs)] [inputs <- (! rest inputs)]
  (! iK inp (~ (copat
   [(outp oK)
    [next-pos <- (! <<v coord-add ptr 'o coord<-arrow inp '$)]
    ((copat
      [((= 0))
       (! c 'write next-pos #\#)
       (! oK (~ (! oxy-fill-driver c ptr oxy-posn inputs count)))]
      [((= 1))
       (cond [(! equal? ptr oxy-posn) (! c 'write ptr #\O)] [else (! c 'write ptr #\.)])
       (! c 'write next-pos #\D)
       (! oK (~ (! oxy-fill-driver c next-pos oxy-posn inputs count)))]
      [((= 2))
       (! c 'write ptr #\.)
       (! c 'write next-pos #\O)
       (! display-canvas c)
       (! oK (~ (! oxy-fill-driver c next-pos next-pos inputs count)))])
     outp)]))))]))

(def-thunk (! initialize-driver-b)
  [sz <- (! * 2 sz/2)]
  [c <- (! mk-canvas sz sz #\space)]
  [ptr <- (! mk-coord sz/2 sz/2)]
  (! c 'write ptr #\D)
  (ret (~ (! msg-parser (~ (! oxy-fill-driver c ptr #f '() 100))))))



(def-thunk (! main-b)
  [syn <- (! parse-intcode-program "input")]
  [driver <- (! initialize-driver-b)]
  (! interp-intcode-program syn driver))
