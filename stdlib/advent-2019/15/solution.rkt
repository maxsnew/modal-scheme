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

(def-thunk (! trace-inputs pos came_from dirs)
  ;; (! <<v displayall 'trace-inputs pos)
  ;; (! <<v displayall 'o came_from 'to-list)
  [next-dir <- (! came_from 'get pos #f)]
  (cond [(ret next-dir)
         [pos <- (! <<v mv-pos pos 'o reverse-dir next-dir)]
         (! trace-inputs pos came_from (cons next-dir dirs))]
        [else (ret dirs)]))

;; c : canvas
;; ptr: Posn
;; came_from : Table Posn Posn
;; frontier : Queue Posn
;; Returns a Listof Dir
(def-thunk (! bfs-loop c came_from frontier)
  (cond [(! empty? frontier)
         (! error 'no-frontier)]
        [else
         [rev_front <- (! reverse frontier)]
         [next <- (! first rev_front)] [frontier <- (! <<v reverse 'o rest rev_front '$)]
         [adjs = '(1 2 3 4)]
         [unknowns <- (! filter (~ (! <<v equal? #\space 'o c 'read 'o mv-pos next)) adjs)]
         [knowns <- (! <<v
                       filter (~ (! <<v not 'o came_from 'has-key? 'o mv-pos next)) 'o
                       filter (~ (! <<v equal? #\. 'o c 'read 'o mv-pos next)) adjs)]
         (cond [(! <<v not 'o empty? unknowns)
                [final-dir <- (! <<v List 'o first unknowns)]
                (! trace-inputs next came_from final-dir)]
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
                (! bfs-loop c came_from frontier)])]))

(def-thunk (! bfs c ptr)
  [came_from <- (! empty-table 'set ptr #f)]
  [frontier <- (! List ptr)]
  (! bfs-loop c came_from frontier))

(def-thunk (! next-move c ptr)
  [unknown-dir?
   = (~ (λ (dir) (! <<v equal? #\space 'o c 'read 'o coord-add ptr 'o coord<-arrow dir '$)))]
  [unexplored <- (ret '())
              ;(! filter unknown-dir? '(1 2 3 4))
              ]
  (! <<v displayall 'o bfs c ptr))

(def-thunk (! repair-driver c ptr inputs count iK)
  [count <- (cond [(! zero? count) (! display-canvas c) (ret 20)]
                  [else (! - count 1)])]
  [inputs <- (cond [(! empty? inputs) (! bfs c ptr)]
                   [else (ret inputs)])]
  [inp <- (! first inputs)] [inputs <- (! rest inputs)]
  (! iK inp (~ (copat
   [(outp oK)
    [next-pos <- (! <<v coord-add ptr 'o coord<-arrow inp '$)]
    ((copat
      [((= 0))
       (! c 'write next-pos #\#)
       (! oK (~ (! repair-driver c ptr inputs count)))]
      [((= 1))
       (! c 'write ptr #\.)
       (! c 'write next-pos #\D)
       (! oK (~ (! repair-driver c next-pos inputs count)))]
      [((= 2))
       (! c 'write ptr #\.)
       (! c 'write next-pos #\O)
       (! display-canvas c)
       (ret next-pos)])
     outp)]))))

(define sz/2 25)

(def-thunk (! initialize-driver-a)
  [sz <- (! * 2 sz/2)]
  [c <- (! mk-canvas sz sz #\space)]
  [ptr <- (! mk-coord sz/2 sz/2)]
  (! c 'write ptr #\D)
  (ret (~ (! msg-parser (~ (! repair-driver c ptr '() 100))))))

(def-thunk (! main-a)
  [syn <- (! parse-intcode-program "input")]
  (! displayall 'parsed)
  [driver <- (! initialize-driver-a )]
  [oxy-posn <- (! interp-intcode-program syn driver)]
  (! List 'start-posn sz/2 sz/2 'oxy-posn oxy-posn))

(def-thunk (! main-b)
  (ret 'not-done-yet))
