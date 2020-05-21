#lang fiddle

(require fiddle/prelude
         fiddle/stdlib/CoList)
(provide mk-coord x-coord y-coord scale coord-add
         pt<-ix ix<-pt

         canvas<-vec
         mk-square-canvas
         mk-canvas)

(def/copat (! mk-coord) [(x y #:bind) (! List x y)])
(def/copat (! x-coord) [(c #:bind) (! first c)])
(def/copat (! y-coord) [(c #:bind) (! second c)])
(def/copat (! scale pt n)
  [(#:bind)
   (! idiom (~ (ret mk-coord)) (~ (! <<v * n 'o x-coord pt '$))
                               (~ (! <<v * n 'o y-coord pt '$)))])
(def-thunk (! coord-add c1 c2)
  (! idiom^ mk-coord (~ (! idiom^ + (~ (! x-coord c1)) (~ (! x-coord c2))))
                     (~ (! idiom^ + (~ (! y-coord c1)) (~ (! y-coord c2))))))

(def-thunk (! pt<-ix w h ix)
  (! idiom^ mk-coord (~ (! modulo ix w)) (~ (! quotient ix w))))

;; x y |-> y * width + x
(def-thunk (! ix<-pt w h pt)
  [x <- (! x-coord pt)] [y <- (! y-coord pt)]
  (! <<v + x 'o * y w '$))

(def/copat (! canvas<-vec w h v)
  [((= 'width)) (ret w)] [((= 'height)) (ret h)]
  [((= 'read) pt)
   (! <<v vector-ref v 'o ix<-pt w h pt '$)]
  [((= 'write) pt x)
   [ix <- (! ix<-pt w h pt)]
   (! vector-set! v ix x)]
  [((= 'paint) char<-elt) ;; CoList String
   [len <- (! * w h)]
   ;; (! displayall 'time-to-paint)
   (! <<n
      cl-map list->string 'o
      chunks w 'o
      cl-map (~ (! <<v char<-elt 'o vector-ref v)) 'o
      range 0 len '$)])

(def/copat (! mk-square-canvas side)
  [(#:bind) (! mk-square-canvas side 0)]
  [(val)
   [len <- (! * side side)]
   [vec <- (! make-vector len val)]
   (ret (~ (! canvas<-vec side side vec)))])

(def-thunk (! mk-canvas w h val)
  [len <- (! * w h)]
  [vec <- (! make-vector len val)]
  (ret (~ (! canvas<-vec w h vec))))
