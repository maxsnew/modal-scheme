#lang sbpv

(require "stdlib.rkt")
;; This looks like the definition of a stream, but because it's CBName
;; they are actually "batch"
;; A batch-stream (BS) is a computation with two methods:
;;   'hd |- F ?v
;;   'tl |- BS

;; BS[X] = /*\ ('hd -> F X) ('tl -> BS[X])
; const-bs : /\ X (X -> BS[X])
(define-rec-thunk (! const-bs x)
  (copat
   [('hd #:bind) (ret x)]
   [('tl) (! const-bs x)]))

;; A resumable batch-stream RBS has a third method:
;;   #:bind |- F (U (RBS))
;; that can be used to save things in progress
;; this gives a subtyping relationship:  BS <: RBS
;; These things are proper streams!

;; RBS[X] = (/*\ (F (U RBS[X])) ('hd -> F X) ('tl -> RBS[X])
;; const-rbs : /\X (X -> RBS[X])
(define-rec-thunk (! const-rbs x)
  (copat
   [(#:bind) (ret (thunk (! const-rbs x)))]
   [('hd #:bind) (ret x)]
   [('tl) (! const-rbs x)]))

;; TODO: n-ary map
;; (X -> F Y) 
(define-rec-thunk (! map f str)
  (copat
   [(#:bind)
    (do [str^ <- (! str)]
        (ret (thunk (! map f str^))))]
   [('hd #:bind) (! .v f str 'hd)]
   [('tl) (! map f (thunk (! str 'tl)))]))

(do [fives <- (! const-rbs 5)]
    [tens  <- (! map (thunk (λ (x) (! + x x))) fives)]
  [saved <- (! tens 'tl 'tl 'tl 'tl 'tl 'tl 'tl 'tl 'tl 'tl 'tl)]
  (! saved 'hd)
  )
