#lang fiddle

(require fiddle/prelude)
(require fiddle/stdlib/IO)
(require fiddle/stdlib/CoList)
(require "../../Parse.rkt")

(provide main-a main-b)

(def-thunk (! mass<-string s)
  [l <- (! string->list s)]
  (! parse-num l))

(def-thunk (! fuel<-mass-a x)
  [x/3 <- (! quotient x 3)]
  (! - x/3 2))

(def-thunk (! iter x)
  (ifc (! <= x 0)
       (ret clv-nil)
       (do [fuel <- (! fuel<-mass-a x)]
           (! Cons x fuel))))

(def-thunk (! fuel<-mass-iterate x)
  [y <- (! fuel<-mass-a x)]
  [masses = (~ (! cl-unfold iter y))]
  (! cl-foldl masses + 0))

(def-thunk (! p-main fuel<-mass)
  [lines = (~ (! slurp-lines~))]
  [masses = (~ (! cl-map (~ (! .v fuel<-mass mass<-string)) lines))]
  (! cl-foldl masses + 0))

(def-thunk (! main-a) (! p-main fuel<-mass-a))

(def-thunk (! main-b) (! p-main fuel<-mass-iterate))
