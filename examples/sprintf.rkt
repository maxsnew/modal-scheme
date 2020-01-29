#lang sbpv

(require sbpv/prelude)

;; llsprintf
;; llsprintf (String|'%d|'%i)* $ arg* -> String

(define-rec-thunk (! rev-apply rev-args f)
  (ifc (! empty? rev-args)
       (! f)
       (do [hd <- (! first rev-args)]
           [tl <- (! rest rev-args)]
         (! rev-apply tl f hd))))

(define-rec-thunk (! cupto-loop prompt acc return)
  (copat
   [((= prompt)) (! return acc)]
   [(x) (! cupto-loop prompt (cons x acc) return)]
   [(#:bind) (! error "I was supposed to find a prompt here!")]))

(define-rec-thunk (! cupto prompt return)
  (! cupto-loop prompt '() return))

;;
(define-rec-thunk (! llsprintf-loop acc)
  (copat
   [((= '$))  (ret acc)]
   [((= '%s))
    (do^ [fmtstr <- (! cupto '$)]
      (copat
       [(x)
        (do [acc^ <- (! string-append acc x)]
            (! rev-apply fmtstr (thunk (! llsprintf-loop acc^)) '$))]))]
   [(literal)
    (do [acc^ <- (! string-append acc literal)]
        (! llsprintf-loop acc^))]))

(define-thunk (! ll-sprintf) (! llsprintf-loop ""))

(! ll-sprintf "user " '%s " reported error " '%s " at 12pm." '$ "Max New" "too hot")
