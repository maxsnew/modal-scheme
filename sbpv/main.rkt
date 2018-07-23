#lang racket/base

(require "sbpv.rkt" syntax/parse
         (for-syntax racket/base syntax/parse)
         (only-in turnstile define-primop))
(provide (all-from-out "sbpv.rkt")
         define-primop
         (rename-out [module-begin #%module-begin]
                     [typed-define define]))

(begin-for-syntax
  (define-syntax-class def
    (pattern ((~literal typed-define) . _))
    (pattern ((~literal define-primop) . _))))
(define-syntax (module-begin syn)
  (syntax-parse syn
    [(_ (~or d:def e) ...)
     #;#;#:do
     [(displayln (syntax->datum #`(#%module-begin
                                   (begin
                                     d ...
                                     (main e) ...))))]
     #`(#%module-begin
        (begin
          d ...
          (main e) ...))]))

#;
(define-syntax (top-interaction syn)
  (syntax-case syn
      [(_ . e)
       #`(#%top-interaction (bind (x e) (ret x)))]))
;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(module+ test
  ;; Tests to be run with raco test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
