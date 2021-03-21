#lang at-exp racket

(provide assign-number->oracle-path
         assign-number->validated-tests-path
         assign-number->submitted-tests-path
         assign-number->deliverable-exe-path)

(require racket/runtime-path
         "assignments.rkt"
         "util.rkt")

(define-runtime-path validated-tests-dir "../backgammon-oracle/validated-tests")
(define-runtime-path submitted-tests-dir "../backgammon-oracle/submitted-tests")
(define-runtime-path oracle-binary-dir "../backgammon-oracle/oracle")

(define/contract (assign-number->validated-tests-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path validated-tests-dir
              (assign-number->dir-path-part assign-number)))

(define/contract (assign-number->submitted-tests-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path submitted-tests-dir
              (assign-number->dir-path-part assign-number)))

(define/contract (assign-number->deliverable-exe-path assign-number)
  (assign-number? . -> . path-string?)

  (build-path "Deliverables"
              (assign-number->dir-path-part assign-number)
              "run"))


(define/contract (assign-number->oracle-path assign-number #:racket-based-oracle? racket-based-oracle?)
  (assign-number? #:racket-based-oracle? boolean? . -> . (or/c path-to-existant-file? #f))

  (define candidate
    (build-path oracle-binary-dir (format "oracle~a.~a~a" (car assign-number) (cdr assign-number)
                                          (if racket-based-oracle? ".rkt" ""))))
  (and (file-exists? candidate)
       candidate))
