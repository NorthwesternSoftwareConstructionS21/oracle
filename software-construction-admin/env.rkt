#lang at-exp racket

(provide write-env!)

(require "util.rkt"
         "assignments.rkt")

(define/contract (write-env! dir env-file team assign-number)
  (path-to-existant-directory? string? string? assign-number? . -> . any)

  (display-to-file @~a{
                       export ASSIGN_MAJOR=@(car assign-number)
                       export ASSIGN_MINOR=@(cdr assign-number)
                       export TEAM_NAME=@team

                       }
                   (build-path dir env-file)
                   #:exists 'truncate))