#lang racket

(struct state
  (sp input)
  #:transparent)

(define (split-function s)
  (match s
    []
)
)

;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))
 