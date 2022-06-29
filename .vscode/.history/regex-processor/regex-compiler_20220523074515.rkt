#lang racket

(struct state
  (sp input)
  #:transparent)

(struct split
(x y)
#:transparent)

(define (split-function s)
  (match s
    [(state sp input) (split (sub1 (state-sp s)) (add1 (state-sp s)))]
))

(split-function (state 1 "ab"))

;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))
 