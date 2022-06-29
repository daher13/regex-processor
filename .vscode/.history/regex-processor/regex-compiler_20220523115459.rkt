#lang racket

(struct char
  (ch)
  #:transparent)

(struct state
  (sp input codes)
  #:transparent)

(struct split
  (x y)
  #:transparent)

(define (fetch-char s)
  (cond
    [(>= (state-sp s) (string-length (state-input s))) '()]
    [else (string-ref (state-input s) (state-sp s))]))


(define (append-code s c)
  (match s
    [(state sp input codes) (state sp input (append codes c))]
    ))


(define test (state 0 "a+b+" '()))

(append-code test)

test

;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))
