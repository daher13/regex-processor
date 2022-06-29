#lang racket

(struct char
  (ch)
  #:transparent)

(struct state
  (sp input)
  #:transparent)

(struct split
  (x y)
  #:transparent)

(define (fetch-char s)
  (match s
    [(state sp input) (list-ref (state-input s) (state-sp s))]
    ))

(define (split-function s)
  (match s
    [(state sp input) (split (sub1 (state-sp s)) (add1 (state-sp s)))]
    ))

(define (compile s)
  (match (fetch-char (state-input s))
    [(char #\a) (#t)]
    ))

(fetch-char (state 0 (list (char #\a) (char #\+))))

;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))
