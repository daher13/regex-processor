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
  (cond
    [(>= (state-sp s) (string-length (state-input s))) '()]
    [else (string-ref (state-input s) (state-sp s))]))

(define (split-function s)
  (match s
    [(state sp input) (split (sub1 (state-sp s)) (add1 (state-sp s)))]
    ))

(define (next-char s)
  (match s
    [(state sp input) (state (add1 sp) input)]
    ))

(define (compile s)
  (match (fetch-char s)
    [#\a (cons (char #\a) (compile (next-char s)))]
    [#\b (cons (char #\b) (compile (next-char s)))]
    [#\+ (cons (split-function s) (compile (next-char s)))]

    ['() "match!"]
    )
  )

(compile (state 0 "a+b+"))

;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))
