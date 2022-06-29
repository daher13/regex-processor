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

(define (next-char s)
  (match s
    [(state sp input codes) (state (add1 sp) input codes )]))

(define (append-code s c)
  (match s
    [(state sp input codes) (state sp input (append codes (list c)))]
    ))

(define (split-function s)
  (match s
    [(state sp input codes) (split (sub1 (state-sp s)) (add1 (state-sp s)))]
    ))

(define (compile s)
  (match (fetch-char s)
    [#\a (compile (next-char (append-code s (char #\a))))]
    [#\b (compile (next-char (append-code s (char #\b))))]
    [#\+ (compile (next-char (append-code s (split-function s))))]
    ['() (append-code s "match!")]
    ))

(define test (state 0 "a+b+" '()))

(compile test)

;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))
