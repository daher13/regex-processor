#lang racket

(struct char
  (ch)
  #:transparent)

(struct _match
  ()
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

(define (compile s)
  (match (fetch-char s)
    [#\a (char "a")]
    [#\b (char "b")]
    [#\+ (split-function s)]
    )
  (compile s)
  )

(compile (state "a+b+"))

;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))