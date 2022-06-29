#lang racket

(require "re.rkt")

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

(define (jmp-function s x)
  (match s
    [(state sp input codes) (jmp (+ x sp))]
    ))

(define (compile s)
  (match s
    [(lam) (list (mtch))]
    [(char ch) ((list (char ch)))]
    [(cat e1 e2) (cond
                   [(is-null e1) (cat-simply (derivate e1 a) e2)]
                   [else (cat-simply (derivate e1 a) e2)])]
    [(choice e1 e2) (choice-simply (derivate e1 a) (derivate e2 a)) ]
    [(star e) (cat-simply (derivate e a) (star-simply e))]
    ))

(define test (state 0 "a+b+" '()))

(compile test)

; a | b
; (choice (char a) (char b) )
;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))

; (a|b)*