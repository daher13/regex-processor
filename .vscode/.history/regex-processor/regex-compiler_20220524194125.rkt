#lang racket

(require "re.rkt")
(require "vm-struct.rkt")

(define (compile re)
  (match re
    [(lam) (list (mtch))]
    [(char ch) ((list (char ch)))]
    [(choice e1 e2) (list (compile e1) (list (compile e2)))]
    [(choice e1 e2) (choice-simply (derivate e1 a) (derivate e2 a)) ]
    [(star e) (cat-simply (derivate e a) (star-simply e))]
    ))

(compile (choice (char a) (char b) ))

; a | b
; (choice (char a) (char b) )
;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))

; (a|b)*