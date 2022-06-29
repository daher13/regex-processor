#lang racket

(require "re.rkt")
(require "vm-struct.rkt")

(define (compile re c)
  (match re
    [(lam) (list (mtch))]
    [(char ch) ((list (char ch)))]
    [(choice e1 e2) (append (split ) (compile e1 (add1 c)) (compile e2 (+ c 2)))]
    ))

; let para calcular os tamanhos e montar

(compile (choice (char "a") (char "b") ))

; a | b
; (choice (char a) (char b) )
;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))

; (a|b)*