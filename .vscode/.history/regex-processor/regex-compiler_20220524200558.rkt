#lang racket

(require "re.rkt")
(require "vm-struct.rkt")

(define (compile1 re c)
  (match re
    [(lam) (list (mtch))]
    [(char ch) (list (char ch))]
    [(choice e1 e2) (let* ([c1 (compile1 e1 (add1 c))]
                           [c2 (compile1 e2 (+ c 2))]
                           [l1 (add1 c)]
                           [l2 (+ (length c1) 2)]
                           [l3 (+ (length c1) (length c2) 2)]
                           )
                      (append (list (split l1 l2)) c1 (list (jmp l3)) c2)
                      )]
    ))

; let para calcular os tamanhos e montar

(compile1 (choice (char "a") (char "b")) 0)

; a | b
; (choice (char a) (char b) )
;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))

; (a|b)*