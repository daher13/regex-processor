#lang racket

(require "re.rkt")
(require "vm.rkt")

(define (compile1 re c)
  (match re
    [(re-lam) (list (vm-match))]
    [(re-cat e1 e2) (let* ([c1 (compile1 e1 (add1 c))]
                           [c2 (compile1 e2 (+ c 2))])
                      (append (list c1 c2)))]
    [(re-char ch) (list (vm-char ch))]
    [(re-choice e1 e2) (let* ([c1 (compile1 e1 (add1 c))]
                              [c2 (compile1 e2 (+ c 2))] ; o jump é so uma instrução
                              [l1 (add1 c)]
                              [l2 (+ (length c1) 2)]
                              [l3 (+ c (length c1) (length c2) 2)])
                         (append (list (vm-split l1 l2)) c1 (list (vm-jmp l3)) c2))]
    [(re-interrogation e) (let ([c1 (compile1 e (add1 c))]
                                [l1 (add1 c)]
                                [l2 (+ c 2)])
                            (append (list (vm-split l1 l2)) c1))]
    [(re-plus e) (let* ([c1 (compile1 e (add1 c))]
                        [l1 (add1 c)]
                        [l3 (+ (length c1) 2)])
                   (append (list c1 (vm-split l1 l1))))]
    [(re-star e) (let* ([c2 (compile1 e (add1 c))]
                        [l2 (add1 c)]
                        [l3 (+ c (length c2) 1)]
                        [l1 (+ 2 l3)])
                   (append (list (vm-split l2 l3) c2 (vm-jmp l1)))
                   )]))

(define (compile re)
  (append (compile1 re 0) (list (vm-match)))
  )

(provide compile)

; let para calcular os tamanhos e montar

; choice(e1 e2) inst...

; (compile (re-interrogation (re-choice (re-char "a") (re-char "b"))))
; (compile (re-star (re-char "a")))

; a | b
; (choice (char a) (char b) )
;"a+b+"
; (list (char #\a) (split 0 2) (char #\b) (split 2 4) (mtch))

; (a|b)*