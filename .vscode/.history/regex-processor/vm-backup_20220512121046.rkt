#lang racket

(struct char
  (c)
  #:transparent)

(struct match-clause
  ()
  #:transparent)

(struct jmp
  (x)
  #:transparent)

(struct split
  (x y)
  #:transparent)

(struct state
  (pc sp input code matches)
  #:transparent)

(define (fetch-code pc code)
  (cond
    [(>= pc (length code)) '()]
    [else (list-ref code pc)]))

(define (fetch-char s p)
  (cond
    [(>= p (string-length s)) '()]
    [else (string-ref s p)]))

(define (insert-match s ch) ; trocar para true e false
  (match s
    [(state pc sp input code '()) (state pc sp input code (list (list ch)))]
    [(state pc sp input code (cons p ps)) (state pc sp input code (cons (append p (list ch)) ps))])
  )

(define (inc-pc s)
  (match s
    [(state pc sp input code matches) (state (add1 pc) sp input code matches)])
  )

(define (inc-sp s)
  (match s
    [(state pc sp input code matches) (state pc (add1 sp) input code matches)])
  )

(define (jump s x)
  (match s
    [(state pc sp input code matches) (state x sp input code matches)]))

(define (create-new-match s)
  (match s
    [(state pc sp input code '()) (state pc sp input code (list '()))]
    [(state pc sp input code (cons '() ps)) s]
    [(state pc sp input code (cons p ps)) (state pc sp input code (cons '() (cons p ps)))]))

(define (process s)
  (match (fetch-code (state-pc s) (state-code s))
    ['() s]
    [(char ch) (cond
                 [(eq? (fetch-char (state-input s) (state-sp s)) ch)
                  (process (insert-match (inc-pc (inc-sp s)) ch))
                  ]
                 [else (create-new-match s)]
                 )]
    [(jmp x) (process(jump s x))]
    [(split x y) (match (process (jump s x))
                   [(state pc sp input code (list '() matches)) (process (jump s y))]
                   [(state pc sp input code matches) (process (jump s x))]
                   )]
    ))

; havera uma instrucao match tbm

(define state-test (state 0 0 "abcabd" (list (char #\a) (char #\b) (char #\d)) '()))
; (define state-test (state 0 0 "abd" (list (char #\a) (char #\b) (char #\c)) '()))
; (define state-test (state 0 0 "abcdef" (list (char #\a) (split 2 3) (char #\b) (char #\c) (match-clause)) '()))

(process state-test)

; (0,0, "acd", [char a, char b, char c ],  [])

; (0,0, "acd", [char a, char b, char c ],  [(0 0)]) => o segundo zero representa char a
; (1,1, "acd", [char a, char b, char c ],  #f) => stop => #f => b != c
; Terminou - nao deu match


; abc "a/c"
; (0,0, "acd", [split ? ?, char a, jmp ?, char c, ],  [])
;                     1 3              4 // para as instrucoes

; (0,0, "acd", [split 1 3, char a, jmp 4, char c, ],  []) => thread 1
;    (1,0, "acd", [split 1 3, char a, jmp 4, char c, ],  [])
;    (1,1 "acd", [split 1 3, char a, jmp 4, char c, ],  [(0 0)])
;    (2,1 "acd", [split 1 3, char a, jmp 4, char c, ],  [(0 0)])
;    (4,1 "acd", [split 1 3, char a, jmp 4, char c, ],  [(0 0)]) => vazia
; (3,0, "acd", [split 1 3, char a, jmp 4, char c, ],  [()])
;    (4,0, "acd", [split 1 3, char a, jmp 4, char c, ],  #f) => nao deu match

; so muda o estado quando da match
; semantica formal