#lang racket

(require "vm-struct.rkt")

(struct state
  (pc sp input code mtch)
  #:transparent)

(define (fetch-code s)
  (cond
    [(>= (state-pc s) (length (state-code s))) '()]
    [else (list-ref (state-code s) (state-pc s))]))

(define (fetch-char s)
  (cond
    [(>= (state-sp s) (string-length (state-input s))) '()]
    [else (string-ref (state-input s) (state-sp s))]))

(define (inc-pc s)
  (match s
    [(state pc sp input code mtch) (state (add1 pc) sp input code mtch)]))

(define (inc-sp s)
  (match s
    [(state pc sp input code matches) (state pc (add1 sp) input code matches)]))

(define (jump s x)
  (match s
    [(state pc sp input code matches) (state x sp input code matches)]))

(define (matches s)
  (match s
    [(state pc sp input code mtch) (state pc sp input code #t)]))

(define (not-match s)
  (match s
    [(state pc sp input code mtch) (state pc sp input code #f)]))

(define (process s)
  (match (fetch-code s)
    [(vm-match) (matches s)]
    [(vm-char ch) (cond
                    [(eq? (fetch-char s) ch) (process (inc-pc (inc-sp s)))]
                    [else (not-match s)]
                    )]
    [(vm-jmp x) (process(jump s x))]
    [(vm-split x y) (match (process (jump s x))
                      [(state pc sp input code #f) (process (jump s y))]
                      [s s]
                      )]
    ))

(define (search s)
  (cond
    [(or (>= (state-pc s) (length (state-code s))) (>= (state-sp s) (string-length (state-input s)))) '()]
    [else (match (process s)
            [(state pc sp input code #t) (cons (cons (state-sp s) sp) (search (state 0 sp input code #f)))]
            )]
    ))

(define test (state 0 0 "abab" (list (vm-char #\a) (vm-split 0 2) (vm-char #\b) (vm-split 2 4) (vm-match)) #f)) ; a+b+

(search test)


; > (match '(1 3 5)
;    [(list (? odd?) ...) 'yes])

; 'yes
