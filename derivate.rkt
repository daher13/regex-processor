#lang racket

(require "./re.rkt")

(define (is-null e)
  (match e
    [(re-emp) #f]
    [(re-lam) #t]
    [(re-char _) #f]
    [(re-cat e1 e2) (and (is-null e1)
                         (is-null e2))]
    [(re-choice e1 e2) (or (is-null e1)
                           (is-null e2))]
    [(re-star _) #t]))

(define (cat-simply e1 e2)
  (match (cons e1 e2)
    [(cons (re-emp) e) (re-emp)]
    [(cons e (re-emp)) (re-emp)]
    [(cons (re-lam) e) e]
    [(cons e (re-lam)) e]
    [(cons e1 e2) (re-cat e1 e2)]))

(define (choice-simply e1 e2)
  (match (cons e1 e2)
    [(cons (re-emp) e) e]
    [(cons e (re-emp)) e]
    [(cons e1 e2) (re-choice e1 e2)]))

(define (star-simply e1)
  (match e1
    [(re-emp) (re-lam)]
    [(re-lam) (re-lam)]
    [e2 (re-star e2)]))

(define (derivate e a)
  (match e
    [(re-emp) (re-emp)]
    [(re-lam) (re-lam)]
    [(re-char ch) (cond
                    [(eq? ch a) (re-lam)]
                    [else (re-emp)])]
    [(re-cat e1 e2) (cond
                      [(is-null e1) (cat-simply (derivate e1 a) e2)]
                      [else (cat-simply (derivate e1 a) e2)])]
    [(re-choice e1 e2) (choice-simply (derivate e1 a) (derivate e2 a)) ]
    [(re-star e) (cat-simply (derivate e a) (star-simply e))]
    ))

(define (accept e s)
  (match s
    ['() (is-null e)]
    [(cons c s1) (accept (derivate e c) s1)]))

(accept (re-star (re-char 1)) (list 1 1 2))