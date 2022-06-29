#lang racket

(struct emp
  ()
  #:transparent)

(struct lam
  ()
  #:transparent)

(struct chr
  (char)
  #:transparent)

(struct cat
  (left right)
  #:transparent)

(struct choice
  (left right)
  #:transparent)

(struct star
  (expr)
  #:transparent)

(struct interrogation
  (expr)
  #:transparent)

(define (is-null e)
  (match e
    [(emp) #f]
    [(lam) #t]
    [(chr _) #f]
    [(cat e1 e2) (and (is-null e1)
                      (is-null e2))]
    [(choice e1 e2) (or (is-null e1)
                        (is-null e2))]
    [(star _) #t]))

(define (cat-simply e1 e2)
  (match (cons e1 e2)
    [(cons (emp) e) (emp)]
    [(cons e (emp)) (emp)]
    [(cons (lam) e) e]
    [(cons e (lam)) e]
    [(cons e1 e2) (cat e1 e2)]))

(define (choice-simply e1 e2)
  (match (cons e1 e2)
    [(cons (emp) e) e]
    [(cons e (emp)) e]
    [(cons e1 e2) (choice e1 e2)]))

(define (star-simply e1)
  (match e1
    [(emp) (lam)]
    [(lam) (lam)]
    [e2 (star e2)]))

(define (derivate e a)
  (match e
    [(emp) (emp)]
    [(lam) (lam)]
    [(chr char) (cond
                  [(eq? char a) (lam)]
                  [else (emp)])]
    [(cat e1 e2) (cond
                   [(is-null e1) (cat-simply (derivate e1 a) e2)]
                   [else (cat-simply (derivate e1 a) e2)])]
    [(choice e1 e2) (choice-simply (derivate e1 a) (derivate e2 a)) ]
    [(star e) (cat-simply (derivate e a) (star-simply e))]
    ))

(define (accept e s)
  (match s
    ['() (is-null e)]
    [(cons c s1) (accept (derivate e c) s1)]))

(accept (star (chr 1)) (list 1 1 2))
