#lang racket

(struct re-emp
  ()
  #:transparent)

(struct re-lam
  ()
  #:transparent)

(struct re-char
  (ch)
  #:transparent)

(struct re-cat
  (left right)
  #:transparent)

(struct re-choice
  (left right)
  #:transparent)

(struct re-star
  (expr)
  #:transparent)

(struct re-interrogation
  (expr)
  #:transparent)

(struct re-plus
  (expr)
  #:transparent)

(provide (all-defined-out))