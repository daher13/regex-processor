#lang racket

(struct char
  (ch)
  #:transparent)

(struct mtch
  ()
  #:transparent)

(struct jmp
  (x)
  #:transparent)

(struct split
  (x y)
  #:transparent)

(provide (all-defined-out))