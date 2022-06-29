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

(struct state
  (pc sp input code mtch)
  #:transparent)

(provide (all-defined-out))