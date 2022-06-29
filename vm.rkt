#lang racket

(struct vm-char
  (ch)
  #:transparent)

(struct vm-match
  ()
  #:transparent)

(struct vm-jmp
  (x)
  #:transparent)

(struct vm-split
  (x y)
  #:transparent)

(provide (all-defined-out))