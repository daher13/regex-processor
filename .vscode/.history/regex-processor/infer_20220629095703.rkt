#lang racket

;; main type inference driver

(require "./parser.rkt")
(require "./compiler.rkt")
(require "./processor.rkt")

(provide infer)

(define (infer g)
        (parse (open-input-string g)))
