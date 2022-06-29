#lang racket

(require "../parser.rkt")
(require "../compiler.rkt")
(require "../processor.rkt")

(define parsed (parse (open-input-string "(ab)+cd")))
parsed
(define compiled (compile parsed))
compiled
(process compiled "ababd")
