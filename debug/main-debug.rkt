#lang racket

(require "../parser.rkt")
(require "../compiler.rkt")
(require "../processor.rkt")
(require "../re.rkt")

(define parsed (parse (open-input-string "regex ab* input \"abc\"")))
(lang-re parsed)
(define compiled (compile (lang-re parsed)))
compiled
(process compiled (lang-input parsed))
