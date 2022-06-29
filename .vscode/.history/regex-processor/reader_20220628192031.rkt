#lang racket

(require "../parser.rkt")
(require "../compiler.rkt")
(require "../processor.rkt")

(provide (rename-out [regex-read read]
                     [regex-read-syntax read-syntax]))

(define (regex-read in)
  (syntax->datum
   (regex-read-syntax #f in)))

(define (regex-read-syntax path port)
  (define grammar (parse port))
  (let ([types (infer grammar)])
    (if (eq? (cdr types) 'unsat)
        (error "The grammar isn't well-typed! It can loop on some inputs.")
        (datum->syntax
         #f
         `(module peg-mod racket
            (provide parser
                     pretty
                     (all-from-out typed-peg/tree))

            (require typed-peg/parser
                     typed-peg/pretty
                     typed-peg/tree
                     typed-peg/typing/infer)

            (define (parser s)
              (peg-parse ,grammar s))
            (define (pretty t)
              (peg-pretty ,grammar t)))))))