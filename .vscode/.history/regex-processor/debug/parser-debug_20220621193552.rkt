#lang racket


(require "../parser.rkt")

(module reader racket
  (require "../parser.rkt")
  (provide (rename-out [regex-read read]
                       [regex-read-syntax read-syntax]))


  (define (regex-read in)
    (syntax->datum
     (regex-read-syntax #f in)))

  (define (regex-read-syntax path port)
    (datum->syntax
     #f
     `(module imp-mod racket
        ,@(parse port)))))

(parse (open-input-string "ab+c*d"))