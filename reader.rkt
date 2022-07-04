#lang racket

(require "parser.rkt")
(require "compiler.rkt")
(require "processor.rkt")

(provide (rename-out [regex-read read]
                     [regex-read-syntax read-syntax]))

(define (regex-read in)
  (syntax->datum
   (regex-read-syntax #f in)))

(define (regex-read-syntax path port)
  (datum->syntax
   #f
   `(module regex-mod racket
      ,(finish (parse port)))))

(define (finish env)
  (displayln "Finished!"))
  