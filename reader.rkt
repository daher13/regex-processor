#lang racket

(require regex-processor/parser
         regex-processor/compiler
         regex-processor/processor
         regex-processor/re)

(provide (rename-out [regex-read read]
                     [regex-read-syntax read-syntax]))

(define (regex-read in)
  (syntax->datum
   (regex-read-syntax #f in)))

(define (regex-read-syntax path port)
  (define parsed (parse port))
  ; (define compiled (compile (car parsed)))
(define compiled (compile (lang-re parsed)))
  (datum->syntax
   #f
   `(module regex-mod racket

      (require regex-processor/parser
               regex-processor/compiler
               regex-processor/processor
               regex-processor/re)
      (process (compile (lang-re ,parsed)) (lang-input ,parsed))
      )))