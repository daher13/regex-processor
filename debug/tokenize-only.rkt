#lang racket

(require "../lexer.rkt"
         syntax/strip-context
         parser-tools/lex)

(define (regex-lex-test ip)
  (port-count-lines! ip)
  (letrec ([one-line
            (lambda ()
              (let ([result (next-token ip)])
                (unless (equal?	(position-token-token result) 'EOF)
                  (printf "~a\n" result)
                  (one-line)
                  )))])
    (one-line)))


(define (regex-read in)
  (syntax->datum
   (regex-read-syntax #f in)))

(define (regex-read-syntax path port)
  (datum->syntax
   #f
   `(module peg-tokenize-mod racket
      ,(regex-lex-test port))))

(module+ reader (provide (rename-out [regex-read read]
                                     [regex-read-syntax read-syntax])))

(regex-read  (open-input-string "regex ab+ input \"aab\""))
