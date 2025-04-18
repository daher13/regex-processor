#lang racket

(require "re.rkt")

(require "lexer.rkt" parser-tools/yacc)

(define regex-parser
  (parser
   (start language) ;;
   (end EOF)
   (tokens value-tokens syntax-tokens)
   (src-pos)
   (error
    (lambda (a b c d e)
      (begin
        (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n"
                a b c d e) (void))))
   (grammar
    (language [(REGEX expr INPUT STRING) (lang $2 $4) ])
    (expr [(expr OR term) (re-choice $1 $3)]
          [(term) $1])
    (term [(term factor) (re-cat $1 $2)]
          [(factor) $1])
    (factor [(factor STAR) (re-star $1)]
            [(LPAREN expr RPAREN) $2]
            [(atom) $1])
    (atom  [(CHAR) (re-char $1)])
    )))

(define (parse ip)
  (regex-parser (lambda () (next-token ip))))

(provide parse)
