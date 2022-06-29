#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER CHAR))
(define-empty-tokens syntax-tokens
  (EOF
   ADD
   STAR
   LPAREN
   RPAREN))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [#\+ (token-ADD)]
   [#\* (token-STAR)]
   ["("  (token-LPAREN)]
   [")"  (token-RPAREN)]
   [(:: (:+ numeric) (:* (:: #\. (:+ numeric) ))) (token-NUMBER (string->number lexeme))]
   [(:: alphabetic (:* (:or alphabetic numeric))) (token-CHAR lexeme)]
   ))


(provide next-token value-tokens syntax-tokens)
