#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER))
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
   [(:: alphabetic (:* (:+ alphabetic numeric)))
    (token-IDENTIFIER lexeme)]
   [(:: (:+ alphabetic) (:* (:: #\. (:+ alphabetic) ))) (token-CHAR (string->alphabetic lexeme))]))

(provide next-token value-tokens var-tokens syntax-tokens)
