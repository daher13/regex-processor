#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER CHAR))
(define-empty-tokens syntax-tokens
  (EOF
   PLUS
   STAR
   LPAREN
   RPAREN
   OR
   ))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [#\* (token-STAR)]
   ["("  (token-LPAREN)]
   [")"  (token-RPAREN)]
   [#\+  (token-OR)]
   [(:: numeric) (token-NUMBER lexeme)]
   [(:: alphabetic) (token-CHAR lexeme)]
   ))

(provide next-token value-tokens syntax-tokens)
