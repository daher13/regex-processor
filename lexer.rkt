#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)) ;; : is prefix for all parser-tools/lex-sre

(define-tokens value-tokens (CHAR STRING))
(define-empty-tokens syntax-tokens
  (EOF
   PLUS
   STAR
   LPAREN
   RPAREN
   OR
   REGEX
   INPUT
   ))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ whitespace) (return-without-pos (next-token input-port))]
   [#\* (token-STAR)]
   ["("  (token-LPAREN)]
   [")"  (token-RPAREN)]
   [#\+  (token-OR)]
   ["regex" (token-REGEX)]
   ["input" (token-INPUT)]
   [(:or alphabetic numeric) (token-CHAR (string-ref lexeme 0))]
   [(:seq #\" (complement (:seq any-string #\" any-string)) #\")
    (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))]
   ))

(provide next-token value-tokens syntax-tokens)
