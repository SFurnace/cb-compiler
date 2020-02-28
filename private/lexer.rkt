#lang racket/base
(require silver-parser)

(define-tokens
  VOID CHAR SHORT INT LONG STRUCT UNION
  ENUM STATIC EXTERN CONST SIGNED UNSIGNED
  IF ELSE SWITCH CASE DEFAULT WHILE
  DO FOR RETURN BREAK CONTINUE GOTO
  TYPEDEF IMPORT SIZEOF IDENTIFIER)

(define lex
  (lexer
   ;; empty
   [(:+ blank) ignored]
   
   ;; keywords
   ["void" (VOID)]
   ["char" (CHAR)]
   ["short" (SHORT)]
   ["int" (INT)]
   ["long" (LONG)]
   ["struct" (STRUCT)]
   ["union" (UNION)]
   ["enum" (ENUM)]
   ["static" (STATIC)]
   ["extern" (EXTERN)]
   ["const" (CONST)]
   ["signed" (SIGNED)]
   ["unsigned" (UNSIGNED)]
   ["if" (IF)]
   ["else" (ELSE)]
   ["switch" (SWITCH)]
   ["case" (CASE)]
   ["default" (DEFAULT)]
   ["while" (WHILE)]
   ["do" (DO)]
   ["for" (FOR)]
   ["return" (RETURN)]
   ["break" (BREAK)]
   ["continue" (CONTINUE)]
   ["goto" (GOTO)]
   ["typedef" (TYPEDEF)]
   ["import" (IMPORT)]
   ["sizeof" (SIZEOF)]

   ;; id
   [(:: (:/ "azAZ__") (:* (:/ "azAZ09__")))
    (IDENTIFIER lexeme)]))


(module+ test
  (define str "a1 a2 a3")
  (define in (open-input-string str))
  (define ts (do-lex lex in))

  (send-generic ts ppeek 100)
  )
