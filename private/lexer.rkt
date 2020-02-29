#lang racket/base
(require silver-parser)
;; for test
(provide (all-defined-out) (all-from-out silver-parser))

(define-tokens
  VOID CHAR SHORT INT LONG STRUCT UNION
  ENUM STATIC EXTERN CONST SIGNED UNSIGNED
  IF ELSE SWITCH CASE DEFAULT WHILE
  DO FOR RETURN BREAK CONTINUE GOTO
  TYPEDEF IMPORT SIZEOF IDENTIFIER
  INTEGER COMMENT)

(define lex
  (lexer
   ;; empty
   [(:+ (union blank (char-set "\r\n"))) ignored]
   
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
    (IDENTIFIER lexeme)]

   ;; num
   [(:: (union (:: (:+ (:/ "09")))
               (:: #\0 (union #\x #\X) (:+ (:/ "09afAF")))
               (:: #\0 (union #\o #\O) (:+ (:/ "07"))))
        (:? (union #\U #\L)))
    (INTEGER lexeme)]

   ;; comment
   [(:: "//" (:* (:- any-char "\r" "\n"))) (COMMENT lexeme)]
   ["/*" (read-comment input-port start-pos)]
   ))

;; Helper
(define (make-position in)
  (let-values ([(l c p) (port-next-location in)])
    (position p l c)))

(define (read-comment in start)
  (define out (open-output-string))
  (parameterize ([current-input-port in]
                 [current-output-port out])
    (let loop ([count 1])
      (define s (peek-string 2 0))
      (cond 
        [(eof-object? s)
         (when (> count 0)
           (error 'read-comment "get eof-object"))]
        [(string=? "*/" s)
         (write-string (read-string 2))
         (set! count (sub1 count))
         (unless (= count 0)
           (loop count))]
        [(string=? "/*" s)
         (write-string (read-string 2))
         (loop (add1 count))]
        [else
         (write-char (read-char))
         (loop count)])))
  (Token 'COMMENT (string-append "/*" (get-output-string out))
         start (make-position in)))
