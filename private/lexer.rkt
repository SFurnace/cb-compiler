#lang racket/base
(require silver-parser)
(provide (all-defined-out))

(define-tokens
  SEMICOLON COMMA DOT ADD SUB MUL DIV ASSIGN
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

   ;; termainal
   [";" (SEMICOLON)]
   ["," (COMMA)]
   ["." (DOT)]
   ["+" (ADD)]
   ["-" (SUB)]
   ["*" (MUL)]
   ["/" (DIV)]
   ["=" (ASSIGN)]

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
   ["/*" (BLOCK-COMMENT)]

   ;; string
   [#\" (STRING)]

   ;; char
   [#\' (CHARACTER)]))

;; Helper
(define (map-escape-char c)
  (cdr (or (assv c '((#\\ . #\\)
                     (#\' . #\')
                     (#\" . #\")
                     (#\newline . #\newline)
                     (#\return . #\return)
                     (#\n . #\newline)
                     (#\r . #\return)
                     (#\0 . #\null)
                     (#\b . #\backspace)
                     (#\t . #\tab)
                     (#\v . #\vtab)))
           (raise (format "bad escape char: ~s" c)))))

;; Special Token
(define-special-token CHARACTER
  (let ([c (read-char)])
    (cond
      [(eof-object? c) (raise "get eof")]
      [(char=? c #\') (raise "empty character token")]
      [(char=? c #\\) (write-char (map-escape-char (read-char)))]
      [else (write-char c)]))
  (unless (char=? (read-char) #\')
    (raise "bad character token")))

(define-special-token STRING
  (let loop ()
    (define c (read-char))
    (cond
      [(eof-object? c) (raise "get eof")]
      [(memv c '(#\newline #\return)) (raise "unexpected newline")]
      [(char=? c #\")]
      [(char=? c #\\)
       (write-char
        (cond
          [(regexp-match-peek #px"^[0-7]{3}" (current-input-port))
           =>
           (λ (x)
             (read-string 3)
             (integer->char (string->number (bytes->string/utf-8 (car x)) 8)))]
          [(regexp-match-peek #px"^\r\n" (current-input-port))
           =>
           (λ (x)
             (read-string 2))]
          [else (map-escape-char (read-char))]))
       (loop)]
      [else
       (write-char c)
       (loop)])))

(define-special-token BLOCK-COMMENT
  (write-string "/*")
  (let loop ([count 1])
    (define s (peek-string 2 0))
    (cond
      [(eof-object? s) (when (> count 0) (raise "get eof"))]
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
