#lang racket/base
(require silver-parser
         "../private/lexer.rkt"
         "../private/parser.rkt")

(define ts (do-lex lex (open-input-string #<<TXT

import a1.a2 .a3;

TXT
  )))

(import_stmt ts)

