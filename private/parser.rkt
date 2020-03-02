#lang racket/base
(require silver-parser "./lexer.rkt")
(provide (all-defined-out))

;; combined parser
(@ compilation_unit (@: import_stmts top_defs <EOF>))

(@ import_stmts (@* import_stmt))

(@ top_defs 1)

(@ import_stmt (@: <IMPORT> <IDENTIFIER> (@* (@: <DOT> <IDENTIFIER>)) <SEMICOLON>
                   => (displayln (list $1 $2))))
