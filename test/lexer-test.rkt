#lang racket/base
(require silver-parser "../private/lexer.rkt")

(define str #<<STR
00
a1 a2 a3 _fds_ _FDS012 _0dfsA

//
//sdfa
// fdsa //
/* /*123 */ */ /*.*/
/* //fdsa
  /* 100 */
//jfdlsa/
*/

""
"  adf  \\ a \t \0 \123 \\"
"\"\
\
\""

'a'
'\''
'"'
'\"'
'\n'
STR
  )

(define in (open-input-string str))
(define ts (do-lex lex in))

(for ([v (send-generic ts ppeek 100)])
  (fprintf (current-output-port) "~a(~a): ~s\n"
           (Token-name v)
           (srcloc->string (Token-srcloc v))
           (Token-value v)))
