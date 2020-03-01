#lang racket/base
(require "../private/lexer.rkt")

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

(define (pos->str p)
  (format "~a:~a:~a" (position-line p) (position-col p) (position-offset p)))

(for ([v (send-generic ts ppeek 100)])
  (fprintf (current-output-port) "~a(~a-~a): ~s\n"
           (Token-name v) (pos->str (Token-start-pos v))
           (pos->str (Token-end-pos v)) (Token-value v)))
