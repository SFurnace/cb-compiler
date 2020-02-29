#lang racket/base
(require "../private/lexer.rkt")

(define str "
00 a1 a2 a3
/* /*123 */ */ /*.*/
/* //fdsa
  /* 100 */
//jfdlsa/
*/
")
(define in (open-input-string str))
(define ts (do-lex lex in))

(define (pos->str p)
  (format "~a:~a:~a" (position-line p) (position-col p) (position-offset p)))

(for ([v (send-generic ts ppeek 100)])
  (fprintf (current-output-port) "~a(~a-~a): ~a\n"
           (Token-name v) (pos->str (Token-start-pos v))
           (pos->str (Token-end-pos v)) (Token-value v)))
