#lang racket
(require rackunit rackunit/gui
         "../private/parser/parser.rkt")

(port-count-lines-enabled #t)

(define-test-suite Test-Parsers
  "tests for parsers"

  (test-case
   "test except end"
   (current-source-name "text")
   
   (check-not-false
    (let ([in (open-input-string "\n\r\r  \n\t\n\n")])
      (parse-eof in)))
   
   (check-exn exn:fail?
              (λ ()
                (let ([in (open-input-string "\n \r\n \r \na\n")])
                  (parse-eof in)))))

  (test-case
   "test parse import"
   (current-source-name "text")

   (let ([in (open-input-string "
import a0.b1.c2._d3;

   import jfdsla_.dfj.fsal;
    import     fj_fd;
")])
     (check-eqv? (length (parse-imports in)) 3))))

(define-test-suite Test-Utils
  "test some utils"

  (test-case
   "test match on struct"
   (define x (Program '_ '_ '_))
   (match x
     [(ast-node l) l]))

  (test-case
   "test make srcloc"
   (define str "a\nb\nc\nd\ne\nf\n")
   (define in (open-input-string str))
   (port-count-lines! in)
   (current-source-name "text")
   (read-string (random 0 (sub1 (string-length str))) in)
   (displayln (srcloc->string (new-srcloc in)))))

(test/gui Test-Parsers Test-Utils)
