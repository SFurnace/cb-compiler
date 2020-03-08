#lang racket
(require rackunit "../private/parser/parser.rkt")

(port-count-lines-enabled #t)

(define-test-suite Test-Parsers
  "tests for parsers"

  (test-case
   "test except end"
   
   (check-not-false
    (let ([in (open-input-string "\n\r\r  \n\t\n\n")])
      (parse-eof in)))
   
   (check-exn exn:fail?
              (λ ()
                (let ([in (open-input-string "\n \r\n \r \na\n")])
                  (parse-eof in)))))

  (test-case
   "test parse import"

   (let ([in (open-input-string "
import a0.b1.c2._d3;

   import jfdsla_.dfj.fsal;
    import     fj_fd;
")])
     (check-eqv? (length ((many parse-import) in)) 3))

   (let ([in (open-input-string "
import 123.345;
")])
     (check-exn exn:fail? (λ () (parse-import in))))))

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
   (read-string (random 0 (sub1 (string-length str))) in)
   (displayln (srcloc->string (new-srcloc in)))))


;; Runner
(module+ test
  (require rackunit/gui)

  (test/gui #:wait? #t
            Test-Utils Test-Parsers))
