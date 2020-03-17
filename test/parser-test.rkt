#lang racket
(require rackunit rackunit/gui racket/pretty
         silver-parser
         "../private/lexer.rkt" "../private/parser.rkt")

(parsable-name "test")

(define (parse p s)
  (pretty-print (p (do-lex lex (open-input-string s)))))

(define-test-suite test-primary
  (test-case "base"
    (parse (@* primary) "
12U
0o127
'x'
\"fdjsalkfjkdls\"
xyx_xyx
(1 + 1)
(-a[12]++--+12*\"adf\")
a+
")))

(define-test-suite test-statement
  (test-case "base"
    (parse (@* statement) "
x:
if (a++)  {12;} else {13;};

y:switch((*&x[a++])*12) {
case 1:
   return x;
default:
   return y;
}

{ struct T x = 12; !x;}
")))

(define-test-suite test-program
  (test-case "base"
    (parse compilation-unit "
typedef int var;

static int i=1,b,a,c;
var i = 12;
")))
