#lang racket
(require rackunit silver-parser "../private/lexer.rkt" "../private/parser.rkt")

(parsable-name "")

(define (parse p s)
  (p (do-lex lex (open-input-string s))))

(parse (@* primary) "
12U
0o127
'x'
\"fdjsalkfjkdls\"
xyx_xyx
(1 + 1)
(-a[12]++--+12*\"adf\")
a+
")

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
")

(parse compilation-unit "
typedef int var;

var i = 12;
")
