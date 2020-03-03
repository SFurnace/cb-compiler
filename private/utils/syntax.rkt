#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide (all-defined-out))

(define-syntax (aif stx)
  (syntax-parse stx
    [(_ c:expr t:expr f:expr)
     (let ([it (datum->syntax #'c 'it #'c #'c)])
       #`(let ([#,it c])
           (if #,it t f)))]))
