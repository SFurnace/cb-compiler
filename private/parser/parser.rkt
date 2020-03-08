#lang racket/base
(require "../utils/syntax.rkt")
(provide (all-defined-out))

;;; data definition

(struct ast-node [location])

(struct Program ast-node [Imports Defines])

(struct Import ast-node [name])

;;; praser ::=  Input-Port -> (U #f Any)

(define (parse-program in #:source-name [name (current-source-name)])
  (parameterize ([current-source-name name])
    (port-count-lines! in)
    (let ([fn (srcloc-getter in)]
          [is ((many parse-import) in)]
          [ds ((many parse-define) in)])
      (parse-eof in)
      (Program (fn in) is ds))))

(define (parse-eof in)
  (define (report-err)
    (error 'parse-eof "synatx error: ~a" (srcloc->string (new-srcloc in))))
  (expect in #px"^$" #:fail report-err))

(define (parse-import in)
  (aif (expect in #px"^import" #:ok (λ (_ loc) loc))
       (expect in #px"^([a-zA-Z_][a-zA-Z0-9_]*(\\.[a-zA-Z_][a-zA-Z0-9_]*)*)[[:space:]]*;"
               #:merge-srcloc it
               #:ok (λ (lst loc) (Import loc (cadr lst)))
               #:fail (λ () (error 'parse-import "invalid import format:~a" (srcloc->string it))))
       #f))

(define (parse-define in)
  (or (parse-defun in)
      (parse-defvars in)
      (parse-defconst in)
      (parse-defstruct in)
      (parse-defunion in)
      (parse-typedef in)))

(define (parse-defun in)
  #f)

(define (parse-defvars in)
  (let ([storage (expect in "^static" #:ok (λ _ #t))]
        [type (parse-type in)]
        [initial-list ((many parse-initial-list) in)])
    #f))

(define (parse-defconst in)
  #f)

(define (parse-defstruct in)
  #f)

(define (parse-defunion in)
  #f)

(define (parse-typedef in)
  #f)

(define (parse-type in)
  #f)

(define (parse-initial-list in)
  #f)

;;; parser helper
(define ((many parser) in)
  (let loop ([lst '()])
    (aif (parser in)
         (loop (cons it lst))
         (reverse lst))))

(define (merge-srcloc s0 s1)
  (srcloc (srcloc-source s0) (srcloc-line s0) (srcloc-column s0) (srcloc-position s0)
          (+ (- (srcloc-position s1) (srcloc-position s0)) (srcloc-span s1))))

(define (clean-whitespace in)
  (regexp-match #px"^[[:space:]]*" in))

(define (expect in regex
                #:ok [fn-ok (λ (r s) r)]
                #:fail [fn-fail (λ () #f)]
                #:merge-srcloc [s0 #f]
                #:clean-whitespace [clean #t])
  (define (convert regex-result)
    (if (pair? regex-result)
        (map (λ (x) (if (bytes? x) (bytes->string/utf-8 x) x)) regex-result)
        regex-result))
  
  (when clean (clean-whitespace in))
  (let* ([fn (srcloc-getter in)]
         [result (convert (regexp-try-match regex in))])
    (if result
        (let* ([s1 (fn in)]
               [sl (if s0 (merge-srcloc s0 s1) s1)])
          (fn-ok result sl))
        (fn-fail))))

;;; srcloc helper
(define current-source-name (make-parameter "unknown"))

(define (new-srcloc in)
  (let-values ([(l c p) (port-next-location in)])
    (srcloc (current-source-name) l c p #f)))

(define (srcloc-getter in0)
  (let-values ([(l c p0) (port-next-location in0)])
    (λ (in1)
      (let-values ([(source) (current-source-name)]
                   [(_0 _1 p1) (port-next-location in1)])
        (srcloc source l c p0 (- p1 p0))))))

;;; for debug
(define parser-debug-output (make-parameter (open-output-string)))
