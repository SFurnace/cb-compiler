#lang racket/base
(require "../utils/syntax.rkt")
(provide (all-defined-out))

;;; data definition
;; ast ::= srcloc
(struct ast-node [location])

(struct Program ast-node [Imports Defines])

(struct Import ast-node [name])

;;; praser
;; input-port -> Program
(define (parse-program in #:source-name [name #f])
  (parameterize ([current-source-name name])
    (port-count-lines! in)
    (let ([fn (srcloc-getter in)]
          [is (parse-imports in)]
          [ds (parse-defines in)])
      (parse-eof in)
      (Program (fn in) is ds))))

(define (parse-eof in)
  (unless (expect in #px"^$")
    (error 'parse-eof "synatx error: ~a" (srcloc->string (new-srcloc in)))))

(define (parse-imports in)
  (let loop ([lst '()])
    (aif (parse-import in)
         (loop (cons it lst))
         (reverse lst))))

(define (parse-import in)
  (let ([loc0 (expect in #px"^import" #:handler (λ (_ loc) loc))])
    (expect in #px"^([a-zA-Z_][a-zA-Z0-9_]*(\\.[a-zA-Z_][a-zA-Z0-9_]*)*)[[:space:]]*;"
            #:handler (λ (lst loc1) (Import (merge-srcloc loc0 loc1) (cadr lst))))))

(define (parse-defines in)
  (error))

;;; parser helper
(define (merge-srcloc s0 s1)
  (srcloc (srcloc-source s0) (srcloc-line s0) (srcloc-column s0) (srcloc-position s0)
          (+ (- (srcloc-position s1) (srcloc-position s0)) (srcloc-span s1))))

(define (clean-whitespace in)
  (regexp-match #px"^[[:space:]]*" in))

(define (expect in regex #:handler [handler #f])
  (define (convert lst)
    (map (λ (x) (if (bytes? x) (bytes->string/utf-8 x) x))
         lst))
  
  (clean-whitespace in)
  (let ([fn (srcloc-getter in)])
    (aif (regexp-try-match regex in)
         (let ([result (if it (convert it) #f)])
           (if handler (handler result (fn in)) result))
         #f)))

;;; srcloc helper
(define current-source-name (make-parameter #f))

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
