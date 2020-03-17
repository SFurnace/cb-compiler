#lang racket/base
(require racket/match silver-parser "./ast.rkt" "./lexer.rkt")
(provide (all-defined-out))

;;; Top

(@ compilation-unit (@: import-stmts top-defs <EOF>))

(@ library (@: import-stmts top-decls <EOF>))

(@ import-stmts (@* import-stmt))

(@ top-defs (@* (@u funcdef vardef constdef structdef uniondef typedef)))

(@ top-decls (@* (@u funcdecl vardecl constdef structdef uniondef typedef)))

;;; Import

(@ import-stmt
   (@: <IMPORT> ! (@sepBy <DOT> <IDENTIFIER>) <SEMICOLON>
       => (Import (Token-srcloc $0) (map Token-value $1))))

;;; Declaration

(@ funcdecl
   (@: <EXTERN> type <IDENTIFIER> <LEFT-S> ! params <RIGHT-S> <SEMICOLON>
       => (FuncDecl (Token-srcloc $0) (Token-value $2) $4 $1)))

(@ vardecl
   (@: <EXTERN> type <IDENTIFIER> <SEMICOLON>
       => (VarDecl (Token-srcloc $0) (Token-value $2) $1)))

;; Definition

(@ funcdef
   (@: (@? <STATIC>) type <IDENTIFIER> <LEFT-S> ! params <RIGHT-S> block
       => (if (null? $0)
              (FuncDef (Ast-location $1) (Token-value $2) $1 $4 $6 #f)
              (FuncDef (Token-srcloc (car $0)) (Token-value $2) $1 $4 $6 #t))))

(@ vardef
   (@: (@? <STATIC>) type vars <SEMICOLON>
       => (if (null? $0)
              (VarDef (Ast-location $1) $1 $2 #f)
              (VarDef (Token-srcloc (car $0)) $1 $2 #t))))

(@ typedef
   (@: <TYPEDEF> ! type <IDENTIFIER> <SEMICOLON>
       => (Typedef (Token-srcloc $0) (Token-value $2) $1)))

(@ constdef
   (@: <CONST> ! type <IDENTIFIER> <ASSIGN> expr <SEMICOLON>
       => (ConstDef (Token-srcloc $0) (Token-value $2) $1 $4)))

(@ structdef
   (@: <STRUCT> ! <IDENTIFIER> members <SEMICOLON>
       => (StructDef (Token-srcloc $0) (Token-value $1) $2)))

(@ uniondef
   (@: <UNION> ! <IDENTIFIER> members <SEMICOLON>
       => (UnionDef (Token-srcloc $0) (Token-value $1) $2)))

;;; Definition Helper

(@ vars
   (@sepBy <COMMA> (@u (@: <IDENTIFIER> <ASSIGN> expr => (cons (Token-value $0) $2))
                       (@: <IDENTIFIER> => (Token-value $0)))))

(@ members (@: <LEFT-B> (@sepBy <SEMICOLON> (@: type <IDENTIFIER>)) <RIGHT-B>))

(@ params
   (@u (@: <VOID> => '())
       (@: (@sepBy <COMMA> param) (@? <ELLIPSIS>)
           => (list (if (null? $1) 'normal 'vararg) $0))))
(@ param
   (@: type (@? <IDENTIFIER>)
       => (if (null? $1)
              (Param (Ast-location $0) #f $0)
              (Param (Ast-location $0) (Token-value (car $1)) $0))))

(@ type
   (@u (@postfix type-decorator base-type
                 #:constructor
                 (λ (decorator base)
                   (if (null? decorator)
                       base
                       (let ([p0 (Ast-location base)])
                         (match decorator
                           [`(array ,n ,p) (ArrayType (merge-srcloc p0 p) base n)]
                           [`(star ,p) (PointerType (merge-srcloc p0 p) base)]
                           [`(func (vararg ,args) ,p) (FuncType (merge-srcloc p0 p) args base #t)]
                           [`(func (normal ,args) ,p) (FuncType (merge-srcloc p0 p) args base #f)])))))
       base-type))

(@ base-type
   (@u (@: <VOID>
           => (BaseType (Token-srcloc $0) (Token-name $0) 'void))
       (@: (@? <UNSIGNED>) (@u <CHAR> <SHORT> <INT> <LONG>)
           => (if (null? $0)
                  (BaseType (Token-srcloc $1) (Token-name $1) 'int)
                  (BaseType (Token-srcloc (car $0)) (Token-name $1) 'unsigned-int)))
       (@: <STRUCT> ! <IDENTIFIER>
           => (BaseType (Token-srcloc $0) (Token-value $1) 'struct))
       (@: <UNION> ! <IDENTIFIER>
           => (BaseType (Token-srcloc $0) (Token-value $1) 'union))
       (@: <IDENTIFIER>
           => (BaseType (Token-srcloc $0) (Token-value $0) 'defined))))

(@ type-decorator
   (@u (@: <LEFT-M> ! (@? <INTEGER>) <RIGHT-M>
           => (list 'array (if (null? $1) #f (Token-value (car $1))) (Token-srcloc $2)))
       (@: <MUL> => (list 'star (Token-srcloc $0)))
       (@: <LEFT-S> ! params <RIGHT-S> => (list 'func $1 (Token-srcloc $2)))))

;;; Statement

(@ block
   (@: <LEFT-B> ! (@* vardef) (@* statement) <RIGHT-B>
       => (Block (Token-srcloc $0) $1 $2)))

(@ statement
   (@u <SEMICOLON>
       (@: expr <SEMICOLON> => $0)
       block
       labled-statement
       if-statement
       while-statement
       dowhile-statement
       for-statement
       switch-statement
       break-statement
       continue-statement
       goto-statement
       return-statement))

(@ labled-statement
   (@: <IDENTIFIER> <COLON> statement
       => (let ([r $2]) (set-Statement-lable! r (Token-value $0)) r)))

(@ if-statement
   (@: <IF> ! <LEFT-S> expr <RIGHT-S> statement (@? (@: <ELSE> ! statement => $1))
       => (If (Token-srcloc $0) #f $2 $4 (if (null? $5) #f (car $5)))))

(@ while-statement
   (@: <WHILE> ! <LEFT-S> expr <RIGHT-S> statement
       => (While (Token-srcloc $0) #f $2 $4)))

(@ dowhile-statement
   (@: <DO> ! statement <WHILE> <LEFT-S> expr <RIGHT-S> <SEMICOLON>
       => (DoWhile (Token-srcloc $0) #f $4 $1)))

(@ for-statement
   (@: <FOR> ! <LEFT-S> (@? expr) <SEMICOLON> (@? expr) <SEMICOLON> (@? expr) <RIGHT-S> statement
       => (For (Token-srcloc $0) #f
               (if (null? $2) #f (car $2))
               (if (null? $4) #f (car $4))
               (if (null? $6) #f (car $6))
               $8)))

(@ switch-statement
   (@: <SWITCH> ! <LEFT-S> expr <RIGHT-S> <LEFT-B> (@* clause) (@? default-clause) <RIGHT-B>
       => (Switch (Token-srcloc $0) #f $2 $5 (if (null? $6) #f (car $6)))))

(@ break-statement
   (@: <BREAK> ! <SEMICOLON>
       => (Break (Token-srcloc $0) #f)))

(@ continue-statement
   (@: <CONTINUE> ! <SEMICOLON>
       => (Continue (Token-srcloc $0) #f)))

(@ goto-statement
   (@: <GOTO> ! <IDENTIFIER> <SEMICOLON>
       => (Goto (Token-srcloc $0) #f (Token-value $1))))

(@ return-statement
   (@: <RETURN> ! (@? expr) <SEMICOLON>
       => (Return (Token-srcloc $0) #f (if (null? $1) #f $1))))

;;; Statement Helper

(@ clause
   (@: (@+ (@: <CASE> ! primary <COLON> => $1)) (@+ statement)
       => (cons $0 $1)))

(@ default-clause
   (@: <DEFAULT> ! <COLON> (@+ statement) => $2))

;;; Expr

(@ expr (@u (@: term assign-op ! expr
                => (Assign (Token-srcloc $1) (Token-value $1) $0 $2))
            expr10))

(@ expr10
   (@: expr9 (@? (@: <QUESTION> ! expr <COLON> expr10))
       => (if (null? $1)
              $0
              (Question (Ast-location $0) $0 (cadr $1) (cadddr $1)))))

(@ expr9
   (@infix-left <OR> expr8 #:constructor
                (λ (op x y)
                  (if (Or? x)
                      (Or (Token-srcloc op) (append (Or-conditions x) (list y)))
                      (Or (Token-srcloc op) (list x y))))))

(@ expr8
   (@infix-left <AND> expr7 #:constructor
                (λ (op x y)
                  (if (And? x)
                      (And (Token-srcloc op) (append (And-conditions x) (list y)))
                      (And (Token-srcloc op) (list x y))))))

(@ expr7 (@infix-left comparator expr6 #:constructor make-BinaryOp))

(@ expr6 (@infix-left <BIT-OR> expr5 #:constructor make-BinaryOp))

(@ expr5 (@infix-left <BIT-XOR> expr4 #:constructor make-BinaryOp))

(@ expr4 (@infix-left <BIT-AND> expr3 #:constructor make-BinaryOp))

(@ expr3 (@infix-left (@u <LSH> <RSH>) expr2 #:constructor make-BinaryOp))

(@ expr2 (@infix-left (@u <ADD> <SUB>) expr1 #:constructor make-BinaryOp))

(@ expr1 (@infix-left (@u <MUL> <DIV> <MOD>) term #:constructor make-BinaryOp))

(@ term
   (@u (@: <LEFT-S> type <RIGHT-S> term
           => (Cast (Token-srcloc $0) $1 $3))
       unary))

(@ unary
   (@u (@: unary-op unary
           => (UnaryPrefix (Token-srcloc $0) (Token-value $0) $1))
       (@: <SIZEOF> ! <LEFT-S> (@u type unary) <RIGHT-S>
           => (Sizeof (Token-srcloc $0) $2))
       postfix))

(@ postfix
   (@: primary (@* (@u (@: (@u <ADD2> <SUB2>) => (list (Token-value $0)))
                       (@: <LEFT-M> ! expr <RIGHT-M> => (list 'array $1))
                       (@: (@u <DOT> <ARROW>) ! <IDENTIFIER> => (list 'member (Token-value $0) (Token-value $1)))
                       (@: <LEFT-S> ! (@sepBy <COMMA> expr) <RIGHT-S> => (list 'call $1))))
       => (for/fold ([r $0] #:result r)
                    ([post (in-list $1)])
            (match post
              [`(,(and x (or '++ '--))) (UnaryPostfix (Ast-location r) x r #f)]
              [`(array ,x) (UnaryPostfix (Ast-location r) 'array-ref r x)]
              [`(member ,x ,y) (UnaryPostfix (Ast-location r) x r y)]
              [`(call ,args) (UnaryPrefix (Ast-location r) 'call r args)]))))

(@ primary
   (@u (@: <INTEGER> => (Int (Token-srcloc $0) (Token-value $0)))
       (@: <CHARACTER> => (Char (Token-srcloc $0) (Token-value $0)))
       (@: <STRING> => (Str (Token-srcloc $0) (Token-value $0)))
       (@: <IDENTIFIER> => (Id (Token-srcloc $0) (Token-value $0)))
       (@: <LEFT-S> ! expr <RIGHT-S> => $1)))

;;; Expr Helper

(@ assign-op
   (@u <ASSIGN> <ADD-ASSIGN> <SUB-ASSIGN> <MUL-ASSIGN> <DIV-ASSIGN> <MOD-ASSIGN> <BAND-ASSIGN>
       <BOR-ASSIGN> <BXOR-ASSIGN> <BLSH-ASSIGN> <BRSH-ASSIGN>))

(@ comparator
   (@u <BIGGER> <SMALLER> <LEAST> <MOST> <EQ> <NEQ>))

(@ unary-op
   (@u <ADD2> <SUB2> <ADD> <SUB> <NOT> <BIT-NOT> <MUL> <BIT-AND>))

;; helper
(define (merge-srcloc start end)
  (srcloc (srcloc-source start) (srcloc-line start) (srcloc-column start) (srcloc-position start)
          (+ (- (srcloc-position end) (srcloc-position start)) (srcloc-span end))))

(define (make-BinaryOp op x y) 
  (BinaryOp (Token-srcloc op) (Token-value op) x y))
