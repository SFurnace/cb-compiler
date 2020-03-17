#lang racket/base
(provide (all-defined-out))

(define old (current-inspector))
(current-inspector (make-inspector))

(struct Ast [location])

(struct Import Ast [ids])

(struct FuncDecl Ast [name params rtype])

(struct VarDecl Ast [name type])

(struct Typedef Ast [name type])

(struct VarDef Ast [type vars static])

(struct FuncDef Ast [name r-type params body static])

(struct ConstDef Ast [name type expr])

(struct StructDef Ast [name members])

(struct UnionDef Ast [name members])

(struct BaseType Ast [name kind])

(struct ArrayType Ast [base len])

(struct PointerType Ast [base])

(struct FuncType Ast [params rtype vararg])

(struct Param Ast [name type])

(struct Block Ast [vars statements])

(struct Statement Ast [(lable #:mutable)])

(struct If Statement [condition true-branch false-branch])

(struct While Statement [condition body])

(struct DoWhile Statement [condition body])

(struct For Statement [init condition update body])

(struct Switch Statement [target clauses default])

(struct Break Statement [])

(struct Continue Statement [])

(struct Goto Statement [id])

(struct Return Statement [val])

(struct Assign Ast [op term val])

(struct Question Ast [condition true-branch false-branch])

(struct Or Ast [conditions])

(struct And Ast [conditions])

(struct BinaryOp Ast [op left right])

(struct UnaryPrefix Ast [op term])

(struct UnaryPostfix Ast [op term extra])

(struct Cast Ast [type term])

(struct Sizeof Ast [target])

(struct Int Ast [str])

(struct Char Ast [str])

(struct Str Ast [str])

(struct Id Ast [str])

(current-inspector old)
