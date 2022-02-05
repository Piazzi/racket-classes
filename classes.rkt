#lang racket
#|
Sintaxe:
Program → {ClassDecl}∗ Expression
ClassDecl → class Identifier extends Identifier {field Identifier}∗ {MethodDecl}∗
MethodDecl → method Identifier ({Identifier}∗(,))
Expression → new Identifier ({Expression}∗(,))
| send Expression Identifier ({Expression}∗(,))
| super Identifier ({Expression}∗(,))
| self
| Number
| -(Expression , Expression)
| zero? ( Expression )
| if Expression then Expression else Expression
| Identifier
| let Identifier = Expression in Expression
| proc ( Identifier ) Expression
| ( Expression , Expression )
| letrec Identifier ( Identifier ) = Expression in Expression
| set Identifier = Expression
| begin {Expression}∗(;) end

Exemplo:

class c1 extends object
method initialize () 1
method m1 () send self m2 ()
method m2 () 13
class c2 extends c1
method m1 () 22
method m2 () 23
method m3 () super m1 ( )
class c3 extends c2
method m1 ( ) 32
method m2 ( ) 33
let o3 = new c3 ( )
in send o3 m3( )
|#
