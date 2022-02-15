#lang racket
(require racket/trace)
;Calltrace is a tool that displays all calls to user procedures. It displays the arguments to the calls, and it indents to show the depth of the continuation.
#|

Lucas Piazzi de Castro - 201635003
Cristiano Nascimento da Silva - 201635029

*********************** IERF ***************************

σ => denota um estado (memória, ou store)
[l = v]σ => um estado que é igual a σ, exceto que no location l tem o valor v
σ(l) => valor no estado σ referenciado pelo location l |#

; Representando um estado como um par: próximo endereço e um vetor. 100 é o tamanho da memória
(define TAM 100) 
(define σ (cons 0 (make-vector TAM)))

;empty-store
(define (empty-store) (set! σ (cons 0 (cdr σ))))

;newref :: ExpVal -> Ref
(define (newref v)
  (define addr (car σ))
  (define mem (cdr σ))
  (vector-set! mem addr v)
  (set! σ (cons (add1 addr) mem))
  addr)

; deref :: Ref -> ExpVal
(define (deref addr)
  (if (< addr (car σ))
         (vector-ref (cdr σ) addr)
         (error "invalid location")))

; ********** ENV **********
#|
Environment
Env = Var -> Value

empty-env :: Env
extend-env :: Var x Value x Env -> Env
apply-env :: Env x Var -> Value |#
; setref! :: Ref x ExpVal -> ?

(define (setref! addr v)
  (if (< addr (car σ))
      (vector-set! (cdr σ) addr v)
      (error "invalid location")))

(define empty-env
  (lambda (var)
    (error "No bind")))

(define (extend-env var value env)
  (lambda (svar)
    (if (equal? svar var) value
        (apply-env env svar))))

(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

(define (apply-env env var)
  (env var))

(define init-env empty-env)


#|
; Definição dos Valores

(apply-proc (procedure var body Δ) val) = (value-of body [var=l]Δ [l=val]σ)
|#
; call-by-value
; proc-val :: Var x Expr x Env -> Proc
#;(define (proc-val var exp Δ)
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
#;(define (apply-proc proc val)
  (proc val))


; call-by-reference
(define (proc-val var exp Δ)
  (lambda (val flag)
    (if flag (value-of exp (extend-env var (newref val) Δ))
        (value-of exp (extend-env var val Δ)))))

(define (apply-proc proc val)
  (proc val #t))

(define (apply-proc-ref proc val)
  (proc val #f))


; ----------- value-of exp . Base IREF + operacoes: New, Super, Send, Self e Begin
(struct thunk (env exp))

  (define (value-of exp Δ)
  (define type (car exp))

  (cond [(equal? type 'lit) (cadr exp)]
        [(equal? type 'var) (define v (cadr exp))(deref (apply-env Δ (cadr exp)))]
        [(equal? type 'dif) (- (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
        [(equal? type 'zero?) (= (value-of (cadr exp) Δ) 0)]
        [(equal? type 'let) (value-of (cadddr exp) (extend-env (cadr exp) (newref (value-of (caddr exp) Δ)) Δ))]
        [(equal? type 'if) (if (value-of (cadr exp) Δ)
                               (value-of (caddr exp) Δ) (value-of (cadddr exp) Δ))]
        [(equal? type 'proc) (proc-val (cadr exp) (caddr exp) Δ)]
        [(equal? type 'call) (if (equal? (car (caddr exp)) 'var)
                                 (apply-proc-ref (value-of (cadr exp) Δ) (apply-env Δ (cadr (caddr exp))))
                                 (apply-proc (value-of (cadr exp) Δ) (value-of (caddr exp) Δ)))] 
        ;call-by-name
        [(equal? type 'call) (if (equal? (car (caddr exp)) 'var)
                                 (apply-proc-ref (value-of (cadr exp) Δ) (apply-env Δ (cadr (caddr exp))))
                                 (apply-proc (value-of (cadr exp) Δ) (thunk Δ (caddr exp))))] 

       
        [(equal? type 'letrec) (value-of (car (cddddr exp)) (extend-env-rec (cadr exp) (caddr exp) (cadddr exp) Δ))] 
        [(equal? type 'set) (let ([v (value-of (caddr exp) Δ)])
                              (setref! (apply-env Δ (cadr exp)) v)
                              v)] 
        
        [(equal? type 'begin) (foldr (lambda (e acumulador) (value-of e Δ)) (value-of (cadr exp) Δ) (cddr exp))] 

        [(equal? type 'super) (error "Falta Implementar") ]
        [(equal? type 'self ) (apply-env Δ '%self)] ; corresponde ao método self-exp() do livro
        [(equal? type 'new) (error "Falta Implementar") ]
        [(equal? type 'send) (error "Falta Implementar") ] 
       
        
        [else (error "operação não existe")])

  )

; ********** Cap 9. CLASSES **********
 
; Struct da classe, cada classe vai conter o nome de sua classe, uma superclass, membros e metodos
(struct class (classname super-name field-names method-env))

; Struct do metodo, cada metodo possui um nome (method name), parametros (method parameters) e corpo (method body)
(struct method (method-body method-name method-parameters))

; Struct do objeto, cada objeto possui o nome de sua classe e uma lista de referencias dos seus campos
(struct object (classname fields-refs))


; ********** Cap 9.4.3 Classes and Class Environments **********

;Nossa implementação depende da capacidade de obter informações sobre uma classe a partir de seu nome.
;Portanto, precisamos de um ambiente de classes para realizar essa tarefa.
;O ambiente de classe associará cada nome de classe a uma estrutura de dados que descreve a classe
(define the-class-env '()) ; <- representa uma lista de classes

; Adiciona uma classe ao ambiente de classes
(define add-class-to-env
  (λ (name obj_class)
    (set! the-class-env (append (list (cons name obj_class))
          the-class-env) )
    )
 )

(define lookup-class
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair (cadr maybe-pair)
          (display name)))))

; Retorna uma classe do ambiente de classes
(define (get-class name classes-list)
     (if (empty? classes-list) (void)
     (if (equal? name (caar classes-list)) (cdar classes-list)   ; Procurar na lista de classes o struct dado o nome
         (get-class name (cdr classes-list)))
     )
 )


;O procedimento append-field-name é usado para criar os nomes de campo para a classe atual.
;Acrescenta os campos da superclasse e os campos declarados pela nova classe
(define append-field-names
  (λ (super-fields child-fields)
    (cond
      ((null? super-fields) child-fields) ; <-- tratar campos duplicados, e usar o campo do filho como o certo
   )
))


; Retorna o nome dos campos de uma classe que esteja no ambiente de classes
(define get-field-names
 (lambda(class-name)
  (get-class class-name the-class-env)
   )
 )

; Inicializa uma classe, adicionando ela e seus campos no ambiente
(define initialize-class-decl
(lambda (decl)
  (display (cadr decl)) ;class-name
  (display (caddr decl)) ;super-clas-name
  (display (cdr (cadddr decl))) ;fields names
  (display  (cadddr (cdr decl))) ;method env (methods names)
  
  (let ([fields (append-field-names (get-field-names (caddr decl)) (cdr (cadddr decl)))] 
        [method-env (append (get-class (caddr decl) the-class-env) (create-methods-env (cadddr (cdr decl)) (caddr decl) (cdr (cadddr decl)) '())  ) ])
    (add-class-to-env (cadr decl) ( class (cadr decl) (caddr decl) fields method-env )) ))
  )

;Inicializa o ambiente de classes, chamando initialize-class-decl para todoas as classes no ambiente
(define initialize-class-env
  (lambda (classes-decls)
    (add-class-to-env 'object (class 'object 'object null null))
    (map initialize-class-decl classes-decls))
 )


; ********** Cap 9.4.4 Methods Environments **********

; Cria o método 
(define create-method
  (λ (method-decl super-name field-names)
    (let ([method-name  (car method-decl)])
      (cons method-name (method (cddr method-decl) super-name field-names))
      ) )
 )

; Define o ambiente 
(define create-methods-env
  (λ(methods-decls super-name fields method-env)
    (if (empty? methods-decls) method-env
    (create-methods-env-aux methods-decls super-name fields method-env))
 ))

; Coloca os métodos em uma list
(define create-methods-env-aux
 (λ(method-decl super-name fields method-env)
   (set! method-env (append (list (create-method (cdar method-decl) super-name fields) )
           method-env) )
   (create-methods-env (cdr method-decl) super-name fields method-env)
   )
  )


; Procura pelo método na lista de methods, se não encontrar retorna um erro;
(define find-method
  (λ (class-name method-name)
    (let ((method-env (class-method-env (get-class class-name the-class-env))))
      (let ((maybe-pair (assq method-name method-env)))
        (if (pair? maybe-pair) maybe-pair
            (display 'metodo-nao-existe)
         )
        )
      )
    )
 )

; ********** Testes **********

(define example '(
            (class c1 object (fields x y)  (( method initialize()(v1 lit 1)) (method test() (lit 2 )) ))
             (class c2 classe1 (fields xx yy)  ((method initialize(2) (lit 2 ))))
             (class c3 classe2 (fields xxx yyy zzz )  ((method initialize() (lit 3 ))))
            ))

(define methods '(( method initialize()(v1 lit 1)) (method test() (lit 1 )) ))

(initialize-class-env example)