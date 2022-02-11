#lang racket
#|

Lucas Piazzi de Castro - 201635003
Cristiano Nascimento da Silva - 201635029

**************************************************

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

        ;Exp implementadas para o trabalho
        [(equal? type 'super) (error "A") ]
        [(equal? type 'self ) (error "A")]
        [(equal? type 'new) (error "A") ]
        [(equal? type 'send) (error "A") ] 
       
        
        [else (error "operação não existe")])

  )