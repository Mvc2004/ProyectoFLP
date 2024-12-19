#lang eopl

(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)
    (comentario ("(*" (arbno (not #\newline)) "*)") skip)
    (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

(define especificacion-gramatical
  '(
    ;(programa ((arbno declaracion-clase) expresion) a-program)
    (programa (expresion) a-program)

    ;; Expresiones principales
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero) lit-exp)
    (expresion (letter) caracter-exp)
    (expresion (letter (arbno (or letter digit "?" "$"))) cadena-exp)
    (expresion (ok) lit-exp)
    
    ;;Agregamos la gramática de los condicionales y las ligaduras
    ;(expresion ("true") true-exp)
    ;(expresion ("false") false-exp)
    
    (expresion ("if" bool-expresion "then" expresion ("else-if" arbno bool-expresion "then" expresion) "else" expresion "end") if-exp)
    
    ;;Ligaduras locales
    (expresion ("let" (arbno "," identificador "=" expresion) "in" expresion "end") let-exp)
    (expresion ("var" (arbno "," arbno identificador "=" expresion) "in" expresion "end") var-exp)
    
    ;;Fin de condicionales y ligaduras
    
    ;;procedimientos
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
    (expresion ("apply" identificador "(" (separated-list expresion ",")")") apply-exp)
    ;;fin procedimientos
    
    ;metodo
   (expresion ("meth" "(" identificador "," (separated-list identificador ",") ")" expresion "end") method-decl)

    ;--------------
    (expresion ("(" expresion (arbno expresion) ")") app-exp)

    ;iteracion
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)

    ;----------

    
    ;;procedimientos recursivos
   (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion "end") letrec-exp)

    ;;fin de procedimientos recursivos

    (expresion (primitiva "(" (arbno expresion) ")") prim-exp)


    ;;Asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador ":=" expresion) set-exp)

    ;;Objetos

    (expresion ("object" "{" (arbno identificador "=>" expresion) "}") object-exp)
    (expresion ("get" identificador "." identificador ) get-exp)
    (expresion ("send" identificador "." identificador "(" (separated-list identificador ",") ")") send-exp)
    (expresion ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion ("clone" "(" identificador (separated-list identificador ",") ")") clone-exp)
    






    
    ;;Creacion
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-object-exp)
    ;;Send (aplicar método)
    ;(expresion("send" expresion identificador "("  (separated-list expresion ",") ")") method-app-exp)

    ;;Super llamados
    (expresion ("super" identificador    "("  (separated-list expresion ",") ")") super-call-exp)
    
    ;;Primitivas
    (expresion (primitiva "(" (arbno expresion) ")") prim-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") porcentaje-prim)
    (primitiva ("&") text-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)
    
    ;;primitivas booleanas
    (bool-primitiva (">") mayor-prim)
    (bool-primitiva (">=") mayorigual-prim)
    (bool-primitiva ("<") menor-prim)
    (bool-primitiva ("<=") menorigual-prim)
    (bool-primitiva ("is") is-prim)
    
    ;primitiva clase
    (primitiva ("list") list-prim)

    ;;expresiones boolenas
    (bool-expresion ("true") true-bool-expn)
    (bool-expresion ("false") false-bool-exp)
    (bool-expresion (bool-primitiva "("(arbno expresion) ")") bool-prim-exp)
    (bool-expresion (bool-oper "(" (arbno bool-expresion)")")bool-oper-exp)

    ;;operaciones booleanas
    (bool-oper (not) not-bool)
    (bool-oper (and) and-bool)
    (bool-oper (or) or-bool)))

    (declaracion-clase ("class" identificador "extends" identificador (arbno "field" identificador)(arbno meth-decl)) a-class-decl)
)


; Definición del entorno de ejecución
(define empty-env '())  ; entorno vacío

; Añadir una asociación al entorno
(define (extend-env var value env)
  (cons (cons var value) env))

; Buscar el valor de una variable en el entorno
(define (lookup-env var env)
  (cond ((null? env) (error "Variable no encontrada" var))
        ((eq? (caar env) var) (cdar env))
        (else (lookup-env var (cdr env)))))

; Evaluación de expresiones
(define (eval-exp exp env)
  (cond
    ; Número o literal
    ((number? exp) exp)
    ((boolean? exp) exp)
    ((string? exp) exp)
    
    ; Identificadores
    ((symbol? exp) (lookup-env exp env))

    ; Expresión `if`
    ((list? exp) 
     (cond
       [(eq? (car exp) 'if)
        (let* ((cond-exp (eval-exp (cadr exp) env))
               (then-exp (caddr exp))
               (else-exp (if (cadddr exp) (eval-exp (cadddr exp) env) '())))
          (if cond-exp
              (eval-exp then-exp env)
              else-exp))]
       
       ; Expresiones let y letrec
       [(eq? (car exp) 'let) (eval-let exp env)]
       [(eq? (car exp) 'letrec) (eval-letrec exp env)]
       
       ; Evaluación de procedimientos
       [(eq? (car exp) 'proc) (eval-proc exp env)]
       
       ; Evaluación de expresiones de aplicación
       [(eq? (car exp) 'apply) (eval-apply (cadr exp) (cddr exp) env)]
       
       ; Evaluación de objetos y métodos
       [(eq? (car exp) 'object) (eval-object exp env)]
       [(eq? (car exp) 'send) (eval-send exp env)]
       
       ; Otros casos
       [else (error "Expresión no válida" exp)])))

; Evaluación de `let`
(define (eval-let expr env)
  (let* ((bindings (cadr expr))  ; los enlaces "let"
         (body (cddr expr))      ; el cuerpo del `let`
         (new-env (extend-env (car (car bindings))
                              (eval-exp (cadr bindings) env)
                              env)))
    (eval-exp body new-env)))

; Evaluación de `letrec`
(define (eval-letrec expr env)
  (let* ((bindings (cadr expr))
         (body (cddr expr))
         (new-env (extend-env-recursive bindings env)))
    (eval-exp body new-env)))

; Función auxiliar para manejar `letrec` (más compleja, dado que permite recursión)
(define (extend-env-recursive bindings env)
  (define (make-bindings bs env)
    (if (null? bs)
        env
        (make-bindings (cdr bs)
                       (extend-env (car (car bs)) 'uninitialized env))))
  (define (initialize-bindings bs env)
    (if (null? bs)
        env
        (initialize-bindings (cdr bs)
                             (extend-env (car (car bs))
                                         (eval-exp (cadr (car bs)) env)
                                         env))))
  (initialize-bindings bindings (make-bindings bindings env)))

; Evaluación de expresión `apply` (invocación de función)
(define (eval-apply fun-expr arg-exprs env)
  (let* ((fun (eval-exp fun-expr env))  ; Evaluamos la función
         (args (map (lambda (arg) (eval-exp arg env)) arg-exprs)))
    (apply fun args)))  ; Aquí se aplicaría la función, según sea un procedimiento o método

; Evaluación de la creación de objetos
(define (eval-object expr env)
  (let* ((fields (map (lambda (pair)
                        (cons (car pair)
                              (eval-exp (cdr pair) env)))  ; Evaluamos los valores de los campos
                      (cadr expr))))
         (new-object (lambda (field-name) 
                       (lookup-env field-name fields))))
    new-object))  ; Retorna la función que es el objeto

; Evaluación de `send` (invocar un método de un objeto)
(define (eval-send expr env)
  (let* ((obj (eval-exp (cadr expr) env))  ; Evaluamos el objeto
         (method (caddr expr))            ; Obtenemos el método
         (args (map (lambda (arg) (eval-exp arg env)) (cdddr expr))))
    (apply method args)))  ; Se invoca el método con los argumentos
    