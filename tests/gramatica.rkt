#lang eopl

(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)
    (comentario ("(*" (arbno (not #\newline)) "*)") skip)
    (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit)"." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)"." digit (arbno digit)) number)
  )
)

(define especificacion-gramatical
  (
    (programa (expresion) a-program)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero) lit-exp)
    (expresion (letter) caracter-exp)
    (expresion (letter (arbno (or letter digit "?" "$"))) cadena-exp)
    (expresion ("ok") lit-exp)

    ;; Condicionales
    (expresion ("if" bool-expresion "then" expresion (arbno ("else-if" bool-expresion "then" expresion)) "else" expresion "end") if-exp)

    ;; Ligaduras locales
    (expresion ("let" (separated-list (identificador "=" expresion) ",") "in" expresion "end") let-exp)
    (expresion ("var" (separated-list (identificador "=" expresion) ",") "in" expresion "end") var-exp)

    ;; Procedimientos
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)

    ;; Métodos
    (expresion ("meth" "(" identificador "," (separated-list identificador ",") ")" expresion "end") method-decl)

    ;; Iteración
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)

    ;; Procedimientos recursivos
    (expresion ("letrec" (separated-list (identificador "(" (separated-list identificador ",") ")" "=" expresion) ",") "in" expresion "end") letrec-exp)

    ;; Asignación
    (expresion ("begin" (separated-list expresion ";") "end") begin-exp)
    (expresion ("set" identificador ":=" expresion) set-exp)

    ;; Objetos
    (expresion ("object" "{" (arbno (identificador "=>" expresion)) "}") object-exp)
    (expresion ("get" identificador "." identificador) get-exp)
    (expresion ("send" identificador "." identificador "(" (separated-list expresion ",") ")") send-exp)
    (expresion ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion ("clone" "(" identificador (separated-list identificador ",") ")") clone-exp)

    ;; Creación
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-object-exp)
    (expresion ("super" identificador "(" (separated-list expresion ",") ")") super-call-exp)

    ;; Primitivas
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") porcentaje-prim)
    (primitiva ("&") text-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)

    ;; Primitivas booleanas
    (bool-primitiva (">") mayor-prim)
    (bool-primitiva (">=") mayorigual-prim)
    (bool-primitiva ("<") menor-prim)
    (bool-primitiva ("<=") menorigual-prim)
    (bool-primitiva ("is") is-prim)

    ;; Expresiones booleanas
    (bool-expresion ("true") true-bool-exp)
    (bool-expresion ("false") false-bool-exp)
    (bool-expresion (bool-primitiva "(" (separated-list expresion ",") ")") bool-prim-exp)
    (bool-expresion (bool-oper "(" (separated-list bool-expresion ",") ")") bool-oper-exp)

    ;; Operaciones booleanas
    (bool-oper ("not") not-bool)
    (bool-oper ("and") and-bool)
    (bool-oper ("or") or-bool)

    ;; Declaración de clases
    (declaracion-clase ("class" identificador "extends" identificador (arbno ("field" identificador)) (arbno expresion)) a-class-decl)
)
)
 ;; enviroments

(define (make-env)
  (let ((env '()))
    (define (add-var name value)
      (set! env (cons (cons name value) env)))
    (define (lookup name)
      (define (find env)
        (cond [(empty? env) (error "Variable not found" name)]
              [(equal? (car (car env)) name) (cdr (car env))]
              [else (find (cdr env))]))
      (find env))
    (define (extend-env bindings)
      (for-each (lambda (binding) (add-var (car binding) (cdr binding))) bindings))
    (define (get-env) env)
    (define (set-env new-env) (set! env new-env))
    (list add-var lookup extend-env get-env set-env)))

;; Crear un objeto
(define (make-object fields methods)
  (lambda (msg)
    (cond [(equal? msg 'get) fields]  ;; Retorna los campos del objeto
          [(equal? msg 'method) methods]  ;; Retorna los métodos
          [else (error "Unknown message" msg)])))  ;; Mensaje no reconocido

;; Evaluar expresiones
(define (eval expr env)
  (cond
    [(number? expr) expr]  ;; Si es un número, retorna el número
    [(symbol? expr) (lookup expr)]  ;; Si es una variable, busca en el entorno
    [(pair? expr) (eval-application expr env)]))  ;; Evaluación de aplicaciones

(define (eval-application expr env)
  (let* ([operator (eval (car expr) env)]  ;; Evalúa el operador
         [args (map (lambda (arg) (eval arg env)) (cdr expr))])  ;; Evalúa los argumentos
    (apply operator args)))  ;; Aplica el operador a los argumentos