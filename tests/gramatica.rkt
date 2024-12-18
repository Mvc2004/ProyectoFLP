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
    (caracter ("'" letter "'") symbol)
    (cadena ("\"" (letter (arbno (or letter digit "?" "$")))"\"") string)
    (vacio ("ok") symbol)
    
    
  ))

(define especificacion-gramatical
  '(
    
    (programa (expresion) a-program)

    ;; Expresiones principales
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero) lit-exp)
    (expresion (caracter) char-exp)
    (expresion (cadena) string-exp)
    (expresion (vacio) ok-expresion)

    ;; Expresiones booleanas
    (bool-expresion ("true") true-bool-exp)
    (bool-expresion ("false") false-bool-exp)
    (bool-expresion (bool-primitiva "(" (separated-list expresion ",") ")") bool-prim-exp)
    (bool-expresion (bool-oper "(" (separated-list bool-expresion ",") ")") bool-oper-exp)

    ;; Primitivas booleanas
    (bool-primitiva (">") mayor-prim)
    (bool-primitiva (">=") mayorigual-prim)
    (bool-primitiva ("<") menor-prim)
    (bool-primitiva ("<=") menorigual-prim)
    (bool-primitiva ("is") is-prim)

    ;; Operadores booleanos
    (bool-oper ("not") not-bool)
    (bool-oper ("and") and-bool)
    (bool-oper ("or") or-bool)

    ;; Condicionales
    (expresion ("if" bool-expresion "then" expresion (arbno "elseif" bool-expresion "then" expresion) "else" expresion "end") if-exp)

    ;; Ligaduras locales
    (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)

    ;; Procedimientos
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
    (expresion ("meth" "(" identificador "," (separated-list identificador ",") ")" expresion "end") meth-exp)

    ;;Procedimiento Recursivo
    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in → "expresion "end") letrec-exp) 

    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion "end") varr-exp)

    ;iteracion
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end => ") for-exp)

     ;; Asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador ":=" expresion) set-exp)

    ;; Objetos
    (expresion ("object" "{" (arbno identificador "=>" expresion) "}") object-exp)
    (expresion ("get" identificador "." identificador) get-exp)
    (expresion ("send" identificador "." identificador "(" (separated-list expresion ",") ")") send-exp)
    (expresion ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion ("clone" "(" identificador (separated-list identificador ",") ")") clone-exp)

    ;; Primitivas aritméticas
    (expresion (primitiva "(" (arbno expresion) ")") prim-exp)
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva ("&") mod-prim)
  ))

;; Crear los datatypes automáticamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)
