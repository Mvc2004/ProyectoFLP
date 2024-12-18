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
    (expresion ("meth" "(" identificador "," (separeted-list identificador ",") ")" expresion "end") method-decl)
    ;--------------
    (expresion ("(" expresion (arbno expresion) ")") app-exp)

    ;iteracion
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end → ")
    ;----------

    
    ;;procedimientos recursivos
    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in → "expresion "end") letrec-exp) 
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
    "(primitiva ("/") div-prim)"
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







