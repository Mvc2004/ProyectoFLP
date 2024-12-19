#lang eopl

(define especificacion-lexica
  '(
    (espacio-blanco (whitespace) skip)
    (comentario ("(" (arbno (not #\newline)) ")") skip)
    (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (caracter ("'" letter "'") symbol)
    (cadena ("\"" (letter (arbno (or letter digit "?" "$")))"\"") string)
    ;(vacio ("ok") symbol)
    
    
  ))

(define especificacion-gramatical
  '(
    
    (programa ((arbno declaracion-clase) expresion) a-program)
    
    ;(programa (expresion) a-program)

    ;; Expresiones principales
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero) lit-exp)
    (expresion (caracter) char-exp)
    (expresion (cadena) string-exp)
    (expresion ("ok") ok-empty-exp)

    ;; Expresiones booleanas
    (bool-expresion ("true") true-bool-exp)
    (bool-expresion ("false") false-bool-exp)
    (bool-expresion (bool-oper "(" (separated-list bool-expresion ",") ")") bool-oper-exp)

    ;; Primitivas booleanas
    (bool-expresion ("(" bool-primitiva (separated-list expresion ",") ")") bool-prim-exp)
    ;; bool primitiva
    (bool-primitiva (">") mayor-prim)
    (bool-primitiva (">=") mayorigual-prim)
    (bool-primitiva ("<") menor-prim)
    (bool-primitiva ("<=") menorigual-prim)
    (bool-primitiva ("is") is-prim)

    ;; Operadores booleanos
    (bool-expresion (bool-oper "(" (separated-list bool-expresion ",") ")") bool-oper-exp)
    
    ;; bool oper
    (bool-oper ("not") not-bool)
    (bool-oper ("and") and-bool)
    (bool-oper ("or") or-bool)

    ;; Condicionales
    (expresion ("if" bool-expresion "then" expresion (arbno "elseif" bool-expresion "then" expresion) "else" expresion "end") if-exp)

    ;; Ligaduras locales
    (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)

    ;; Procedimientos
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
    (expresion ("(" expresion (arbno expresion) ")") app-exp)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
    (expresion ("meth" method-decl) meth-exp)

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
    (primitiva ("&") text-prim)

     ;;Super llamados
    (expresion                                
      ("super" identificador    "("  (separated-list expresion ",") ")")
      super-call-exp)
    
    (declaracion-clase ("class" identificador "extends" identificador(arbno "field" identificador)(arbno expresion))a-class-decl)
    (method-decl("method" identificador "("  (separated-list identificador ",") ")" expresion) a-method-decl)
  ))

;; Crear los datatypes automáticamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;;Evaluar programa
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (a-program (c-decls exp) 
                 (elaborar-declaraciones-clase! c-decls)
                 (evaluar-expresion exp ambiente-inicial))
      ))
  )

;;ambientes
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-ref
   (lids (list-of symbol?))
   (lvalue vector?)
   (old-env ambiente?)))

(define ambiente-extendido
  (lambda (lids lvalue old-env)
    (ambiente-extendido-ref lids (list->vector lvalue) old-env)))

;;Implementación ambiente extendido recursivo

(define ambiente-extendido-recursivo
  (lambda (procnames lidss cuerpos old-env)
    (let
        (
         (vec-clausuras (make-vector (length procnames)))
         )
      (letrec
          (
           (amb (ambiente-extendido-ref procnames vec-clausuras old-env))
           (obtener-clausuras
            (lambda (lidss cuerpos pos)
              (cond
                [(null? lidss) amb]
                [else
                 (begin
                   (vector-set! vec-clausuras pos
                                (closure (car lidss) (car cuerpos) amb))
                   (obtener-clausuras (cdr lidss) (cdr cuerpos) (+ pos 1)))]
                )
              )
            )
           )
        (obtener-clausuras lidss cuerpos 0)
        )
      )
    )
  )


(define apply-env
  (lambda (env var)
    (deref (apply-env-ref env var))))


(define apply-env-ref
  (lambda (env var)
    (cases ambiente env
      (ambiente-vacio () (eopl:error "No se encuentra la variable " var))
      (ambiente-extendido-ref (lid vec old-env)
                          (letrec
                              (
                               (buscar-variable (lambda (lid vec pos)
                                                  (cond
                                                    [(null? lid) (apply-env-ref old-env var)]
                                                    [(equal? (car lid) var) (a-ref pos vec)]
                                                    [else
                                                     (buscar-variable (cdr lid) vec (+ pos 1)  )]
                                                    )
                                                  )
                                                )
                               )
                            (buscar-variable lid vec 0)
                            )
                          
                          )
      
      )
    )
  )

(define ambiente-inicial
  (ambiente-extendido '(x y z) '(4 2 5)
                      (ambiente-extendido '(a b c) '(4 5 6)
                                          (ambiente-vacio))))

(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (dato) dato)
      (var-exp (id) (apply-env amb id))
      (prim-exp (prim args)
                (let ((lista-numeros (map (lambda (x) (evaluar-expresion x amb)) args)))
                  (evaluar-primitiva prim lista-numeros)))
      (bool-exp (value) value)
      (char-exp (char) char)
      (string-exp (str) str)
      (ok-empty-exp () empty)
      
      ;; Condicionales
      (if-exp (condicion hace-verdadero elseif-condiciones elseif-cuerpos hace-falso)
              (letrec ((procesar-elseif
                        (lambda (conds cuerpos)
                          (if (null? conds)
                              (evaluar-expresion hace-falso amb)
                              (if (evaluar-expresion (car conds) amb)
                                  (evaluar-expresion (car cuerpos) amb)
                                  (procesar-elseif (cdr conds) (cdr cuerpos)))))))
                (if (evaluar-expresion condicion amb)
                    (evaluar-expresion hace-verdadero amb)
                    (procesar-elseif elseif-condiciones elseif-cuerpos))))
      
      ;; Ligaduras
      (let-exp (ids rands body)
               (let ((lvalues (map (lambda (x) (evaluar-expresion x amb)) rands)))
                 (evaluar-expresion body (ambiente-extendido ids lvalues amb))))
      
      ;; Procedimientos
      (proc-exp (ids body)
                (closure ids body amb))
      
      (app-exp (rator rands)
               (let ((lrands (map (lambda (x) (evaluar-expresion x amb)) rands))
                     (procV (evaluar-expresion rator amb)))
                 (if (procval? procV)
                     (cases procval procV
                       (closure (lid body old-env)
                               (if (= (length lid) (length lrands))
                                   (evaluar-expresion body (ambiente-extendido lid lrands old-env))
                                   (eopl:error "El número de argumentos no es correcto, debe enviar" (length lid) " y usted ha enviado" (length lrands)))))
                     (eopl:error "No puede evaluarse algo que no sea un procedimiento" procV))))
      
      (apply-exp (id args)
                 (let* ((proc (apply-env amb id))
                        (eval-args (map (lambda (x) (evaluar-expresion x amb)) args)))
                   (if (procval? proc)
                       (cases procval proc
                         (closure (params body proc-amb)
                                 (if (= (length params) (length eval-args))
                                     (evaluar-expresion body (ambiente-extendido params eval-args proc-amb))
                                     (eopl:error "Número incorrecto de argumentos para apply-exp"))))
                       (eopl:error "El identificador no es un procedimiento" proc))))
      
      (meth-exp (method)
                (evaluar-method method amb))
      
      ;; Recursión y variables
      (letrec-exp (procnames idss cuerpos cuerpo-letrec)
                  (evaluar-expresion cuerpo-letrec
                                   (ambiente-extendido-recursivo procnames idss cuerpos amb)))
      
      (varr-exp (ids rands body)
                (let ((lvalues (map (lambda (x) (evaluar-expresion x amb)) rands)))
                  (evaluar-expresion body (ambiente-extendido ids lvalues amb))))
      
      ;; Iteración
      (for-exp (var start-expr end-expr body)
               (let* ((start (evaluar-expresion start-expr amb))
                      (end (evaluar-expresion end-expr amb)))
                 (if (and (number? start) (number? end))
                     (let loop ((i start) (env amb))
                       (if (<= i end)
                           (begin
                             (evaluar-expresion body (ambiente-extendido (list var) (list i) env))
                             (loop (+ i 1) env))
                           #f))
                     (eopl:error "for-exp requiere números para los límites" start end))))
      
      ;; Asignación y secuencia
      (begin-exp (exp lexp)
                 (if (null? lexp)
                     (evaluar-expresion exp amb)
                     (begin
                       (evaluar-expresion exp amb)
                       (letrec ((evaluar-begin
                                (lambda (lexp)
                                  (cond
                                    [(null? (cdr lexp)) (evaluar-expresion (car lexp) amb)]
                                    [else
                                     (begin
                                       (evaluar-expresion (car lexp) amb)
                                       (evaluar-begin (cdr lexp)))]))))
                         (evaluar-begin lexp)))))
      
      (set-exp (id exp)
               (begin
                 (setref! (apply-env-ref amb id)
                         (evaluar-expresion exp amb))
                 1))
      
      ;; Objetos
      (object-exp (class-name rands)
                  (let ((args (map (lambda (x) (evaluar-expresion x amb)) rands))
                        (obj (new-object class-name)))
                    (find-method-and-apply 'initialize class-name obj args)
                    obj))
      
      (send-exp (obj-exp method-name rands)
                (let ((args (map (lambda (x) (evaluar-expresion x amb)) rands))
                      (obj (evaluar-expresion obj-exp amb)))
                  (find-method-and-apply method-name (object->class-name obj) obj args)))
      
      (get-exp (obj-id attr-id)
               (let ((obj (apply-env amb obj-id)))
                 (if (object? obj)
                     (object-lookup obj attr-id)
                     (eopl:error "El identificador no es un objeto" obj))))
      
      (clone-exp (obj-id field-ids)
                 (let ((obj (apply-env amb obj-id)))
                   (if (object? obj)
                       (let ((new-obj (new-object (object->class-name obj))))
                         (find-method-and-apply 'initialize 
                                              (object->class-name obj) 
                                              new-obj '()))
                       (eopl:error "El identificador no es un objeto" obj))))
      
      (update-exp (obj-id field-id exp)
                  (let ((obj (apply-env amb obj-id))
                        (val (evaluar-expresion exp amb)))
                    (if (object? obj)
                        (begin
                          (object-update! obj field-id val)
                          val)
                        (eopl:error "El identificador no es un objeto" obj))))

      
      (super-call-exp (method-name rands)
                      (let ((args (map (lambda (x) (evaluar-expresion x amb)) rands))
                            (obj (apply-env amb 'self)))
                        (find-method-and-apply method-name (apply-env amb '%super) obj args)))
      )))
;;Evaluar Expresiones Booleanas

(define evaluar-bool-expresion
  (lambda (exp amb)
    (cases bool-expresion exp
      (true-bool-exp () #t)
      
      (false-bool-exp () #f)
      
      (bool-prim-exp (prim args)
       (evaluar-bool-primitiva prim 
                              (map (lambda (x) (evaluar-expresion x amb)) args)))
      
      (bool-oper-exp (op args)
       (evaluar-bool-operacion op 
                              (map (lambda (x) (evaluar-bool-expresion x amb)) args)))
      
      (else (eopl:error "No es una expresión booleana válida:" exp)))))




;;Evaluar Primitivas Booleanas

(define evaluar-bool-primitiva
  (lambda(prim lval)
    (cases bool-primitiva prim
      (mayor-prim ()(operacion-bool-prim lval > #f))
      (mayorigual-prim () (>= (car lval) (cadr lval)))
      (menor-prim () (operacion-bool-prim lval < #f))
      (menorigual-prim () (operacion-bool-prim lval <= #f))
      (is-prim () (operacion-bool-prim lval = #f))
      [else (eopl:error "Operación booleana primitiva no reconocida" prim)]
      )))

(define operacion-bool-prim
  (lambda (lval op term)
    (cond
      [(null? lval) term]
      [else
       (op
        (car lval)
        (operacion-bool-prim (cdr lval) op term))
       ]
      )
    )
  )

;;Evaluar Operaciones Booleanas

(define evaluar-bool-operacion
  (lambda (op args)
    (cases bool-oper op
      (not-bool () (operacion-bool-oper args args not))  ;; Negación (not)
      (and-bool () (operacion-bool-oper args (lambda (x y) (and x y)))) ;; Conjunción (and)
      (or-bool () (operacion-bool-oper args (lambda (x y) (or x y))))  ;; Disyunción (or)
)))

(define operacion-bool-oper
  (lambda (lval op)
    (cond
      [(null? lval) #t]  ;; Si la lista está vacía, devolvemos true para 'and', ya que 'and' no debe fallar si no hay elementos.
      [(= (length lval) 1) (car lval)]  ;; Si solo hay un elemento, devolvemos ese elemento directamente.
      [else
       (op
        (car lval)
        (operacion-bool-oper (cdr lval) op))]  ;; Recursión: aplica la operación entre el primer valor y el resto de la lista.
    )
  )
)


;;Manejo de primitivas
(define evaluar-primitiva
  (lambda (prim lval)
    (cases primitiva prim
      (sum-prim () (operacion-prim lval + 0))
      (minus-prim () (operacion-prim lval - 0))
      (mult-prim () (operacion-prim lval * 1))
      (div-prim () (operacion-prim lval / 1))
      (mod-prim () (if (= (length lval) 2)
                       (modulo (car lval) (cadr lval))
                       (eopl:error "mod-prim requiere exactamente 2 argumentos" lval)))
      (text-prim() (operacion-prim lval string-append "")) ;; Concatenamos con acumulador vacío.
      )
    )
  )


(define operacion-prim
  (lambda (lval op term)
    (cond
       [(null? lval) term]
       [(string? (car lval)) ;; Si es una cadena, aplicamos la concatenación.
        (string-append
         (car lval)
         (operacion-prim (cdr lval) op term))]
      [else
       (op
        (car lval)
        (operacion-prim (cdr lval) op term))
       ]
      )
    )
  )
;; Implementación de object-update! para actualizar campos de objetos
(define object-update!
  (lambda (obj field-id new-value)
    (cases object obj
      (an-object (class-name fields)
        (let ((pos (find-field-position field-id class-name)))
          (if pos
              (begin
                (vector-set! fields pos new-value)
                new-value)
              (eopl:error "Campo no encontrado en el objeto:" field-id)))))))

;; Función auxiliar para encontrar la posición de un campo en una clase
(define find-field-position
  (lambda (field-id class-name)
    (if (eqv? class-name 'object)
        #f
        (let* ((c (lookup-class class-name))
               (field-ids (class->field-ids c))
               (pos (list-find-position field-id field-ids)))
          (if pos
              pos
              (find-field-position field-id (class->super-name c)))))))

;; Función auxiliar para encontrar la posición de un elemento en una lista
(define list-find-position
  (lambda (sym los)
    (let loop ((los los) (pos 0))
      (cond
        ((null? los) #f)
        ((eqv? sym (car los)) pos)
        (else (loop (cdr los) (+ pos 1)))))))


;;Definiciones para los procedimientos
(define-datatype procval procval?
  (closure (lid (list-of symbol?))
           (body expresion?)
           (amb-creation ambiente?)))

;;Referencias

(define-datatype referencia referencia?
  (a-ref (pos number?)
         (vec vector?)))

;;Extractor de referencias
(define deref
  (lambda (ref)
    (primitiva-deref ref)))

(define primitiva-deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;;Asignación/cambio referencias
(define setref!
  (lambda (ref val)
    (primitiva-setref! ref val)))

(define primitiva-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;;Clases


(define the-class-env '())

(define initialize-class-env!
  (lambda ()
    (set! the-class-env '())))

(define add-to-class-env!
  (lambda (class)
    (set! the-class-env (cons class the-class-env))))  

(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))  

(define elaborar-declaraciones-clase!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))

(define elaborate-class-decl!
  (lambda (c-decl)
    (let ((super-name (class-decl->super-name c-decl)))
      (let ((field-ids  (append
                          (class-name->field-ids super-name)
                          (class-decl->field-ids c-decl))))
        (add-to-class-env!
          (a-class
            (class-decl->class-name c-decl)
            super-name
            (length field-ids)
            field-ids
            (roll-up-method-decls
              c-decl super-name field-ids)))))))

(define roll-up-method-decls
  (lambda (c-decl super-name field-ids)
    (map
      (lambda (m-decl)
        (a-method m-decl super-name field-ids))
      (class-decl->method-decls c-decl))))

(define lookup-class                    
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) (eopl:error 'lookup-class
                       "Unknown class ~s" name))
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))



;;Implementación plana: representación del objeto

(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))


;;Objeto es un datatype object
(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (make-vector (class-name->field-length class-name)))))


(define roll-up-field-length
  (lambda (class-name)
    (if (eqv? class-name 'object)
      0
      (+ (roll-up-field-length
           (class-name->super-name class-name))
         (length (class-name->field-ids class-name))))))

;;Implementacion object-lookup (buscar atributo del objeto)
(define object-lookup
  (lambda (obj attr-id)
    ;; Verifica si el objeto tiene el atributo
    (let ((fields (object->fields obj)))  ;; Obtener los campos del objeto
      (cond
        ;; Si el atributo está en los campos del objeto, devuelve su valor
        ((assoc attr-id fields) => cdr)  ;; Si la clave 'attr-id' está en los campos, devuelve el valor.
        ;; Si no se encuentra el atributo, genera un error
        (else (eopl:error "Atributo no encontrado en el objeto" attr-id))))))

;;Aplicación de métodos

(define apply-method
  (lambda (method self args)               
    (let ((ids (method->ids method))
          (body (method->body method))
          (super-name (method->super-name method))
          (field-ids (method->field-ids method))       
          (fields (object->fields self)))
      (evaluar-expresion body
                         (ambiente-extendido
                          (cons '%super (cons 'self ids))
                          (cons super-name (cons self args))
                          (ambiente-extendido-ref field-ids fields (ambiente-vacio)))))))
          
 
(define lookup-method                   
  (lambda (m-name methods)
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))
      

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let loop ((host-name host-name))
      (if (eqv? host-name 'object)
          (eopl:error 'find-method-and-apply
            "No method for name ~s" m-name)
          (let ((method (lookup-method m-name 
                          (class-name->methods host-name))))
            (if (method? method)
                (apply-method method host-name self args)
                (loop (class-name->super-name host-name))))))))


(define roll-up-field-ids               
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (append
        (roll-up-field-ids
          (class-name->super-name class-name))
        (class-name->field-ids class-name)))))

;;Metodos


(define-datatype method method?
  (a-method
   (method-decl method-decl?)
   (super-name symbol?)
   (field-ids (list-of symbol?))))

;;evaluacion de metodos

(define evaluar-method
  (lambda (method amb)
    (let ((method-name (method-decl->method-name method))
          (params (method-decl->ids method))
          (body (method-decl->body method)))
      ;; Aquí puedes realizar las operaciones necesarias
      (list method-name params body)))) 


;;Clases

(define-datatype class class?
  (a-class
    (class-name symbol?)  
    (super-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)))

;;Esto define lo que contiene
(define method-environment? (list-of method?))



;;Esta función nos determina en donde termina en el vector de campos en cada clase
(define rib-find-position
  (lambda (name symbols)
    (list-find-last-position name symbols)))
   
    
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))



;;;;;;;;;;;;;;;; Selectores para las declaraciones ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases declaracion-clase c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases declaracion-clase c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases declaracion-clase c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases declaracion-clase c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))
        

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define class->class-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        class-name))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        super-name))))

(define class->field-length
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-length))))

(define class->field-ids
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-ids))))

(define class->methods
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        methods))))

(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
        class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
        fields))))

(define object->class-decl
  (lambda (obj)
    (lookup-class (object->class-name obj))))

(define object->field-ids
  (lambda (object)
    (class->field-ids
      (object->class-decl object))))

(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->field-ids (lookup-class class-name)))))

(define class-name->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->methods (lookup-class class-name)))))

(define class-name->field-length
  (lambda (class-name)
    (if (eqv? class-name 'object)
        0
        (class->field-length (lookup-class class-name)))))

(define method->method-decl
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) meth-decl))))

(define method->super-name
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) super-name))))

(define method->field-ids
  (lambda (meth)
    (cases method meth
      (a-method (method-decl super-name field-ids) field-ids))))

(define method->method-name
  (lambda (method)
    (method-decl->method-name (method->method-decl method))))

(define method->body
  (lambda (method)
    (method-decl->body (method->method-decl method))))

(define method->ids
  (lambda (method)
    (method-decl->ids (method->method-decl method))))


;;Interpretador
(define interpretador
  (sllgen:make-rep-loop "<3" evaluar-programa
                        (sllgen:make-stream-parser
                         especificacion-lexica especificacion-gramatical)))





(interpretador)