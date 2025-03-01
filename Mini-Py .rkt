#lang eopl
; Sergio Escudero Tabares - 2040801
; Jose Miguel Becerra Casierra - 2043246
; Natalia Andrea Marín Hernandez - 2041622
; Esteban Andres Hernandez - 2042817
; Juan Esteban Brand Tovar - 2043291
; Link github: https://github.com/JoseBecerra02/Proyecto-final-FLP

;<programa> :=  <expresion>
;               un-programa (exp)
;<expresion> := <numero>
;               numero-lit (num)
;            := "\""<texto> "\""
;               texto-lit (txt)
;            := <boolExp>
;               bool-lit (bool)
;            := <identificador>
;               var-exp (id)
;            := (<expresion> <primitiva-binaria> <expresion>)
;               primapp-bin-exp (exp1 prim-binaria exp2)
;            := <primitiva-unaria> (<expresion>)
;               primapp-un-exp (prim-unaria exp)
;<primitiva-binaria> :=  + (primitiva-suma)
;                    :=  ~ (primitiva-resta)
;                    :=  / (primitiva-div)
;                    :=  * (primitiva-multi)
;                    :=  concat (primitiva-concat)
;
;<primitiva-unaria>:=  longitud (primitiva-longitud)
;                  :=  add1 (primitiva-add1)
;                  :=  sub1 (primitiva-sub1)

;Especificación Léxica

(define scanner-spec-simple-interpreter
  
'((white-sp (whitespace) skip)
  
  (comment (";" (arbno (not #\newline))) skip)
  
  (identifier (letter (arbno (or letter digit))) symbol)
  
  (number (digit (arbno digit)) number)
  
  (number ("-" digit (arbno digit)) number)
  
  (number (digit (arbno digit) "." digit (arbno digit)) number)
  
  (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
  
  (text ("'" (arbno (or digit letter whitespace)) "'") string)

  (base ("x" (arbno whitespace) digit (arbno digit)) string)  
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter

  '((program (expression) a-program)
    
    ;;Declarar variables y constantes
    (expression ("var" (separated-list identifier "=" expression ",") "in" expression) variable-exp)
    (expression ("const" (separated-list identifier "=" expression ",") "in" expression) constante-exp)

    
    ;;Identificador 
    (expression (identifier) var-exp)


    ;;Valores
    (expression (number) lit-exp)
    (expression (text) txt-exp)


    ;;Hexadecimal
    (expression (base "(" (arbno expression) ")") hexa-exp)

    ;; ASIGNACION DE VARIABLES
    (expression ("set" identifier "=" expression) set-exp)

    ;;Booleanos
    (expression (expr-bool) boolean-exp)

    (expr-bool (boolean) booleano)
    (expr-bool (oper-un-bool "(" expression")") una-bool-exp)
    (expr-bool (oper-bin-bool "(" expression "," expression")") bin-bool-exp)
    (expr-bool (pred-prim "(" expression "," expression ")") pred-bool-exp)

    ;;Print
    (expression ("print("expression")") print-exp)
    
    (boolean ("true") true-boolean)
    (boolean ("false") false-boolean)

    (oper-un-bool ("not") not-bool-prim)

    (oper-bin-bool ("or")  or-bool-prim)
    (oper-bin-bool ("and") and-bool-prim)

    (pred-prim ("<") less-prim)
    (pred-prim (">") more-prim)
    (pred-prim ("==") equal-prim)
    (pred-prim ("<>") unequal-prim)
    (pred-prim (">=") more-equal-prim)
    (pred-prim ("<=") less-equal-prim)


     ;;Lista
    (expression ("[" (separated-list expression ",") "]") list-exp)


    ;;Tupla
    (expression ("tupla" "[" expression "," expression "]") tupla-exp)


    ;;Registro
    (expression ("{" (separated-list identifier "=" expression ",") "}")  registro-exp)


    ;;Prima
    (expression ("(" expression primitive expression ")") primapp-bin-exp)
    (expression (primitive-un "(" expression ")") primapp-un-exp)

    
    ;;Primitivas aritmeticas para enteros
    (primitive ("+") primitiva-suma)
    (primitive ("~") primitiva-resta)
    (primitive ("*") primitiva-multi)
    (primitive ("/") primitiva-div)
    (primitive-un ("add1") primitiva-add1)
    (primitive-un ("sub1") primitiva-sub1)

    ;;Primitivas sobre cadenas
    (primitive-un ("longitud") primitiva-longitud)
    (primitive ("concat") primitiva-concat)
    


    ;;Primitivas sobre listas
    (primitive-un ("vacio?") primitiva-vacio?)
    (expression ("vacio") primitiva-vacio)
    (expression ("crear-lista" "(" (separated-list expression ",") ")") primitiva-crear-lista)
    (primitive-un ("lista?") primitiva-lista?)
    (primitive-un ("cabeza") primitiva-cabeza)
    (primitive-un ("cola") primitiva-cola)
    (primitive ("append") primitiva-append)
    (expression ("ref-list" "(" expression "," number ")") primitiva-ref-list)
    (primitive-un ("set-list") primitiva-set-list)

    ;;Primitivas sobre tuplas
    (expression ("crear-tupla" "(" expression "," expression ")") primitiva-crear-tupla)
    (primitive-un ("tupla?") primitiva-tupla?)
    (expression ("ref-tupla" "(" expression "," number ")") primitiva-ref-tuple)
    

    ;;Estructura begin
    (expression ("begin" "{" expression (arbno "," expression) "}" "end")  begin-exp)
    
    
    ;;Estructura if 
    (expression ("if" expression "then" expression "[" "else" expression "]" "end") condicional-exp)
    
    
    ;;Estructura while 
    (expression ("while" expression "do" expression "done") while-exp)
    
    
    ;;Estructura for
    (expression ("for" identifier "=" expression to-down-exp expression "do" expression "done") for-exp)
    (to-down-exp ("to") to-exp)
    (to-down-exp ("down") down-exp)
    
    )
  )

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> " (lambda (pgm) (eval-program  pgm))(sllgen:make-stream-parser scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env)))
      )
    )
  )

(define init-env
  (lambda ()
    (extend-env '() '()
     (empty-env)
     )
    )
  )

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp

      (var-exp (id) (apply-env env id))
      
      (variable-exp (vars vals body) (creacion-variable vars vals body env))
      
      (constante-exp (vars vals body) (creacion-constante vars vals body env))
      
      (txt-exp (text) (creacion-texto text env))
      
      (lit-exp (num) num)

      (hexa-exp (base lista) (creacion-hexa base lista env))
      
      (boolean-exp (expres-bool) (creacion-bool expres-bool env))

      (print-exp (exp) (display  (eval-expression exp env)))
       
      (list-exp (list) (creacion-listas list env))

      (tupla-exp (exp1 exp2) (creacion-tuplas exp1 exp2 env))

      (registro-exp (identificadores registros) (creacion-registros identificadores registros env))

      (primapp-bin-exp (num1 prim num2) (apply-primitive prim (cons (eval-rand num1 env) (cons (eval-rand num2 env) '()))))
      
      (primapp-un-exp (prim num) (apply-primitive-un prim (eval-rand num env)))

      (primitiva-vacio () '())

      (primitiva-crear-lista (num) (creacion-listas num env))

      (primitiva-ref-list (num index) (list-set-aux (eval-expression num env) index))

      (primitiva-crear-tupla (exp1 exp2) (creacion-tuplas exp1 exp2 env))

      (primitiva-ref-tuple (num index) (tupla-set-aux (eval-expression num env) index))
      
      (condicional-exp (test-exp true-exp false-exp) (creacion-if test-exp true-exp false-exp env))

      (begin-exp (exp exps) (creacion-begin exp exps env))

      (while-exp (exp-cond exp-do) (creacion-while exp-cond exp-do env))
      
      (for-exp (ident exp-cond to-down-exp exp-cond-final exp-do) (creacion-for ident exp-cond to-down-exp exp-cond-final exp-do env))

      (set-exp (id exp) (creacion-set id exp env))
      
      )

    )

  )
  
  
; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)

(define prueba
  (lambda (ides rando)
    rando))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim num)
    (cases primitive prim
      (primitiva-suma () (+ (car num) (cadr num)))
      (primitiva-resta () (- (car num) (cadr num)))
      (primitiva-multi () (* (car num) (cadr num)))
      (primitiva-div () (/ (car num) (cadr num)))
      (primitiva-concat () (string-append (car num)(cadr num)))
      (primitiva-append () (append (car num) (cadr num)))
      )
    ))
    
(define apply-primitive-un
  (lambda (prim num)
    (cases primitive-un prim
      (primitiva-longitud () (string-length num))
      (primitiva-add1 () (+ num 1))
      (primitiva-sub1 () (- num 1))
      (primitiva-vacio? () (null? num))
      (primitiva-lista? () (list? num))
      (primitiva-cabeza () (car num))
      (primitiva-cola () (cdr num))
      (primitiva-set-list () (set-list num))
      (primitiva-tupla? () (pair? num))
     ))
    )


;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (vars (list-of variable?))
                       (vec vector?)
                       (env environment?))
  )

(define scheme-value? (lambda (v) #t))

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expression?)
   (env environment?)
   )

  )

(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (eval-expression body (extend-env ids args env))))))

(define-datatype variable variable?
  (mutable (id symbol?))
  (inmutable (id symbol?))
)

(define-datatype target target?
  (indirect-target (ref  target-ref-directo? ))
)
;; Crea una referencia, pos es la posicion de la referencia en el vector
(define-datatype reference reference?
  (refere (pos integer?)
         (vec vector?)
         (mutable symbol?)
        )
)

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (vars vals env)
    (extended-env-record vars (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record (map (lambda (id) (mutable id))proc-names) vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (cerradura ids body env))
            )
            (numsCien len) idss bodies
          )
          env
      )
     )
   )
  )
)

(define apply-env
  (lambda (env sym)
    (ref-val (apply-env-ref env sym))
  )
)

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define true?
  (lambda (x)
    (equal? x 'true)
  )
)

(define creacion-variable
  (lambda (vars vals body env)
    (eval-expression body (extend-env (map (lambda (var) (mutable var)) vars )
                                      (creacion-listas vals env) env) )
  )
  )

(define creacion-constante
  (lambda (vars vals body env)
    (eval-expression body (extend-env (map (lambda (var) (inmutable var))vars)
                                      (creacion-listas vals env) env))
  )
  )

(define creacion-texto
  (lambda (txt env)
    (apply string-append (reverse (cdr (reverse (cdr (map string (string->list txt)))))))
    )
  )


(define creacion-hexa
  (lambda (base lista env)
    (list (creacion-listas lista env) 'in 'base (string->symbol (remove-spaces (string->list base))))
    )
  )


(define remove-spaces
  (lambda (l)
    (if  (null? l)
         ""
        (if (eqv? (car l) #\space)
        (remove-spaces (cdr l))
        (string-append (make-string 1 (car l)) (remove-spaces (cdr l)))
        )
     )
    )
  )


(define creacion-listas
  (lambda (expre env)
    (cond
      ((null? expre) empty)
      (else
       (cons (eval-expression (car expre) env) (creacion-listas (cdr expre) env))
       )
      )
    )
  )


(define creacion-tuplas
  (lambda (expre1 expre2 env)
    (cons (eval-expression expre1 env) (eval-expression expre2 env))
    )
  )


(define creacion-registros
  (lambda (identi expre env)
    (cond
      ((null? expre) empty)
      (else
       (cons (append (list (car identi) '=) (cons (eval-expression (car expre) env) empty)) (creacion-registros (cdr identi) (cdr expre) env))
       )
      )
    )
  )

(define creacion-bool
  (lambda (expression env)
    (cases  expr-bool expression
      (booleano (bool)
                (cases boolean bool
                  (true-boolean () 'true)
                  (false-boolean () 'false)
                  )
                )
      (una-bool-exp (unary-prim bool-exp)
                    (cases oper-un-bool unary-prim
                      (not-bool-prim () (if (or (eqv? (eval-expression bool-exp env) 'true) (eqv? (eval-expression bool-exp env) 'false)) (if (true? (eval-expression bool-exp env)) 'false 'true) (eopl:error 'apply-env-ref "Estas comparando elementos no booleanos"))))
                    )
      (bin-bool-exp (pred first-expr second-expr)
                    (cases oper-bin-bool pred
                      (and-bool-prim () (if (or (eqv? (eval-expression first-expr env) 'true) (eqv? (eval-expression first-expr env) 'false)) (if (and (true? (eval-expression first-expr env)) (true? (eval-expression second-expr env))) 'true 'false) (eopl:error 'apply-env-ref "Estas comparando elementos no booleanos")))
                      (or-bool-prim () (if (or (eqv? (eval-expression first-expr env) 'true) (eqv? (eval-expression first-expr env) 'false)) (if (or  (true? (eval-expression first-expr env)) (true? (eval-expression second-expr env))) 'true 'false) (eopl:error 'apply-env-ref "Estas comparando elementos no booleanos")))
                      )
                    )
      (pred-bool-exp (pred first-expr second-expr)
                     (cases pred-prim pred
                       (less-prim () (if (< (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       (more-prim () (if (> (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       (equal-prim () (if (equal? (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       (unequal-prim () (if (not (equal? (eval-expression first-expr env) (eval-expression second-expr env))) 'true 'false))
                       (more-equal-prim () (if (>= (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       (less-equal-prim () (if (<= (eval-expression first-expr env) (eval-expression second-expr env)) 'true 'false))
                       )
                     )
      )
    )
  )


(define creacion-begin
  (lambda (exp exps env)
     (if (null? exps) (eval-expression exp env) (begin (eval-expression exp env) (creacion-begin (car exps) (cdr exps) env)))
    )
  )


(define creacion-if
  (lambda (test-exp true-exp false-exp env)
    (if (or (equal? 'true (eval-expression test-exp env)) (equal? 'false (eval-expression test-exp env))) (if (true? (eval-expression test-exp env)) (eval-expression true-exp env) (eval-expression false-exp env)) (eopl:error "No binding for boolean"))
    )
  )

(define creacion-while
  (lambda (exp-cond exp-do env)
    (if (true? (eval-expression exp-cond env)) (begin (eval-expression exp-do env) (creacion-while exp-cond exp-do env)) 'done)
    )
  )

(define creacion-for
  (lambda (ident exp-cond exp-cond-final exp-do env)
    (0)
    )
  )
(define creacion-set
  (lambda (id exp env)
     (begin
       (if  (mutable? id env)
            (cases reference (apply-env-ref env id)
              (refere (pos vals mut)  
                  (if (target? (vector-ref vals pos))
                    (cases target (vector-ref vals pos) 
                      (indirect-target (ref)  (cambiarRef! ref (eval-expression exp env)))
                    )
                  (cambiarRef! (apply-env-ref env id) (eval-expression exp env))
                )
              )
            )      
            (eopl:error 'set-exp "Estas tratando de mdificar la constante ~s" id)
       )
       1
     )
  )
)

;;Aux
(define list-set-aux
  (lambda (L n)
    (cond
      [(= 0 n) (car L)]
      [else (list-set-aux (cdr L) (- n 1))]
      )
    )
  )
(define tupla-set-aux
  (lambda (L n)
    (cond
      [(= 0 n) (car L)]
      [(= 1 n) (cdr L)]
      [else (eopl:error 'apply-env-ref "Estas accediendo a una posicion que no existe en la tupla")]
      )
    )
  )


(define (set-list num)
  (cond
    ((null? num) num)
    ((miembro? (car num) (cdr num)) (set-list (cdr num)))
    (else (cons (car num) (set-list (cdr num)))))
  )

(define (miembro? x Lista1)
  (if (null? Lista1)
      #f
      (if (eq? x (car Lista1))
          #t
          (miembro? x (cdr Lista1)))))

(define mutable?
  (lambda (sym env)
      (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-en-vars sym vars)))
                             (if (number? pos)
                                 (cases variable (list-ref vars pos)
                                   (mutable (id) #t)
                                   (inmutable (id) #f)
                                 )
                                 (mutable? sym env)
                             )
        )
      )
  )
 )
)
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No existe la variable ~s" sym))
      (extended-env-record (vars vals env)
                           (let ((pos (encontrar-en-vars sym vars)) (mut (buscar-muta sym vars 0)) )
                             (if (and (number? pos) (symbol? mut) )
                                 (refere pos vals mut)
                                 (apply-env-ref env sym)))))))
(define target-ref-directo?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (refere (pos vec mut)
                  ( if  (not (reference?  (vector-ref vec pos) ) )  #t #f ))))))
(define cambiarRef!
  (lambda (ref val)
    (if (target? ref)
        (cases target ref
          (indirect-target (valorRef) (primitive-cambiaRef! valorRef val))
        )
        (primitive-cambiaRef! ref val)
    )
  )
)
(define primitive-cambiaRef!
     (lambda (ref val)
       (cases reference ref
         (refere (pos vec mut) (vector-set! vec pos val))
       )
     )
    )

;función que retorna una lista de los números desde 0 hasta end
(define numsCien
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;; ENCONTRAR EL SIMBOLO EN UNA LISTA DE VARIABLES
(define encontrar-en-vars
  (lambda (sym vars)
    (buscar-en-vars sym vars 0)
  )
)
;; AUX SEARCH SIMBOLO EN LISTA DE VARIABLES
(define buscar-en-vars
  (lambda (sym vars pos)
    (cond
      ( (null? vars) #f)
      ( (equal? sym (id-variable (car vars))) pos )
      ( else (buscar-en-vars sym (cdr vars) (+ pos 1)) )
    )
  )
)

;; RETORNA SI LA VARIABLE ES MUTABLE O INMUTABLE
(define buscar-muta
  (lambda (sym vars pos)
    (cond
      ( (null? vars) #f)
      ( (equal? sym (id-variable (car vars))) (cases variable (car vars) (mutable (id) 'M) (inmutable (id) 'I) )  )
      ( else (buscar-muta sym (cdr vars) (+ pos 1)) )
    )
  )
)

;;deref retorna el valor de la referencia en el vector
(define ref-val
  (lambda (ref)
    (cases reference ref
      (refere (pos vals mut) 
        (if (target? (vector-ref vals pos))
           (cases target (vector-ref vals pos)
              (indirect-target (valorRe) (primitive-refval valorRe))
           )
          (primitive-refval ref)
        )
      )
    )       
  )
)
(define primitive-refval
  (lambda (ref)
    (cases reference ref
      (refere (pos vec mut)
             (vector-ref vec pos)
      )
    )
  )
)

;; RETORNAR EL SIMBOLO DE LA VARIABLE
(define id-variable
  (lambda (var)
    (cases variable var
      (mutable (id) id)                             
      (inmutable (id) id)
    )
  )
)
(interpretador)
