#lang racket

(require environment)

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
