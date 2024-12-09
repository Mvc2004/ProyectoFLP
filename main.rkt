(require environment)
(require evaluator)
(require objects)

(define global-env (make-env))

;; Función principal para ejecutar expresiones
(define (run-program expr)
  (eval expr global-env))

;; Ejemplo de ejecución
(define test-expression '(let ((x 10)) (+ x 5)))
(display (run-program test-expression))
