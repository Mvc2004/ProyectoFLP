#lang racket

;; Crear un objeto
(define (make-object fields methods)
  (lambda (msg)
    (cond [(equal? msg 'get) fields]  ;; Retorna los campos del objeto
          [(equal? msg 'method) methods]  ;; Retorna los métodos
          [else (error "Unknown message" msg)])))  ;; Mensaje no reconocido

;; Ejemplo de un método
(define object-example
  (make-object '(name "Objeto") '(get-name (lambda () "Nombre del Objeto"))))

;; Llamar a un método
((object-example 'method))  ;; Debería devolver los métodos
