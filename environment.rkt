#lang racket

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

