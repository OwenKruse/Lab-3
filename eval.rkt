#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;;
;; Lab #4
;;
;; Owen Kruse
;; W01600488
;;
;; The purpose of this program is to
;; Evaluate functions given arguments and an enviroment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require racket/trace)
(provide evaluate
         lookup
         special-form?
         evaluate-if
         evaluate-cond
         evaluate-let          
         evaluate-special-form)

;; Evaluate
;; Recursively evaluates expr in env
(define evaluate
  (lambda (expr env)
    (cond
      [(number? expr) expr]
      [(special-form? expr) (evaluate-special-form expr env)]
      [(symbol? expr) (lookup expr env)]
      [(pair? expr)
       (let* ([evaluated (map (lambda (e) (evaluate e env)) expr)]
              [operator (car evaluated)]
              [operands (cdr evaluated)])
         (unless (procedure? operator)
           (error '"Not a procedure..."))
         ((lambda (proc args) (apply proc args)) operator operands))]
      [else (error '"Something bad happened in evaluate...")])))




;; Lookup
;; Returns the value bound to sym in env, or an error
(define lookup
  (lambda (sym env)
    (unless (symbol? sym)
      (error '"Not a symbol buddy."))
    (cond
      [(null? env) (error sym "That isn't in the enviroment... Are you sure you added it?")]
      [else
       (let ([key (car env)])
         (if (eq? sym (car key))
             (cadr key)
             ((lambda (rest-env) (lookup sym rest-env)) (cdr env))))])))

;; Special Form?
;; Recognise the form and return a bool.
(define special-form?
  (lambda (expr)
    (and (pair? expr)
         (let ([tag (car expr)])
           (or (eq? tag 'if)
               (eq? tag 'cond)
               (eq? tag 'let))))) )        


;; Evaluate the special form.       
(define evaluate-special-form
  (lambda (form env)
    (let ([tag (car form)]
          [body (cdr form)])
      (cond
        [(eq? tag 'if)   (evaluate-if   body env)]
        [(eq? tag 'cond) (evaluate-cond body env)]
        [(eq? tag 'let)  (evaluate-let  body env)]
        [else (error '"That isn't a part of the special form..." tag)]))))

; Evaluate if
(define evaluate-if
  (lambda (pieces env)
    (let ([test-expr  (car   pieces)]
          [then-expr  (cadr  pieces)]
          [else-expr  (caddr pieces)])
      (if (evaluate test-expr env)
          (evaluate then-expr env)
          (evaluate else-expr env)))))


(define evaluate-cond
  (lambda (clauses env)
    (cond
      [(null? clauses)
       (error '"The clauses are null.")]
      [else
       (let* ([clause (car clauses)]
              [test   (car clause)]
              [body   (cdr clause)])
         (cond
           [(eq? test 'else)
            (if (null? (cdr clauses))
                (evaluate-seq body env)
                (error '"Else has to be last"))]
           [(evaluate test env) (evaluate-seq body env)]
           [else (evaluate-cond (cdr clauses) env)]))])))

;; evaluate-let
;; Binds each symbol in bindings‑list to the value of its expression
(define evaluate-let
  (lambda (pieces env)
    (let* ([bindings (car  pieces)]          
           [body     (cdr pieces)])          
      (define symbols (map car  bindings))
      (define exprs   (map cadr bindings))

      (define values (map (lambda (e) (evaluate e env)) exprs))

      (define new-env (append (map list symbols values) env))

      (evaluate-seq body new-env))))      


;; helper method to evaluate a sequence of expressions, return last value
(define evaluate-seq
  (lambda (exprs env)
    (cond
      [(null? exprs) (void)]
      [(null? (cdr exprs)) (evaluate (car exprs) env)]
      [else (evaluate (car exprs) env)
            (evaluate-seq (cdr exprs) env)])))

(let ((x 10))
(+ (let ((x (+ x x))) (+ x x)) x))