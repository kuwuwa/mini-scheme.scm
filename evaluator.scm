; evaluator.scm
(load "./state.scm")
(load "./result.scm")


(define (evaluate tree env)
  ; parameter: syntax tree & environment
  ; return: the result of evaluation of the syntax tree (& possibly side effects)

  ; empty
  (define (evaluate-empty tree env)
    (and
      (eq? (tree 'type) 'empty)
      (new-state tree env)))

  ; bool
  (define (evaluate-bool tree env)
    (and
      (eq? (tree 'type) 'bool)
      (new-state tree env)))

  (define (evaluate-char tree env)
    (and
      (eq? (tree 'type) 'char)
      (new-state tree env)))

  ; number
  (define (evaluate-number tree env)
    (define (string->integer str)
      (define (scan ind acc)
        (if (>= ind (string-length str))
          acc
          (scan (+ ind 1)
                (+ (* 10 acc)
                   (- (char->integer (string-ref str ind)) 48)))))

      (scan 0 0))

    (and
      (eq? (tree 'type) 'number)
      (let ((val (string->integer (tree 'value))))
        (new-state (v/t 'number val) env))))

  ; string
  (define (evaluate-string tree env)
    (and
      (eq? (tree 'type) 'string)
      (new-state tree env)))

  ; symbol
  (define (evaluate-symbol tree env)
    (and
      (eq? (tree 'type) 'symbol)
      (new-state ((env 'find) (tree 'value)) env)))

  ; expr
  (define (evaluate-expr tree env)
    (define (execute-proc proc-t args env)
      (let* ((proc-st (evaluate proc-t env))
             (proc (proc-st 'value))
             (next-env (proc-st 'env)))
        (cond ((proc 'error?) proc)
              ((eq? 'syntax (proc 'type))  ((proc 'value) args next-env))
              ((eq? 'closure (proc 'type)) ((proc 'value) args next-env))
              (else (new-state (v/t 'error "invalid application") next-env)))))

    ; (let ((po
    (and
      (eq? (tree 'type) 'p-expr)
      (let ((proc (car (tree 'value)))
            (args (cdr (tree 'value))))
        (execute-proc proc args env)))
    ; )) (display ((po 'value) 'value)) po)
    )

  (or 
    (evaluate-empty tree env)
    (evaluate-bool tree env)
    (evaluate-number tree env)
    (evaluate-string tree env)
    (evaluate-symbol tree env)
    (evaluate-expr tree env)
    (new-state (v/t 'error "undefined") env)))
