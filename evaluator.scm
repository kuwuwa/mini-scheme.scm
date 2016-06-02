; evaluator.scm
(load "./result.scm")


(define (evaluate tree env)
  ; parameter: syntax tree & environment
  ; return: the result of evaluation of the syntax tree (& possibly side effects)

  ; empty
  (define (evaluate-empty tree env)
    (and
      (eq? (tree 'type) 'empty)
      tree))

  ; bool
  (define (evaluate-bool tree env)
    (and
      (eq? (tree 'type) 'bool)
      tree))

  (define (evaluate-char tree env)
    (and
      (eq? (tree 'type) 'char)
      tree))

  ; number
  (define (evaluate-number tree env)
    (and
      (eq? (tree 'type) 'number)
      tree))

  ; string
  (define (evaluate-string tree env)
    (and
      (eq? (tree 'type) 'string)
      tree))

  ; symbol
  (define (evaluate-symbol tree env)
    (and
      (eq? (tree 'type) 'symbol)
      ((env 'find) (tree 'value))))

  ; quote
  (define (evaluate-quote tree env)
    (and
      (eq? (tree 'type) 'quote)
      (tree 'value)))

  ; expr
  (define (evaluate-list tree  env)
    (define (evaluate-args _args env)
      (let loop ((args _args))
        (if (null? args)
          (v/t 'empty '())
          (let ((result (evaluate (car args) env)))
            (if (result 'error?)
              result
              (let ((rest (loop (cdr args))))
                (cond ((rest 'error?) rest)
                      ((eq? 'empty (rest 'type))
                       (v/t 'args (cons result '())))
                      (else
                        (v/t 'args (cons result (rest 'value)))))))))))

    (define (execute-proc proc-t args env)
      (let* ((proc (evaluate proc-t env)))
        (cond ((proc 'error?) proc)
              ((eq? 'syntax (proc 'type))  ((proc 'value) args env))
              ((eq? 'closure (proc 'type))
                  (let ((vals (evaluate-args args env)))
                    (if (vals 'error?)
                      vals
                      ((proc 'value) (vals 'value) env))))
              (else (v/t 'error "invalid application")))))

    ; (let ((po
    (and
      (eq? (tree 'type) 'list)
      (let ((proc (car (tree 'value)))
            (args (cdr (tree 'value))))
        (execute-proc proc args env)))
    ; )) (display ((po 'value) 'value)) po)
    )

  (or 
    (evaluate-empty tree env)
    (evaluate-bool tree env)
    (evaluate-char tree env)
    (evaluate-number tree env)
    (evaluate-string tree env)
    (evaluate-symbol tree env)
    (evaluate-quote tree env)
    (evaluate-list tree env)
    (v/t 'error "undefined")))
