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
      tree))

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

  ; label
  (define (evaluate-label tree env)
    (and
      (eq? (tree 'type) 'label)
      (new-state ((env 'find) (tree 'value)) env)))

  ; expr
  (define (evaluate-expr tree env)
    (define (execute-proc proc-t args env)
      (let* ((next-state (evaluate proc-t env))
             (proc (next-state 'value))
             (next-env (next-state 'env)))
        (cond ((proc 'error?) next-state)
              ((eq? 'syntax (proc 'type))  ((proc 'value) args next-env))
              ((eq? 'closure (proc 'type)) ((proc 'value) args next-env))
              (else (new-state (v/t 'error "invalid application") next-env)))))

    (and
      (eq? (tree 'type) 'p-expr)
      (let ((proc (car (tree 'value)))
            (args (cdr (tree 'value))))
        (execute-proc proc args env))))

  (or 
    (evaluate-empty tree env)
    (evaluate-bool tree env)
    (evaluate-number tree env)
    (evaluate-string tree env)
    (evaluate-label tree env)
    (evaluate-expr tree env)
    (new-state (v/t 'error "undefined") env)))
