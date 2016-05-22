; evaluator.scm
; (load "./state.scm")
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
        (v/t 'number val))))

  ; string
  (define (evaluate-string tree env)
    (and
      (eq? (tree 'type) 'string)
      (new-state tree env)))

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
  (define (evaluate-expr tree env)
    (define (execute-proc proc-t args env)
      (let* ((proc (evaluate proc-t env)))
        (cond ((proc 'error?) proc)
              ((eq? 'syntax (proc 'type))  ((proc 'value) args env))
              ((eq? 'closure (proc 'type)) ((proc 'value) args env))
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
    (evaluate-number tree env)
    (evaluate-string tree env)
    (evaluate-symbol tree env)
    (evaluate-quote tree env)
    (evaluate-expr tree env)
    (v/t 'error "undefined")))
