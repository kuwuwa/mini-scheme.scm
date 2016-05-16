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
      (v/t 'char (string-ref (tree 'value) 0))))

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
      tree))

  ; label
  (define (evaluate-label tree env)
    (and
      (eq? (tree 'type) 'label)
      ((env 'find) (tree 'value))))

  ; expr
  (define (evaluate-expr tree env)
    (define (execute-proc _proc args env)
      (let ((proc (evaluate _proc env)))
        (cond ((proc 'error?) proc)
              ((eq? 'syntax (proc 'type))  ((proc 'value) args env))
              ((eq? 'closure (proc 'type)) ((proc 'value) args env))
              (else (v/t 'error "invalid application")))))

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
    (v/t 'error "undefined")))



