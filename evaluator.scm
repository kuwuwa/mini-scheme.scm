; evaluator.scm

(load "./value-with-type.scm")
(load "./list-utils.scm")


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

  ; list
  (define (evaluate-list tree env)
    (define (evaluate-args _args env)
      (let loop ((args _args))
        (cond ((w-null? args) (v/t 'empty ()))
              ((not (eq? 'list (args 'type))) (v/t 'error
                                                   "improper list is not available"))
              (else
                (let ((result (evaluate (w-car args) env)))
                  (if (result 'error?)
                    result
                    (let ((rest (loop (w-cdr args))))
                      (cond ((rest 'error?) rest)
                            ((eq? 'empty (rest 'type))
                               (v/t 'list (list result)))
                            (else (v/t 'list (cons result (rest 'value))))))))))))

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

    (and
      (eq? (tree 'type) 'list)
      (let ((proc (w-car tree))
            (args (w-cdr tree)))
        (execute-proc proc args env))))

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


(define (evaluate-bindings tree env)
  (define invalid-syntax (v/t 'error "invalid syntax"))

  (let loop ((bindings tree))
    (if (null? bindings)
      (v/t 'bindings '())
      (let ((cell (car bindings)))
        (if (or (not (w-list? cell))
                (not (= (w-length cell) 2)))
          invalid-syntax
          (let ((label (w-car cell))
                (value (evaluate (w-cadr cell) env)))
            (cond ((not (eq? 'symbol (label 'type))) invalid-syntax)
                  ((value 'error?) value)
                  (else
                    (let ((binding (cons (label 'value) value))
                          (rest (loop (cdr bindings))))
                      (if (rest 'error?)
                        rest
                        (v/t 'bindings (cons binding (rest 'value)))))))))))))


(define (evaluate-body _body env)
  (let loop ((body _body)
             (return-value (v/t 'undef '())))
    (if (null? body)
      return-value
      (let* ((term (car body))
             (rest (cdr body)))
        (let ((value (evaluate term env)))
          (if (value 'error?)
            value
            (loop rest value)))))))


(define (check-body _body)
  (let loop ((body _body)
             (prev-type 'define))
    (if (null? body)
      (if (eq? prev-type 'define)
        (v/t 'error "invalid body form")
        (v/t 'ok "no problem :)"))
      (let ((term (car body))
            (rest (cdr body)))
        (if (and (not (eq? prev-type 'define)) (eq? (term 'type) 'define))
          (v/t 'error "invalid body form")
          (loop rest (term 'type)))))))
