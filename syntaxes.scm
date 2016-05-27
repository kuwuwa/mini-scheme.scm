; syntaxes.scm
(load "./evaluator.scm")
(load "./frame.scm")
(load "./result.scm")

; Exp ::= Const                                   定数
;       | Id                                      変数
;       | (lambda Arg Body)                       λ 抽象
;       | (Exp Exp*)                              関数適用
;       | (quote S-Exp)                           クオート (注1)
;       | (set! Id Exp)                           代入
;       | (let [Id] Bindings Body)                let
;       | (let* Bindings Body)                    let* (注4)
;       | (letrec Bindings Body)                  letrec
;       | (if Exp Exp [Exp])                      条件式(if)
;       | (cond (Exp Exp+)* [(else Exp+)])        条件式(cond) (注5)
;       | (and Exp*)                              条件式(and)
;       | (or Exp*)                               条件式(or)
;       | (begin Exp*)                            逐次実行
;       | (do ((Id Exp Exp)*) (Exp Exp*) Body)    繰り返し


(define (syntax-lambda args env)
  (define err-malformed (v/t 'error "malformed lambda"))

  (if (< (length args) 1)
    err-malformed
    (let* ((arg0 (car args)))
      (if (or (not (memq (arg0 'type) '(list empty)))
              (not (check-all (lambda (t) (eq? 'symbol (t 'type)))
                              (arg0 'value))))
        err-malformed
        (let* ((symbols (map (lambda (p) (p 'value)) (arg0 'value)))
               (closure (new-closure symbols (cdr args) env)))
          closure)))))


(define (syntax-quote args env)
  (define err-malformed (v/t 'error "malformed quote"))

  (if (not (= (length args) 1))
    err-malformed
    (let ((tree (car args)))
      tree)))


(define (syntax-set! args env)
  (define err-malformed (v/t 'error "malformed set!"))

  (if (or (not (= (length args) 2))
          (not (eq? 'symbol ((car args) 'type))))
    err-malformed
    (let ((name ((car args) 'value))
          (value (evaluate (cadr args) env)))
      (if (value 'error?)
        value
        (let ((rc ((env 'replace!) name value)))
          (if (rc 'error?)
            rc
            value))))))


(define (syntax-let args env)
  (define err-malformed (v/t 'error "malformed let"))

  (cond ((< (length args) 2) err-malformed)
        ((eq? ((car args) 'type) 'label) (syntax-named-let args env))
        (else
          (let ((bindings (evaluate-bindings (car args) env)))
            (if (bindings 'error?)
              bindings
              (let ((new-env (frame (bindings 'value) env)))
                (evaluate-body (cdr args) new-env)))))))


(define (syntax-named-let args env)
  (define err-malformed (v/t 'error "malformed let"))

  (if (not (eq? ((car args) 'type) 'symbol))
    err-malformed
    (let ((bindings (evaluate-bindings (cadr args) env)))
      (if (bindings 'error?)
        bindings
        (let* ((symbols (map car bindings))
               (closname ((car args) 'value))
               (body (cddr args))
               (clos (new-closure symbols body env))
               (new-env (frame (cons (cons closname clos) (bindings 'value)) env)))
          (evaluate-body body new-env))))))


(define (syntax-let* args env)
  (define err-malformed (v/t 'error "malformed let"))

  (if (< (length args) 2) err-malformed
    (let* ((new-env (frame '() env))
           (bindings (evaluate-bindings* (car args) new-env)))
      (if (bindings 'error?)
        bindings
        (evaluate-body (cdr args) new-env)))))


(define (syntax-letrec args env)
  (define err-malformed (v/t 'error "malformed let"))

  (if (< (length args) 2) err-malformed
    (let* ((new-env (frame '() env))
           (bindings (evaluate-bindings-rec (car args) env)))
      (if (bindings 'error?)
        bindings
        (evaluate-body (cdr args) new-env)))))


(define (syntax-if args env)
  (if (not (eq? 3 (length args)))
    (v/t 'error "malformed if")
    (let ((condition (evaluate (car args) env)))
      (cond ((condition 'error?) condition)
            ((condition 'value) (evaluate (cadr args) env))
            (else (evaluate (caddr args) env))))))


(define (syntax-cond args _env)
  (define (check-cond args)
    (if (null? args) (v/t 'ok "valid cond form")
      (let ((cell (car args)))
        (if (or (not (eq? 'list (cell 'type)))
                (not (= 2 (length (cell 'value)))))
          (v/t 'error "malformed cond")
          (check-cond (cdr args))))))

  (define (loop args env)
    (if (null? args) (v/t 'undef '())
      (let ((condition (evaluate (car ((car args) 'value)) env)))
        (cond ((condition 'error?) condition)
              ((condition 'value) (evaluate (cadr ((car args) 'value)) env))
              (else (loop (cdr args) env))))))

  (if ((check-cond args) 'error?)
    check-cond
    (loop args _env)))


(define (syntax-else args env)
  (v/t 'error "invalid syntax"))

(define (syntax-and args env))

(define (syntax-or args env))

(define (syntax-begin args env))

(define (syntax-do args env))

; utils


(define (evaluate-bindings tree env)
  (define (loop bindings)
    (define invalid-syntax (v/t 'error "invalid syntax"))

    (if (null? bindings)
      (v/t 'bindings '())
      (let ((cell (car bindings)))
        (if (or (not (eq? (cell 'type) 'list))
                (not (= (length (cell 'value)) 2)))
          invalid-syntax
          (let ((label (car (cell 'value)))
                (value (evaluate (cadr (cell 'value)) env)))
            (cond ((not (eq? 'symbol (label 'type))) invalid-syntax)
                  ((value 'error?) value)
                  (else
                    (let ((binding (cons (label 'value) value))
                          (rest (loop (cdr bindings))))
                      (if (rest 'error?)
                        rest
                        (v/t 'bindings (cons binding (rest 'value))))))))))))

  (loop (tree 'value)))


(define (evaluate-bindings* tree env)
  (define (loop bindings)
    (define invalid-syntax (v/t 'error "invalid syntax"))

    (if (null? bindings)
      (v/t 'bindings '())
      (let ((cell (car bindings)))
        (if (or (not (eq? (cell 'type) 'list))
                (not (= (length (cell 'value)) 2)))
          invalid-syntax
          (let ((label (car (cell 'value)))
                (value (evaluate (cadr (cell 'value)) env)))
            (cond ((not (eq? 'symbol (label 'type))) invalid-syntax)
                  ((value 'error?) value)
                  (else (begin
                    ((env 'push!) (label 'value) value)
                    (let ((binding (cons (label 'value) value))
                          (rest (loop (cdr bindings))))
                      (if (rest 'error?)
                        rest
                        (v/t 'bindings
                             (cons binding (rest 'value)))))))))))))

  (loop (tree 'value)))


(define (evaluate-bindings-rec tree env)
  (define (loop bindings)
    (define invalid-syntax (v/t 'error "invalid syntax"))

    (if (null? bindings)
      (v/t 'bindings '())
      (let ((cell (car bindings)))
        (if (or (not (eq? (cell 'type) 'list))
                (not (= (length (cell 'value)) 2)))
          invalid-syntax
          (let ((label (car (cell 'value))))
            (if (not (eq? 'symbol (label 'type)))
              invalid-syntax
              (begin
                ((env 'push!) (label 'value) (v/t 'error "circular reference"))
                (let ((value (evaluate (cadr (cell 'value)) env)))
                  (if (value 'error?)
                    value
                    (let ((binding (cons (label 'value) value))
                          (rest (loop (cdr bindings))))
                      (if (rest 'error?)
                        rest
                        (begin
                          ((env 'push!) (label 'value) value)
                          (v/t 'bindings (cons binding (rest 'value)))))))))))))))

  (loop (tree 'value)))


(define (new-closure symbols body callee-env)
  (define (evaluate-args args env)
    (define (loop args)
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
                      (v/t 'args (cons result (rest 'value))))))))))

    (loop args))

  (define closure (lambda (_args caller-env)
    (if (not (equal? (length symbols) (length _args)))
      (v/t 'error "wrong number of arguments")
      (let ((args (evaluate-args _args caller-env)))
        (if (args 'error?)
          args
          (let* ((new-env (frame (map cons symbols (args 'value)) callee-env)))
            (evaluate-body body new-env)))))))

  (let ((check-result (check-body body)))
    (if (check-result 'error?)
      check-result
      (v/t 'closure closure))))


(define (evaluate-body body env)
  (define (loop body return-value)
    (if (null? body)
      return-value
      (let* ((term (car body))
             (rest (cdr body))
             (next-value (evaluate term env)))
        (if (next-value 'error?)
          next-value
          (loop rest next-value)))))

  (loop body (v/t 'undef '())))


(define (check-all pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (check-all pred (cdr lst)))))


(define (check-body body)
  (define (loop body prev-type)
    (if (null? body)
      (if (eq? prev-type 'define)
        (v/t 'error "invalid body form")
        (v/t 'ok "no problem :)"))
      (let* ((term (car body))
             (rest (cdr body)))
        (if (and (not (eq? prev-type 'define)) (eq? (term 'type) 'define))
          (v/t 'error "invalid body form")
          (loop rest (term 'type))))))

  (loop body 'define))
