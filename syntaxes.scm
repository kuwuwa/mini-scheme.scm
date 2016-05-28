; syntaxes.scm
(load "./evaluator.scm")
(load "./frame.scm")
(load "./result.scm")


; Define ::= (define Id Exp)
;     | (define (Id Id* [. Id]) Body)


(define (syntax-define args env)
  (define err-malformed (v/t 'error "malformed define"))

  (if (< (length args) 1)
    err-malformed
    (let ((symbol (car args)))
      (cond ((and (eq? 'symbol (symbol 'type))
                  (= (length args) 2))
               (syntax-define-value args env))
            ((eq? 'list (symbol 'type)) (syntax-define-closure args env))
            (else err-malformed)))))


(define (syntax-define-value args env)
  (define err-malformed (v/t 'error "malformed define"))

  (if (< (length args) 2)
    err-malformed
    (let* ((_symbol (car args)))
      (if (not (eq? 'symbol (_symbol 'type)))
        err-malformed
        (let* ((symbol (_symbol 'value))
               (value (evaluate (cadr args) env)))
          (if (value 'error?)
            value
            (begin
              ((env 'push!) symbol value)
              _symbol)))))))


(define (syntax-define-closure args env)
  (define err-malformed (v/t 'error "malformed define"))
  (define invalid-syntax (v/t 'error "invalid syntax"))

  (let ((lst ((car args) 'value)))
    (if (< (length lst) 1)
      err-malformed
      (let ((closure-symbol (car lst))
            (params (cdr lst)))
        (if (and (eq? 'symbol (closure-symbol 'type))
                 (check-all (lambda (t) (eq? 'symbol (t 'type))) params))
          (let ((clos (new-closure (map (lambda (p) (p 'value)) params)
                                   (cdr args)
                                   env)))
            (if (clos 'error?)
              clos
              (begin
                ((env 'push!) (closure-symbol 'value) clos)
                closure-symbol)))
          invalid-syntax)))))


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
  (define invalid-syntax (v/t 'error "invalid syntax"))

  (define (evaluate-bindings* tree env)
    (let loop ((bindings (tree 'value)))
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
                                     (cons binding (rest 'value))))))))))))))

  (if (< (length args) 2) err-malformed
    (let* ((new-env (frame '() env))
           (bindings (evaluate-bindings* (car args) new-env)))
      (if (bindings 'error?)
        bindings
        (evaluate-body (cdr args) new-env)))))


(define (syntax-letrec args env)
  (define err-malformed (v/t 'error "malformed let"))

  (define (evaluate-bindings-rec tree env)
    (let loop ((bindings (tree 'value)))
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
                            (v/t 'bindings (cons binding (rest 'value))))))))))))))))

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


(define (syntax-and _args env)
  (let loop ((args _args)
             (ret (v/t 'bool #t)))
    (if (null? args)
      ret
      (let ((val (evaluate (car args) env)))
        (cond ((val 'error?) bool)
              ((not (val 'value)) (v/t 'bool #f))
              (else (loop (cdr args) val)))))))


(define (syntax-or _args env)
  (let loop ((args _args)
             (ret (v/t 'bool #f)))
    (if (null? args)
      ret
      (let ((val (evaluate (car args) env)))
        (cond ((val 'error?) val)
              ((val 'value) val)
              (else (loop (cdr args) val)))))))


(define (syntax-begin _args env)
  (let loop ((args _args)
             (ret (v/t 'undef '())))
    (if (null? args)
      ret
      (let ((val (evaluate (car args) env)))
        (if (val 'error?)
          val
          (loop (cdr args) val))))))


(define (syntax-do args env)
  (define err-malformed (v/t 'error "malformed do"))
  (define ok (v/t 'ok "OK"))

  (define (check-do args)
    (define (check-vars _vars)
      (define err-malformed (v/t 'error "malformed do"))
      (define (loop vars)
        (if (null? vars)
          (v/t 'ok "OK")
          (let ((var (car vars)))
            (if (or (not (eq? 'list (var 'type)))
                    (not (= 3 (length (var 'value)))))
              err-malformed
              (let ((label (car (var 'value))))
                (if (or (not (eq? 'symbol (label 'type))))
                  err-malformed
                  (loop (cdr vars))))))))

      (if (eq? 'list (_vars 'type))
        (loop (_vars 'value))))

    (define (check-return ret)
      (cond ((not (eq? 'list (ret 'type))) err-malformed)
            ((< (length (ret 'value)) 1) err-malformed)
            (else (let ((body-status (check-body (cdr (ret 'value)))))
                    (if (body-status 'error?)
                      err-malformed
                      ok)))))

    (if (< (length args) 2)
      err-malformed
      (let* ((vars (car args))
             (return (cadr args))
             (vars-status (check-vars vars))
             (return-status (check-return return)))
        (cond ((vars-status 'error?) vars-status)
              ((return-status 'error?) return-status)
              (else ok)))))

  (let ((check-result (check-do args)))
    (if (check-result 'error?)
      err-malformed
      (let* ((vars ((car args) 'value))
             (symbols (map (lambda (b) (car (b 'value))) vars))
             (bindings (let ((thunks (map (lambda (b) (cadr (b 'value))) vars)))
                         (evaluate-bindings (v/t 'bindings
                                                 (map (lambda (b) (v/t 'list b))
                                                      (map list symbols thunks)))
                                            env)))
             (steps (map (lambda (b) (caddr (b 'value))) vars))
             ;
             (ret ((cadr args) 'value))
             (end-test (car ret))
             (return-body (cdr ret))
             (body (cddr args))
             (local-env (frame (bindings 'value) env)))
        (if (bindings 'error?)
          bindings
          (let loop ()
            (let ((test-result (evaluate end-test local-env)))
              (cond ((test-result 'error?) test-result)
                    ((test-result 'value) (evaluate-body return-body local-env))
                    (else
                      (let ((new-bindings (evaluate-bindings
                                            (v/t 'list (map (lambda (b) (v/t 'list b))
                                                            (map list symbols steps)))
                                            local-env))
                            (replacer (lambda (binding)
                                        (let ((symbol (car binding))
                                              (value  (cdr binding)))
                                          ((local-env 'replace!) symbol value)))))
                        (map replacer (new-bindings 'value))
                        (loop)))))))))))


; utils


(define (evaluate-bindings tree env)
  (let loop ((bindings (tree 'value)))
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
                        (v/t 'bindings (cons binding (rest 'value)))))))))))))


(define (new-closure symbols body callee-env)
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


(define (evaluate-body _body env)
  (let loop ((body _body)
             (return-value (v/t 'undef '())))
    (if (null? body)
      return-value
      (let* ((term (car body))
             (rest (cdr body))
             (next-value (evaluate term env)))
        (if (next-value 'error?)
          next-value
          (loop rest next-value))))))


(define (check-all pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (check-all pred (cdr lst)))))


(define (check-body _body)
  (let loop ((body _body)
             (prev-type 'define))
    (if (null? body)
      (if (eq? prev-type 'define)
        (v/t 'error "invalid body form")
        (v/t 'ok "no problem :)"))
      (let* ((term (car body))
             (rest (cdr body)))
        (if (and (not (eq? prev-type 'define)) (eq? (term 'type) 'define))
          (v/t 'error "invalid body form")
          (loop rest (term 'type)))))))
