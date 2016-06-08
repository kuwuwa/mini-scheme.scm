; syntaxes.scm

(load "./list-util.scm")
(load "./evaluator.scm")
(load "./frame.scm")
(load "./result.scm")

; Define ::= (define Id Exp)
;     | (define (Id Id* [. Id]) Body)

(define (syntax-define args env)
  (define err-malformed (v/t 'error "malformed define"))

  (if (< (w-length args) 1)
    err-malformed
    (let ((symbol (w-car args)))
      (cond ((and (eq? 'symbol (symbol 'type))
                  (= (w-length args) 2))
               (syntax-define-value args env))
            ((eq? 'list (symbol 'type)) (syntax-define-closure args env))
            (else err-malformed)))))


(define (syntax-define-value args env)
  (define err-malformed (v/t 'error "malformed define"))

  (if (< (w-length args) 2)
    err-malformed
    (let* ((symbol (w-car args)))
      (if (not (eq? 'symbol (symbol 'type)))
        err-malformed
        (let* ((symbol-name (symbol 'value))
               (value (evaluate (w-cadr args) env)))
          (if (value 'error?)
            value
            (begin
              ((env 'push!) symbol-name value)
              symbol)))))))


(define (syntax-define-closure args env)
  (define err-malformed (v/t 'error "malformed define"))

  (let ((lst (w-car args)))
    (if (not (eq? 'list (lst 'type)))
      err-malformed
      (let* ((closure-symbol (w-car lst))
             (params (to-plain-list (w-cdr lst)))
             (body (to-plain-list (w-cdr args)))
             (clos (new-closure params body env)))
        (if (clos 'error?)
          clos
          (begin
            ((env 'push!) (closure-symbol 'value) clos)
            closure-symbol))))))


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

  (if (< (w-length args) 1)
    err-malformed
    (let ((symbols (w-car args)))
      (new-closure (to-plain-list symbols)
                   (to-plain-list (w-cdr args)) env))))


(define (syntax-quote args env)
  (define err-malformed (v/t 'error "malformed quote"))

  (if (not (= (w-length args) 1))
    err-malformed
    (w-car args)))


(define (syntax-set! args env)
  (define err-malformed (v/t 'error "malformed set!"))

  (if (or (not (= (w-length args) 2))
          (not (eq? 'symbol ((w-car args) 'type))))
    err-malformed
    (let ((name ((w-car args) 'value))
          (value (evaluate (w-cadr args) env)))
      (if (value 'error?)
        value
        (let ((rc ((env 'replace!) name value)))
          (if (rc 'error?)
            rc
            value))))))


(define (syntax-let args env)
  (define err-malformed (v/t 'error "malformed let"))

  (cond ((< (w-length args) 2) err-malformed)
        ((eq? ((w-car args) 'type) 'symbol) (syntax-named-let args env))
        (else
          (let ((bindings (evaluate-bindings (to-plain-list (w-car args)) env)))
            (if (bindings 'error?)
              bindings
              (let ((new-env (frame (bindings 'value) env)))
                (evaluate-body (to-plain-list (w-cdr args)) new-env)))))))


(define (syntax-named-let args env)
  (define err-malformed (v/t 'error "malformed let"))

  (if (not (eq? 'symbol ((w-car args) 'type)))
    err-malformed
    (let ((bindings (evaluate-bindings (to-plain-list (w-cadr args)) env)))
      (if (bindings 'error?)
        bindings
        (let* ((symbols (map (lambda (s) (v/t 'symbol s))
                             (map car (bindings 'value))))
               (closname ((w-car args) 'value))
               (body (to-plain-list (w-cddr args)))
               (local-env (frame () env))
               (clos (new-closure symbols body local-env)))
          ((local-env 'push!) closname clos)
          ((clos 'value) (map cdr (bindings 'value)) local-env))))))


(define (syntax-let* args env) ; TODO: more validation
  (define err-malformed (v/t 'error "malformed let"))
  (define invalid-syntax (v/t 'error "invalid syntax"))

  (define (evaluate-bindings* tree env)
    (let loop ((bindings tree))
      (if (null? bindings)
        (v/t 'bindings '())
        (let ((cell (car bindings)))
          (if (or (not (eq? (cell 'type) 'list))
                  (not (= 2 (w-length cell))))
            invalid-syntax
            (let ((label (w-car cell))
                  (value (evaluate (w-cadr cell) env)))
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

  (if (< (w-length args) 2)
    err-malformed
    (let* ((new-env (frame '() env))
           (bindings (evaluate-bindings* (to-plain-list (w-car args)) new-env)))
      (if (bindings 'error?)
        bindings
        (evaluate-body (to-plain-list (w-cdr args)) new-env)))))


(define (syntax-letrec args env) ; TODO: more validations
  (define err-malformed (v/t 'error "malformed let"))

  (define (evaluate-bindings-rec tree env)
    (define invalid-syntax (v/t 'error "invalid syntax"))
    (let loop ((bindings tree))
      (if (null? bindings)
        (v/t 'bindings '())
        (let ((cell (car bindings)))
          (if (or (not (eq? 'list (cell 'type)))
                  (not (= 2 (w-length cell))))
            invalid-syntax
            (let ((label (w-car cell)))
              (if (not (eq? 'symbol (label 'type)))
                invalid-syntax
                (begin
                  ((env 'push!) (label 'value) (v/t 'error "circular reference"))
                  (let ((value (evaluate (w-cadr cell) env)))
                    ((env 'replace!) (label 'value) value)
                    (if (value 'error?)
                      value
                      (let ((binding (cons (label 'value) value))
                            (rest (loop (cdr bindings))))
                        (if (rest 'error?)
                          rest
                          (v/t 'bindings (cons binding (rest 'value)))))))))))))))

  (if (< (w-length args) 2) err-malformed
    (let* ((new-env (frame '() env))
           (bindings (evaluate-bindings-rec (to-plain-list (w-car args)) env)))
      (if (bindings 'error?)
        bindings
        (evaluate-body (to-plain-list (w-cdr args)) new-env)))))


(define (syntax-if args env)
  (if (or (not (w-list? args))
          (not (eq? 3 (w-length args))))
    (v/t 'error "malformed if")
    (let ((condition (evaluate (w-car args) env)))
      (cond ((condition 'error?) condition)
            ((condition 'value) (evaluate (w-cadr args) env))
            (else (evaluate (w-caddr args) env))))))


(define (syntax-cond args _env)
  (define (check-cond _args)
    (if (null? _args)
      (v/t 'error "syntax-error")
      (let loop ((args _args))
        (if (null? args) (v/t 'ok "valid cond form")
          (let ((cell (car args)))
            (if (or (not (eq? 'list (cell 'type)))
                    (not (= 2 (w-length cell))))
              (v/t 'error "malformed cond")
              (loop (cdr args))))))))

  (define (loop args env)
    (if (null? args) (v/t 'undef '())
      (let ((condition (evaluate (w-car (car args)) env)))
        (cond ((condition 'error?) condition)
              ((condition 'value) (evaluate (w-cadr (car args)) env))
              (else (loop (cdr args) env))))))

  (if ((check-cond (to-plain-list args)) 'error?)
    check-cond
    (loop (to-plain-list args) _env)))


(define (syntax-else args env)
  (v/t 'error "invalid syntax"))


(define (syntax-and _args env)
  (let loop ((args (to-plain-list _args))
             (ret (v/t 'bool #t)))
    (if (null? args)
      ret
      (let ((val (evaluate (car args) env)))
        (cond ((val 'error?) bool)
              ((not (val 'value)) (v/t 'bool #f))
              (else (loop (cdr args) val)))))))


(define (syntax-or _args env)
  (let loop ((args (to-plain-list _args))
             (ret (v/t 'bool #f)))
    (if (null? args)
      ret
      (let ((val (evaluate (car args) env)))
        (cond ((val 'error?) val)
              ((val 'value) val)
              (else (loop (cdr args) val)))))))


(define (syntax-begin _args env)
  (let loop ((args (to-plain-list _args))
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
      (define (loop vars)
        (if (w-null? vars)
          ok
          (let ((var (w-car vars)))
            (if (or (not (eq? 'list (var 'type)))
                    (not (= 3 (w-length var))))
              err-malformed
              (let ((label (w-car var)))
                (if (or (not (eq? 'symbol (label 'type))))
                  err-malformed
                  (loop (w-cdr vars))))))))

      (and (eq? 'list (_vars 'type))
        (loop _vars )))

    (define (check-return ret)
      (cond ((not (eq? 'list (ret 'type))) err-malformed)
            ((< (w-length ret) 1) err-malformed)
            (else (let ((body-status (check-body (to-plain-list (w-cdr ret)))))
                    (if (body-status 'error?)
                      (v/t 'error "dame")
                      ok)))))

    (if (< (w-length args) 2)
      err-malformed
      (let* ((vars (w-car args))
             (return (w-cadr args))
             (vars-status (check-vars vars))
             (return-status (check-return return)))
        (cond ((vars-status 'error?) vars-status)
              ((return-status 'error?) return-status)
              (else ok)))))

  (let ((check-result (check-do args)))
    (if (check-result 'error?)
      check-result
      (let* ((vars (to-plain-list (w-car args)))
             (symbols (map w-car vars))
             (bindings (let ((thunks (map w-cadr vars)))
                         (evaluate-bindings (map (lambda (a b)
                                                   (to-typed-list (list a b)))
                                                 symbols thunks)
                                            env)))
             (steps (map w-caddr vars))
             ;
             (ret (w-cadr args))
             (end-test (w-car ret))
             (return-body (to-plain-list (w-cdr ret)))
             (body (to-plain-list (w-cddr args)))
             (local-env (frame (bindings 'value) env)))
        (if (bindings 'error?)
          bindings
          (let loop ()
            (let ((test-result (evaluate end-test local-env)))
              (cond ((test-result 'error?) test-result)
                    ((test-result 'value) (evaluate-body return-body local-env))
                    (else
                      (evaluate-body body local-env)
                      (let ((new-bindings (evaluate-bindings
                                            (map (lambda (a b)
                                                   (to-typed-list (list a b)))
                                                 symbols steps)
                                            local-env))
                            (replacer (lambda (binding)
                                        (let ((symbol (car binding))
                                              (value  (cdr binding)))
                                          ((local-env 'replace!) symbol value)))))
                        (map replacer (new-bindings 'value))
                        (loop)))))))))))


; macro


(define (syntax-define-macro _args callee-env)
  (define (new-macro params expr)
    (define (macro args caller-env)
      (if (not (eq? (length params) (w-length args)))
        (v/t 'error "wrong number of arguments")
        (let* ((bindings (map cons params (to-plain-list args)))
               (new-env (frame bindings callee-env))
               (new-expr (evaluate expr new-env)))
          (if (new-expr 'error?)
            new-expr
            (evaluate new-expr caller-env)))))
    (v/t 'syntax macro))

  (cond ((not (= (w-length _args) 2)) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'list ((w-car _args) 'type))) (v/t 'error "malformed define-macro"))
        ((not (check-all (lambda (a) (eq? 'symbol (a 'type)))
                         (to-plain-list (w-car _args))))
           (v/t 'error "symbols required"))
        (else
          (let* ((symbols (map (lambda (a) (a 'value)) (to-plain-list (w-car _args))))
                 (macro-name (car symbols))
                 (params (cdr symbols))
                 (expr (w-cadr _args)))
            ((callee-env 'push!) macro-name (new-macro params expr))
            (v/t 'symbol macro-name)))))


; utils

(define (evaluate-bindings tree env)
  (let loop ((bindings tree))
    (define invalid-syntax (v/t 'error "invalid syntax"))

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


(define (new-closure symbols body callee-env)
  (define (evaluate-args _args env)
    (let loop ((args _args))
      (if (null? args)
        (v/t 'empty '())
        (let ((result (evaluate (w-car args) env)))
          (if (result 'error?)
            result
            (let ((rest (loop (w-cdr args))))
              (cond ((rest 'error?) rest)
                    ((eq? 'empty (rest 'type))
                     (v/t 'args (cons result '())))
                    (else
                      (v/t 'args (cons result (rest 'value)))))))))))


  (define (get-bindings symbols args)
    (cond ((and (null? symbols) (null? args)) (v/t 'empty '()))
          ((null? symbols) (v/t 'error "wrong number of arguments"))
          ((not (pair? symbols))
            (if (eq? 'symbol (symbols 'type))
              (v/t 'bindings (list (cons (symbols 'value)
                                         (to-typed-list args))))
              (v/t 'error "invalid syntax")))
          ((null? args) (v/t 'error "wrong number of arguments"))
          (else
            (let ((symbol (car symbols)))
              (if (not (eq? 'symbol (symbol 'type)))
                (v/t 'error "invalid syntax2")
                (let ((rest (get-bindings (cdr symbols) (cdr args))))
                  (cond ((rest 'error?) rest)
                        ((eq? 'empty (rest 'type))
                          (v/t 'bindings (list (cons (symbol 'value) (car args)))))
                        (else
                          (v/t 'bindings (cons (cons (symbol 'value) (car args))
                                               (rest 'value)))))))))))

  (define closure (lambda (args caller-env)
    (let ((bindings (get-bindings symbols args)))
      (if (bindings 'error?)
        bindings
        (let ((new-env (frame (bindings 'value) callee-env)))
          (evaluate-body body new-env))))))

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
