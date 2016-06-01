; functions.scm
(load "./result.scm")
(load "./evaluator.scm")

(define (check-all pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (check-all pred (cdr lst)))))


; 整数
; number?, +, -, *, /, =, <, <=, >, >=


(define (subr.number? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (eq? 'number ((car args) 'type)))))


(define (subr.+ args env)
  (display (check-all number? (map (lambda (a) (a 'value)) args)))
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'number (apply + nums))
      (v/t 'error "number required"))))


(define (subr.- args env)
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'number (apply - nums))
      (v/t 'error "number required"))))


(define (subr.* args env)
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'number (apply * nums))
      (v/t 'error "number required"))))


(define (subr./ args env)
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'number (apply / nums))
      (v/t 'error "number required"))))


(define (subr.= args env)
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'bool (apply = nums))
      (v/t 'error "number required"))))


(define (subr.< args env)
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'bool (apply < nums))
      (v/t 'error "number required"))))


(define (subr.<= args env)
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'bool (apply <= nums))
      (v/t 'error "number required"))))


(define (subr.> args env)
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'bool (apply > nums))
      (v/t 'error "number required"))))


(define (subr.>= args env)
  (let ((nums (map (lambda (a) (a 'value)) args)))
    (if (check-all number? nums)
      (v/t 'bool (apply >= nums))
      (v/t 'error "number required"))))


; リスト
; null?, pair?, list?, symbol?,
; car, cdr, cons, list, length, memq, last, append,
; set-car!, set-cdr!

(define (subr.null? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (null? ((car args) 'value)))))


(define (subr.pair? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (pair? ((car args) 'value)))))


(define (subr.list? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (list? ((car args) 'value)))))


(define (subr.length args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (list? ((car args) 'value))) (v/t 'error "list required"))
        (else (v/t 'number (length ((car args) 'value))))))


(define (subr.symbol? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (symbol? ((car args) 'value)))))


(define (subr.car args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        (else (car ((car args) 'value)))))


(define (subr.cdr args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        (else (v/t 'list (cdr ((car args) 'value))))))


(define (subr.cons args env)
  (if (not (= 2 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'list (cons (car args) (cadr args)))))


(define (subr.list args env)
  (v/t 'list (apply list args)))


; TODO: FIX ME
(define (subr.memq args env)
  (if (not (= 2 (length args)))
    (v/t 'error "wrong number of arguments")
    (let ((result (memq ((car args) 'value) ((cadr args) 'value))))
      (if (eq? result #f)
        (v/t 'bool #f)
        (v/t 'list result)))))


(define (subr.last args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        (else (last ((car args) 'value)))))


(define (subr.append args env)
  (if (check-all (lambda (a) (list? (a 'value))) args)
    (v/t 'list (apply append (map (lambda (a) (a 'value)) args)))
    (v/t 'error "list required")))


(define (subr.set-car! args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        (else
          (set-car! ((car args) 'value) (cadr args))
          (v/t 'undef "undefined"))))


(define (subr.set-cdr! args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        (else
          (set-cdr! ((car args) 'value) (cadr args))
          (v/t 'undef "undefined"))))

 
; ブール値
; boolean?, not


(define (subr.boolean? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (eq? 'bool (car args)))))


(define (subr.not args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (not ((car args) 'value)))))

 
; 文字列
; string?, string-append,
; symbol->string, string->symbol, string->number, number->string


(define (subr.string? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (eq? 'string (car args)))))


(define (subr.string-append args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (check-all (lambda (a) (eq? 'string (a 'value))) args))
          (v/t "string is required"))
        (else
          (v/t 'string (apply string-append
                              (map (lambda (a) (a 'value)) ((car args) 'value)))))))


(define (subr.symbol->string args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'symbol ((car args) 'type))) (v/t "symbol is required"))
        (else (v/t 'string (symbol->string ((car args) 'value))))))

 
(define (subr.string->symbol args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t "string is required"))
        (else (v/t 'symbol (string->symbol ((car args) 'value))))))


(define (subr.string->number args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t "string is required"))
        (else (let ((result (string->number ((car args) 'value))))
                (if (eq? result #f) (v/t 'bool #f) (v/t 'number result))))))


(define (subr.number->string args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t "number is required"))
        (else (let ((result (number->string ((car args) 'value))))
                (if (eq? result #f) (v/t 'bool #f) (v/t 'string result))))))


; 関数
; procedure?

(define (subr.procedure? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (procedure? ((car args) 'value)))))
 

; 比較
; eq?, neq?, equal?


(define (subr.eq? args env)
  (if (not (= 2 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (eq? ((car args) 'value) ((cadr args) 'value)))))


(define (subr.neq? args env)
  (if (not (= 2 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (not (eq? ((car args) 'value) ((cadr args) 'value))))))


(define (subr.equal? args env)
  (if (not (= 2 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (equal? ((car args) 'value) ((cadr args) 'value)))))


; call/cc

(define (subr.call/cc args env)
  (call/cc (lambda (cont)
    (define (continuation args env)
      (if (not (= 1 (length args)))
        (v/t 'error "wrong number of arguments")
        (cont (car args))))

    (if (not (= 1 (length args)))
      (v/t 'error "wrong number of arguments")
      (let ((proc (car args)))
        (evaluate (v/t 'list (list (v/t 'quote proc) (v/t 'quote (v/t 'quote (v/t 'closure continuation)))))
                  env))))))

; TODO: implement load
