; functions.scm


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


(define (subr.caar args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        ((not (pair? ((car ((car args) 'value)) 'value))) (v/t 'error "pair required"))
        (else (car ((car ((car args) 'value)) 'value)))))


(define (subr.cadr args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        ((not (pair? ((cdr ((car args) 'value)) 'value))) (v/t 'error "pair required"))
        (else (car ((cdr ((car args) 'value)) 'value)))))


(define (subr.cdr args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        (else (v/t 'list (cdr ((car args) 'value))))))


(define (subr.cdar args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        ((not (pair? ((car ((car args) 'value)) 'value))) (v/t 'error "pair required"))
        (else (cdr ((car ((car args) 'value)) 'value)))))


(define (subr.cddr args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        ((not (pair? ((cdr ((car args) 'value)) 'value))) (v/t 'error "pair required"))
        (else (cdr ((cdr ((car args) 'value)) 'value)))))


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
    (v/t 'bool (apply boolean? ((car args) 'value)))))


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
    (v/t 'bool (eq? 'string ((car args) 'type)))))


(define (subr.string-append args env)
  (cond ((< (length args) 1) (v/t 'error "wrong number of arguments"))
        ((not (check-all (lambda (a) (eq? 'string (a 'type))) args))
          (v/t 'error "string is required"))
        (else
          (v/t 'string (apply string-append
                              (map (lambda (a) (a 'value)) args))))))


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
        (evaluate (v/t 'list
                       (list (v/t 'quote proc)
                             (v/t 'quote (v/t 'closure continuation))))
                  env))))))


(define (subr.map args env)
  (define (exec-map proc args-list env)
    (if (memq () args-list)
      (v/t 'empty ())
      (let* ((args (map car args-list))
             (result (evaluate (v/t 'list (cons (v/t 'quote proc) args)) env)))
        (if (result 'error?)
          result 
          (let ((rest (exec-map proc (map cdr args-list) env)))
            (cond ((rest 'error?) rest)
                  ((eq? 'empty (rest 'type)) (v/t 'list (list result)))
                  (else (v/t 'list (cons result (rest 'value))))))))))

  (if (< (length args) 1)
    (v/t 'error "wrong number of arguments")
    (let ((proc (car args))
          (as (cdr args)))
      (if (not (check-all (lambda (a) (eq? 'list (a 'type))) as))
        (v/t 'error "argument lists contained an improper list")
        (exec-map proc (map (lambda (a) (a 'value)) as) env)))))


(define (subr.apply args env)
  (define (get-args args)
    (if (null? (cdr args))
      ((car args) 'value)
      (cons (car args) (get-args (cdr args)))))

  (cond ((< (length args) 2) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'list ((last args) 'type))) (v/t 'error
                                                    "improper list not allowed"))
        (else
          (let ((proc (car args))
                (rest (cdr args)))
            (evaluate (v/t 'list
                           (cons (v/t 'quote proc)
                                 (map (lambda (a) (v/t 'quote a)) (get-args rest))))
                      env)))))


(define (subr.string-ref args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        ((not (eq? 'number ((cadr args) 'type))) (v/t 'error "number required"))
        ((>= ((cadr args) 'value) (string-length ((car args) 'value)))
            (v/t 'error "out of range"))
        (else (v/t 'string (string-ref ((car args) 'value) ((cadr args) 'value))))))


(define (subr.string-length args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (string? ((car args) 'value))) (v/t 'error "string required"))
        (else (v/t 'number (string-length ((car args) 'value))))))


(define (subr.char-alphabetic? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (char-alphabetic? ((car args) 'value)))))


(define (subr.char-numeric? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (char-numeric? ((car args) 'value)))))


(define (subr.member args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'list ((cadr args) 'type))) (v/t 'error "list required"))
        (else
          (let ((result (member ((car args) 'value) ((cadr args) 'value))))
            (if (eq? result #f)
              (v/t 'bool #f)
              (v/t 'list result))))))


(define (subr.string->list args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        (else (v/t 'list (string->list ((car args) 'value))))))


(define (subr.string-copy args env)
  (cond ((not (= 3 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        ((not (eq? 'number ((cadr args) 'type))) (v/t 'error "number required"))
        ((not (eq? 'number ((caddr args) 'type))) (v/t 'error "number required"))
        (else (v/t 'string (apply string-copy
                                  (map (lambda (a) (a 'value)) args))))))


; others


(define (subr.eof-object? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (eof-object? ((car args) 'value)))))


(define (subr.display args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        (else (begin
                (display ((car args) 'value))
                (v/t 'undef ())))))


(define (subr.write args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        (else (begin
                (write ((car args) 'value))
                (v/t 'undef ())))))


(define (subr.newline args env)
  (if (not (= 0 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'undef (newline))))


(define (subr.flush args env)
  (if (not (= 0 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'undef (flush))))


; TODO: implement load


(define (subr.read-string args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'number ((car args) 'type))) (v/t 'error "number required"))
        (else (v/t 'string (read-string ((car args) 'value))))))


(define (subr.read-line args env)
  (if (not (= 0 (length args)))
    (v/t 'error "wrong number of arguments")
    (let ((result (read-line)))
      (if (eof-object? result)
        (v/t 'eof-object result)
        (v/t 'string result)))))


(define (subr.with-input-from-file args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string  ((car args) 'type))) (v/t 'error "string required"))
        ((not (eq? 'closure ((cadr args) 'type))) (v/t 'error "closure required"))
        (else (with-input-from-file
                ((car args) 'value)
                (lambda ()
                  (evaluate (v/t 'list (list (v/t 'quote (cadr args)))) env))))))


(define (subr.load args env)
  (define (read-terms code env)
    (let loop ((ind 0))
      (if (>= ind (string-length code))
        (v/t 'number ind)
        (let* ((parse-result (parse-term code ind))
               (tree (car parse-result))
               (next-ind (cdr parse-result)))
          (cond ((eq? 'non-eot next-ind)  (v/t 'number ind))
                ((eq? 'none (tree 'type)) (v/t 'number (string-length code)))
                ((tree 'error?) tree)
                (else
                  (let ((result (evaluate tree env)))
                    (if (result 'error?)
                      result
                      (loop next-ind)))))))))

  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        (else (with-input-from-file
                ((car args) 'value)
                (lambda ()
                  (let* ((code (read-string 1000000000000))
                         (result (read-terms code env)))
                    (cond ((result 'error?) result)
                          ((= (result 'value) (string-length code)) (v/t 'bool #t))
                          (else (v/t 'error "EOL in the list")))))))))
