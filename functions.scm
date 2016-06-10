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
    (cond ((memq 0 (cdr nums)) (v/t 'error "division by zero"))
          ((check-all number? nums) (v/t 'number (apply / nums)))
          (else (v/t 'error "number required")))))


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
    (v/t 'bool (w-null? (car args)))))


(define (subr.pair? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (pair? ((car args) 'value)))))


(define (subr.list? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (w-list? (car args)))))


(define (subr.length args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (w-list? (car args))) (v/t 'error "list required"))
        (else (v/t 'number (w-length (car args))))))


(define (subr.symbol? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (symbol? ((car args) 'value)))))


(define (subr.car args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        (else (w-car (car args)))))


(define (subr.caar args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        ((not (pair? (w-car (car args)))) (v/t 'error "pair required"))
        (else (w-caar (car args)))))


(define (subr.cadr args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        ((not (pair? ((w-cdr (car args)) 'value))) (v/t 'error "pair required"))
        (else (w-cadr (car args)))))


(define (subr.cdr args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        (else (w-cdr (car args)))))


(define (subr.cdar args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        ((not (pair? ((w-car (car args)) 'value))) (v/t 'error
                                                        "pair required"))
        (else (w-cdar (car args)))))


(define (subr.cddr args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (pair? ((car args) 'value))) (v/t 'error "pair required"))
        ((not (pair? ((w-cdr (car args)) 'value))) (v/t 'error
                                                        "pair required"))
        (else (w-cddr (car args)))))


(define (subr.cons args env)
  (if (not (= 2 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'list (cons (car args) (cadr args)))))


(define (subr.list args env) (to-typed-list args))


(define (subr.list->string args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'list ((car args) 'type))) (v/t 'error "list required"))
        ((not (check-all (lambda (ch) (eq? 'char (ch 'type))) (to-plain-list (car args))))
           (v/t 'error "char required"))
        (else (v/t 'string (list->string (map (lambda (a) (a 'value))
                                              (to-plain-list (car args))))))))


(define (subr.memq args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'list ((cadr args) 'type))) (v/t 'error "list required"))
        (else
          (let loop ((lst (cadr args)))
            (cond ((w-null? lst) (v/t 'bool #f))
                  ((eq? ((car args) 'value) ((w-car lst) 'value)) (v/t 'list lst))
                  ((not (w-pair? (w-cdr lst))) (v/t 'bool #f))
                  (else (loop (w-cdr lst))))))))


(define (subr.member args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'list ((cadr args) 'type))) (v/t 'error "list required"))
        (else
          (let loop ((lst (cadr args)))
            (cond ((w-null? lst) (v/t 'bool #f))
                  ((equal? ((car args) 'value) ((w-car lst) 'value)) lst)
                  ((not (w-pair? (w-cdr lst))) (v/t 'bool #f))
                  (else (loop (w-cdr lst))))))))


(define (subr.last args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (w-pair? (car args))) (v/t 'error "pair required"))
        (else (last (to-plain-list (car args))))))


(define (subr.append args env)
  (if (check-all (lambda (a) (w-list? (a 'value))) args)
    (to-typed-list (apply append (map to-plain-list args)))
    (v/t 'error "list required")))


(define (subr.set-car! args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (w-pair? (car args))) (v/t 'error "pair required"))
        (else
          (set-car! ((car args) 'value) (cadr args))
          (v/t 'undef "undefined"))))


(define (subr.set-cdr! args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (w-pair? (car args))) (v/t 'error "pair required"))
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


(define (subr.string-ref args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        ((not (eq? 'number ((cadr args) 'type))) (v/t 'error "number required"))
        ((>= ((cadr args) 'value) (string-length ((car args) 'value)))
            (v/t 'error "out of range"))
        (else (v/t 'char (string-ref ((car args) 'value) ((cadr args) 'value))))))


(define (subr.string-append args env)
  (cond ((< (length args) 1) (v/t 'error "wrong number of arguments"))
        ((not (check-all (lambda (a) (eq? 'string (a 'type))) args))
          (v/t 'error "string is required"))
        (else
          (v/t 'string (apply string-append
                              (map (lambda (a) (a 'value)) args))))))


(define (subr.string-copy args env)
  (cond ((not (= 3 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        ((not (eq? 'number ((cadr args) 'type))) (v/t 'error "number required"))
        ((not (eq? 'number ((caddr args) 'type))) (v/t 'error "number required"))
        (else (v/t 'string (apply string-copy
                                  (map (lambda (a) (a 'value)) args))))))


(define (subr.string-length args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        (else (v/t 'number (string-length ((car args) 'value))))))


(define (subr.symbol->string args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'symbol ((car args) 'type))) (v/t "symbol is required"))
        (else (v/t 'string (symbol->string ((car args) 'value))))))


(define (subr.string->list args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t 'error "string required"))
        (else (to-typed-list (map (lambda (ch) (v/t 'char ch))
                                  (string->list ((car args) 'value)))))))


(define (subr.string->number args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t "string is required"))
        (else (let ((result (string->number ((car args) 'value))))
                (if (eq? result #f) (v/t 'bool #f) (v/t 'number result))))))


(define (subr.string->symbol args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string ((car args) 'type))) (v/t "string is required"))
        (else (v/t 'symbol (string->symbol ((car args) 'value))))))


(define (subr.char-alphabetic? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (char-alphabetic? ((car args) 'value)))))


(define (subr.char-numeric? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (char-numeric? ((car args) 'value)))))


(define (subr.char->integer args env)
  (cond ((not (= 1 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'char ((car args) 'type))) (v/t 'error "char required"))
        (else (v/t 'number (char->integer ((car args) 'value))))))


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
        (evaluate (to-typed-list
                       (list (v/t 'quote proc)
                             (v/t 'quote (v/t 'closure continuation))))
                  env))))))


(define (subr.map args env)
  (define (exec-map proc args-list env)
    (if (memq 'empty (map (lambda (a) (a 'type)) args-list))
      (v/t 'empty ())
      (let* ((args (map (lambda (a) (v/t 'quote a))
                        (map w-car args-list)))
             (result (evaluate (to-typed-list (cons (v/t 'quote proc) args)) env)))
        (if (result 'error?)
          result 
          (let ((rest (exec-map proc (map w-cdr args-list) env)))
            (if (rest 'error?)
              rest
              (v/t 'list (cons result rest))))))))

  (if (< (length args) 1)
    (v/t 'error "wrong number of arguments")
    (let ((proc (car args))
          (as (cdr args)))
      (if (not (check-all w-list? as))
        (v/t 'error "argument lists contained an improper list")
        (exec-map proc as env)))))


(define (subr.apply args env)
  (define (get-args args)
    (if (null? (cdr args))
      (car args)
      (v/t 'list (cons (car args) (get-args (cdr args))))))

  (cond ((< (length args) 2) (v/t 'error "wrong number of arguments"))
        ((not (w-list? (last args))) (v/t 'error "improper list not allowed"))
        (else
          (let ((proc (car args))
                (rest (cdr args)))
            (evaluate (v/t 'list
                           (cons (v/t 'quote proc)
                                 (w-map (lambda (a) (v/t 'quote a)) (get-args rest))))
                      env)))))


(define (subr.assoc args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'list ((cadr args) 'type))) (v/t 'error "list required"))
        (else
          (let ((m (car args)))
            (let loop ((lst (cadr args)))
              (cond ((w-null? lst) (v/t 'bool #f))
                    ((not (pair? ((w-car lst) 'value))) (loop (w-cdr lst)))
                    (else
                      (let ((p ((w-car lst) 'value)))
                        (if (eq? (m 'value) ((car p) 'value))
                          (v/t 'list p)
                          (loop (w-cdr lst)))))))))))

; text


(define (subr.eof-object? args env)
  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'bool (eof-object? ((car args) 'value)))))


(define (subr.display args env)
  (define (rec tree)
    (if (not (eq? 'list (tree 'type)))
      (display (tree 'value))
      (let loop ((first #t)
                 (lst tree))
        (cond ((w-null? lst) (display ")"))
              ((not (pair? (lst 'value))) (begin
                                            (display " . ")
                                            (rec lst)
                                            (display ")")))
              (else (begin
                      (display (if first "(" " "))
                      (rec (w-car lst))
                      (loop #f (w-cdr lst))))))))

  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'undef (rec (car args)))))


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


(define (subr.write args env)
  (define (rec tree)
    (if (not (eq? 'list (tree 'type)))
      (write (tree 'value))
      (let loop ((first #t)
                 (lst tree))
        (cond ((w-null? lst) (display ")"))
              ((not (pair? (lst 'value))) (begin
                                            (display " . ")
                                            (rec lst)
                                            (display ")")))
              (else (begin
                      (display (if first "(" " "))
                      (rec (w-car lst))
                      (loop #f (w-cdr lst))))))))

  (if (not (= 1 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'undef (rec (car args)))))


(define (subr.newline args env)
  (if (not (= 0 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'undef (newline))))


(define (subr.flush args env)
  (if (not (= 0 (length args)))
    (v/t 'error "wrong number of arguments")
    (v/t 'undef (flush))))


(define (subr.with-input-from-file args env)
  (cond ((not (= 2 (length args))) (v/t 'error "wrong number of arguments"))
        ((not (eq? 'string  ((car args) 'type))) (v/t 'error "string required"))
        ((not (eq? 'closure ((cadr args) 'type))) (v/t 'error "closure required"))
        (else (with-input-from-file
                ((car args) 'value)
                (lambda ()
                  (evaluate (to-typed-list (list (v/t 'quote (cadr args)))) env))))))


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
                          (else (v/t 'error "EOT in the list")))))))))
