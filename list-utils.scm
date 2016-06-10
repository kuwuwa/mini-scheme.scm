; list-util.scm

(load "./value-with-type.scm")

(define (w-car v) (car (v 'value)))
(define (w-cdr v) (cdr (v 'value)))
(define (w-caar v) (w-car (w-car v)))
(define (w-cadr v) (w-car (w-cdr v)))
(define (w-cdar v) (w-car (w-car v)))
(define (w-cddr v) (w-cdr (w-cdr v)))
(define (w-caddr v) (w-car (w-cdr (w-cdr v))))

(define (w-null? v) (eq? 'empty (v 'type)))

(define (w-pair? v) (pair? (v 'value)))

(define (w-list? v)
  (or (w-null? v)
      (and (eq? 'list (v 'type))
           (w-list? (w-cdr v)))))

(define (w-map f v)
  (if (eq? 'empty (v 'type))
    (v/t 'empty ())
    (v/t 'list (cons (f (w-car v)) (w-map f (w-cdr v))))))

(define (w-length v)
  (let loop ((len 0)
             (rest v))
    (cond ((eq? 'empty (rest 'type)) len)
          ((not (eq? 'list (rest 'type))) #f)
          (else (loop (+ 1 len) (w-cdr rest))))))

(define (to-plain-list v)
  (let loop ((rest v))
    (cond ((w-null? rest) ())
          ((not (pair? (rest 'value))) rest)
          (else (cons (w-car rest) (to-plain-list (w-cdr rest)))))))

(define (to-typed-list v)
  (let loop ((rest v))
    (cond ((null? rest) (v/t 'empty ()))
          ((not (pair? rest)) rest)
          (else (v/t 'list (cons (car rest) (to-typed-list (cdr rest))))))))
