; type.scm

(define (v/t type value)
  ; a closure that represents a result of parsing
  (lambda (cmd)
    (cond ((eq? 'error? cmd) (eq? 'error type))
          ((eq? 'type cmd) type)
          ((eq? 'value cmd) value))))

