; result.scm

(define (value-with-type type value)
  ; a closure that represents a result of parsing/evaluation
  (lambda (cmd)
    (cond ((eq? 'error? cmd) (eq? 'error type))
          ((eq? 'type cmd) type)
          ((eq? 'value cmd) value))))

(define v/t value-with-type)
