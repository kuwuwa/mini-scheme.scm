; syntaxes.scm
(load "./evaluator.scm")
(load "./result.scm")

(define (syntax-if args env)
  (if (not (eq? 3 (length args)))
    (v/t 'error "malformed if")
    (let ((condition (evaluate (car args) env)))
      (cond ((condition 'error?) condition)
            ((condition 'value) (evaluate (cadr args) env))
            (else (evaluate (caddr args) env))))))


