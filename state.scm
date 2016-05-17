; state.scm
(load "./result.scm")

(define (new-state val env)
  (lambda (cmd)
    (cond ((eq? cmd 'value) val)
          ((eq? cmd 'type) (val 'type))
          ((eq? cmd 'error?) (eq? (val 'type) 'error))
          ((eq? cmd 'env) env)
          ((eq? cmd 'find) (env 'find))
          ((eq? cmd 'push) (lambda (label value)
                             (state type
                                    value
                                    ((env 'push) label value)))))))
