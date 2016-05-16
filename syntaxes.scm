; syntaxes.scm
(load "./evaluator.scm")
(load "./result.scm")

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

(define (syntax-lambda args env))

(define (syntax-quote args env))

(define (syntax-set! args env))

(define (syntax-let args env))

(define (syntax-let* args env))

(define (syntax-letrec args env))

(define (syntax-if args env)
  (if (not (eq? 3 (length args)))
    (v/t 'error "malformed if")
    (let ((condition (evaluate (car args) env)))
      (cond ((condition 'error?) condition)
            ((condition 'value) (evaluate (cadr args) (condition 'env)))
            (else (evaluate (caddr args) (condition 'env)))))))

(define (syntax-cond args env))

(define (syntax-and args env))

(define (syntax-or args env))

(define (syntax-begin args env))

(define (syntax-do args env))
