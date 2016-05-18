; frame.scm
(load "./result.scm")

(define (frame alist outer-frame)
  (lambda (cmd)
    (cond ((eq? cmd 'find)
           (lambda (name)
             (let ((pair (assoc name alist)))
               (cond (pair (cdr pair))
                     ((null? outer-frame)
                      (v/t 'error
                                   (string-append "variable `" name "' not found")))
                     (else ((outer-frame 'find) name))))))
          ((eq? cmd 'push)
           (lambda (symbol value)
             (frame (cons (cons symbol value) alist)
                    outer-frame))))))
