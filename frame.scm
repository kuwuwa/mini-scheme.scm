; frame.scm
(load "./result.scm")

(define (frame alist outer-frame)
  (lambda (cmd)
    (cond ((eq? cmd 'find)
           (lambda (name)
             (let ((pair (assoc name alist)))
               (cond (pair (cdr pair))
                     ((null? outer-frame)
                      (v/t 'error (string-append "variable `" name "' not found")))
                     (else ((outer-frame 'find) name))))))
          ((eq? cmd 'push)
           (lambda (symbol value)
             (frame (cons (cons symbol value) alist)
                    outer-frame)))
          ((eq? cmd 'replace)
           (lambda (name value)
             (let ((pair (assoc name alist)))
               (cond (pair (frame (cons (cons name value) alist) outer-frame))
                     ((null? outer-frame)
                      (v/t 'error (string-append "variable `" name "' not found")))
                     (else
                       (let ((new-outer-frame ((outer-frame 'replace) name value)))
                         (if (new-outer-frame 'error?)
                           new-outer-frame
                           (v/t 'frame (frame alist new-outer-frame))))))))))))

