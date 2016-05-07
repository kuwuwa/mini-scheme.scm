; frame.scm

(define (frame labels outer-frame)
  (lambda (cmd)
    (cond ((eq? cmd 'find) (lambda (name)
                             (let ((label (assoc name labels)))
                               (or label
                                   (if (null? outer-frame)
                                     ()
                                     ((outer-frame 'find) name))))))
          ((eq? cmd 'push) (lambda (label value)
                             (frame (cons (cons label value)
                                          labels)
                                    outer-frame))))))
