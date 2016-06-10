; repl.scm
(load "./parser.scm")
(load "./evaluator.scm")
(load "./functions.scm")
(load "./syntaxes.scm")
(load "./global-env.scm")

(define (repl)
  (define (display-tree tree)
    (if (not (eq? 'list (tree 'type)))
      (display (tree 'value))
      (begin
        (display "(")
        (let loop ((first #t)
                   (lst tree))
          (cond ((w-null? lst) (display ")"))
                ((not (pair? (lst 'value))) (begin
                                              (display " . ")
                                              (display-tre lst)
                                              (display ")")))
                (else (begin
                        (display (if first "" " "))
                        (display-tree (w-car lst))
                        (loop #f (w-cdr lst)))))))))

  (define (read-terms code)
    (let loop ((ind 0))
      (if (>= ind (string-length code))
        ind
        (let* ((parse-result (parse-term code ind))
               (tree (car parse-result))
               (next-ind (cdr parse-result)))
          (cond ((eq? 'non-eot next-ind) ind)
                ((eq? 'none (tree 'type)) (string-length code))
                ((tree 'error?)
                  (begin (display-tree tree)
                         (string-length code)))
                (else
                  (let ((result (evaluate tree global-env)))
                    (display "output -> ")
                    (display-tree result)
                    (display " : ")
                    (display (result 'type))
                    (newline)

                    (loop next-ind))))))))

  (define (loop left-code)
    (let ((raw-line (begin
                      (display "left code: ")
                      (display left-code)
                      (newline)
                      (display "MINI-SCHEME >>> ")
                  (flush)
                  (read-line))))
      (if (and (= 0 (string-length left-code)) (eof-object? raw-line))
        'bye
        (let* ((line (if (eof-object? raw-line) "" raw-line))
               (code (string-append left-code "\n" line))
               (end-ind (read-terms code)))
          (loop (string-copy code end-ind (string-length code)))))))
  
  (loop "")) 
