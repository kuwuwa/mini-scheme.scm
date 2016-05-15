; parser.scm

(define (make-result type value)
  ; a closure that represents a result of parsing
  (lambda (cmd)
    (cond ((eq? 'error? cmd) (eq? 'error type))
          ((eq? 'type cmd) type)
          ((eq? 'value cmd) value))))


(define (skip-delimiter code ind)
  ; return the index of the leftmost delimiter(space, newline)
  ; in the right side of `ind`-th character in `str`
  (cond ((>= ind (string-length code)) ind)
        ((memq (string-ref code ind) '(#\  #\newline))
            (skip-delimiter code (+ ind 1)))
        (else ind)))


(define (parse-term code ind)
  ; parameter: scheme expressions
  ; return: (a result of parsing the first expr . end index of the expr)
  ;         if the given expression is invalid, the end index is empty.
  (define (parse-empty code _ind)
    (let ((ind (skip-delimiter code _ind)))
      (and (< ind (- (string-length code) 1))
           (equal? #\( (string-ref code ind))
           (equal? #\) (string-ref code (+ ind 1)))
           (cons (make-result 'empty '()) (+ ind 2)))))

  (define (parse-bool code _ind)
    (let ((ind (skip-delimiter code _ind)))
      (and (< ind (- (string-length code) 1))
           (equal? #\# (string-ref code ind))
           (let ((ch (string-ref code (+ ind 1))))
             (cond ((equal? ch #\t) (cons (make-result 'bool #t) (+ ind 2)))
                   ((equal? ch #\f) (cons (make-result 'bool #f) (+ ind 2)))
                   (else (cons (make-result 'error "invalid syntax") ())))))))

  (define (parse-number code _ind)
    ; parameter: (possibly) the index of the first character in a number literal
    ; return (the value . the end index) or #f
    (define (scan ind acc)
      (if (>= ind (string-length code))
        (cons (make-result 'number acc) ind)
        (let ((ch (string-ref code ind)))
          (cond ((char-numeric? ch) (scan (+ ind 1)
                                          (+ (* 10 acc) (- (char->integer ch) 48))))
                ((memq ch (string->list " \n()'")) (cons (make-result 'number acc)
                                                         ind))
                (else (cons (make-result 'error "invalid identifier name") ()))))))

    (let ((first-ch (string-ref code (skip-delimiter code _ind))))
      (if (char-numeric? first-ch)
        (scan _ind 0)
        #f)))

  (define (parse-label code _ind)
    ; parameter: (possibly) the index of the first character in a label in string
    ; return: (the result and its end index) or #f
    (define (end-of-label code ind)
      ; parameter: the index of the first character in an identifier in string
      ; return: the end index of the id
      (define (char-label? ch)
        (or (char-alphabetic? ch)
            (char-numeric? ch)
            (member ch (string->list "!$%&*+-./<=>?@^_"))))

      (if (>= ind (string-length code))
        ind
        (let ((ch (string-ref code ind)))
          (if (char-label? ch) (end-of-label code (+ ind 1)) ind))))

    (let* ((ind (skip-delimiter code _ind))
           (ch (string-ref code ind)))
      (if (or
            (char-alphabetic?  ch)
            (member ch (string->list "!$%&*+-./<=>?@^_")))
        (let ((end-index (end-of-label code ind)))
          (cons (make-result 'label (string-copy code ind end-index))
                end-index))
        #f)))

  (define (parse-string code _ind)
    ; parameter: (possibly) the index of the beginning character of string literal
    ; return: (the the content of string and its end index) or #f
    (define (end-of-string code ind)
      (if (>= ind (string-length code))
        ()
        (let ((ch (string-ref code ind)))
          (cond ((and (equal? ch #\\)
                      (end-of-string code (+ ind 2)))) ; escape sequence
                ((equal? ch #\") ind)
                (else (end-of-string code (+ ind 1)))))))

    (let ((ind (skip-delimiter code _ind)))
      (if (and (< ind (string-length code))
               (equal? #\" (string-ref code ind)))
        (let ((end-index (end-of-string code (+ ind 1))))
          (if (null? end-index)
            (cons (make-result 'error
                               "EOT in a string literal")
                  ())
            (cons (make-result 'string
                               (string-copy code ind end-index))
                  (+ end-index 1))))
        #f)))

  (define (parse-expr code _index)
    ; parameter: (possibly) the index of the beginning parenthesized expression
    ; return: (the list of terms) or #f
    (define (parse-terms code _ind)
      (let ((ind (skip-delimiter code _ind)))
        (cond ((>= ind (string-length code)) (cons '() ind))
              ((equal? #\) (string-ref code ind)) (cons '() (+ ind 1)))
              (else
                (let* ((result (parse-term code ind))
                       (term (car result))
                       (next-ind (cdr result)))
                  (if (term 'error?)
                    result
                    (let* ((rest (parse-terms code next-ind))
                           (terms (car rest))
                           (last-ind (cdr rest)))
                      (cond ((null? terms) (cons (make-result 's-expr
                                                               (cons term '()))
                                                  last-ind))
                            ((terms 'error?) rest)
                            (else
                              (cons (make-result 's-expr (cons term terms))
                                    last-ind))))))))))

    (let ((index (skip-delimiter code _index)))
      (if (equal? #\( (string-ref code index))
        (parse-terms code (+ index 1))
        #f)))

  (define (parse-quote code _index)
    ; parameter: (possibly) the index of single quotation character
    ; return: (quoted term) or #f
    (let* ((ind (skip-delimiter code _index)))
      (if (equal? #\' (string-ref code ind))
        (let* ((result (parse-term code (+ ind 1)))
               (term (car result))
               (next-ind (cdr result)))
          (if (term 'error?)
            result
            (cons (make-result 'quote term)
                  next-ind)))
        #f)))

  (or 
      (parse-empty code ind)
      (parse-bool code ind)
      (parse-number code ind)
      (parse-label code ind)
      (parse-string code ind)
      (parse-expr code ind)
      (parse-quote code ind)))

