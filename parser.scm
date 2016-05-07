; parser.scm

; parse result value
(define (make-result type value)
  (lambda (cmd)
    (cond ((eq? 'error? cmd) (eq? 'error type))
          ((eq? 'type cmd) type)
          ((eq? 'value cmd) value))))

(define (skip-delimiter code ind)
  ; return the index of the leftmost delimiter(space, newline)
  ; in the right side of `ind`-th character in `str`
  (cond ((>= ind (string-length code)) ind)
        ((memq (string-ref code ind) '(#\  #\n))
            (skip-delimiter code (+ ind 1)))
        (else ind)))

(define (get-label code _ind)
  ; parameter: (possibly) the index of the first character in a label in string
  ; return: (its result and its end index) or #f
  (define (end-of-label code ind)
    ; parameter: the index of the first character in an identifier in string
    ; return: the end value of the id
    (define (char-label? ch)
      (or (char-alphabetic? ch)
          (char-numeric? ch)
          (member ch (string->list "!$%&*+-./<=>?@^_"))))

    (if (>= ind (string-length code)) ind
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


(define (get-string code _ind)
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

(define (get-s-expr code _index)
  ; parameter: (possibly) the index of the beginning parenthesized expression
  ; return: (the list of terms) or #f
  (let ((index (skip-delimiter code _index)))
    (if (equal? #\( (string-ref code index))
      (get-terms code (+ index 1))
      #f)))

(define (get-quote code _index)
  ; parameter: (possibly) the index of single quotation character
  ; return: (quoted term) of #f
  (let* ((ind (skip-delimiter code _index)))
    (if (equal? #\' (string-ref code ind))
      (let* ((result (get-term code (+ ind 1)))
             (term (car result))
             (next-ind (cdr result)))
        (if (term 'error?)
          result
          (cons (make-result 'quote term)
                next-ind)))
      #f )))

(define (get-term code ind)
  (or 
      (get-label code ind)
      (get-string code ind)
      (get-s-expr code ind)
      (get-quote code ind)))

(define (get-terms code _ind)
  (let ((ind (skip-delimiter code _ind)))
    (cond ((>= ind (string-length code)) (cons (make-result 'const  ())
                                               ind))
          ((equal? #\) (string-ref code ind)) (cons (make-result 'const ())
                                                   (+ ind 1)))
          (else
            (let* ((result (get-term code ind))
                   (term (car result))
                   (next-ind (cdr result)))
              (if (term 'error?) result
                (let* ((rest (get-terms code next-ind))
                       (terms (car rest))
                       (last-ind (cdr rest)))
                  (if (terms 'error?) rest
                    (cons (make-result 's-expr
                                       (cons term terms))
                          last-ind)))))))))
