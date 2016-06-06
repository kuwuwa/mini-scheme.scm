; parser.scm

(define (delimiter? ch) (memq ch '(#\  #\newline)))


(define (skip-delimiter code ind)
  ; return the index of the leftmost delimiter(space, newline)
  ; in the right side of `ind`-th character in `str`
  (cond ((>= ind (string-length code)) ind)
        ((delimiter? (string-ref code ind)) (skip-delimiter code (+ ind 1)))
        (else ind)))


(define (parse-term code _ind)
  ; parameter: scheme expressions
  ; return: (a result of parsing the first expr . end index of the expr)
  ;         if the given expression is invalid, the end index is empty.

  (define invalid-syntax (cons (v/t 'error "invalid syntax") ()))

  (define (skip-comment code _ind)
    (define (find-eol code ind)
      (cond ((>= ind (string-length code)) ind)
            ((equal? #\newline (string-ref code ind)) (+ ind 1))
            (else (find-eol code (+ ind 1)))))

    (let* ((ind (skip-delimiter code _ind)))
      (and (< ind (string-length code))
           (equal? #\; (string-ref code ind))
           (parse-term code (find-eol code (+ ind 1))))))

  (define (parse-char code _ind)
    (define (end-of-symbol code ind)
      ; parameter: the index of the first character in an identifier in string
      ; return: the end index of the id
      (define (char-symbol? ch)
        (or (char-alphabetic? ch)
            (char-numeric? ch)
            (member ch (string->list "!$%&*+-./<=>?@^_"))))

      (if (>= ind (string-length code))
        ind
        (let ((ch (string-ref code ind)))
          (if (char-symbol? ch) (end-of-symbol code (+ ind 1)) ind))))

    (let ((ind (skip-delimiter code _ind)))
      (and (< ind (- (string-length code) 2))
           (equal? #\# (string-ref code ind))
           (equal? #\\ (string-ref code (+ ind 1)))
           (let* ((end (end-of-symbol code (+ ind 3)))
                  (label (string-copy code (+ ind 2) end)))
             (cond ((= 1 (string-length label)) (cons (v/t 'char (car (string->list label)))
                                               end))
                   ((equal? label "newline") (cons (v/t 'char #\newline) end))
                   (else (cons (v/t 'error "unknown character name") ())))))))
    
  (define (parse-dot code _ind)
    (let ((ind (skip-delimiter code _ind)))
      (and (< ind (string-length code))
           (equal? #\. (string-ref code ind))
           (if (or (>= ind (- (string-length code) 1))
                   (memq (string-ref code (+ ind 1)) (string->list " \n()'")))
             (cons (v/t 'dot ".") (+ ind 1))
             invalid-syntax))))

  (define (parse-bool code _ind)
    (let ((ind (skip-delimiter code _ind)))
      (and (< ind (string-length code))
           (equal? #\# (string-ref code ind))
           (if (or (>= ind (- (string-length code) 1))
                   (and (< ind (- (string-length code) 2))
                        (not (memq (string-ref code (+ ind 2))
                                   (string->list " \n()'")))))
             invalid-syntax
             (let ((ch (string-ref code (+ ind 1))))
               (cond ((equal? ch #\t) (cons (v/t 'bool #t) (+ ind 2)))
                     ((equal? ch #\f) (cons (v/t 'bool #f) (+ ind 2)))
                     (else (cons (v/t 'error "invalid syntax") ()))))))))

  (define (parse-number code _ind)
    (define (string->integer str)
      (let loop ((ind 0) (acc 0))
        (if (>= ind (string-length str))
          acc
          (loop (+ ind 1)
                (+ (* 10 acc)
                   (- (char->integer (string-ref str ind)) 48))))))

    (define (scan ind)
      (if (>= ind (string-length code))
        ind
        (let ((ch (string-ref code ind)))
          (cond ((char-numeric? ch) (scan (+ ind 1)))
                ((memq ch (string->list " \n()'")) ind)
                (else #f)))))

    (let ((ind (skip-delimiter code _ind)))
      (and
        (char-numeric? (string-ref code ind))
        (let ((end-ind (scan ind)))
          (if end-ind
            (cons (v/t 'number (string->integer (string-copy code ind end-ind)))
                  end-ind)
            (cons (v/t 'error "invalid identifier name") ()))))))

  (define (parse-symbol code _ind)
    ; parameter: (possibly) the index of the first character in a symbol in string
    ; return: (the result and its end index) or #f
    (define (end-of-symbol code ind)
      ; parameter: the index of the first character in an identifier in string
      ; return: the end index of the id
      (define (char-symbol? ch)
        (or (char-alphabetic? ch)
            (char-numeric? ch)
            (member ch (string->list "!$%&*+-./<=>?@^_"))))

      (if (>= ind (string-length code))
        ind
        (let ((ch (string-ref code ind)))
          (if (char-symbol? ch) (end-of-symbol code (+ ind 1)) ind))))

    (let* ((ind (skip-delimiter code _ind))
           (ch (string-ref code ind)))
      (if (or
            (char-alphabetic?  ch)
            (member ch (string->list "!$%&*+-./<=>?@^_")))
        (let ((end-index (end-of-symbol code ind)))
          (cons (v/t 'symbol (string->symbol (string-copy code ind end-index)))
                end-index))
        #f)))

  (define (parse-string code _ind)
    ; parameter: (possibly) the index of the beginning character of string literal
    ; return: (the the content of string and its end index) or #f

    (define (take-string code ind)
      (if (>= ind (string-length code))
        (cons (v/t 'error "EOF in a string literal") ())
        (let ((ch (string-ref code ind)))
          (cond ((equal? ch #\") (cons (v/t 'list ()) (+ ind 1)))
                ((equal? ch #\\)
                 (let ((rest (take-string code (+ ind 2))))
                   (if ((car rest) 'error?)
                     rest
                     (if (equal? #\n (string-ref code (+ ind 1)))
                       (cons (v/t 'list (cons #\newline ((car rest) 'value)))
                             (cdr rest))
                       (cons (v/t 'error (string-append "undefined character"))
                             ())))))
                (else
                 (let ((rest (take-string code (+ ind 1))))
                   (if ((car rest) 'error?)
                     rest
                     (cons (v/t 'list (cons (string-ref code ind)
                                            ((car rest) 'value)))
                           (cdr rest)))))))))

    (let ((ind (skip-delimiter code _ind)))
      (and (< ind (string-length code))
           (equal? #\" (string-ref code ind))
           (let* ((result (take-string code (+ ind 1)))
                  (result-list (car result))
                  (end-ind (cdr result)))
             (if (result-list 'error?)
               result
               (cons (v/t 'string (list->string (result-list 'value)))
                     end-ind))))))

  (define (parse-list code _index)
    ; parameter: (possibly) the index of the beginning parenthesized expression
    ; return: (the list of terms) or #f
    (define (parse-terms code _ind)
      (let ((ind (skip-delimiter code _ind)))
        (cond ((>= ind (string-length code)) (cons (v/t 'error "EOF inside a list")
                                                   'non-eot))
              ((equal? #\) (string-ref code ind)) (cons (v/t 'empty ()) (+ ind 1)))
              (else
                (let* ((result (parse-term code ind))
                       (term (car result))
                       (next-ind (cdr result)))
                  (cond ((term 'error?) result)
                        ((eq? 'dot (term 'type))
                          (let* ((last (parse-term code next-ind))
                                 (last-term (car last))
                                 (last-ind (cdr last)))
                            (if (last-term 'error?)
                              last-term
                              (let ((ret-ind (skip-delimiter code last-ind)))
                                (if (or (>= ret-ind (string-length code))
                                        (equal? #\) (string-ref code ret-ind)))
                                  (cons last-term (+ 1 ret-ind))
                                  (cons (v/t 'error "bad dot syntax")
                                        ()))))))
                        (else
                          (let* ((rest (parse-terms code next-ind))
                                 (terms (car rest))
                                 (last-ind (cdr rest)))
                            (cond ((terms 'error?) rest)
                                  ((memq (terms 'type) '(list empty))
                                    (cons (v/t 'list (cons term (terms 'value)))
                                          last-ind))
                                  (else
                                    (cons (v/t 'list (cons term terms))
                                          last-ind)))))))))))


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
            (cons (v/t 'quote term) next-ind)))
        #f)))

  (let ((ind (skip-delimiter code _ind)))
    (if (>= ind (string-length code))
      (cons (v/t 'none ()) ind)
      (or
        (skip-comment code ind)
        (parse-dot code ind)
        (parse-char code ind)
        (parse-bool code ind)
        (parse-number code ind)
        (parse-symbol code ind)
        (parse-string code ind)
        (parse-list code ind)
        (parse-quote code ind)
        (cons (v/t 'error "invalid syntax") ())))))

