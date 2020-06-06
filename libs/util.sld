(define-library (libs util)
  (import (scheme base)
          (scheme cyclone util))
  (export get-parameter-value
          get-parameter-all-occurrences
          trim-trailing-slash
          ->string
          string-join
          string-find-right
          string-contains
          string->proper-symbol)
  (begin
    (define (remove pred lst)
      (filter (lambda (x) (not (pred x))) lst))

    (define (remove-member mem lst)
      (filter (lambda (e)
                (not (equal? e mem)))
              lst))

    ;; Returns a list or false
    (define (get-parameter-value param metadata)
      (let ((param (assoc param metadata)))
        (if (and param (not (null? param)))
            (cadr param)
            #f)))

    ;; Returns a list
    (define (get-parameter-all-occurrences param metadata)
      (filter (lambda (e)
                (eq? param (car e)))
              metadata))

    ;; String procedures imported and adapted from Chibi Scheme
    (define (trim-trailing-slash str)
      (if (string=? str "")
          ""
          (let* ((len (string-length str))
                 (last (- len 1)))
            (if (eqv? #\/ (string-ref str last))
                (substring str 0 last)
                str))))

    (define (->string x)
      (cond ((string? x) x)
            ((symbol? x) (symbol->string x))
            ((number? x) (number->string x))
            ((null? x) "")
            (else (error "Could not convert to string" x))))

    (define (string-join orig-ls . o)
      (let ((sep (if (pair? o) (car o) ""))
            (out (open-output-string)))
        (let lp ((ls orig-ls))
          (cond
           ((pair? ls)
            (if (and sep (not (eq? ls orig-ls)))
                (write-string sep out))
            (write-string (car ls) out)
            (lp (cdr ls)))))
        (get-output-string out)))

    (define (string-find-right str char)
      (let ((len (string-length str)))
        (if (= 0 len)
            0
            (let ((start 0)
                  (end (- (string-length str) 1)))
              (let lp ((end end))
                (cond ((eq? start end) start)
                      ((char=? char (string-ref str end)) end)
                      (else (lp (- end 1)))))))))

    (define (string-contains a b . o)   ; very slow
      (let ((alen (string-length a))
            (blen (string-length b)))
        (let lp ((i (if (pair? o) (car o) 0)))
          (and (<= (+ i blen) alen)
               (if (string=? b (substring a i (+ i blen)))
                   i
                   (lp (+ i 1)))))))

    (define (list->string lst)
      (string-join (map ->string lst) " "))

    ;; Convert command-line string list into proper list
    ;; i.e. "(crypto md5)" -> (crypto md5)
    (define (string->proper-symbol s)
      (let* ((len (string-length s))
             (first-char (string-ref s 0))
             (last-char (string-ref s (- len 1))))
        (if (and (equal? first-char #\( ) (equal? last-char #\) ))
            (map string->symbol (string-split (substring s 1 (- len 1)) #\space))
            (string->symbol s))))
    ))
