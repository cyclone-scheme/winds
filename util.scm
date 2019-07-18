(define (random-temp-dir . prefix)
  (let ((temp-dir (or (get-environment-variable "TMPDIR")
                      (get-environment-variable "TEMP")
                      (get-environment-variable "TMP")
                      "/tmp")))
    (x->path temp-dir
             (string-append (if (null? prefix) "" (car prefix))
                            (number->string (random-integer 10000000000000000000))))))

(define (remove pred lst)
  (filter (lambda (x) (not (pred x))) lst))

(define (assoc-all-occurences symbol alist)
  (filter (lambda (e)
            (eq? symbol (car e)))
          alist))

;; String procedures imported and adapted from Chibi Scheme
(define (trim-trailing-slash s)
  (let* ((len (string-length s))
         (last (- len 1)))
    (if (eqv? #\/ (string-ref s last))
        (substring s 0 last)
        s)))

(define (x->string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        ((list? x) (string-join x #\/))
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
  (let ((start 0)
        (end (- (string-length str) 1)))
    (let lp ((end end))
      (cond ((eq? start end) start)
            ((char=? char (string-ref str end)) end)
            (else (lp (- end 1)))))))

;; Convert command-line string list into proper list
;; i.e. "(crypto md5)" -> (crypto md5)
(define (string->proper-symbol s)
  (let* ((len (string-length s))
         (first-char (string-ref s 0))
         (last-char (string-ref s (- len 1))))
    (if (and (equal? first-char #\( ) (equal? last-char #\) ))
        (map string->symbol (string-split (substring s 1 (- len 1)) #\space))
        (string->symbol s))))

;; Path procedures imported and adapted from Chibi Scheme
(define (path-dir path)
  (if (string=? path "")
      "."
      (let* ((start 0)
             (trimmed-string (trim-trailing-slash path))
             (end (- (string-length trimmed-string) 1)))
        (if (eq? start end)
            "/"
            (let ((slash (string-find-right path #\/)))
              (if (eq? start slash)
                  "."
                  (let ((start2 (string-length (substring path start slash))))
                    (if (eq? start start2)
                        "/"
                        (substring path start start2)))))))))

(define (x->path . args)
  (if (null? args)
      ""
      (let* ((args0 (x->string (car args)))
             (start (trim-trailing-slash args0)))
        (let lp ((ls (cdr args))
                 (res (if (string=? "" start) '() (list start))))
          (cond
           ((null? ls)
            (if (and (null? res) (not (string=? "" args0)))
                "/"
                (string-join (reverse res))))
           ((pair? (car ls))
            (lp (append (car ls) (cdr ls)) res))
           (else
            (let ((x (trim-trailing-slash (x->string (car ls)))))
              (cond
               ((string=? x "")
                (lp (cdr ls) res))
               ((eqv? #\/ (string-ref x 0))
                (lp (cdr ls) (cons x res)))
               (else
                (lp (cdr ls) (cons x (cons "/" res))))))))))))

