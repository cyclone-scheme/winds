(define *default-definition-index* "indexes/definition-index.scm")

(define (exports+library+pkg pkg-name)
  (let ((work-dir (->path pkg-name (*default-code-directory*))))
    (let-values (((sld-files _) (find-code-files-recursively work-dir)))
      (map (lambda (sld)
             (let* ((content (with-input-from-file sld (lambda () (read))))
                    (lib-name (lib:name content)))
               (map (lambda (exp) (list exp lib-name pkg-name))
                    (lib:exports content))))
           sld-files))))

(define *definition-index* '())

(define (update-definition-index! pkg-name)
  (let ((exps+lib+pkg (if (srfi? pkg-name)
                          (parameterize ((*default-code-directory* "srfi"))
                            (exports+library+pkg pkg-name))
                          (exports+library+pkg pkg-name))))
    (set! *definition-index* (append *definition-index* (car exps+lib+pkg)))))

(define (write-definition-index!) 
  (with-output-to-file *default-definition-index*
    (lambda ()
      (pretty-print
       (list-sort (lambda (a b)
                    (string<? (symbol->string (car a))
                              (symbol->string (car b))))
                  *definition-index*)))))

