(define *default-library-index* "indexes/library-index.scm")

(define (libraries+exports pkg-name)
  (let ((work-dir (->path pkg-name (*default-code-directory*))))
    (let-values (((sld-files _) (find-code-files-recursively work-dir)))
      (map (lambda (sld)
             (let ((content (read (open-input-file sld))))
               (list (lib:name content) (lib:exports content))))
           sld-files))))

(define library-index '())

(define (update-library-index! pkg-name)
  (let ((lib+exps (if (srfi? pkg-name)
                      (parameterize ((*default-code-directory* "srfi"))
                        (libraries+exports pkg-name))
                      (libraries+exports pkg-name))))
    (set! library-index (append library-index (list pkg-name lib+exps)))))

(define (write-library-index!) 
  (pretty-print library-index
                (open-output-file *default-library-index*)))

