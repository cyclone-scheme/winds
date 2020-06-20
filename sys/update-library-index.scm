(define *default-library-index* "indexes/library-index.scm")

(define library-index '())

(define (libraries+exports . dir)
  (let ((work-dir (if (null? dir)
                      (*default-code-directory*)
                      (->path (car dir) (*default-code-directory*)))))
    (let-values (((sld-files _) (find-code-files-recursively work-dir)))
      (map (lambda (sld)
             (let ((content (read (open-input-file sld))))
               (list (lib:name content) (lib:exports content))))
           sld-files))))

(define (srfi? pkg)
  (string-contains (->string pkg) "srfi"))

(define (update-library-index! pkg)
  (begin
    (retrieve-package *index* pkg ".")
    (let* ((pkg-path (->path pkg))
           (lib+exps (if (srfi? pkg)
                         (parameterize ((*default-code-directory* "srfi"))
                           (libraries+exports pkg-path))
                         (libraries+exports pkg-path))))
      (delete! (->path pkg-path))
      (set! library-index (append library-index (list pkg lib+exps))))))

(define (write-library-index!) 
  (pretty-print library-index
                (open-output-file *default-library-index*)))

