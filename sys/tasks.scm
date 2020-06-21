(import (scheme base)
        (scheme read)
        (scheme cyclone pretty-print)
        (scheme cyclone libraries)
        (srfi 132)
        (libs metadata)
        (only (libs system-calls) delete!)
        (only (libs common) *default-code-directory* *default-doc-file*)
        (only (libs file) ->path)
        (only (libs index) get-index)
        (only (libs util) ->string string-contains)
        (only (libs package) find-code-files-recursively retrieve-package))

(include "sys/update-wiki-index.scm")
(include "sys/update-packages-wiki.scm")
(include "sys/update-library-index.scm")

(define (file->string file)
  (define file-port (open-input-file file))
  (let lp ((content ""))
    (let ((r (read-line file-port)))
      (if (eof-object? r)
          (string-copy content 1) ; hack to remove first "\n"
          (lp (string-append content "\n" r))))))

(define (srfi? pkg)
  (string-contains (->string pkg) "srfi"))

(define *index* (get-index))

(define *pkg-list*
  (list-sort (lambda (a b)
               (string<? (symbol->string a)
                         (symbol->string b)))
             (map car *index*)))

(define (main)
  (begin
    (for-each (lambda (pkg)
                (retrieve-package *index* pkg ".")
                (update-wiki-index! pkg)
                (update-package-wiki! pkg)
                (update-library-index! pkg)
                (delete! (->path pkg-path)))
              *pkg-list*)
    (write-wiki-index!)
    (write-library-index!)))

(main)
