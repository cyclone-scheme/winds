(import (scheme base)
        (scheme read)
        (scheme file)
        (only (scheme write) display)
        (scheme cyclone pretty-print)
        (scheme cyclone libraries)
        (srfi 28) ; basic format string
        (srfi 132) ; sort algorithms
        (libs metadata)
        (only (libs system-calls) delete!)
        (only (libs common) *default-code-directory* *default-doc-file*)
        (only (libs file) ->path)
        (only (libs index) get-index)
        (only (libs util) ->string string-contains string-join)
        (only (libs package) find-code-files-recursively retrieve-package))

(include "sys/update-wiki-index.scm")
(include "sys/update-packages-wiki.scm")
(include "sys/update-library-index.scm")
(include "sys/update-definition-index.scm")

(define (file->string file)
  (with-input-from-file file
    (lambda ()
      (let lp ((content ""))
        (let ((r (read-line)))
          (if (eof-object? r)
              (string-copy content (min (string-length content) 1)) ; hack to remove first "\n"
              (lp (string-append content "\n" r))))))))

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
                (update-definition-index! pkg)
                (delete! (->path pkg)))
              *pkg-list*)
    (write-wiki-index!)
    (display (format "Wiki index updated.~%"))
    (write-library-index!)
    (display (format "Library index updated.~%"))
    (write-definition-index!)
    (display (format "Definition index updated.~%"))))

(main)
