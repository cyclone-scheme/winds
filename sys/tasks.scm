(import (scheme base)
        (scheme read)
        (scheme cyclone pretty-print)
        (scheme cyclone libraries)
        (libs system-calls)
        (only (libs common) *default-code-directory*)
        (only (libs file) ->path)
        (only (libs index) get-index)
        (only (libs util) ->string string-contains)
        (only (libs package) find-code-files-recursively retrieve-package))

(include "sys/update-library-index.scm")
(include "sys/update-wiki-index.scm")
(include "sys/update-packages-wikis.scm")

(define *index* (get-index))

;; (define finish-hook '())

;; (define (hook-add! hook proc)
;;   (set! hook (append hook proc)))

;; (define (hook-run hook . args)
;;   (for-each (lambda (proc) (apply proc args))
;;             hook))

(define (main)
  (let ((all-pkgs (map car *index*)))
    (for-each (lambda (pkg)
                (update-library-index! pkg)
                (update-wiki-index! pkg)
                (update-packages-wikis! pkg))
              all-pkgs)
    (write-library-index!)))

(main)
