;; Simple utility to attempt to install every package from the index
(import (scheme base)
        (scheme read)
        (scheme write))

(let* ((fp (open-input-file "index.scm"))
       (contents (read-all fp)))
  (for-each
    (lambda (pkg)
      (system (string-append "sudo cyclone-winds install " (symbol->string pkg))))
    (map car (cdar contents))))
