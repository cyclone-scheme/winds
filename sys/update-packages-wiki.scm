;; Some assumptions here:
;; - doc file must be "README.md"
;; - doc file must be in package base directory

(define *default-wiki-directory* "wiki")
(define *default-wiki-file-extension* ".md")

(define wiki-header
  (string-append
   "*Content automatically generated. "
   "Contributions from the community are very welcomed at the **bottom** of this file.*\n\n"))

(define wiki-separator
  (string-append
   "---------------"
   "# Contributions from the community:\n\n"))

(define (update-package-wiki! pkg-name)
  (let* ((work-dir (->path "." pkg-name))
         (doc-path (->path work-dir *default-doc-file*))
         (doc (if (file-exists? doc-path)
                  (file->string doc-path)
                  ""))
         (wiki-file-path (->path *default-wiki-directory*
                                 (string-append (->string pkg-name)
                                                *default-wiki-file-extension*)))
         (wiki (if (file-exists? wiki-file-path)
                   (file->string wiki-file-path)
                   "")))
    (if (and (string-contains wiki doc)
             (> (string-length wiki)
                (string-length doc)))
        (display (format "Wiki of package ~a is up-to-date. Skipping...~%" pkg-name))              
        (let* ((sep (string-contains wiki wiki-separator))
               (wiki-new-content 
                (cond (sep
                       ;; If there is the separator we assume there is
                       ;; contribution from the community...
                       (string-append wiki-header doc (string-copy wiki sep)))
                      ;; We assume no separator = no contribution from the
                      ;; community, so the wiki can mirror the doc file if
                      ;; it exists...
                      ((string=? "" doc)
                       (string-append wiki-header
                                      "No documentation provided.\n\n"
                                      "Please consider contacting the package author.\n\n"
                                      "Back to [index](" *default-wiki-url* ").\n\n"))
                      (else
                       (string-append wiki-header doc wiki-separator)))))
          (display wiki-new-content (open-output-file wiki-file-path))
          (display (format "Wiki of package ~a updated.~%" pkg-name))))))
