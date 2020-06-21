;; Many assumptions here:
;; - doc file must be "README.md"
;; - this doc file must be in package base directory

(define *default-wiki-directory* "wiki")
(define *default-wiki-file-extension* ".md")

(define wiki-header
  "**Contributions from the community are welcomed at the bottom of this file.**")

(define wiki-footer
  "\n\n*The above content was automatically generated on last Cyclone Winds update.*")

(define wiki-separator
  "---------------\n
# Contributions from the community\n\n")

(define (update-package-wiki! pkg-name)
  (let* ((work-dir (->path "." pkg-name))
         (doc-path (->path work-dir *default-doc-file*))
         (doc (if (file-exists? doc-path)
                  (file->string doc-path)
                  ""))
         (wiki-file-path (->path *default-wiki-directory*
                                 pkg-name
                                 *default-wiki-file-extension*))
         (wiki (if (file-exists? wiki-file-path)
                   (file->string wiki-file-path)
                   "")))
    (unless (or (string-contains wiki doc)
                (string=? "" doc))
      (let ((wiki-new-content
             (cond ((string=? "" wiki) doc)
                   ((let ((sep (string-contains wiki wiki-separator)))
                      (if sep
                          ;; If there is a separator we assume there may be
                          ;; contribution from the community...
                          (string-append wiki-header doc wiki-footer (string-copy wiki sep))
                          ;; We assume no separator = no contribution from the
                          ;; community, so the wiki can mirror the doc file...
                          (string-append wiki-header doc wiki-footer wiki-separator)))))))
        (pretty-print wiki-new-content (open-output-file wiki-file-path))))))
