(define *default-wiki-directory* "wiki")
(define *default-wiki-home-file* (->path *default-wiki-directory* "Home.md"))
(define *default-wiki-url* "https://github.com/cyclone-scheme/winds/wiki/")

(define home-header
  "# Welcome to the Cyclone Winds wiki!\n
If you want to **contribute**, please take a look at the [Roadmap](roadmap).\n
Packages available:\n
|Package|Description|License|Author(s)|Maintainer(s)|Version|
|-------|-----------|:-----:|---------|-------------|:-----:|\n")

(define home-footer
  "\n\n*The above content was automatically generated on last Cyclone Winds update.*")

(define (get-info-row pkg-name)
  (let* ((work-dir (->path "." pkg-name))
         (md-path (->path work-dir *default-metadata-file*))
         (pkg (metadata->pkg (cdr (with-input-from-file md-path (lambda () (read)))))))
    (string-append "|"
                   (string-join (list (let ((name (->string (get-name pkg))))
                                        (string-append "[" name "](" *default-wiki-url* name ")"))
                                      (get-description pkg)
                                      (get-license pkg)
                                      (get-authors pkg)
                                      (get-maintainers pkg)
                                      (->string (get-version pkg)))
                                "|")
                   "|\n")))

(define wiki-index "")

(define (update-wiki-index! pkg-name)
  (let ((info-row (if (srfi? pkg-name)
                      (parameterize ((*default-code-directory* "srfi"))
                        (get-info-row pkg-name))
                      (get-info-row pkg-name))))
    (set! wiki-index (string-append wiki-index info-row))))

(define (write-wiki-index!) 
  (with-output-to-file *default-wiki-home-file*
    (lambda ()
      (display (string-append home-header wiki-index home-footer)))))
