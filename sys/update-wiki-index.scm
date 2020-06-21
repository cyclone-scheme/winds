(define *default-wiki-directory* "wiki")
(define *default-wiki-home-file* (->path *default-wiki-directory* "Home.md"))

(define home-header
  "# Welcome to the cyclone-winds wiki!\n
If you want to **contribute**, please take a look at the [Roadmap](roadmap).\n
Packages available:\n
|Package|Description|License|Author|Maintainer|Version|\n")

(define home-footer
  "\n\n*The above content was automatically generated on last Cyclone Winds update.*")

(define (get-info-row pkg-name)
  (let* ((work-dir (->path "." pkg-name))
         (md-path (->path work-dir *default-metadata-file*))
         (pkg (metadata->pkg (read (open-input-file md-path)))))
    (string-append "|"
                   (string-join (list (->string (get-name pkg))
                                      (get-description pkg)
                                      (get-license pkg)
                                      (get-authors pkg)
                                      (get-maintainers pkg)
                                      (->string (get-version pkg)))
                                "|")
                   "|")))

(define wiki-index "")

(define (update-wiki-index! pkg-name)
  (let ((info-row (if (srfi? pkg-name)
                      (parameterize ((*default-code-directory* "srfi"))
                        (get-info-row pkg-name))
                      (get-info-row pkg-name))))
    (set! wiki-index (string-append wiki-index info-row))))

(define (write-wiki-index!) 
  (pretty-print (string-append home-header wiki-index home-footer)
                (open-output-file *default-wiki-home-file*)))
