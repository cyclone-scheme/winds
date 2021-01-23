(define-library (libs index)
  (import (scheme base)
          (scheme read)
          (only (scheme write) display write)
          (scheme file)
          (srfi 28) ; Basic format strings
          (cyclone match)
          (libs common)
          (libs file)
          (libs system-calls)
          (libs util))
  (export get-index
          pkg-info
          get-local-index
          local-index-contains?
          register-installed-package!
          unregister-installed-package!)
  (begin
    ;; Global index.scm has the format bellow. Note that package
    ;; latest version is always at first position and atm there is
    ;; no way to specify a version to install.
    ;;
    ;; (cyclone-packages
    ;;  (pkg1-name
    ;;   (0.2 "url-to-package.scm" "url-to-tarball" "tarball-sha256sum")
    ;;   (0.1 "url-to-package.scm" "url-to-tarball" "tarball-sha256sum"))
    ;;  (pkg2-name
    ;;   (0.9 "url-to-package.scm" "url-to-tarball" "tarball-sha256sum")
    ;;   (0.8 "url-to-package.scm" "url-to-tarball" "tarball-sha256sum"))
    ;;  ...)
    (define *default-index-url*
      "https://raw.githubusercontent.com/cyclone-scheme/winds/master/indexes/index.scm")

    (define (get-index)
      (let* ((tmp-dir (random-temp-dir))
             (index-path (->path tmp-dir "index.scm")))
        (make-dir! tmp-dir)
        (display (format "Retrieving index file...~%"))
        (download! *default-index-url* index-path)
        (let ((content (cdr (read (open-input-file index-path)))))
          (delete! tmp-dir)
          content)))

    (define (pkg-info index pkg-name)
      (match (assoc pkg-name index)
        (#f (error (format "Could not locate package by name: ~s~%" pkg-name)))
        ((pkg-name latest-version old-versions ...) latest-version)))

    
    ;; Local index has the following format:
    ;;      PKG-NAME   PKG-VERSION   CYCLONE-VERSION       LIBS                 PROGS
    ;; (((cyclone pkg1)   0.8           "1.11.3"   ((cyclone libX) ...)    ((progamX) ...))
    ;;  ((cyclone pkg2)   0.2           "1.11.0"   ((cyclone libY) ...)    ((progamY) ...))
    ;;  ...)
    (define *default-local-index*
      (->path (get-library-installation-dir) (*default-code-directory*) "winds-index.scm"))

    ;; Does the local index contain a record for given package/version/compiler?
    (define (local-index-contains? index name pkg-ver cyc-ver)
      (define (->num obj)
        (cond
         ((string? obj)
          (string->number obj))
         ((number? obj)
          obj)
         (else
          (error "Unable to convert to number" obj))))

      (let ((metadata (assoc name index)))
        (cond
         ((pair? metadata)
          (let ((idx-pkg-ver (cadr metadata))
                (idx-cyc-ver (caddr metadata)))
            (and (>= (->num idx-pkg-ver) (->num pkg-ver))
                 (>= (->num idx-cyc-ver) (->num cyc-ver)))))
         (else #f))))

    (define (get-local-index)
      (if (file-exists? *default-local-index*)
          (read (open-input-file *default-local-index*))
          '()))

    (define (register-installed-package! name version cyc-version libs progs)
      (let ((local-index (get-local-index)))
        (with-output-to-file *default-local-index*
          (lambda ()
            (write (cons (list name version cyc-version libs progs)
                         (remove (lambda (pkg)
                                   (equal? (car pkg) name))
                                 local-index)))))
        (display (format "Package ~a ~a successfuly installed with Cyclone ~a.~%" name version cyc-version))))

    (define (unregister-installed-package! name)
      (let* ((local-index (get-local-index)))
        (with-output-to-file *default-local-index*
          (lambda ()
            (write (remove (lambda (pkg)
                             (equal? (car pkg) name))
                           local-index))))
        (display (format "Package ~a successfuly uninstalled.~%" name))))))
