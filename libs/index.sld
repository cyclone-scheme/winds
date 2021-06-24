(define-library (libs index)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (only (scheme write) display write)
          (cyclone and-let*)
          (cyclone format) 
          (cyclone match)
          (libs common)
          (libs file) 
          (libs semantic)
          (libs system-calls)
          (libs util))
  (export get-index
          pkg-info
          get-local-index
          local-index-contains?
          register-installed-package!
          unregister-installed-package!)
  (begin
    ;; Global index.scm has the following format:
    ;;
    ;; (cyclone-packages
    ;;  (pkg1-name
    ;;   ("0.2.0" "url-to-package.scm" "url-to-tarball" "tarball-sha256sum")
    ;;   ("0.1.9" "url-to-package.scm" "url-to-tarball" "tarball-sha256sum"))
    ;;  (pkg2-name
    ;;   ("0.9.3" "url-to-package.scm" "url-to-tarball" "tarball-sha256sum")
    ;;   ("0.8.7" "url-to-package.scm" "url-to-tarball" "tarball-sha256sum"))
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

    (define (pkg-info index name/maybe-version)
      (define (pkg-versions)
        (match (assoc name/maybe-version index)
          (#f #f)
          ((pkg-name versions ..1) versions)
          (else
           (error (format "Could not find versions for package ~s~%" name/maybe-version)))))

      (define (split-name-version)
        (or (and-let* ((len (string-length name/maybe-version))
                       ((> len 0))
                       (dash-position (string-find-right name/maybe-version #\-))
                       ((> dash-position 0))
                       (name (substring name/maybe-version 0 dash-position))
                       (version (substring name/maybe-version (+ 1 dash-position) len)))
              (values name version))
            (values #f #f)))

      (let ((versions (pkg-versions index name/maybe-version)))
        (if versions
            ;; 'name/maybe-version' contains a valid package name, but no version. Get latest...
            (assoc (latest-version (map car versions)) versions) 
            ;; Try to extract name and version from 'name/maybe-version', e.g. 'pkg-name-0.1.2"
            (let-values (((name version) (split-name-version)))
              (or (and-let* ((versions (pkg-versions index name)))
                    (or (assoc (find-version version (map car versions)) versions)
                        (error (format "Could not find version ~s of package ~s~%" version name))))
                  (error (format "Could not find package by name: ~s~%" name)))))))

    ;; Local index has the following format:
    ;;     PKG-NAME   PKG-VERSION    CYCLONE-VERSION        LIBRARIES             PROGRAMS
    ;; ((   pkg1        "0.8.3"         "1.11.3"       ((cyclone libX) ...)    ((progamX) ...))
    ;;  (   pkg2        "0.2.1"         "1.11.0"       ((cyclone libY) ...)    ((progamY) ...))
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
