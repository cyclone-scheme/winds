(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme process-context)
        (scheme cyclone pretty-print)
        (scheme cyclone util)
        (srfi 27)  ;; random numbers
        (srfi 28)  ;; basic format strings
        (cyclone match)) 

(include "system-calls.scm")
(include "path.scm")

(define (random-temp-dir . prefix)
  (let ((temp-dir (or (get-environment-variable "TMPDIR")
                      (get-environment-variable "TEMP")
                      (get-environment-variable "TMP")
                      "/tmp")))
    (x->path temp-dir
          (string-append (if (null? prefix) "" (car prefix))
                         (number->string (random-integer 10000000000000000000))))))

;; Index-related procedures

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

(define *default-index-url* "https://raw.githubusercontent.com/cyclone-scheme/cyclone-winds/master/index.scm")

(define (get-index url)
  (let* ((tmp-dir (random-temp-dir))
         (index-path (x->path tmp-dir "index.scm")))
    (make-dir tmp-dir)
    (display "~%Retrieving index file...~%")
    (download url index-path)
    (let ((content (read (open-input-file index-path))))
      (delete tmp-dir)
      content)))

(define (pkg-info index pkg-name)
  (match (assoc pkg-name (cdr index))
    (#f (error (format "Could not locate package by name: ~a~%" name)))
    ((pkg-name latest-version old-versions ...) latest-version)))

(define (version pkg-info)
  (car pkg-info))

(define (metadata-url pkg-info)
  (cadr pkg-info))

(define (tarball-url pkg-info)
  (caddr pkg-info))

(define (sha256sum pkg-info)
  (cadddr pkg-info))

;; Local index
(define *default-local-index-file*
  (x->path (*library-installation-dir*) "local-index.scm"))

;; End of index-related procedures


;; Metadata-related procedures (i.e. package.scm)
(define *default-metadata-file* "package.scm")

(define (keys alist)
  (map car alist))

;; TODO: review this validation strategy - seems too naive.
(define mandatory-parameters
  `((version ,(list number?))
    (license ,(list string?))
    (authors ,(list string?))
    (maintainers ,(list string?))
    (description ,(list string?))
    (tags ,(list string?))
    (docs ,(list string?))))

(define optional-parameters
  `((dependencies ,(list string?))
    (test-dependencies ,(list string?))
    (foreign-dependencies ,(list string?))))

;; TODO: implement 'custom' parameter for specilized installation.
(define code-parameters `((library ,(list list?))
                          (program ,(list symbol?))))

(define essential-parameters (append mandatory-parameters code-parameters))
(define available-parameters (append essential-parameters optional-parameters))

(define (valid-metadata? metadata)
  (for-each
   (lambda (key)
     (let ((parameter-content (cdr (assq key metadata)))
           (check-procedures (cadr (assq key available-parameters))))
       (cond
        ;; Unknown parameters
        ((not (member key (keys available-parameters)))
         (error "Unknown parameter in package.scm" key))
        ;; Empty mandatory parameters
        ((and (member key (keys essential-parameters))
              (null? parameter-content))
         (error `(,key " should not be null in package.scm")))
        ;; Check parameter type
        ((for-each
          (lambda (proc)
            (if (not (null? parameter-content))
                (if (not (apply proc parameter-content))
                    (error
                     (format "~a is expected to be of a type that satisfies ~a~%"
                             key proc)))))
          check-procedures)))))
   (keys metadata))
  (for-each
   (lambda (param)
     (if (not (member param (keys metadata)))
         (error param "is a mandatory parameter in package.scm")))
   (keys mandatory-parameters))
  (let ((keys (keys metadata)))
    (if (not (or (member 'library keys)
                 (member 'program keys)))
        (error "At least one library/program must be defined in package.scm.")))
  #t) ;; returns gracefully if everything is ok

(define (assoc-all-occurences symbol alist)
  (filter (lambda (e)
            (eq? symbol (car e)))
          alist))

(define (libraries-list metadata)
  (match (assoc-all-occurences 'library (cdr metadata))
    (() '())
    (lst (map cadadr lst))))

(define (programs-list metadata)
  (match (assoc-all-occurences 'program (cdr metadata))
    (() '())
    (lst (map cadadr lst))))

(define (dependencies-list metadata)
  (assoc 'dependencies (cdr metadata)))
;; End of metadata-related procedures (i.e. package.scm)


;; Package-related procedures
(define *library-installable-extensions* '(".o" ".so" ".sld" ".meta"))

(define (*library-installation-dir*)
  (let ((lib-path (get-environment-variable "CYCLONE_LIBRARY_PATH")))
    (if lib-path
        (car (string-split lib-path #\:)) ;; return only the first path listed
        (Cyc-installation-dir 'sld))))

(define (*program-installation-dir*)
  (let ((bin-path (Cyc-installation-dir 'bin)))
    (if (string=? bin-path "")
        "/usr/local/bin"
        bin-path)))

(define (build-libraries lib-list . dir)
  (let ((dir (if (null? dir) "." (car dir))))
    (for-each
     (lambda (lib)
       (let ((lib-name-path (x->path dir lib)))
         (compile (string-append lib-name-path ".sld") dir)))
     lib-list)))

(define (install-libraries lib-list . dir)
  (let ((dir (if (null? dir) "." (car dir))))
    (for-each
     (lambda (lib)
       (let* ((lib-name-path (x->path lib))
              (full-lib-name-path (x->path dir lib)))
         (for-each
          (lambda (ext)
            (copy-file (string-append full-lib-name-path ext)
                       (x->path (*library-installation-dir*)
                             (path-dir lib-name-path))))
          *library-installable-extensions*)))
     lib-list)))

(define (build-programs prog-list . dir)
  (let ((dir (if (null? dir) "." (car dir))))
    (for-each
     (lambda (prog)
       (let ((prog-name-path (x->path dir prog)))
         (compile (string-append prog-name-path ".scm") dir)))
     prog-list)))

(define (install-programs prog-list . dir)
  (let ((dir (if (null? dir) "." (car dir))))
    (for-each
     (lambda (prog)
       (let ((prog-name-path (x->path dir prog)))
         (copy-file prog-name-path (*program-installation-dir*))))
     prog-list)))

(define (retrieve-package index name . dir)
  (match-let (((version _ tarball-url sha256sum) (pkg-info index name)))
    (let* ((pkg-name (if (list? name)
                         (string-join (map x->string name) #\-)
                         (x->string name)))
           (work-dir (if (null? dir)
                         (random-temp-dir pkg-name)
                         (x->path (car dir) pkg-name)))
           (tarball
            (string-append (string-join (list pkg-name (x->string version)) #\-)
                           ".tar.gz"))
           (outfile (x->path work-dir tarball)))
      (make-dir work-dir)
      (display (format "~%Downloading ~a (version ~a)...~%" name version))
      (download tarball-url outfile)
      (validate-sha256sum sha256sum outfile)
      (extract outfile work-dir)
      (delete outfile)               
      work-dir)))


(define (annotate-local-index . args)
  #t)

;; TODO: check pointed metadata in index.scm with downloaded one.
(define (install-package index name)
  (let* ((work-dir (retrieve-package index name))
         (metadata (read (open-input-file (x->path work-dir *default-metadata-file*))))
         (valid-metadata? metadata))
    (if valid-metadata?
        (let ((progs (programs-list metadata))
              (libs (libraries-list metadata))
              (deps (dependencies-list metadata)))
          (and deps (install-packages deps))
          (and libs
               (build-libraries libs work-dir)
               (install-libraries libs work-dir))          
          (and progs
               (build-programs progs work-dir)
               (install-programs progs work-dir))
          (let ((cyc-version (Cyc-version))
                (pkg-version (assoc 'version (cdr metadata))))
            (annotate-local-index (list name pkg-version cyc-version))))
        (error "Invalid package.scm"))
    ;; TODO - use exceptions to clean work-dir even when failing.
    (delete work-dir)))
;; End of package-related procedures

;; User interface procedures
(define (retrieve pkgs . dir)
  (let ((index (get-index *default-index-url*)))
    (for-each
     (lambda (pkg)
       (retrieve-package index pkg dir))
     pkgs)))

(define (install pkgs)
  (let ((index (get-index *default-index-url*)))
    (for-each
     (lambda (pkg)
       (install-package index pkg))
     pkgs)))

(define (uninstall pkgs)
  (let ((index (get-index *default-index-url*)))
    (for-each
     (lambda (pkg)
       (uninstall-package index pkg))
     pkgs)))

(define (test name . version) #t)
(define (search wildcard) #t)
(define (info name . version) #t)
(define (local-status) #t)

(define (usage)
  (display "Usage: cyclone-winds [OPTIONS [PACKAGES]]
     help  -  print usage
     retrieve PACKAGE [PACKAGE2 ...]  - downloads and extracts specified package(s)
     install PACKAGE [PACKAGE2 ...] - retrieve and install specified package(s)
     uninstall PACKAGE [PACKAGE2 ...] - remove specified package(s)
     search WILDCARD - search for packages that partially match the specified wildcard
     info PACKAGE - list all metadata about specified package
     local-status - list all installed packages
     index - pretty-prints index.scm
 "))

(define (main)
  (match (command-line)
   ((_ . '()) (usage))
   ((_ 'help) (usage))
   ((_ 'index) (pretty-print (get-index *default-index-url*)))
   ((_ 'search wildcard) #t) ;; TODO     
   ((_ 'info name) #t)    ;; TODO
   ((_ 'local-status) #t)    ;; TODO
   ((_ 'retrieve pkgs ..1)
    (let ((pkgs (map string->proper-symbol pkgs)))
      (retrieve pkgs)))
   ((_ 'install pkgs ..1)
    (let ((pkgs (map string->proper-symbol pkgs)))
      (install-packages pkgs)))
   ((_ 'uninstall pkgs ..1)
    (let ((pkgs (map string->proper-symbol pkgs)))
      (uninstall-packages pkgs)))
   (else (usage))))

(main)
;; End of user interface procedures
