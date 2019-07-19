(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme repl)
        (scheme file)
        (scheme process-context)
        (scheme cyclone pretty-print)
        (scheme cyclone util)
        (srfi 27)  ;; random numbers
        (srfi 28)  ;; basic format strings
        (cyclone match))

(include "system-calls.scm")
(include "util.scm")

(define *cyclone-winds-version* "0.1")

;; Index-related procedures
;;
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

(define (get-index)
  (let* ((tmp-dir (random-temp-dir))
         (index-path (x->path tmp-dir "index.scm")))
    (make-dir tmp-dir)
    (display (format "~%Retrieving index file...~%"))
    (download *default-index-url* index-path)
    (let ((content (read (open-input-file index-path))))
      (delete tmp-dir)
      content)))

(define (pkg-info index pkg-name)
  (match (assoc pkg-name (cdr index))
    (#f (error (format "Could not locate package by name: ~s~%" pkg-name)))
    ((pkg-name latest-version old-versions ...) latest-version)))

(define (version pkg-info)
  (car pkg-info))

(define (metadata-url pkg-info)
  (cadr pkg-info))

(define (tarball-url pkg-info)
  (caddr pkg-info))

(define (sha256sum pkg-info)
  (cadddr pkg-info))


;; Local index has the following format:
;;      PKG-NAME   PKG-VERSION   CYCLONE-VERSION       LIBS                 PROGS
;; (((cyclone pkg1)   0.8           "1.11.3"   ((cyclone libX) ...)    ((progamX) ...))
;;  ((cyclone pkg2)   0.2           "1.11.0"   ((cyclone libY) ...)    ((progamY) ...))
;;  ...)
(define *default-local-index*
  (x->path (get-library-installation-dir) "cyclone" "cyclone-winds-index.scm"))

(define (get-local-index)
  (with-exception-handler
   ;; could not open file - if not found, create one with empty list
   (lambda (err)
      (with-output-to-file *default-local-index*
        (lambda ()
          (write (format "()" err))))
      '())
   (lambda ()
     (read (open-input-file *default-local-index*)))))

(define (register-installed-package! name version cyc-version libs progs)
  (let ((local-index (get-local-index)))
    (with-output-to-file *default-local-index*
      (lambda ()
        (write (cons (list name version cyc-version libs progs)
                     (remove (lambda (pkg)
                               (equal? (car pkg) name))
                             local-index)))))
    (display (format "~%Package ~a (version ~a) successfuly installed with Cyclone ~a.~%" name version cyc-version))))

(define (unregister-installed-package! name)
  (let* ((local-index (get-local-index)))
    (with-output-to-file *default-local-index*
      (lambda ()
        (write (remove (lambda (pkg)
                         (equal? (car pkg) name))
                       local-index))))
    (display (format "~%Package ~a successfuly uninstalled.~%" name))))
;; End of index-related procedures


;; Metadata-related procedures (i.e. package.scm)
(define *default-metadata-file* (x->path "cyclone" "package.scm"))

(define (keys alist)
  (map car alist))

;; TODO: review this validation strategy - seems too naive.
(define mandatory-parameters
  `((name ,(list (lambda (x) (or (symbol? x) (list? x)))))
    (version ,(list number?))
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

(define (get-library-installation-dir)
  (or (get-environment-variable "CYCLONE_LIBRARY_PATH")
      (Cyc-installation-dir 'sld)))

(define (get-program-installation-dir)
  (or (get-environment-variable "CYCLONE_PROGRAM_PATH")
      (Cyc-installation-dir 'bin)))

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
                       (x->path (get-library-installation-dir)
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
         (copy-file prog-name-path (get-program-installation-dir))))
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

;; TODO: check pointed metadata in index.scm with downloaded one.
(define (install-package index name)
  (let* ((work-dir (retrieve-package index name))
         (metadata
          (cdr (read
                (open-input-file (x->path work-dir
                                          *default-metadata-file*))))))
    (if (valid-metadata? metadata)
        (let ((progs (programs-list metadata))
              (libs (libraries-list metadata))
              (deps (dependencies-list metadata)))
          (and deps
               (for-each
                (lambda (dep)
                  (install-package index dep))
                deps))
          (and (not (null? libs))
               (build-libraries libs work-dir)
               (install-libraries libs work-dir))          
          (and (not (null? progs))
               (build-programs progs work-dir)
               (install-programs progs work-dir))
          (let ((pkg-version (car (pkg-info index name)))
                (cyc-version (Cyc-version)))
            (register-installed-package! name pkg-version cyc-version libs progs)))
        (error (format "~%Invalid package.scm in package ~a~%" name)))
    ;; TODO - use exceptions to clean work-dir even when failing.
    (delete work-dir)))

(define (uninstall-package index name)
  (let ((pkg (assoc name index)))
    (if pkg
        (let ((libs (cadddr pkg))
              (progs (cadddr (cdr pkg))))
          (for-each
           (lambda (lib)
             (for-each
              (lambda (ext)
                (delete (x->path (get-library-installation-dir)
                                 (string-append (x->path lib) ext)))
                ;; Also delete directories if appropriate
                (if (>= (length lib) 3)
                    (delete (x->path (get-library-installation-dir)
                                     (path-dir (x->path lib))))))
              
              *library-installable-extensions*))
           libs)
          (for-each
           (lambda (prog)
             (delete (x->path (get-program-installation-dir) prog)))
           progs)
          (unregister-installed-package! name))
        (display (format "~%Package ~a not installed. Skipping...~%" name)))))
;; End of package-related procedures


;; User interface procedures
(define (retrieve pkgs)
  (let ((index (get-index)))
    (for-each
     (lambda (pkg)
       (display
        (format "~%Package ~a retrieved into ~a~%"
                pkg (retrieve-package index pkg "."))))
     pkgs)))

(define (install pkgs)
  (let ((index (get-index)))
    (for-each
     (lambda (pkg)
       (install-package index pkg))
     pkgs)))

(define (uninstall pkgs)
  (let ((index (get-local-index)))
    (for-each
     (lambda (pkg)
       (uninstall-package index pkg))
     pkgs)))

(define (search wildcard) #t)
(define (info name . version) #t)

(define (local-status)
  (let ((index (get-local-index)))
    (display (format "~%  Installed packages: "))
    (if (null? index)
        (display (format "None~%~%"))
        (for-each
         (lambda (pkg)
           (display
            (format
             "~%~%  Name: ~a       version: ~a       Cyclone version: ~a~%  Installed libraries: ~a~%  Installed programs: ~a~%"
             (car pkg) (cadr pkg) (caddr pkg) (cadddr pkg) (cadddr (cdr pkg)))))
         index))))

(define (index)
  (pretty-print (get-index)))

(define *banner*
  (format
   "
  Cyclone-winds - a package manager for Cyclone Scheme 
  https://github.com/cyclone-scheme/cyclone-winds 
  (c) 2019 - Cyclone Team 
  Version ~a~%"
   *cyclone-winds-version*))

(define (usage)
  (display
   (format
    "~a
  Usage: cyclone-winds [OPTIONS [PACKAGES]]
  
  OPTIONS:
       help  -  print usage
       retrieve PACKAGE [PACKAGE2 ...]  - downloads and extracts specified package(s)
       install PACKAGE [PACKAGE2 ...] - retrieve and install specified package(s)
       uninstall PACKAGE [PACKAGE2 ...] - remove specified package(s)
       TODO - search WILDCARD - search for packages that partially match the specified wildcard
       TODO - info PACKAGE - list all metadata about specified package
       local-status - list all installed packages
       index - pretty-prints index.scm
  
  PACKAGES:
       a quoted list of two or more symbols. Ex.: \"(cyclone iset)\"~%~%"
    *banner*)))

(define (main)
  (match (map string->proper-symbol (command-line))
    ((_ . ()) (display (usage)))
    ((_ 'help) (usage))
    ((_ 'retrieve pkgs ..1) (retrieve pkgs))
    ((_ 'install pkgs ..1) (install pkgs))
    ((_ 'uninstall pkgs ..1) (uninstall pkgs))
    ((_ 'search wildcard) #t) ;; TODO     
    ((_ 'info name) #t)       ;; TODO
    ((_ 'repl) (repl))
    ((_ 'local-status) (local-status))
    ((_ 'index) (index))    
    (else (usage))))

(main)
;; End of user interface procedures
