(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme file)
        (scheme process-context)
        (scheme cyclone pretty-print)
        (scheme cyclone util)
        (scheme cyclone libraries)
        (srfi 1)
        (srfi 27) ;; random numbers
        (srfi 28) ;; basic format strings
        (cyclone match))

(include-c-header "<dirent.h>")

(include "path.scm")
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
         (index-path (->path tmp-dir "index.scm")))
    (make-dir tmp-dir)
    (display (format "~%Retrieving index file...~%"))
    (download *default-index-url* index-path)
    (let ((content (cdr (read (open-input-file index-path)))))
      (delete tmp-dir)
      content)))

(define (pkg-info index pkg-name)
  (match (assoc pkg-name index)
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
  (->path (get-library-installation-dir) "cyclone" "cyclone-winds-index.scm"))

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
(define *default-metadata-file* "package.scm")

(define (keys alist)
  (if (equal? 'package (car alist))
      (map car (cdr alist))
      (map car alist)))

;; TODO: review this validation strategy - seems too naive.
(define mandatory-parameters
  `((name ,(list (lambda (x) (or (symbol? x) (list? x)))))
    (version ,(list number?))
    (license ,(list string?))
    (authors ,(list string?))
    (maintainers ,(list string?))
    (description ,(list string?))
    (tags ,(list string?))
    (docs ,(list string?))
    (test ,(list string?))))

(define optional-parameters
  `((dependencies ,(list string?))
    (test-dependencies ,(list string?))
    (foreign-dependencies ,(list string?))))

;; TODO: implement 'custom' parameter for specilized installation.
(define code-parameters `((library ,(list list?))
                          (program ,(list symbol?))))

(define essential-parameters (append mandatory-parameters code-parameters))
(define available-parameters (append essential-parameters optional-parameters))

(define-record-type pkg
  (make-pkg _name _version _license _authors _maintainers _description _tags _docs _test
            _dependencies _test-dependencies _foreign-dependencies
            _libraries _programs _libraries-names _program-names)
  pkg?
  (_name get-name set-name!)
  (_version get-version set-version!)
  (_license get-license set-license!)
  (_authors get-authors set-authors!)
  (_maintainers get-maintainers set-maintainers!)
  (_description get-description set-description!)
  (_tags get-tags set-tags!)
  (_docs get-docs set-docs!)
  (_test get-test set-test!)
  (_dependencies get-dependencies set-dependencies!)
  (_test-dependencies get-test-dependencies set-test-dependencies!)
  (_foreign-dependencies get-foreign-dependencies set-foreign-dependencies!)
  (_libraries get-libraries set-libraries!)
  (_programs get-programs set-programs!)
  (_libraries-names get-libraries-names set-libraries-names!)
  (_programs-names get-programs-names set-programs-names!))

(define (metadata->pkg metadata)
  (let* ((libraries (get-parameter-all-occurrences 'library metadata))
         (programs (get-parameter-all-occurrences 'program metadata))
         (libraries-names (if (null? libraries) #f (map cadadr libraries)))
         (programs-names (if (null? programs) #f (map cadadr programs))))
    (make-pkg
     (get-parameter-value 'name metadata)
     (get-parameter-value 'version metadata)
     (get-parameter-value 'license metadata)
     (get-parameter-value 'authors metadata)
     (get-parameter-value 'maintainers metadata)
     (get-parameter-value 'description metadata)
     (get-parameter-value 'tags metadata)
     (get-parameter-value 'docs metadata)
     (get-parameter-value 'test metadata)
     (get-parameter-value 'dependencies metadata)
     (get-parameter-value 'test-dependencies metadata)
     (get-parameter-value 'foreign-dependencies metadata)
     libraries
     programs
     libraries-names
     programs-names)))

(define (validate-metadata metadata)
  (let ((metadata (cdr metadata)))
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
    (metadata->pkg metadata))) ;; returns a pkg record if everything is ok
;; End of metadata-related procedures (i.e. package.scm)


;; Package-related procedures (non-exported)
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
       (let ((lib-name-path (->path dir lib)))
         (compile (string-append lib-name-path ".sld") dir)))
     lib-list)))

(define (install-libraries lib-list . dir)
  (let ((dir (if (null? dir) "." (car dir))))
    (for-each
     (lambda (lib)
       (let* ((lib-name-path (->path lib))
              (full-lib-name-path (->path dir lib)))
         (for-each
          (lambda (ext)
            (copy-file (string-append full-lib-name-path ext)
                       (->path (get-library-installation-dir)
                               (path-dir lib-name-path))))
          *library-installable-extensions*)))
     lib-list)))

(define (build-programs prog-list . dir)
  (let ((dir (if (null? dir) "." (car dir))))
    (for-each
     (lambda (prog)
       (let ((prog-name-path (->path dir prog)))
         (compile (string-append prog-name-path ".scm") dir)))
     prog-list)))

(define (install-programs prog-list . dir)
  (let ((dir (if (null? dir) "." (car dir))))
    (for-each
     (lambda (prog)
       (let ((prog-name-path (->path dir prog)))
         (copy-file prog-name-path (get-program-installation-dir))))
     prog-list)))

(define (retrieve-package index name . dir)
  (match-let (((version _ tarball-url sha256sum) (pkg-info index name)))
    (let* ((pkg-name (if (list? name)
                         (string-join (map ->string name) #\-)
                         (->string name)))
           (work-dir (if (null? dir)
                         (random-temp-dir pkg-name)
                         (->path (car dir) pkg-name)))
           (tarball
            (string-append (string-join (list pkg-name (->string version)) #\-)
                           ".tar.gz"))
           (outfile (->path work-dir tarball)))
      (make-dir work-dir)
      (display (format "~%Downloading ~a (version ~a)...~%" name version))
      (download tarball-url outfile)
      (validate-sha256sum sha256sum outfile)
      (extract outfile work-dir)
      (delete outfile)               
      work-dir)))

(define (get-package-remote-metadata index name . dir)
  (let* ((work-dir (if (null? dir)
                       (random-temp-dir (string-append (->string name) "-metadata"))
                       (->path (car dir) (string-append (->string name) "-metadata"))))
         (metadata-url (cadr (pkg-info index name)))
         (metadata-path (->path work-dir *default-metadata-file*)))
    (make-dir work-dir)
    (download metadata-url metadata-path)
    (let ((metadata (read (open-input-file metadata-path))))
      (delete work-dir)
      metadata)))

(define (build-and-install pkg . dir)
  (let ((work-dir (if (null? dir) "." (->path (car dir)))))
    (let ((name (get-name pkg))
          (progs (get-programs-names pkg))
          (libs (get-libraries-names pkg))
          (version (get-version pkg)))
      (and libs
           (build-libraries libs work-dir)
           (install-libraries libs work-dir))          
      (and progs
           (build-programs progs work-dir)
           (install-programs progs work-dir))
      (register-installed-package! name version (Cyc-version) libs progs))))

(define (install-package index name)
  (let* ((work-dir (retrieve-package index name))
         (local-pkg
          (validate-metadata
           (read (open-input-file (->path work-dir *default-metadata-file*)))))
         (remote-pkg (validate-metadata (get-package-remote-metadata index name))))
    (if (equal? local-pkg remote-pkg)
        (let ((deps (get-dependencies local-pkg)))
          (and deps
               (for-each
                (lambda (dep)
                  (install-package index dep))
                deps))
          (build-and-install local-pkg work-dir))
        (begin
          (delete work-dir)
          (error
           (format "Attention: metadata mismatch between remote package and downloaded one!~%"))))
    (delete work-dir)))

(define (uninstall-package index name)
  (let ((pkg (assoc name index)))
    (if pkg
        (let ((libs (cadddr pkg))
              (progs (cadddr (cdr pkg))))
          (and libs
               (for-each
                (lambda (lib)
                  (for-each
                   (lambda (ext)
                     (delete (->path (get-library-installation-dir)
                                     (string-append (->path lib) ext)))
                     ;; Also delete directories if appropriate
                     (if (>= (length lib) 3)
                         (delete (->path (get-library-installation-dir)
                                         (path-dir (->path lib))))))
                   *library-installable-extensions*))
                libs))
          (and progs
               (for-each
                (lambda (prog)
                  (delete (->path (get-program-installation-dir) prog)))
                progs))
          (unregister-installed-package! name))
        (display (format "Package ~a not installed. Skipping...~%" name)))))

;; Package authoring
(define (test-local . dir)
  (let* ((work-dir (if (null? dir) "." (->path (car dir))))
         (pkg
          (validate-metadata
           (read
            (open-input-file (->path work-dir
                                     *default-metadata-file*)))))
         (test-dependencies (get-test-dependencies pkg))
         (test-file (get-test pkg)))
    (and test-dependencies
         (for-each
          (lambda (test-dep)
            (install test-dep))
          test-dependencies))
    (and test-file
         (compile test-file)
         (if (ok? (system (path-strip-extension (->path work-dir test-file))))
             (begin
               (and test-dependencies
                    (for-each
                     (lambda (test-dep)
                       (uninstall test-dep))
                     test-dependencies))
               (display (format "[OK] Tests passed~%")))
             (error (format "Could not run tests or tests failed. Running them without building package first?~%"))))))

(define (build-local . dir)
  (let* ((work-dir (if (null? dir) "." (->path (car dir))))
         (pkg
          (validate-metadata
           (read
            (open-input-file (->path work-dir
                                     *default-metadata-file*))))))
    (let ((progs (get-programs-names pkg))
          (libs (get-libraries-names pkg)))
      (and libs (build-libraries libs work-dir))          
      (and progs (build-programs progs work-dir)))))

(define *default-code-directory* "cyclone")

(define (package . dir)
  (let* ((work-dir (if (null? dir) "." (->path (car dir))))
         (metadata-path (->path work-dir *default-metadata-file*))
         (pkg
          (if (file-exists? metadata-path)
              (metadata->pkg (cdr (read (open-input-file metadata-path))))
              (metadata->pkg '())))
         (current-directory-content (directory-content work-dir))
         (directories (cadr current-directory-content))
         (files (car current-directory-content))
         (sld-files (filter (lambda (f)
                              (string=? (path-extension f "sld")))
                            files))
         (scm-files (filter (lambda (f)
                              (string=? (path-extension f "scm")))
                            files))
         (other-files (lset-difference string=?
                                       (lset-union sld-files
                                                   scm-files
                                                   *default-metadata-file*
                                                   (get-test pkg))
                                       files)))
    (map (lambda (f)
           (copy-file f (->path *default-code-directory*))
           (delete f))
         code-files)))
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

(define (info name . version)
  (pretty-print (get-package-remote-metadata (get-index) name)))

(define (local-status)
  (let ((index (get-local-index)))
    (if (null? index)
        (display (format "None~%"))
        (for-each
         (lambda (pkg)
           (display
            (format "~%  ~a  Version: ~a  Cyclone: ~a~%  Libraries: ~a~%  Programs:  ~a~%"
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
       COMMON USE:
       help  -  print usage
       retrieve PACKAGE [PACKAGE2 ...]  - downloads and extracts specified package(s)
       install PACKAGE [PACKAGE2 ...] - retrieve and install specified package(s)
       uninstall PACKAGE [PACKAGE2 ...] - remove specified package(s)
       TODO - search WILDCARD - search for packages that partially match the specified wildcard
       info PACKAGE - list all metadata about specified package
       local-status - list all installed packages
       index - pretty-prints cyclone-winds packages index

       PACKAGE AUTHORING:
       build-local [DIRECTORY] - build local package using package.scm from DIRECTORY or \".\"
       test-local [DIRECTORY] - test local package using (test ...) from package.scm in DIRECTORY or \".\"
       TODO - package - scaffold directory layout and a package.scm stub
  
  PACKAGES:
       a quoted list of two or more symbols, starting with 'cyclone'. Ex.: \"(cyclone iset)\"~%~%"
    *banner*)))

(define (main)
  (match (map string->proper-symbol (command-line))
    ((_ . ()) (display (usage)))
    ((_ 'help) (usage))
    ((_ 'retrieve pkgs ..1) (retrieve pkgs))
    ((_ 'install pkgs ..1) (install pkgs))
    ((_ 'uninstall pkgs ..1) (uninstall pkgs))
    ((_ 'search wildcard) #t) ;; TODO     
    ((_ 'info name) (info name))
    ((_ 'repl) (repl))
    ((_ 'local-status) (local-status))
    ((_ 'index) (index))
    ((_ 'build-local) (build-local))
    ((_ 'build-local dir) (build-local dir))    
    ((_ 'test-local) (test-local))
    ((_ 'test-local dir) (test-local dir))    
    ((_ 'package) (package))
    ((_ 'package dir) (package dir))    
    (else (usage))))

(main)

;; End of user interface procedures
