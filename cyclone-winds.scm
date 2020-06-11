(import (scheme base)
        (scheme read)
        (only (scheme write) display)
        (scheme file)
        (scheme process-context)
        (scheme repl)
        (scheme cyclone pretty-print)
        (except (scheme cyclone util) delete! remove string-join)
        (scheme cyclone libraries)
        (except (srfi 1) delete!)
        (srfi 28) ; basic format strings
        (cyclone match)
        (libs common)
        (libs system-calls)
        (only (libs file) ->path path-strip-extension)
        (only (libs util) ->string string-contains string->proper-symbol)
        (libs index)
        (libs metadata)
        (libs package)
        (libs lock))

;;; User interface procedures

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
         (compile! (->path work-dir test-file))
         (if (ok? (system (path-strip-extension (->path work-dir test-file))))
             (begin
               (and test-dependencies
                    (for-each
                     (lambda (test-dep)
                       (uninstall test-dep))
                     test-dependencies))
               (display (format "Tests performed~%")))
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

(define (package . dir)
  (let* ((work-dir (if (null? dir) "." (->path (car dir))))
         (metadata-path (->path work-dir *default-metadata-file*))
         (pkg (if (file-exists? metadata-path)
                  ;; reads 'package.scm' skipping the initial (package ...) tag
                  (let ((md (cdr (read (open-input-file metadata-path))))) 
                    (copy-file! metadata-path (string-append metadata-path ".old")) ;; backup old one
                    (delete! metadata-path)
                    (metadata->pkg md))
                  (metadata->pkg '()))))
    
    (structure-directory-tree! pkg work-dir)
    (let-values (((libs defs progs) (libraries+defines+programs work-dir)))
      (set-libraries-names! pkg libs)
      (set-programs-names! pkg progs)
      (set-exported-defines! pkg defs) 

      ;; Try to guess package name if not already present in an old 'package.scm' file.
      (if (and (not (get-name pkg)) (not (or (null? libs) (null? (car libs)))))
          (set-name! pkg (if (null? (cdar libs)) ;; is the first library name not compound?
                             (caar libs) ;; ((libA) ...) -> libA
                             (cadar libs)))) ;; ((cyclone libA) ...)) -> libA

      (write-metadata-file! pkg metadata-path)
      (write-doc-file! pkg work-dir)
      (display (format "Scaffolded directory tree and generated stubs for ~a and ~a.~%"
                       *default-metadata-file* *default-doc-file*)))))

(define (retrieve pkgs)
  (let ((index (get-index)))
    (for-each
     (lambda (pkg)
       (display
	(format "Package ~a retrieved into ~a~%"
		pkg (retrieve-package index pkg "."))))
     pkgs)))

(define (install pkgs)
  (with-file-lock
   (let ((index (get-index)))
     (for-each
      (lambda (pkg)
        (with-handler
         (lambda (e)
           (display (format "An error occurred ~a" e)))
         (install-package index pkg)))
      pkgs))))

(define (reinstall pkgs)
  (with-file-lock
   (let ((index (get-index)))
     (for-each
      (lambda (pkg)
        (with-handler
         (lambda (e)
           (display (format "An error occurred ~a" e)))
         (reinstall-package index pkg)))
      pkgs))))

(define (upgrade . pkgs)
  (cond
   ((pair? pkgs)
    (install (car pkgs)))
   (else
    ;; Update all installed packages
    (install (map car (get-local-index))))))

(define (uninstall pkgs)
  (with-file-lock
   (let ((index (get-local-index)))
     (for-each
      (lambda (pkg)
        (uninstall-package index pkg))
      pkgs))))

(define (search term)
  (pretty-print
   (filter (lambda (pkg)
             (if (and (not (null? pkg)))
                 (string-contains (->string (car pkg)) (symbol->string term))))
           (get-index))))

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
  Cyclone-Winds - a package manager for Cyclone Scheme 
  https://github.com/cyclone-scheme/cyclone-winds 
  (c) 2020 - Cyclone Team 
  Version ~a~%"
   *cyclone-winds-version*))

(define (usage)
  (display
   (format
    "~a
  Usage: cyclone-winds [-v] COMMAND [PACKAGES]
  
  COMMANDS:

    COMMON USE:
    help  -  print usage
    retrieve PACKAGE [...]  - downloads and extracts specified PACKAGE(s)
    install PACKAGE [...] - retrieve and install specified PACKAGE(s)
    reinstall PACKAGE [...] - retrieve and reinstall specified PACKAGE(s)
    upgrade [PACKAGE ...] - upgrade all installed packages or specified PACKAGE(s)
    uninstall PACKAGE [...] - remove specified PACKAGE(s)
    search TERM - search for packages whose name (partially) match the specified TERM
    info PACKAGE - list all metadata about specified PACKAGE
    local-status - list all installed packages
    index - pretty-prints cyclone-winds packages index

    PACKAGE AUTHORING:
    build-local [DIR] - build local package using package.scm from DIR or \".\"
    test-local [DIR] - test local package using package.scm in DIR or \".\"
    package [DIR] - scaffold DIR layout and a package.scm stub
    package-srfi [DIR] - scaffold DIR layout and a package.scm stub for SRFIs
 
  PACKAGES:
       Name of the package. Note this can be a symbol or a quoted list of 
       two or more symbols, e.g. \"(cyclone iset)\"~%"
    *banner*)))

(define (main)
  (match (map string->proper-symbol (command-line))
    ((_ '-v . rest)
     (parameterize ((*log-level* 'debug))
       (dispatch rest)))
    ((_ . rest)
     (dispatch rest))))

(define (dispatch cmds)
  (match cmds
    (('retrieve pkgs ..1) (retrieve pkgs))
    (('install pkgs ..1) (install pkgs))
    (('reinstall pkgs ..1) (reinstall pkgs))
    (('upgrade) (upgrade))
    (('upgrade pkgs ..1) (upgrade pkgs))
    (('uninstall pkgs ..1) (uninstall pkgs))
    (('search term) (search term))
    (('info name) (info name))
    (('local-status) (local-status))
    (('index) (index))
    (('build-local) (build-local))
    (('build-local dir) (build-local dir))    
    (('test-local) (test-local))
    (('test-local dir) (test-local dir))    
    (('package) (package))
    (('package dir) (package dir))    
    (('package-srfi)
     (parameterize ((*default-code-directory* "srfi"))
       (package)))
    (('package-srfi dir)
     (parameterize ((*default-code-directory* "srfi"))
       (package dir)))    
    (('repl) (repl)) ;; Allow interactive debugging
    (else (usage))))

(main)
;; End of user interface procedures
