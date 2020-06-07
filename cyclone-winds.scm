(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (scheme repl)
        (scheme cyclone pretty-print)
        (except (scheme cyclone util) delete remove string-join)
        (scheme cyclone libraries)
        (except (srfi 1) delete)
        (srfi 28) ; basic format strings
        (cyclone match)
        (libs common)
        (libs system-calls)
        (libs path)
        (libs util)
        (libs index)
        (libs metadata))

;; Package-related procedures (non-exported)
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
       (let ((lib-name-path (->path lib))
	     (full-lib-name-path (->path dir lib)))
         (for-each
          (lambda (ext)
            (copy-file-to-dir (string-append full-lib-name-path ext)
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
         (copy-file-to-dir prog-name-path (get-program-installation-dir))))
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
      (display (format "Downloading ~a (version ~a)...~%" name version))
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
      (display (format "Building and installing ~a...~%" (->string name)))
      (and libs
           (begin (build-libraries libs work-dir)
                  (install-libraries libs work-dir)))          
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
        (let ((deps (get-dependencies local-pkg))
              (pkg-ver (get-version local-pkg)))
          (cond
           ((local-index-contains? 
             (get-local-index) 
             name 
             pkg-ver 
             (Cyc-version))
            (display (format "Package ~a version ~a already installed. Skipping...~%" 
                             name pkg-ver)))
           (else
            (and deps
                 (for-each
                  (lambda (dep)
                    (install-package index dep))
                  deps))
            (build-and-install local-pkg work-dir))))
        (begin
          (delete work-dir)
          (error
           (format "Attention: metadata mismatch between remote package and downloaded one!~%"))))
    (delete work-dir)))

;; Similar to install, but do not check package versions. Always build+install
(define (reinstall-package index name)
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
                  (reinstall-package index dep))
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
         (compile (->path work-dir test-file))
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

(define (test-file? file pkg)
  (or (string-contains file "test")
      (if (null? pkg)
          #f
          (string=? file (or (get-test pkg) "")))))

(define (write-metadata-file! pkg metadata-path)
  (touch metadata-path)
  (pretty-print (pkg->metadata pkg) (open-output-file metadata-path)))

(define (doc-file? file)
  (any (lambda (e)
         (not (eq? e #f)))
       (map (lambda (c)
              (string-contains file c))
            *doc-candidates*)))

(define (defines->type-and-signature defines)
  (map
   (lambda (d)
     (match d
       (('define var . body)
        (cond
         ((list? var) ;; e.g. (define (a x) x)
          (list 'procedure var))
         ((pair? var) ;; e.g. (define (a x . y) x)
          (list 'procedure var))
         ((and (list? (car body)) ;; e.g. (define a (lambda (x) x))
               (lambda? (caar body))               )
          (list 'procedure (cons var (cadar body))))
         ;; TODO - retrieve all options of parameters for case-lambda
         ;; and think of all other procedure definition forms (cut?)
         ((and (list? (car body)) ;; e.g. (define a (case-lambda ((x) ...)))
               (equal? 'case-lambda (caar body)))
          (list 'procedure (cons var (caadar body))))
         (else (list 'variable var)))) ;; variable
       (('define-syntax var body) ;; macro - can't infer params automatically
        (list 'syntax (list var 'PARAMS)))))
   defines))

(define (write-doc-file! pkg . dir)
  (let* ((work-dir (if (null? dir) "." (->path (car dir))))
         (doc-path (->path work-dir *default-doc-file*))
         (libraries+defines (zip (get-libraries-names pkg)
                                 (get-exported-defines pkg)))
         ;; We urgently need a Mardown parser...
         (markdown
          (string-append
           "# " (->string (or (get-name pkg) "")) "\n\n"
           "## Index \n"
           "- [Intro](#Intro)\n"
           "- [Dependencies](#Dependencies)\n"
           "- [Test dependencies](#Test-dependencies)\n"
           "- [Foreign dependencies](#Foreign-dependencies)\n"
           "- [API](#API)\n"
           "- [Examples](#Examples)\n"
           "- [Author(s)](#Author(s))\n"
           "- [Maintainer(s)](#Maintainer(s))\n"
           "- [Version](#Version) \n"
           "- [License](#License) \n"
           "- [Tags](#Tags) \n\n"

           "## Intro \n"
           (->string (or (get-description pkg) "")) "\n\n" 

           "## Dependencies \n"
           (let* ((deps (or (get-dependencies pkg) '()))
                  (md-deps (map (lambda (d)
                                  (let ((str-d (->string d)))
                                    (string-append
                                     "- [" str-d "]"
                                     "(" *default-doc-url* str-d ")\n")))
                                deps)))
             (if (null? md-deps)
                 "None"
                 (apply string-append md-deps)))
           "\n\n" 

           "## Test-dependencies \n"
           (let ((test-deps (list->string (or (get-test-dependencies pkg) '()))))
             (if (string=? "" test-deps)
                 "None"
                 test-deps))
           "\n\n" 

           "## Foreign-dependencies \n"
           (let ((foreign-deps (list->string (or (get-foreign-dependencies pkg) '()))))
             (if (string=? "" foreign-deps)
                 "None"
                 foreign-deps))
           "\n\n" 

           "## API \n\n"
           (string-join
            (map (lambda (lib+defs)
                   (string-append
                    "### (" (string-join (or (car lib+defs) "") " ") ")\n\n"
                    (string-join
                     (map (lambda (type+signature)
                            (string-append
                             "#### [" (->string (car type+signature)) "]   "
                             "`"
                             (let ((signature (cadr type+signature)))
                               (if (pair? signature)
                                   ;; Procedure or syntax
                                   (string-append 
                                    "(" (string-join (map ->string signature) " ") ")")
                                   ;; Variable
                                   (->string signature)))
                             "`\n\n\n"))
                          (defines->type-and-signature (cadr lib+defs))))))
                 libraries+defines))

           "## Examples\n"
           "```scheme\n"
           "(import (scheme base)\n"
           "        (" (let ((name (->string (or (get-name pkg) "____"))))
                         (if (string-contains name "srfi")
                             (string-join (string-split name #\-) " ")
                             (string-append (*default-code-directory*) name)))
           "))\n```\n\n"

           "## Author(s)\n"
           (->string (or (get-authors pkg) "")) "\n\n"

           "## Maintainer(s) \n"
           (->string (or (get-maintainers pkg) "")) "\n\n" 

           "## Version \n"
           (->string (or (get-version pkg) "")) "\n\n"

           "## License \n"
           (->string (or (get-license pkg) "")) "\n\n"

           "## Tags \n"
           (let ((tags (or (get-tags pkg) "")))
             (if (string? tags)
                 tags
                 (string-join tags " "))))))

    (if (file-exists? doc-path)
        (begin 
          (copy-file doc-path (string-append doc-path ".old")) ;; backup old one
          (delete doc-path)))
    (touch doc-path)
    (display markdown (open-output-file doc-path))))

(define (code-files files . pkg)
  (let ((pkg (if (null? pkg) '() (car pkg))))
    (filter (lambda (f)
              (or (string=? (path-extension f) "sld")
                  (and (string=? (path-extension f) "scm")
                       (not (test-file? f pkg))
                       (not (string=? f *default-metadata-file*)))))
            files)))

(define (sld-files files)
  (filter (lambda (f)
            (string=? (path-extension f) "sld"))
          files))

(define (scm-files files)
  (filter (lambda (f)
            (string=? (path-extension f) "scm"))
          files))

(define (structure-directory-tree! pkg dir)
  (let ((dir-content (directory-content dir)))
    ;; Move directories and code files into *default-code-directory* (except
    ;; *default-code-directory* itself, hidden directories and files,
    ;; *default-metadata-file* and test files).
    (for-each (lambda (d)
                (copy-dir-to-dir d (->path dir (*default-code-directory*)))
                (delete d))
              (remove (lambda (d)
                        (or (string=? d (*default-code-directory*))
                            (char=? (string-ref d 0) #\.)))
                      (cadr dir-content)))
    (for-each (lambda (f)
                (copy-file-to-dir f (->path dir (*default-code-directory*)))
                (delete f))
              (remove (lambda (f)
                        (char=? (string-ref f 0) #\.))
                      (code-files (car dir-content) pkg)))))

(define (find-code-files-recursively . dir)
  (let* ((work-dir (if (null? dir)
                       (*default-code-directory*)
                       (car dir))))
    (define (traverse dir)
      (let ((dir-content (directory-content dir)))
        (if (null? (cadr dir-content))
            ;; no more directories to traverse beyond current one
            (code-files (map (lambda (f)
                               (->path dir f))
                             (car dir-content)))
            ;; keep traversing remaining directories...
            (append (code-files (map (lambda (f)
                                       (->path dir f))
                                     (car dir-content)))
                    (reduce-right append '() (map traverse
                                                  (map (lambda (d)
                                                         (->path dir d))
                                                       (cadr dir-content))))))))
    (let ((sld+scm (traverse work-dir)))
      (values (sld-files sld+scm) (scm-files sld+scm)))))

(define (get-all-defines ast)
  (append (get-parameter-all-occurrences 'define ast)
          (get-parameter-all-occurrences 'define-syntax ast)))

(define (get-defines ast work-dir)
  (let ((body-defines (get-all-defines (lib:body ast)))
        (include-defines
         (map (lambda (inc)
                (get-all-defines
                 (read-all
                  (open-input-file
                   (->path work-dir inc)))))
              (lib:includes ast))))
    (append body-defines
            (or (and (pair? include-defines)
                     (car include-defines))
                '()))))

(define (filter-exported-defines exports defines)
  (filter (lambda (d)
            (let ((var (cadr d)))
              (if (list? var) ;; procedure
                  (member (car var) exports)
                  (member var exports))))
          defines))

(define (libraries+defines+programs . dir)
  (let ((work-dir (if (null? dir)
                      (*default-code-directory*)
                      (->path (car dir) (*default-code-directory*)))))
    (let-values (((sld-files scm-files) (find-code-files-recursively work-dir)))
      (let* ((libs+defs+incls
              (map (lambda (sld)
                     (let* ((content (read (open-input-file sld)))
                            (lib-name (lib:name content))
                            (include-dir (cdr (reverse (cdr (reverse lib-name))))))
                       (list lib-name
                             (filter-exported-defines
                              (lib:exports content)
                              (get-defines content(->path work-dir include-dir)))
                             (lib:includes content))))
                   sld-files))
             (libs (remove null? (fold-right cons '() (map car libs+defs+incls))))
             (defs (remove null? (map cadr libs+defs+incls)))   
             ;; We can consider 'programs' those .scm files that are not included by .sld ones.
             (includes (flatten (map caddr libs+defs+incls)))	 
             (progs
              (lset-difference (lambda (f1 f2)
                                 (string=? (path-strip-directory f1) f2))
                               scm-files
                               includes)))
        (values libs defs progs)))))
;; End of package-related procedures


;; User interface procedures
(define (package . dir)
  (let* ((work-dir (if (null? dir) "." (->path (car dir))))
         (metadata-path (->path work-dir *default-metadata-file*))
         (pkg (if (file-exists? metadata-path)
                  ;; reads 'package.scm' skipping the initial (package ...) tag
                  (let ((md (cdr (read (open-input-file metadata-path))))) 
                    (copy-file metadata-path (string-append metadata-path ".old")) ;; backup old one
                    (delete metadata-path)
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
  (let ((index (get-index)))
    (for-each
     (lambda (pkg)
       (with-handler
        (lambda (e)
          (display (format "An error occurred ~a" e)))
        (install-package index pkg)))
     pkgs)))

(define (reinstall pkgs)
  (let ((index (get-index)))
    (for-each
     (lambda (pkg)
       (with-handler
        (lambda (e)
          (display (format "An error occurred ~a" e)))
        (reinstall-package index pkg)))
     pkgs)))

(define (upgrade . pkgs)
  (cond
   ((pair? pkgs)
    (install (car pkgs)))
   (else
    ;; Update all installed packages
    (install (map car (get-local-index))))))

(define (uninstall pkgs)
  (let ((index (get-local-index)))
    (for-each
     (lambda (pkg)
       (uninstall-package index pkg))
     pkgs)))

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
