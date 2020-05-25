(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme file)
        (scheme process-context)
        (scheme repl)
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
    (display (format "Retrieving index file...~%"))
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
    (display (format "Package ~a (version ~a) successfuly installed with Cyclone ~a.~%" name version cyc-version))))

(define (unregister-installed-package! name)
  (let* ((local-index (get-local-index)))
    (with-output-to-file *default-local-index*
      (lambda ()
        (write (remove (lambda (pkg)
                         (equal? (car pkg) name))
                       local-index))))
    (display (format "Package ~a successfuly uninstalled.~%" name))))
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
    (tags ,(list (lambda (t) (or (string? t) (list? t)))))
    (docs ,(list string?))
    (test ,(list string?))))

(define optional-parameters
  `((dependencies ,(list list?))
    (test-dependencies ,(list list?))
    (foreign-dependencies ,(list list?))))

;; TODO: implement 'custom' parameter for specilized installation.
(define code-parameters `((library ,(list list?))
                          (program ,(list list?))))
(define essential-parameters (append mandatory-parameters code-parameters))
(define available-parameters (append essential-parameters optional-parameters))

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

(define-record-type pkg
  (make-pkg _name _version _license _authors _maintainers _description _tags _docs _test
            _dependencies _test-dependencies _foreign-dependencies
            _libraries _programs _libraries-names _program-names _exports)
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
  (_programs-names get-programs-names set-programs-names!)
  (_exported-defines get-exported-defines set-exported-defines!))

(define (metadata->pkg metadata)
  (let* ((md (if (null? metadata) metadata (cdr metadata)))
         (libraries (get-parameter-all-occurrences 'library md))
         (programs (get-parameter-all-occurrences 'program md))
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
     programs-names
     (get-parameter-value 'exports metadata))))

(define *default-doc-url* "https://github.com/cyclone-scheme/cyclone-winds/wiki/")

(define (pkg->metadata pkg)
  `(package
    (name                  ,(or (get-name pkg) '____))
    (version               ,(or (get-version pkg) 0.1))
    (license               ,(or (get-license pkg) "BSD"))
    (authors               ,(or (get-authors pkg) ""))
    (maintainers           ,(or (get-maintainers pkg) ""))
    (description           ,(or (get-description pkg) ""))
    (tags                  ,(or (get-tags pkg) ""))
    (docs                  ,(or (get-docs pkg)
				(string-append *default-doc-url*
					       (symbol->string (or (get-name pkg) '____)))))
    (test                  ,(or (get-test pkg) "test.scm"))
    (dependencies          ,(or (get-dependencies pkg) '()))
    (test-dependencies     ,(or (get-test-dependencies pkg) '()))
    (foreign-dependencies  ,(or (get-foreign-dependencies pkg) '()))
    
    ,@(let ((libs (get-libraries-names pkg))
	    (progs (get-programs-names pkg)))
        (if (or (not libs) (null? libs))
	    (if (or (not progs) (null? libs))
		'((library
                      (name ____)
                    (description "")))
		'())
	    (map (lambda (l)
		   `(library
			(name ,l)
		      (description "")))
		 libs)))
    
    ,@(let ((progs (get-programs-names pkg))
	    (libs (get-libraries-names pkg)))
	(if (or (not progs) (null? progs))
	    (if (or (not libs) (null? libs))
		'((program
                   (name ____)
                   (description "")))
		'())
	    (map (lambda (p)
		   `(program
		     (name ,p)
		     (description "")))
		 progs)))))
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

;; The only global variable that is a parameter
(define *default-code-directory* (make-parameter "cyclone"))

(define *internal-cyclone-libs*
  ;; No need to list (scheme ...) libs because they are obviously internal.
  `(,@(map (lambda (s) `(srfi ,s))
           '(1 2 8 18 27 28 60 69 106 111 113 117 121 128 132 133 143))
    (cyclone concurrent) (cyclone match) (cyclone test)))

(define (test-file? file pkg)
  (or (string-contains file "test")
      (if (null? pkg)
          #f
          (string=? file (or (get-test pkg) "")))))

(define (write-metadata-file! pkg metadata-path)
  (touch metadata-path)
  (pretty-print (pkg->metadata pkg) (open-output-file metadata-path)))

(define *default-doc-file* "README.md")
(define *doc-candidates* '("README" "Readme" "readme"))
(define (doc-file? file)
  (any (lambda (e)
         (not (eq? e #f)))
       (map (lambda (c)
              (string-contains file c))
            *doc-candidates*)))


(define (lib:defines->type-and-signature defines)
  (map
   (lambda (d)
     (match d
       (('define var . body)
        (cond
         ((list? var) ;; e.g. (define (a x) x)
          (list 'procedure var))
         ((pair? var) ;; e.g. (define (a x . y) x)
          (list 'procedure var))
         ((lambda? (car body)) ;; e.g.: (define proc (lambda (x) x))
          (list 'procedure (cons var (cadar body))))
         (else (list 'variable var)))) ;; variable
       (('define-syntax var body) ;; macro - can't infer params automatically
        (list 'syntax (list var 'PARAMS)))))
   defines))

(define (write-doc-file! pkg . dir)
  (let* ((work-dir (if (null? dir) "." (->path (car dir))))
         (doc-path (->path work-dir *default-doc-file*))
         (libraries+defines (zip (get-libraries-names pkg) (get-exported-defines pkg)))
         (markdown
          (string-append
           "# " (->string (or (get-name pkg) "")) "\n"
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
           (->string (or (get-dependencies pkg) "None")) "\n\n" 

           "## Test-dependencies \n"
           (->string (or (get-test-dependencies pkg) "None")) "\n\n" 

           "## Foreign-dependencies \n"
           (->string (or (get-foreign-dependencies pkg) "None")) "\n\n" 

           "## API \n\n"
           (string-join
            (map (lambda (lib+def)
                   (string-append
                    "### (" (string-join (or (car lib+def) "") " ") ")\n\n"
                    (string-join
                     (map (lambda (def)
                            (string-append
                             "#### "
                             (or
                              (and def
                                   (string-append "[" (->string (car def)) "]"
                                                  "  " (->string (cadr def)))
                                   )
                              "")
                             "\n\n\n"))
                          (cadr lib+def)))))
                 libraries+defines))

           "## Examples\n"
           "```scheme\n"
           "(import (scheme base)\n"
           "        (cyclone " (->string (or (get-name pkg) "____")) "))"
           "\n```\n\n"

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
    (pretty-print markdown (open-output-file doc-path))))

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
                    (reduce-right cons '() (map traverse
                                                (map (lambda (d)
                                                       (->path dir d))
                                                     (cadr dir-content))))))))
    (let ((sld+scm (traverse work-dir)))
      (values (sld-files sld+scm) (scm-files sld+scm)))))

(define (get-all-defines ast)
  (append (get-parameter-all-occurrences 'define ast)
          (get-parameter-all-occurrences 'define-syntax ast)))

(define (get-exported-defines exports defines)
  (filter (lambda (d)
            (let ((var (cadr d)))
              (if (list? var) ;; procedure
                  (member (car var) exports)
                  (member var exports))))
          defines))

(define (lib:defines ast work-dir)
  (let ((body-defines (get-all-defines (lib:body ast)))
        (include-defines
         (map (lambda (i)
                (get-all-defines
                 (read-all
                  (open-input-file
                   (string-append work-dir i)))))
              (lib:includes ast))))
    (append body-defines
            (or (and (pair? include-defines)
                     (car include-defines))
                '()))))

(define (libraries+defines+programs . dir)
  (let ((work-dir (if (null? dir)
                      (*default-code-directory*)
                      (->path (car dir) (*default-code-directory*)))))
    (let-values (((sld-files scm-files) (find-code-files-recursively work-dir)))
      (let* ((libs+defs+incls
              (map (lambda (sld)
                     (let ((content
                            (read (open-input-file sld))))
                       (list (lib:name content) (lib:defines content work-dir) (lib:includes content))))
                   sld-files))
             (libs (remove null? (fold-right cons '() (map car libs+defs+incls))))
             (defs (remove null? (map cadr libs+defs+incls)))   
             ;; We can consider 'programs' those .scm files that are not included by .sld ones.
             (includes (flatten (map caddr libs+defs+incls)))	 
             (progs
              (lset-difference (lambda (f1 f2)
                                 (string=? (path-strip-directory f1) f2)) scm-files includes)))
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
          (format "An error occurred ~a" e))
        (install-package index pkg)))
     pkgs)))

(define (reinstall pkgs)
  (let ((index (get-index)))
    (for-each
     (lambda (pkg)
       (with-handler
        (lambda (e)
          (format "An error occurred ~a" e))
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
                 (string-contains (slist->string (car pkg)) (symbol->string term))))
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
  Cyclone-winds - a package manager for Cyclone Scheme 
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
