(define-library (libs package)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (scheme cyclone pretty-print)
          (only (scheme cyclone util) lambda? string-split filter flatten)
          (scheme cyclone libraries)
          (cyclone match)
          (except (srfi 1) delete!)
          (srfi 28) ; basic format strings
          (libs common)
          (libs util)
          (libs file)
          (libs system-calls)
          (libs index)
          (libs metadata))
  (export build-libraries
          build-programs
          libraries+defines+programs
          structure-directory-tree!
          write-doc-file!
          write-metadata-file!
          retrieve-package
          install-package
          reinstall-package
          uninstall-package
          get-package-remote-metadata)
  (begin
    (define (build-libraries lib-list . dir)
      (let ((dir (if (null? dir) "." (car dir))))
        (for-each
         (lambda (lib)
           (let ((lib-name-path (->path dir lib)))
             (compile! (string-append lib-name-path ".sld") dir)))
         lib-list)))

    (define (install-libraries lib-list . dir)
      (let ((dir (if (null? dir) "." (car dir))))
        (for-each
         (lambda (lib)
           (let ((lib-name-path (->path lib))
                 (full-lib-name-path (->path dir lib)))
             (for-each
              (lambda (ext)
                (copy-file-to-dir! (string-append full-lib-name-path ext)
                                   (->path (get-library-installation-dir)
                                           (path-dir lib-name-path))))
              *library-installable-extensions*)))
         lib-list)))

    (define (build-programs prog-list . dir)
      (let ((dir (if (null? dir) "." (car dir))))
        (for-each
         (lambda (prog)
           (let ((prog-name-path (->path dir prog)))
             (compile! (string-append prog-name-path ".scm") dir)))
         prog-list)))

    (define (install-programs prog-list . dir)
      (let ((dir (if (null? dir) "." (car dir))))
        (for-each
         (lambda (prog)
           (let ((prog-name-path (->path dir prog)))
             (copy-file-to-dir! prog-name-path (get-program-installation-dir))))
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
          (make-dir! work-dir)
          (display (format "Downloading ~a ~a...~%" name version))
          (download! tarball-url outfile)
          (validate-sha256sum sha256sum outfile)
          (extract! outfile work-dir)
          (delete! outfile)               
          work-dir)))

    (define (get-package-remote-metadata index name . dir)
      (let* ((work-dir (if (null? dir)
                           (random-temp-dir (string-append (->string name) "-metadata"))
                           (->path (car dir) (string-append (->string name) "-metadata"))))
             (metadata-url (cadr (pkg-info index name)))
             (metadata-path (->path work-dir *default-metadata-file*)))
        (make-dir! work-dir)
        (download! metadata-url metadata-path)
        (let ((metadata (read (open-input-file metadata-path))))
          (delete! work-dir)
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

    (define (installed? index name)
      (let ((version (car (pkg-info index name))))
        (local-index-contains? (get-local-index) 
                               name 
                               version
                               (Cyc-version))))
    
    (define (install-package index name)
      (if (installed? index name)
          (display (format "Package ~a already installed. Skipping...~%" name))
          (let* ((work-dir (retrieve-package index name))
                 (local-pkg
                  (validate-metadata
                   (read (open-input-file (->path work-dir *default-metadata-file*)))))
                 (remote-pkg (validate-metadata (get-package-remote-metadata index name))))
            (if (equal? local-pkg remote-pkg)
                (let ((deps (get-dependencies local-pkg))
                      (pkg-ver (get-version local-pkg)))
                  (and deps (for-each
                             (lambda (dep)
                               (install-package index dep))
                             deps))
                  (build-and-install local-pkg work-dir))
                (begin
                  (delete! work-dir)
                  (error
                   (format "Attention: metadata mismatch between remote package and downloaded one!~%"))))
            (delete! work-dir))))

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
              (delete! work-dir)
              (error
               (format "Attention: metadata mismatch between remote package and downloaded one!~%"))))
        (delete! work-dir)))

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
                         (delete! (->path (get-library-installation-dir)
                                          (string-append (->path lib) ext)))
                         ;; Also delete directories if appropriate
                         (if (>= (length lib) 3)
                             (delete! (->path (get-library-installation-dir)
                                              (path-dir (->path lib))))))
                       *library-installable-extensions*))
                    libs))
              (and progs
                   (for-each
                    (lambda (prog)
                      (delete! (->path (get-program-installation-dir) prog)))
                    progs))
              (unregister-installed-package! name))
            (display (format "Package ~a not installed. Skipping...~%" name)))))

    (define (write-metadata-file! pkg metadata-path)
      (touch! metadata-path)
      (pretty-print (pkg->metadata pkg) (open-output-file metadata-path)))

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
              (copy-file! doc-path (string-append doc-path ".old")) ;; backup old one
              (delete! doc-path)))
        (touch! doc-path)
        (display markdown (open-output-file doc-path))))

    (define (structure-directory-tree! pkg dir)
      (let ((dir-content (directory-content dir)))
        ;; Move directories and code files into *default-code-directory* (except
        ;; *default-code-directory* itself, hidden directories and files,
        ;; *default-metadata-file* and test files).
        (for-each (lambda (d)
                    (copy-dir-to-dir! d (->path dir (*default-code-directory*)))
                    (delete! d))
                  (remove (lambda (d)
                            (or (string=? d (*default-code-directory*))
                                (char=? (string-ref d 0) #\.)))
                          (cadr dir-content)))
        (for-each (lambda (f)
                    (copy-file-to-dir! f (->path dir (*default-code-directory*)))
                    (delete! f))
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
            (values libs defs progs)))))))
