(define-library (libs metadata)
  (import (scheme base)
          (srfi 28) ; basic format strings
          (only (libs common) *default-doc-url*)
          (libs util))
  (export *default-metadata-file*
          ;; Package record
          make-pkg
          pkg?
          get-name set-name!
          get-version set-version!
          get-license set-license!
          get-authors set-authors!
          get-maintainers set-maintainers!
          get-description set-description!
          get-tags set-tags!
          get-docs set-docs!
          get-test set-test!
          get-dependencies set-dependencies!
          get-test-dependencies set-test-dependencies!
          get-foreign-dependencies set-foreign-dependencies!
          get-libraries set-libraries!
          get-programs set-programs!
          get-libraries-names set-libraries-names!
          get-programs-names set-programs-names!
          get-exported-defines set-exported-defines!

          validate-metadata
          pkg->metadata
          metadata->pkg)
  (begin
    ;; Metadata-related procedures (i.e. package.scm)
    (define *default-metadata-file* "package.scm")

    (define (keys alist)
      (if (equal? 'package (car alist))
          (map car (cdr alist))
          (map car alist)))

    ;; TODO: review this validation strategy - seems too naive.
    (define mandatory-parameters
      `((name ,(list (lambda (x) (or (symbol? x) (list? x)))))
        (version ,(list string?))
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
                _libraries _programs _libraries-names _program-names _exported-defines)
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
         #f)))

    (define (pkg->metadata pkg)
      `(package
        (name                  ,(or (get-name pkg) '____))
        (version               ,(or (get-version pkg) "0.1.0"))
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
                     progs)))))))
