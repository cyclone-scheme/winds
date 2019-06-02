(import (scheme base)
        (scheme process-context)
        (scheme cyclone util)
        (srfi 27)  ;; random numbers
        (srfi 28)) ;; basic format strings

;; String procedures imported and adapted from Chibi Scheme
(define (trim-trailing-slash s)
  (let* ((len (string-length s))
         (last (- len 1)))
    (if (eqv? #\/ (string-ref s last))
        (substring s 0 last)
        s)))

(define (x->string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        ((list? x) (string-join x #\/))
        (else (error "Could not convert to string" x))))

(define (string-join orig-ls . o)
  (let ((sep (if (pair? o) (car o) ""))
        (out (open-output-string)))
    (let lp ((ls orig-ls))
      (cond
       ((pair? ls)
        (if (and sep (not (eq? ls orig-ls)))
            (write-string sep out))
        (write-string (car ls) out)
        (lp (cdr ls)))))
    (get-output-string out)))

(define (string-find-right str char)
  (let ((start 0)
        (end (- (string-length str) 1)))
    (let lp ((end end))
      (cond ((eq? start end) start)
            ((char=? char (string-ref str end)) end)
            (else (lp (- end 1)))))))
;; End of string procedures

;; Path procedures imported and adapted from Chibi Scheme
(define (path-dir path)
  (if (string=? path "")
      "."
      (let* ((start 0)
             (trimmed-string (trim-trailing-slash path))
             (end (- (string-length trimmed-string) 1)))
        (if (eq? start end)
            "/"
            (let ((slash (string-find-right path #\/)))
              (if (eq? start slash)
                  "."
                  (let ((start2 (string-length (substring path start slash))))
                    (if (eq? start start2)
                        "/"
                        (substring path start start2)))))))))

(define (make-path . args)
  (if (null? args)
      ""
      (let* ((args0 (x->string (car args)))
             (start (trim-trailing-slash args0)))
        (let lp ((ls (cdr args))
                 (res (if (string=? "" start) '() (list start))))
          (cond
           ((null? ls)
            (if (and (null? res) (not (string=? "" args0)))
                "/"
                (string-join (reverse res))))
           ((pair? (car ls))
            (lp (append (car ls) (cdr ls)) res))
           (else
            (let ((x (trim-trailing-slash (x->string (car ls)))))
              (cond
               ((string=? x "")
                (lp (cdr ls) res))
               ((eqv? #\/ (string-ref x 0))
                (lp (cdr ls) (cons x res)))
               (else
                (lp (cdr ls) (cons x (cons "/" res))))))))))))
;; End of path procedures

(define (random-temp-dir . prefix)
  (let ((temp-dir (or (get-environment-variable "TMPDIR")
                      (get-environment-variable "TEMP")
                      (get-environment-variable "TMP")
                      "/tmp")))
    (make-path temp-dir
               (string-append (if (null? prefix) "" (car prefix))
                              (number->string (random-integer 10000000000000000000))))))

;; System call procedures
(define (ok? return-code)
  (eq? 0 return-code))

(define (download url outfile)
  ;; --directory-prefix is used so wget can automatically mkdir it if needed.
  (let ((result (system (format "wget --quiet --show-progress -O ~a ~a" outfile url))))
    (if (ok? result)
        (begin (display (format "[OK] File ~a downloaded~%" outfile))
               outfile)
        (error (format "Could not download ~a. Lack of permissions? Return code" outfile) result))))

(define (validate-sha256sum sha256sum file)
  (let ((result (system (format "echo ~a ~a | sha256sum --status --check -" sha256sum file))))
    (if (ok? result)
        (begin (display (format "[OK] Passed sha256sum verification~%" ))
               file)
        (error (format "Incorrect informed sha256sum for file ~a. Return code" file) result))))

(define (extract file dir)
  (let ((result (system (format "tar zxf ~a --strip=1 -C ~a" file dir))))
    (if (ok? result)
        (begin (display (format "[OK] Extracted ~a into ~a~%" file dir))
               dir)
        (error (format "Could not extract ~a into ~a. Lack of permissions? Return code" file dir) result))))

(define (delete file-or-dir)
  (let ((result (system (format "rm -Rf ~a" file-or-dir))))
    (if (ok? result)
        (begin (display (format "[OK] Deleted ~a~%" file-or-dir))
               file-or-dir)
        (error (format "Could not delete ~a. Lack of permissions? Return code" file-or-dir) result))))

(define (compile file . dir)
  (let ((dir (if (null? dir) "." dir))
        (result (system (format "cyclone -A ~a ~a" dir file))))
    (if (ok? result)
        (begin (display (format "[OK] File ~a compiled~%" file))
               file)
        (error (format "Could not compile file ~a. Return code" file) result))))

(define (make-dir path)
  (let ((result (system (format "mkdir -p ~a" path))))
    (if (ok? result)
        path
        (error (format "Could not create path ~a. Lack of permissions? Return code" path) result))))

(define (copy-file file to-dir)
  (make-dir to-dir)
  (let ((result (system (format "cp ~a ~a" file to-dir))))
    (if (ok? result)
        (begin (display (format "[OK] File ~a copied to ~a~%" file to-dir))
               file)
        (error (format "Could not copy file ~a into ~a. Lack of permissions? Return code" file to-dir) result))))
;; End of system call procedures

;; Index-related procedures
(define *default-index-url* "https://raw.githubusercontent.com/cyclone-scheme/cyclone-winds/master/index.scm")

(define (index-content url)
  (let* ((tmp-dir (random-temp-dir))
         (index-path (make-path tmp-dir "index.scm")))
    (make-dir tmp-dir)
    (display (format "~%Retrieving index file...~%"))
    (download url index-path)
    (let ((content (read (open-input-file index-path))))
      (delete tmp-dir)
      content)))

(define (versions package)
  (cdr package))

(define (latest-version versions)
  (car versions))

(define (tarball-url+sha256sum index name . version)
  (let ((pkg (assoc name (cdr index))))
    (if pkg
        (let ((versions (versions pkg)))
          (if (or (not version) (null? version) (not (number? version)))
              (cdr (latest-version versions))
              (let ((pkg-version (assoc (car version) versions)))
                (if pkg-version
                    (cdr pkg-version)
                    (error (format "Could not locate specified version ~a~%" version))))))
        (error (format "Could not locate package by name ~a~%" name)))))

(define (url url+sha256sum)
  (car url+sha256sum))

(define (sha256sum url+sha256sum)
  (cadr url+sha256sum))
;; End of index-related procedures

;; Metadata-related procedures (i.e. package.scm)
(define *default-metadata-file* "package.scm")

(define (keys alist)
  (map car alist))

(define mandatory-parameters
  `((version ,(list number?))
    (license ,(list string?))
    (author ,(list string?))
    (description ,(list string?))
    (tags ,(list string?))))

(define optional-parameters
  `((maintainer ,(list string?))
    (dependencies ,(list string?))
    (test-dependencies ,(list string?))
    (foreign-dependencies ,(list string?))
    (documentation ,(list string?))))

;; TODO: implement 'custom' parameter
(define exclusive-parameters `((libraries ,(list list?))
                               (programs ,(list list?))))

(define available-parameters (append mandatory-parameters
                                     optional-parameters
                                     exclusive-parameters))

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
        ((and (member key (keys mandatory-parameters))
              (null? parameter-content))
         (error `(,key " should not be null in package.scm")))
        ;; Check parameter type
        ((for-each
          (lambda (proc)
            (if (not (null? parameter-content))
                (if (not (apply proc parameter-content))
                    (error key
                           (string->symbol "is expected to be of a type that satisfies")
                           proc))))
          check-procedures)))))
   (keys metadata))
  (for-each
   (lambda (param)
     (if (not (member param (keys metadata)))
         (error param "is a mandatory parameter in package.scm")))
   (keys mandatory-parameters))
  (let ((keys (keys metadata)))
    (if (and (member 'libraries keys)
             (member 'programs keys))
        (error "Library and program both defined. The author must create separate packages for each."))
    (if (not (or (member 'libraries keys)
                 (member 'programs keys)))
        (error "At least one library/program must be defined.")))
  #t) ;; returns gracefully if everything is ok

(define (library-list metadata)
  (assoc 'libraries (cdr metadata)))

(define (library-names lib-list)
  (map car (cdr lib-list)))

(define (program-list metadata)
  (assoc 'programs (cdr metadata)))

(define (program-names prog-list)
  (map car (cdr prog-list)))
;; End of metadata-related procedures (i.e. package.scm)


;; Package-related procedures
(define *library-installable-extensions* '("*.o" "*.so" "*.sld" "*.meta"))

(define (*library-installion-dir*)
  (let ((lib-path (get-environment-variable "CYCLONE_LIBRARY_PATH")))
    (if lib-path
        (car (string-split lib-path #\:)) ;; return only the first path listed
        (Cyc-installation-dir 'sld) "/")))

(define (*program-installation-dir*)
  (let ((bin-path (Cyc-installation-dir 'bin)))
    (if (string=? bin-path "")
        "/usr/local/bin"
        bin-path)))

(define *base-dir* "cyclone")

(define (retrieve index name version)
  (let* ((version (if (or (not version) (null? version)) #f version))
         (url+sha256sum (tarball-url+sha256sum index name version))
         (pkg-name (if (list? name) (string-join name #\-) name))
         (work-dir (make-path (random-temp-dir pkg-name) *base-dir*))
         (tarball (string-append (if version
                                     (string-join (list pkg-name (x->string (car version))) #\-)
                                     pkg-name)
                                 ".tar.gz"))
         (outfile (make-path work-dir tarball)))
    (make-dir work-dir)
    (display (format "~%Downloading ~a...~%" name))
    (download (url url+sha256sum) outfile)
    (validate-sha256sum (sha256sum url+sha256sum) outfile)
    (extract outfile work-dir)
    (delete outfile)               
    work-dir))

(define (build-libraries lib-list . dir)
  (let ((dir (if (null? dir) "." dir)))
    (for-each
     (lambda (lib)
       (let ((lib-name-path (make-path dir lib)))
         (compile (string-append lib-name-path ".sld"))))
     lib-list)))

(define (install-libraries lib-list . dir)
  (let ((dir (if (null? dir) "." dir)))
    (for-each
     (lambda (lib)
       (let* ((lib-name-path ((make-path lib)))
              (full-lib-name-path ((make-path dir lib))))
         (for-each
          (lambda (ext)
            (copy-file (string-append full-lib-name-path ext)
                       (make-path *library-installation-dir*
                                  (path-dir lib-name-path))))
          *library-installable-extensions*)))
     lib-list)))

(define (build-programs prog-list . dir)
  (let ((dir (if (null? dir) "." dir)))
    (for-each
     (lambda (prog)
       (let ((prog-name-path (make-path dir prog)))
         (compile (string-append prog-name-path ".scm"))))
     prog-list)))

(define (install-programs prog-list . dir)
  (let ((dir (if (null? dir) "." dir)))
    (for-each
     (lambda (prog)
       (let ((prog-name-path (make-path dir prog)))
         (copy-file prog-name-path *program-installation-dir*)))
     prog-list)))


;; TODO: resolve and install dependencies
(define (install-package name . version)
  (let* ((index (index-content *default-index-url*))
         (work-dir (retrieve index name (if (null? version) #f version)))
         (metadata (read (open-input-file (make-path work-dir *default-metadata-file*))))
         (valid-metadata? metadata)
         (programs (program-list metadata))
         (libraries (library-list metadata)))
    (cond ((and (not programs) libraries)
           (build-libraries (library-names libraries) work-dir))          
          ((and (not libraries) programs)
           (build-programs (program-names programs) work-dir))
          (else
           (error (format "Could not install package ~a due to invalid metadata" name))))))
;; End of package-related procedures

;; User interface procedures
(define (install name . version) #t)
(define (test name . version) #t)
(define (uninstall name) #t)
(define (search wildcard) #t)
(define (info name . version) #t)
(define (local-status) #t)
(define (remote-status) #t)

(define (usage return-code)
  (display "Usage: cyclone-winds [OPTIONS [PACKAGES]]
     help  -  print usage
     list  -  list the results
     retrieve PACKAGE [PACKAGE2 ...]  - downloads specified package(s)
     install PACKAGE [PACKAGE2 ...] - retrieve and install specified package(s)
     uninstall PACKAGE [PACKAGE2 ...] - remove specified package(s)
     search WILDCARD - search for packages that partially match the specified wildcard
     info PACKAGE - list all metadata about specified package
     local-status - lista all installed packages
 "))  
;; End of user interface procedures
