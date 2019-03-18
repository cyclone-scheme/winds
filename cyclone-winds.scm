(import (scheme base)
        (scheme process-context)
        (scheme cyclone util)
        (srfi 28)) ;;basic format strings

(define (library-install-dir)
  (let ((lib-path (get-environment-variable "CYCLONE_LIBRARY_PATH")))
    (if lib-path
        (car (string-split lib-path #\:)) ;; return only the first path listed
        (Cyc-installation-dir 'sld))))

(define (temp-dir)
  (or (get-environment-variable "TMPDIR")
      (get-environment-variable "TEMP")
      (get-environment-variable "TMP")
      "/tmp"))

(define (usage return-code)
  (display "Usage:
     --help  -  print usage
     --list  -  list the results
 "))

(define l
  '((version 0.1)
    (license "MIT")
    (author "Arthur" "Justin")
    (description "An example package")
    (tags "Web programming" "SXML processor")
    (tarball-url "https://github.com/example/example.tar.gz")
    (sha-256 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    ;; Optional parameters
    (maintainer 2)
    (dependencies "package-a" "package-b")
    (test-dependencies)
    (foreign-dependencies)
    (documentation)))

(define (keys alist)
  (map car alist))

(define mandatory-parameters
  `((version ,(list number?))
    (license ,(list string?))
    (author ,(list string?))
    (description ,(list string?))
    (tags ,(list string?))
    (tarball-url ,(list string?))
    (sha-256 ,(list string?))))

(define optional-parameters
  `((maintainer ,(list string?))
    (dependencies ,(list string?))
    (test-dependencies ,(list string?))
    (foreign-dependencies ,(list string?))
    (documentation ,(list string?))))

(define other-parameters `((library ,(list list?))
                           (program ,(list list?))))

(define available-parameters (append mandatory-parameters
                                     optional-parameters
                                     other-parameters))

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
  #t) ;; returns gracefully if everything is ok


(let ((temp-dir (temp-dir))
      (tarball "b")
      (sha-256 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b856"))
  (let ((return-code
         (system (format "cd ~a; wget ~a" temp-dir tarball))))
    (if (not (eq? 0 return-code))
        (error "Could not download tarball with wget. Return code" return-code)))
  (let ((return-code
         (system (format "cd ~a; echo ~a ~a | sha256sum --status --check -"
                         temp-dir sha-256 tarball))))
    (if (not (eq? 0 return-code))
        (error "sha-256 checksum doesn't match for specified package! Return code" return-code)))
  (let ((return-code
         (system (format "cd ~a; tar zxpvf ~a" temp-dir tarball))))
    (if (not (eq? 0 return-code))
        (error "Could not extract tarball. Return code" return-code)))
  )

(define (retrieve-index)
  "path/to/index.scm")

(define (get-package-metadata name version)
  l) 


(define (retrieve name . version)

  )

(define (build-local name) #t)

(define (install-local name) #t)

(define (test-local name) #t)




(define (build name . version)
  (if (null? version)
      (retrieve name)
      (retrieve name version))
  )


(define (install name . version)
  #t)

(define (test name . version) #t)

(define (uninstall name) #t)

(define (info name . version) #t)

(define (local-status) #t)

(define (remote-status) #t)



;; - implementar procedimentos para lidar com path (string-split URL #\/)
;; - processar linha de commando com cada passo: install <name> [<version>] [--stop-at=retrieve,compile,test], uninstall <name>, info <name> [<version>] (list all info), local-status (num pkgs and list of names), remote-status (num pkgs and list of all packages with desc), install-local 
