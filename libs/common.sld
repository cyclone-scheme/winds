(define-library (libs common)
  (import (scheme base)
          (scheme process-context))
  (export *winds-version*
          *default-code-directory*
          *default-lock-file*
          *default-doc-url*
          *default-doc-file*
          *doc-candidates*
          *library-installable-extensions*
          get-library-installation-dir
          get-program-installation-dir)
  (begin
    (define *winds-version* "0.32.0")

    ;; The only global variable that is a parameter
    (define *default-code-directory* (make-parameter "cyclone"))

    (define *default-lock-file*
      (string-append (get-library-installation-dir) "/" "winds.lock"))
    
    (define *library-installable-extensions* '(".o" ".so" ".sld" ".meta"))
    
    (define *internal-cyclone-libs*
      ;; No need to list (scheme ...) libs because they are obviously internal.
      `(,@(map (lambda (s) `(srfi ,s))
               '(1 2 8 18 27 28 60 69 106 111 113 117 121 128 132 133 143))
        (cyclone concurrent) (cyclone foreign) (cyclone match) (cyclone test)))
    
    (define *default-doc-url* "https://github.com/cyclone-scheme/winds/wiki/")
    (define *default-doc-file* "README.md")
    (define *doc-candidates* '("README" "Readme" "readme"))

    (define (get-library-installation-dir)
      (or (get-environment-variable "CYCLONE_LIBRARY_PATH")
          (Cyc-installation-dir 'sld)))

    (define (get-program-installation-dir)
      (or (get-environment-variable "CYCLONE_PROGRAM_PATH")
          (Cyc-installation-dir 'bin)))))
