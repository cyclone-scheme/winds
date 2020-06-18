(define-library (libs lock)
  (import (scheme base)
          (only (scheme write) display)
          (srfi 28) ; basic format strings
          (only (srfi 18) thread-sleep!)
          (only (libs common) *default-lock-file*)
          (only (libs system-calls) touch! delete!))
  (export with-file-lock
          try-to-lock-file!
          unlock-file!)
  (begin
    (define (try-to-lock-file!)
      (let lp ((t 0))
        (cond ((and (> t 10) (file-exists? *default-lock-file*))
               (display (format "Unable to acquire lock ~a~%Is another instance of Cyclone-Winds still running?~%"
                                *default-lock-file*)
                        (current-error-port))
               (exit 1))
              ((file-exists? *default-lock-file*)
               (display (format "Is another instance of Cyclone-Winds running? Retrying...(~a)~%" t))
               (thread-sleep! 1)
               (lp (+ t 1)))
              (else
               (touch! *default-lock-file*)))))
    
    (define (unlock-file!)
      (delete! *default-lock-file*))

    (define-syntax with-file-lock
      (syntax-rules ()
        ((_ body)
         (begin
           (try-to-lock-file!)
           body
           (unlock-file!)))))))
