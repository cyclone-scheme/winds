(define-library (libs lock)
  (import (scheme base)
          (only (libs common) *default-lock-file*)
          (only (libs system-calls) touch! delete!))
  (export with-file-lock
          try-to-lock-file!
          unlock-file!)
  (begin
    (define (try-to-lock-file!)
      (let lp ((t 0))
        (cond ((and (> t 1000) (file-exists? *default-lock-file*))
               (error "Another Cyclone-Winds instance is running?"))
              ((file-exists? *default-lock-file*)
               ;; (thread-sleep! 1)
               (lp (+ t 1)))
              ((touch! *default-lock-file*)
               *default-lock-file*)
              (else
               (error "Could not create lock!")))))
    
    (define (unlock-file!)
      (delete! *default-lock-file*))

    (define-syntax with-file-lock
      (syntax-rules ()
        ((_ body)
         (begin
           (try-to-lock-file!)
           body
           (unlock-file!)))))))
