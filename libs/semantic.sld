(define-library (libs semantic)
  (import (scheme base)
          (only (libs util) string-contains string-split)
          (only (cyclone match) match)
          )
  (export )

  (begin
    (define (greatest v1 v2)
      (match-let (((major1 minor1 patch1) (version->number-list (sanitize-version v1)))
                  ((major2 minor2 patch2) (version->number-list (sanitize-version v2))))
        (or (and (> major1 major2) v1)
            (and (< major1 major2) v2)
            ;; Majors are equal
            (and (> minor1 minor2) v1)
            (and (< minor1 minor2) v2)
            ;; Majors and minors are equal
            (and (> patch1 patch2) v1)
            (and (< patch1 patch2) v2)
            ;; Versions are equal
            v1)))

    (define (sanitize-version v)
      (cond
       ((or (equal? v "") (equal? v "*")) "0.0.0")
       ((contains-dot? v)
        (chain v
               (sanitize-begin-dot _)
               (sanitize-end-dot _)
               (version->string-list _)
               (match _
                 (() "0.0.0")
                 ;; Sanitize star
                 (("*" min) "0.0.0")
                 (("*" min pat) "0.0.0")
                 ((maj "*") (string-append maj ".0.0"))
                 ((maj "*" pat) (string-append maj ".0.0"))
                 ((maj min "*") (string-append maj "." min ".0"))
                 ;; Sanitize length
                 ((maj min pat) (string-append maj "." min "." pat))
                 ((maj min) (string-append maj "." min ".0"))
                 ((maj) (string-append maj ".0.0"))
                 (else (error "Invalid version length")))))
       ((string->number v) ;; only major and a valid one, eg. "2"
        (string-append v ".0.0"))
       (else
        (error "Unknown version type"))))

    (define (sanitize-begin-dot v) ;; ".2"
      (if (eq? (string-ref v 0) #\.) 
          (string-append "0" v)
          v))

    (define (sanitize-end-dot v) ;; "2."
      (if (eq? (string-ref v (- (string-length v) 1)) #\.) 
          (string-append v "0")
          v))

    (define (contains-dot? v)
      (and (string-contains v ".") #t))

    ;; "1.2.4" -> '(1 2 4)
    (define (version->number-list v)
      (map string->number (string-split v #\.)))

    ;; '(1 2 4) -> "1.2.4"
    (define (number-list->version l)
      (string-join (map number->string l) #\.))

    ;; "1.2.4" -> '("1" "2" "4")
    (define (version->string-list v)
      (string-split v #\.))

    ;; '(1 2 4) -> "1.2.4"
    (define (string-list->version l)
      (string-join l #\.))))
