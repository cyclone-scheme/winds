(define-library (libs semantic)
  (import (scheme base)
          (only (scheme sort) list-sort)
          (cyclone format)
          (only (cyclone match) match)
          (only (libs util) chain string-contains string-split))
  (export contains-dot?
          greatest-version
          sanitize-begin-dot
          sanitize-end-dot
          sanitize-version
          version->number-list
          number-list->version
          version->string-list
          string-list->version)
  (begin

    (define (latest-version versions-list)
      (if (null? versions-list)
          (error "Empty list of versions - check package name-version syntax")
          (car (list-sort greatest-version versions-list))))
    
    (define (greatest-version v1 v2)
      (match-let (((major1 minor1 patch1) (version->number-list (sanitize-version v1)))
                  ((major2 minor2 patch2) (version->number-list (sanitize-version v2))))
        ;; in the conversion to numbers, stars ("*") become #f, so:
        (or (and (not major1) v1)
            (and (not major2) v2)
            (and (> major1 major2) v1)
            (and (< major1 major2) v2)
            ;; Majors are equal
            (and (not minor1) v1)
            (and (not minor2) v2)
            (and (> minor1 minor2) v1)
            (and (< minor1 minor2) v2)
            ;; Majors and minors are equal
            (and (not patch1) v1)
            (and (not patch2) v2)
            (and (> patch1 patch2) v1)
            (and (< patch1 patch2) v2)
            ;; Versions are equal
            v1)))

    (define (sanitize-version v)
      (cond
       ((or (equal? v "") (equal? v "*")) "*.*.*")
       ((contains-dot? v)
        (chain v
               (sanitize-begin-dot _)
               (sanitize-end-dot _)
               (version->string-list _)
               (match _
                 (() "*.*.*")
                 ;; Sanitize star
                 (("*" min) "*.*.*")
                 (("*" min pat) "*.*.*")
                 ((maj "*") (string-append maj ".*.*"))
                 ((maj "*" pat) (string-append maj ".*.*"))
                 ((maj min "*") (string-append maj "." min ".*"))
                 ;; Sanitize length
                 ((maj min pat) (string-append maj "." min "." pat))
                 ((maj min) (string-append maj "." min ".*"))
                 ((maj) (string-append maj ".*.*"))
                 (else (error (format "Invalid package name-version syntax ~s" v))))))
       ((string->number v) ;; only major and a valid one, eg. "2"
        (string-append v ".*.*"))
       (else
        (else (error (format "Invalid package name-version syntax ~s" v))))))

    (define (sanitize-begin-dot v) ;; ".2"
      (if (eq? (string-ref v 0) #\.) 
          (string-append "*" v)
          v))

    (define (sanitize-end-dot v) ;; "2."
      (if (eq? (string-ref v (- (string-length v) 1)) #\.) 
          (string-append v "*")
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
