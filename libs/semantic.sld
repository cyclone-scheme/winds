(define-library (libs semantic)
  (import (scheme base)
          (cyclone format)
          (only (srfi 1) fold)
          (only (cyclone match) match)
          (only (scheme cyclone util) filter)
          (only (libs util) chain string-contains string-join string-split))
  (export find-version
          greatest-version
          latest-version
          sanitize-version)
  (begin

    ;; when converted to numbers by version->number-list, stars ("*") become '#f'.
    (define (star? slot)
      (not slot))

    ;; "1.2.4" -> '(1 2 4)
    ;; "1.2.*" -> '(1 2 #f)
    (define (version->number-list v)
      (map string->number (string-split v #\.)))

    ;; '(1 2 4) -> "1.2.4"
    (define (number-list->version l)
      (string-join (map number->string l) #\.))
    
    (define (find-version v versions)
      ;; "major.minor.patch" -> '(major minor patch)
      (define major-slot car)
      (define minor-slot cadr)
      (define patch-slot caddr)

      (define (filter-by slot value versions)
        (filter (lambda (v)
                  (= value (slot (version->number-list v))))
                versions))

      (let ((v (sanitize-version v)))
        (match-let (((major minor patch) (version->number-list v)))
          (if (star? major)
              (latest-version versions)
              (let* ((versions (map sanitize-version versions))
                     (versions-filtered-by-major
                      (filter-by major-slot major versions))) 
                (if (star? minor)
                    (latest-version versions-filtered-by-major)
                    (let ((versions-filtered-by-minor
                           (filter-by minor-slot minor versions-filtered-by-major)))
                      (if (star? patch)
                          (latest-version versions-filtered-by-minor)
                          (let ((versions-filtered-by-patch
                                 (filter-by patch-slot patch versions-filtered-by-minor)))
                            (if (null? versions-filtered-by-patch)
                                #f
                                (car versions-filtered-by-patch)))))))))))

    (define (latest-version versions-list)
      (fold (lambda (v greatest)
              (greatest-version v greatest))
            "0.0.0"
            (map sanitize-version versions-list)))

    (define (greatest-version v1 v2)
      (let ((v1 (sanitize-version v1))
            (v2 (sanitize-version v2)))
        (match-let (((major1 minor1 patch1) (version->number-list v1))
                    ((major2 minor2 patch2) (version->number-list v2)))
          (or (and (star? major1) v1)
              (and (star? major2) v2)
              (and (> major1 major2) v1)
              (and (< major1 major2) v2)
              ;; Majors are equal
              (and (star? minor1) v1)
              (and (star? minor2) v2)
              (and (> minor1 minor2) v1)
              (and (< minor1 minor2) v2)
              ;; Majors and minors are equal
              (and (star? patch1) v1)
              (and (star? patch2) v2)
              (and (> patch1 patch2) v1)
              (and (< patch1 patch2) v2)
              ;; Versions are equal
              v1))))

    (define (sanitize-start-dot v) ;; ".2"
      (if (eq? (string-ref v 0) #\.) 
          (string-append "*" v)
          v))

    (define (sanitize-end-dot v) ;; "2."
      (if (eq? (string-ref v (- (string-length v) 1)) #\.) 
          (string-append v "*")
          v))

    (define (contains-dot? v)
      (and (string-contains v ".") #t))
    ;; "1.2.4" -> '("1" "2" "4")
    ;; "1.2.*" -> '("1" "2" "*")
    (define (version->string-list v)
      (string-split v #\.))

    ;; '("1" "2" "4") -> "1.2.4"
    (define (string-list->version l)
      (string-join l #\.))

    (define (sanitize-version v)
      (cond
       ((or (equal? v "") (equal? v "*")) "*.*.*")
       ((contains-dot? v)
        (chain v
               (sanitize-start-dot _)
               (sanitize-end-dot _)
               (version->string-list _)
               (match _
                 ;; Due to some reason, the (or ...) in match is not working - BUG? 
                 ;; Sanitize stars
                 (("*" min) "*.*.*")
                 (("*" min pat) "*.*.*")
                 ((maj "*") (string-append maj ".*.*"))
                 ((maj "*" pat) (string-append maj ".*.*"))
                 ((maj min "*") (string-append maj "." min ".*"))
                 ;; Sanitize length
                 ((maj min pat) (string-append maj "." min "." pat))
                 ((maj min) (string-append maj "." min ".*"))
                 ((maj) (string-append maj ".*.*"))
                 (else (error (format "Invalid name-version syntax for package ~s" v))))))
       ((string->number v) ;; only major and a valid one, eg. "2"
        (string-append v ".*.*"))
       (else (error (format "Invalid name-version syntax for package ~s" v)))))))
