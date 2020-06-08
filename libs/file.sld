(define-library (libs file)
  (import (scheme base)
          (scheme process-context)
          (only (scheme cyclone util) filter)
          (srfi 27) ; random numbers
          (only (libs common) *doc-candidates*)
          (libs util)
          (libs metadata))
  (export directory-content
          path-dir
          path-strip-directory
          path-strip-extension
          path-extension
          ->path
          random-temp-dir
          test-file?
          code-files
          sld-files
          scm-files
          doc-file?)
  (include-c-header "<dirent.h>")
  (begin
    (define-c directory-content
      "(void *data, int argc, closure _, object k, object directory)"
      "object file_list = NULL;
       object dir_list = NULL;
       
       Cyc_check_str(data, directory);
     
       DIR* d = opendir(string_str(directory));
       if (d == NULL)
           Cyc_rt_raise2(data, \"Could not open directory: \", (object *) directory);
    
       struct dirent *dir; // for the directory entries
       while((dir = readdir(d)) != NULL)
       {
           object ps = alloca(sizeof(string_type));
           alloca_pair(pl, ps, NULL);
           make_utf8_string(data, s, dir->d_name);
           memcpy(ps, &s, sizeof(string_type));
    
           if(dir->d_type != DT_DIR) {
               ((list) pl)->pair_cdr = file_list;
               file_list = pl;
           }
           else if(dir->d_type == DT_DIR && strcmp(dir->d_name, \".\") != 0 && strcmp(dir->d_name,\"..\") != 0)  {
               ((list) pl)->pair_cdr = dir_list;
               dir_list = pl;
           }
       }
       closedir(d);
    
       make_pair(l2, (object) dir_list, NULL);
       make_pair(l, (object) file_list, &l2);
    
       return_closcall1(data, k, &l);")

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

    (define (path-strip-directory path)
      (let ((i (string-find-right path #\/)))
        (if (= 0 i)
            path
            (substring path (+ 1 i) (string-length path)))))

    (define (path-extension-pos path)
      (let ((start 0)
            (end (string-length path)))
        (let lp ((i end) (dot #f))
          (if (<= i start)
              #f
              (let* ((i2 (- i 1))
                     (ch (string-ref path i2)))
                (cond ((eqv? #\. ch)
                       (and (< i end) (lp i2 (or dot i))))
                      ((eqv? #\/ ch) #f)
                      (dot)
                      (else (lp i2 #f))))))))

    (define (path-strip-extension path)
      (let ((i (path-extension-pos path)))
        (if i
            (substring path 0 (- i 1))
            path)))

    (define (path-extension path)
      (let ((i (path-extension-pos path))
            (len (string-length path)))
        (if i
            (substring path i len)
            "")))

    (define (->path . args)
      (if (null? args)
          ""
          (let* ((arg (car args))
                 (args0 (if (list? arg)
                            (string-join (map ->string arg) #\/)
                            (->string arg)))
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
                (let ((x (trim-trailing-slash (->string (car ls)))))
                  (cond
                   ((string=? x "")
                    (lp (cdr ls) res))
                   ((eqv? #\/ (string-ref x 0))
                    (lp (cdr ls) (cons x res)))
                   (else
                    (lp (cdr ls) (cons x (cons "/" res))))))))))))

    (define (random-temp-dir . prefix)
      (let ((temp-dir (or (get-environment-variable "TMPDIR")
                          (get-environment-variable "TEMP")
                          (get-environment-variable "TMP")
                          ".")))
        (->path temp-dir
                (string-append (if (null? prefix) "" (car prefix))
                               (number->string (random-integer 10000000000000000000))))))

    (define (test-file? file pkg)
      (or (string-contains file "test")
          (if (null? pkg)
              #f
              (string=? file (or (get-test pkg) "")))))

    (define (code-files files . pkg)
      (let ((pkg (if (null? pkg) '() (car pkg))))
        (filter (lambda (f)
                  (or (string=? (path-extension f) "sld")
                      (and (string=? (path-extension f) "scm")
                           (not (test-file? f pkg))
                           (not (string=? f *default-metadata-file*)))))
                files)))

    (define (sld-files files)
      (filter (lambda (f)
                (string=? (path-extension f) "sld"))
              files))

    (define (scm-files files)
      (filter (lambda (f)
                (string=? (path-extension f) "scm"))
              files))

    (define (doc-file? file)
      (any (lambda (e)
             (not (eq? e #f)))
           (map (lambda (c)
                  (string-contains file c))
                *doc-candidates*)))))
