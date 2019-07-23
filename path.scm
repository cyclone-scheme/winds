(define-c directory-list
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

   return_closcall1(data, k, &l);"
  )

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

(define (->path . args)
  (if (null? args)
      ""
      (let* ((args0 (->string (car args)))
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
                      "/tmp")))
    (->path temp-dir
            (string-append (if (null? prefix) "" (car prefix))
                           (number->string (random-integer 10000000000000000000))))))

