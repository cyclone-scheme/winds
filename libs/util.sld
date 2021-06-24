(define-library (libs util)
  (import (scheme base)
          (only (scheme list) split-at)
          (only (scheme cyclone util) filter string-split))
  (export chain
          get-parameter-value
          get-parameter-all-occurrences
          remove
          ->string
          string-contains
          string-find-right
          string-join
          string-split
          string->proper-symbol
          trim-trailing-slash)
  (begin
    (define (remove pred lst)
      (filter (lambda (x) (not (pred x))) lst))

    ;; Returns a list or false
    (define (get-parameter-value param metadata)
      (let ((param (assoc param metadata)))
        (if (and param (not (null? param)))
            (if (and (> (length param) 2)
                     (string? (cadr param)))
                (string-join (cdr param) " ")
                (cadr param))
            #f)))

    ;; Returns a list
    (define (get-parameter-all-occurrences param metadata)
      (filter (lambda (e)
                (eq? param (car e)))
              metadata))

    ;; String procedures imported and adapted from Chibi Scheme
    (define (trim-trailing-slash str)
      (if (string=? str "")
          ""
          (let* ((len (string-length str))
                 (last (- len 1)))
            (if (eqv? #\/ (string-ref str last))
                (substring str 0 last)
                str))))

    (define (->string x)
      (cond ((string? x) x)
            ((symbol? x) (symbol->string x))
            ((number? x) (number->string x))
            ((null? x) "")
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
      (let ((len (string-length str)))
        (if (= 0 len)
            0
            (let ((start 0)
                  (end (- (string-length str) 1)))
              (let lp ((end end))
                (cond ((eq? start end) start)
                      ((char=? char (string-ref str end)) end)
                      (else (lp (- end 1)))))))))

    (define (string-contains a b . o)   ; very slow
      (let ((alen (string-length a))
            (blen (string-length b)))
        (let lp ((i (if (pair? o) (car o) 0)))
          (and (<= (+ i blen) alen)
               (if (string=? b (substring a i (+ i blen)))
                   i
                   (lp (+ i 1)))))))

    ;; Convert command-line string list into proper list
    ;; i.e. "(crypto md5)" -> (crypto md5)
    (define (string->proper-symbol s)
      (let* ((len (string-length s))
             (first-char (string-ref s 0))
             (last-char (string-ref s (- len 1))))
        (if (and (equal? first-char #\( ) (equal? last-char #\) ))
            (map string->symbol (string-split (substring s 1 (- len 1)) #\space))
            (string->symbol s))))

    (define-syntax chain
      (syntax-rules …₁ ()
                    ((_ initial-value) initial-value)
                    ((_ initial-value (step …₁) …₁)
                     (chain initial-value _ ... (step …₁) …₁))
                    ((_ initial-value placeholder (step …₁) …₁)
                     (chain initial-value placeholder ... (step …₁) …₁))
                    ((_ initial-value placeholder ellipsis (first-step …₁) (next-step …₁) …₁)
                     (let ()
                       (define-syntax %chain
                         (syntax-rules …₂ (placeholder ellipsis)
                                        ; (_ in-step out-step in-vars out-vars in-steps out-steps)
                                       ((_ () () () ((var) …₂) () (step …₂ last-step))
                                        (let* ((var step) …₂) last-step))
                                       ((_ () () () (vars …₂) () (step …₂ last-step))
                                        (let*-values ((vars step) …₂) last-step))
                                       ((_ () () () out-vars (step . in-steps) out-steps)
                                        (%chain step () () out-vars in-steps out-steps))
                                       ((_ () step () (out-vars …₂) in-steps (out-steps …₂))
                                        (%chain () () () (out-vars …₂ ignored) in-steps (out-steps …₂ step)))
                                       ((_ () step vars (out-vars …₂) in-steps (out-steps …₂))
                                        (%chain () () () (out-vars …₂ vars) in-steps (out-steps …₂ step)))
                                       ((_ (placeholder ellipsis) (step …₂) () (out-vars …₂) in-steps (out-steps …₂))
                                        (%chain () () () (out-vars …₂ chain-rest-var) in-steps (out-steps …₂ (apply step …₂ chain-rest-var))))
                                       ((_ (placeholder ellipsis) (step …₂) (vars …₂) (out-vars …₂) in-steps (out-steps …₂))
                                        (%chain () () () (out-vars …₂ (vars …₂ . chain-rest-var)) in-steps (out-steps …₂ (apply step …₂ chain-rest-var))))
                                       ((_ (placeholder ellipsis . rest) . _)
                                        (syntax-error "_ ... can only be used as a final argument"))
                                       ((_ (placeholder . in-step) (out-step …₂) (vars …₂) . rest)
                                        (%chain in-step (out-step …₂ chain-var) (vars …₂ chain-var) . rest))
                                       ((_ (x . in-step) (out-step …₂) . rest)
                                        (%chain in-step (out-step …₂ x) . rest))))
                       (%chain (first-step …₁) () () () ((next-step …₁) …₁) (initial-value))))))))
