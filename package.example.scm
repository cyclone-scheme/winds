;; Mandatory parameters
(package
 (name example-package)                 ;; symbol or list of symbols (e.g. (iset constructors))
 (version 0.1)                          ;; floating point
 (license "BSD")                        ;; string
 (author "Arthur" "Justin")             ;; one or more strings
 (description "This is an example package only for demonstration purposes.") ;; string
 (tags "Math" "Numeric computation")    ;; one or more strings

 ;; At the moment libraries and programs are mutually exclusive.
 ;; libraries/programs will be installed in order.
 (libraries
  ((iset constructors) ;; symbol or list of symbols
   (description "constructors for isets"))
  ((iset optimize)
   (description "optimization procedures for iset lib"))
  (iset
   (description "base lib for the iset package")))

 ;; (programs
 ;;  (iset-pp ;; symbol - cyclone-winds will append a .scm extension to find it.
 ;;  (description "Simple pretty printer for isets used in command line.")))
 
 ;; Optional parameters
 (maintainer "Arthur")                  ;; one or more strings
 (dependencies (iset base))             ;; zero or more symbols or list of symbols
 (test-dependencies)                    ;; zero or more symbols or list of symbols
 (foreign-dependencies (debian . (libck-dev libtommath-dev))
                       (ubuntu . (libtommath-dev))
                       (fedora . (libtommath-devel)))
 (documentation))                       ;; string pointing to documentation URL
