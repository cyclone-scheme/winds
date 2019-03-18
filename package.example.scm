;; MANDATORY parameters
(package
 (name (example-package))                  ;; list
 (version 0.1)                          ;; floating point
 (license "BSD")                        ;; string
 (author "Arthur" "Justin")             ;; one or more strings
 (description "This is an example package only for demonstration purposes.") ;; string
 (tags "Math" "Numeric computation") ;; one or more strings

 
 ;; OPTIONAL parameters
 (maintainer "Arthur")                  ;; one or more strings
 (dependencies "package-a" "package-b") ;; zero or more strings
 (test-custom)                          ;; strings with custom testing commands
 (test-dependencies) ;; zero or more strings
 (foreign-dependencies (debian . (libck-dev libtommath-dev))
                       (ubuntu . (libtommath-dev))
                       (fedora . (libtommath-devel)))
 (documentation)                        ;; string pointing to documentation URL
 (tags "math" "data-structures")

 
 ;; AT LEAST ONE parameter ('library' AND/OR 'program') XOR ('custom')
 ;; If set, libraries and programs will be installed in order
 (library    
  (name (cyclone iset constructors))    ;; list
  (description "Constructors for isets"))

 ;; For programs, cyclone-winds will automatically append .scm to the name specified  
 ;; below in order to build it.  
 (program
  (name "iset-pp") ;; string 
  (description "Simple pretty printer for isets used in command line."))

 ;; Libraries and programs will be built and installed by cyclone-winds
 ;; default procedures. If they don't suit your needs, use the 'custom'
 ;; parameter *instead*. Please note that is 'custom' is set, the user
 ;; will be alerted for potential dangerous code.
 ;; (custom
 ;;  ;; Aliases:
 ;;  ;; ~BIN~  - Directory for binaries
 ;;  ;; ~DATA~ - Directory for scheme libraries to be installed 
 ;;  ;; ~LIB~ - Directory for C libraries (not normally used)
 ;;  ;; ~INCLUDE~ - Directory for C headers (not normally used)
 ;;  (build
 ;;   "cyclone -O2 -A . iset/constructors.sld"
 ;;   "cyclone -O2 -A . iset-pp.scm")
 ;;  (install
 ;;   "mkdir -p ~DATA~/cyclone/iset"
 ;;   "install -m0644 ./iset/constructors.{o,a} ~DATA~/cyclone/iset/"
 ;;   "install -m0644 ./iset/constructors.{o,a} ~BIN~") 
 ;;  (uninstall
 ;;   "rm ~DATA~/cyclone/iset/constructors.{o,a}"
 ;;   "rm ~BIN~/iset-pp.scm"))

 )       ;; strings  
