(import (scheme base)
        (cyclone test))

(define (run-tests)
  (test-begin "Cyclone Winds")
  
  (test-group "Remote work (read-only procedures)"
    (test 0 (system "./winds -v index"))
    (test 0 (system "./winds -v local-status"))
    (test 0 (system "./winds -v search srfi"))
    (test 0 (system "./winds -v info iset")))

  (test-group "Remote work (read and write procedures)"
    (test 0 (system "sudo ./winds -v install iset"))
    (test 0 (system "./winds -v local-status"))
    (test 0 (system "sudo ./winds -v reinstall iset"))
    (test 0 (system "sudo ./winds -v upgrade string"))
    (test 0 (system "./winds -v local-status"))
    (test 0 (system "sudo ./winds -v upgrade"))
    (test 0 (system "sudo ./winds -v uninstall iset"))
    (test 0 (system "sudo ./winds -v uninstall string")))
  
  (test-group "Local work (write procedures)"
    (test 0 (system "./winds -v retrieve array-list"))
    (test 0 (system "cd array-list && ../winds -v build-local"))
    (test 0 (system "./winds -v build-local array-list"))
    (test 0 (system "cd array-list && .././winds -v test-local"))
    (test 0 (system "./winds -v test-local array-list"))
    (test 0 (system "cd array-list && ../winds -v package"))
    (test 0 (system "./winds -v package array-list && rm -Rf array-list"))
    (test 0 (system "./winds -v retrieve srfi-26"))
    (test 0 (system "./winds -v package-srfi srfi-26 && rm -Rf srfi-26")))

  (test-exit))

(run-tests)
