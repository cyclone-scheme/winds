(import (scheme base)
        (cyclone test))

(define (run-tests)
  (test-begin "Cyclone Winds")
  
  (test-group "Remote work (read-only procedures)"
    (test 0 (system "cyclone-winds -v index"))
    (test 0 (system "cyclone-winds -v local-status"))
    (test 0 (system "cyclone-winds -v search srfi"))
    (test 0 (system "cyclone-winds -v info iset")))

  (test-group "Remote work (read and write procedures)"
    (test 0 (system "sudo cyclone-winds -v install iset"))
    (test 0 (system "cyclone-winds -v local-status"))
    (test 0 (system "sudo cyclone-winds -v reinstall iset"))
    (test 0 (system "sudo cyclone-winds -v upgrade string"))
    (test 0 (system "cyclone-winds -v local-status"))
    (test 0 (system "sudo cyclone-winds -v upgrade"))
    (test 0 (system "sudo cyclone-winds -v uninstall iset"))
    (test 0 (system "sudo cyclone-winds -v uninstall string")))
  
  (test-group "Local work (write procedures)"
    (test 0 (system "cyclone-winds -v retrieve array-list"))
    (test 0 (system "cd array-list && cyclone-winds -v build-local"))
    (test 0 (system "cyclone-winds -v build-local array-list"))
    (test 0 (system "cd array-list && cyclone-winds -v test-local"))
    (test 0 (system "cyclone-winds -v test-local array-list"))
    (test 0 (system "cd array-list && cyclone-winds -v package"))
    (test 0 (system "cyclone-winds -v package array-list && rm -Rf array-list"))
    (test 0 (system "cyclone-winds -v retrieve srfi-145"))
    (test 0 (system "cyclone-winds -v package-srfi srfi-145 && rm -Rf srfi-145")))

  (test-exit))

(run-tests)
