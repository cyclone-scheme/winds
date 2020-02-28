(import (scheme base)
        (cyclone test))

(define (run-tests)

  (test-begin "Cyclone Winds")

  (test-group "Remote work (read-only procedures)"
	      (test 0 (system "cyclone-winds index"))
	      (test 0 (system "cyclone-winds local-status"))
	      (test 0 (system "cyclone-winds info iset")))

  (test-group "Remote work (read and write procedures)"
	      (test 0 (system "sudo cyclone-winds install iset"))
	      (test 0 (system "cyclone-winds local-status"))
	      (test 0 (system "sudo cyclone-winds uninstall iset")))

  (test-group "Local work (write procedures)"
	      (test 0 (system "cyclone-winds retrieve array-list"))
	      (test 0 (system "cd array-list && cyclone-winds build-local"))
	      (test 0 (system "cyclone-winds build-local \"array-list\""))
	      (test 0 (system "cd array-list && cyclone-winds test-local"))
	      (test 0 (system "cyclone-winds test-local \"array-list\""))
	      (test 0 (system "cd array-list && cyclone-winds package"))
	      (test 0 (system "cyclone-winds package \"array-list\" && rm -Rf array-list cyclone")))

  (test-exit))

(run-tests)
