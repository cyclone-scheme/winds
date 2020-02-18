(import (scheme base)
        (cyclone test))

(test-group "Remote work (read-only procedures)"
  (test 0 (system "cyclone-winds index"))
  (test 0 (system "cyclone-winds local-status"))
  (test 0 (system "cyclone-winds info \"(cyclone iset)\"")))

(test-group "Remote work (read and write procedures)"
  (test 0 (system "sudo cyclone-winds install \"(cyclone iset)\" "))
  (test 0 (system "cyclone-winds local-status"))
  (test 0 (system "sudo cyclone-winds uninstall \"(cyclone iset)\" ")))

(test-group "Local work (write procedures)"
  (test 0 (system "cyclone-winds retrieve \"(cyclone array-list)\""))
  (test 0 (system "cd array-list && cyclone-winds build-local"))
  (test 0 (system "cd array-list && cyclone-winds build-local \".\""))
  (test 0 (system "cd array-list && cyclone-winds test-local"))
  (test 0 (system "cd array-list && cyclone-winds test-local \".\""))
  (test 0 (system "cd array-list && cyclone-winds package"))
  (test 0 (system "cd array-list && cyclone-winds package \".\" && cd .. && rm -Rf array-list cyclone")))

(test-exit)
