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
  (test 0 (system "cyclone-winds retrieve \"(cyclone iset)\""))
  (test 0 (system "cd cyclone-iset && cyclone-winds build-local"))
  (test 0 (system "cd cyclone-iset && cyclone-winds build-local \".\""))
  (test 0 (system "cd cyclone-iset && cyclone-winds test-local"))
  (test 0 (system "cd cyclone-iset && cyclone-winds test-local \".\""))
  (test 0 (system "cd cyclone-iset && cyclone-winds package"))
  (test 0 (system "cd cyclone-iset && cyclone-winds package \".\" && cd .. && rm -Rf cyclone-iset cyclone")))

(test-exit)
