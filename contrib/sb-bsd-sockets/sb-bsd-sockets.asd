;;; -*-  Lisp -*-

(defsystem sb-bsd-sockets
  :version "0.58"
  :defsystem-depends-on (sb-grovel)
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-BSD-SOCKETS;"
  :components
  ((:file "defpackage")
   (:file "split" :depends-on ("defpackage"))
   (:file "win32-lib" :if-feature :win32)
   (:sb-grovel-constants-file "constants" :package :sockint
    :depends-on ("defpackage") :if-feature (:not :win32))
   (:sb-grovel-constants-file "win32-constants" :package
    :sockint :depends-on ("defpackage" "win32-lib") :if-feature :win32)
   (:file "win32-sockets"
    :depends-on ("win32-constants") :if-feature :win32)
   (:file "sockets" :depends-on ("constants" "win32-sockets"))
   (:file "sockopt" :depends-on ("sockets"))
   (:file "inet" :depends-on ("sockets" "split"))
   (:file "local" :depends-on ("sockets" "split"))
   (:file "name-service" :depends-on ("sockets"))
   (:file "misc" :depends-on ("sockets"))
   (:static-file "NEWS")
   ;; (:static-file "INSTALL")
   ;; (:static-file "README")
   ;; (:static-file "index.html")
   (:static-file "TODO"))
  :perform (load-op :after (o c) (provide 'sb-bsd-sockets))
  :perform (test-op (o c) (test-system 'sb-bsd-sockets/tests)))

(defsystem sb-bsd-sockets/tests
  :depends-on (sb-rt sb-bsd-sockets #-win32 sb-posix)
  :components ((:file "tests"))
  :perform (test-op (o c)
             (multiple-value-bind (soft strict pending)
                 (funcall (intern "DO-TESTS" (find-package "SB-RT")))
               (declare (ignorable pending))
               (fresh-line)
               (unless strict
                 #+sb-testing-contrib
                 ;; We create TEST-PASSED from a shell script if tests passed.  But
                 ;; since the shell script only `touch'es it, we can actually create
                 ;; it ahead of time -- as long as we're certain that tests truly
                 ;; passed, hence the check for SOFT.
                 (when soft
                   (with-open-file (s #p"SYS:CONTRIB;SB-BSD-SOCKETS;TEST-PASSED.TEST-REPORT"
                                      :direction :output)
                     (dolist (pend pending)
                       (format s "Expected failure: ~A~%" pend))))
                 (warn "ignoring expected failures in test-op"))
               (unless soft
                 (error "test-op failed with unexpected failures")))))
