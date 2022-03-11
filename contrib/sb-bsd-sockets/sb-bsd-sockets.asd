;;; -*-  Lisp -*-
#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

;;; gethostbyname/gethostbyaddr are generally not thread safe. POSIX
;;; 1003.1-2003 defines an alternative API, which is specified in the
;;; RFC to be thread-safe. If it seems to be available, use it.

(when (sb-alien::find-dynamic-foreign-symbol-address "getaddrinfo")
  (pushnew :sb-bsd-sockets-addrinfo *features*))

(defsystem "sb-bsd-sockets"
  :version "0.59"
  :defsystem-depends-on ("sb-grovel")
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-BSD-SOCKETS;"
  :serial t
  :components
  ((:file "defpackage")
   (:file "win32-lib" :if-feature :win32)
   (:sb-grovel-constants-file "constants"
                              :package :sockint)
   (:sb-grovel-constants-file "constants-unix"
                              :if-feature (:not :win32)
                              :package :sockint)
   (:sb-grovel-constants-file "constants-win32"
                              :if-feature :win32
                              :package :sockint)
   (:sb-grovel-constants-file "constants-addrinfo"
                              :if-feature :sb-bsd-sockets-addrinfo
                              :package :sockint)
   (:sb-grovel-constants-file "constants-gethostbyname"
                              :if-feature (:not :sb-bsd-sockets-addrinfo)
                              :package :sockint)
   (:file "util")
   (:file "protocol")
   (:file "win32-sockets" :if-feature :win32)
   (:file "sockets")
   (:file "sockopt")

   (:file "inet")
   (:file "inet4")
   (:file "inet6")
   (:file "local" :if-feature (:not :win32))

   (:file "name-service")
   (:file "misc"))
  :perform (load-op :after (o c) (provide 'sb-bsd-sockets))
  :in-order-to ((test-op (test-op "sb-bsd-sockets/tests"))))

(defsystem "sb-bsd-sockets/tests"
  :depends-on ("sb-rt"
               "sb-bsd-sockets"
               (:feature (:not :win32) "sb-posix"))
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
