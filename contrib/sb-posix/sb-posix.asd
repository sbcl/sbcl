;;; -*-  Lisp -*-

#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-posix"
  :defsystem-depends-on ("sb-grovel")
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-POSIX;"
  :components ((:file "defpackage")
               (:file "strtod" :depends-on ("defpackage"))
               (:file "designator" :depends-on ("defpackage"))
               (:file "macros" :depends-on ("designator"))
               (:sb-grovel-constants-file "constants"
                :package :sb-posix :depends-on  ("defpackage"))
               (:file "interface" :depends-on ("constants" "macros" "designator")))
  :perform (load-op :after (o c) (provide 'sb-posix))
  :in-order-to ((test-op (test-op "sb-posix/tests"))))

(defsystem "sb-posix/tests"
  :depends-on ("sb-rt")
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-POSIX;"
  :components ((:file "libc-tests")
               (:file "posix-tests"))
  :perform
  (test-op (o c)
    (funcall (intern "DO-TESTS" (find-package "SB-RT")))
    (let ((failures (funcall (intern "PENDING-TESTS" "SB-RT")))
          (ignored-failures (loop for sym being the symbols of :sb-posix-tests
                                  if (search ".ERROR" (symbol-name sym))
                                    collect sym)))
      (cond
        ((null failures)
         t)
        ((null (set-difference failures ignored-failures))
         (warn "~@<some POSIX implementations return incorrect error values for ~
                failing calls, but there is legitimate variation between ~
                implementations too.  If you think the errno ~
                from your platform is valid, please contact the sbcl ~
                developers; otherwise, please submit a bug report to your ~
                kernel distributor~@:>")
         t)
        (t
         (error "non-errno tests failed!"))))))
