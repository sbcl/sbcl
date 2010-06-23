;;; -*-  Lisp -*-
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :sb-grovel))
(defpackage #:sb-posix-system (:use #:asdf #:cl #:sb-grovel))
(in-package #:sb-posix-system)

(defsystem sb-posix
    :depends-on (sb-grovel)
    #+sb-building-contrib :pathname
    #+sb-building-contrib #p"SYS:CONTRIB;SB-POSIX;"
    :components ((:file "defpackage")
                 (:file "designator" :depends-on ("defpackage"))
                 (:file "macros" :depends-on ("designator"))
                 (sb-grovel:grovel-constants-file
                  "constants"
                  :do-not-grovel #.(progn #-sb-building-contrib t)
                  :package :sb-posix :depends-on  ("defpackage"))
                 (:file "interface" :depends-on ("constants" "macros" "designator"))))

(defsystem sb-posix-tests
    :depends-on (sb-rt)
    :components ((:file "posix-tests")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-posix))))
  (provide 'sb-posix))

(defmethod perform ((o test-op) (c (eql (find-system :sb-posix))))
  (operate 'load-op 'sb-posix-tests)
  (operate 'test-op 'sb-posix-tests))

(defmethod perform ((o test-op) (c (eql (find-system :sb-posix-tests))))
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
       (error "non-errno tests failed!")))))
