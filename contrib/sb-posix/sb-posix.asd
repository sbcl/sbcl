;;; -*-  Lisp -*-
(require :sb-grovel)
(defpackage #:sb-posix-system (:use #:asdf #:cl #:sb-grovel))
(in-package #:sb-posix-system)

(defsystem sb-posix
    :depends-on (sb-grovel)
    :components ((:file "defpackage")
		 (:file "designator" :depends-on ("defpackage"))
		 (:file "macros" :depends-on ("designator"))
		 (sb-grovel:grovel-constants-file
		  "constants"
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
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))
