;;; -*-  Lisp -*-

(defpackage #:sb-md5-system
  (:use #:cl #:asdf))

(in-package #:sb-md5-system)

(defsystem sb-md5
  :depends-on (sb-rotate-byte)
  :version "1.8"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-MD5;"
  :components ((:file "md5")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-md5))))
  (provide 'sb-md5))

(defmethod perform ((o test-op) (c (eql (find-system :sb-md5))))
  (operate 'load-op 'sb-md5-tests)
  (operate 'test-op 'sb-md5-tests))

(defsystem sb-md5-tests
  :depends-on (sb-md5 sb-rt)
  :version "1.8"
  :components ((:file "md5-tests")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-md5-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))
