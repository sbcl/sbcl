;;; -*-  Lisp -*-
(require :sb-grovel)
(defpackage #:sb-posix-system (:use #:asdf #:cl #:sb-grovel))
(in-package #:sb-posix-system)

(defsystem sb-posix
    :depends-on (sb-grovel)
    :components ((:file "defpackage")
		 (:file "designator" :depends-on ("defpackage"))
		 (:file "macros" :depends-on ("defpackage"))
		 (sb-grovel:grovel-constants-file
		  "constants"
		  :package :sb-posix :depends-on  ("defpackage"))
		 (:file "interface" :depends-on ("constants" "macros" "designator"))))

(defmethod perform :after ((o test-op) (c (eql (find-system :sb-posix))))
  (provide 'sb-posix))

(defmethod perform ((o test-op) (c (eql (find-system :sb-posix))))
  t)
