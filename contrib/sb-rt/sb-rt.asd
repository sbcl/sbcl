;;; -*- Lisp -*-

(cl:defpackage #:sb-rt-system
  (:use #:asdf #:cl))
(cl:in-package #:sb-rt-system)

(defsystem sb-rt
  :version "0.1.7" ; our version "0", GCL CVS version "1.7"
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-RT;"
  :components ((:file "rt")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-rt))))
  (provide 'sb-rt))

(defmethod perform ((o test-op) (c (eql (find-system :sb-rt))))
  ;; FIXME: Maybe also import rt-tests.lisp?
  t)
