;;; -*-  Lisp -*-

(defpackage #:sb-grovel-system (:use #:asdf #:cl))
(in-package #:sb-grovel-system)

(defsystem sb-grovel
    :version "0.01"
    :components ((:file "defpackage")
		 (:file "def-to-lisp" :depends-on ("defpackage"))))

(defmethod perform ((o test-op) (c (eql (find-system :sb-grovel))))
  t)

