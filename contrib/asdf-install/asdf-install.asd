;;; -*-  Lisp -*-

(defpackage #:asdf-install-system 
  (:use #:cl #:asdf))

(in-package #:asdf-install-system)
(require 'sb-executable)

;;; this is appalling misuse of asdf.  please don't treat it as any
;;; kind of example.  this shouldn't be a compile-op, or if it is, should
;;; define output-files properly instead oif leaving it be the fasl
(defclass exe-file (cl-source-file) ())
(defmethod perform ((o compile-op) (c exe-file))
  (call-next-method)
  (sb-executable:make-executable
   (make-pathname :name "asdf-install"
		  :type nil
		  :defaults (component-pathname c))
   (output-files o c)
   :initial-function "RUN"))

(defmethod perform ((o load-op) (c exe-file)) nil)

(defsystem asdf-install
  :depends-on (sb-posix sb-bsd-sockets)
  :version "0.2"
  :components ((:file "defpackage")
	       (exe-file "loader")
	       (:file "installer")))
	       
(defmethod perform :after ((o load-op) (c (eql (find-system :asdf-install))))
  (provide 'asdf-install))

(defmethod perform ((o test-op) (c (eql (find-system :asdf-install))))
  t)
