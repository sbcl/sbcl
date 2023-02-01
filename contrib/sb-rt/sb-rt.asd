;;; -*- Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-rt"
  :version "0.1.7" ; our version "0", GCL CVS version "1.7"
  :components ((:file "rt")))
