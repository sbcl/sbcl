;;; -*-  Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-cover"
  :depends-on ("sb-md5")
  :components ((:file "cover")))
