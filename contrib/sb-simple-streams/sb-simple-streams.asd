;;; -*- lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-simple-streams"
  :components ((:file "package")
               (:file "fndb")
               (:file "iodefs" :depends-on ("package"))
               ;;(:file "pcl")
               ;;(:file "ext-format" :depends-on ("package"))
               (:file "classes" :depends-on ("iodefs"))
               (:file "internal" :depends-on ("classes"))
               (:file "string" :depends-on ("internal"))
               (:file "strategy" :depends-on ("string"))
               (:file "file" :depends-on ("strategy"))
               (:file "impl" :depends-on ("internal" "fndb" "file" "string"))
               (:file "direct" :depends-on ("strategy"))
               (:file "null" :depends-on ("strategy"))
               (:file "socket" :depends-on ("strategy"))
               (:file "terminal" :depends-on ("strategy"))
               ;;(:file "gray-compat" :depends-on ("package"))
               ))
