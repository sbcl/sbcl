;;; -*-  Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-posix"
  :defsystem-depends-on ("sb-grovel")
  :components ((:file "defpackage")
               (:file "strtod" :depends-on ("defpackage"))
               (:file "designator" :depends-on ("defpackage"))
               (:file "macros" :depends-on ("designator"))
               (:sb-grovel-constants-file "constants"
                :package :sb-posix :depends-on  ("defpackage"))
               (:file "interface" :depends-on ("constants" "macros" "designator"))))
