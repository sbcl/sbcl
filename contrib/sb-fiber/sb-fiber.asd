;;;; -*-  Lisp -*-

(error "Can't build contribs with ASDF")

(defsystem "sb-fiber"
  :defsystem-depends-on ("sb-grovel")
  :components ((:file "package")
               (:sb-grovel-constants-file "constants"
                :package :sb-fiber
                :if-feature (:and :sb-fiber (:or :x86-64 :arm64) (:not :win32))
                :depends-on ("package"))
               (:file "x86-64-vops"
                :if-feature (:and :sb-fiber :x86-64)
                :depends-on ("package"))
               (:file "arm64-vops"
                :if-feature (:and :sb-fiber :arm64)
                :depends-on ("package"))
               (:file "fiber-ffi"
                :depends-on ("package" "constants"))
               (:file "fiber"
                :depends-on ("package" "constants" "fiber-ffi"))))
