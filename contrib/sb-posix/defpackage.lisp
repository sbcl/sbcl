(defpackage :sb-posix (:use)
  (:export #:syscall-error #:syscall-errno))

(defpackage :sb-posix-internal (:use #:sb-alien #:cl))
