(defpackage :sb-posix (:use )
  (:export #:syscall-error))

(defpackage :sb-posix-internal (:use #:sb-alien #:cl))
