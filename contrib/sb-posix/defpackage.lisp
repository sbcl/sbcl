(defpackage :sb-posix (:use #:sb-alien #:cl)
  (:shadow abort close open ftruncate truncate time read write)
  (:import-from #:sb-impl #:string-dispatch)
  (:export #:syscall-error #:syscall-errno #:syscall-name

           ;; types and type conversion
           #:file-descriptor-designator
           #:file-descriptor
           #:filename-designator
           #:filename

           ;; grovel structure accessors
           #:dirent-name #-win32 #:dirent-ino

           ;; wrapper class accessors
           #:passwd-name #:passwd-passwd #:passwd-uid #:passwd-gid
           #:passwd-gecos #:passwd-dir #:passwd-shell
           #:group-name #:group-gid #:group-passwd #:group-mem
           #:stat-mode #:stat-ino #:stat-dev #:stat-nlink #:stat-uid
           #:stat-gid #:stat-size #:stat-atime #:stat-mtime #:stat-ctime
           #:stat-rdev
           #:termios-iflag #:termios-oflag #:termios-cflag
           #:termios-lflag #:termios-cc #:timeval-sec #:timeval-usec
           #:flock-type #:flock-whence #:flock-start #:flock-len
           #:flock-pid

           ;; libc routines
           #:getenv #:strtod
           ))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-POSIX")) t))
