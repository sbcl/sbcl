(defpackage :sb-posix (:use #:sb-alien #:cl)
  (:shadow close open ftruncate truncate time)
  (:export #:syscall-error #:syscall-errno

           ;; grovel structure accessors

           #:dirent-name

           ;; wrapper class accessors

           #:passwd-name #:passwd-passwd #:passwd-uid #:passwd-gid
           #:passwd-gecos #:passwd-dir #:passwd-shell
           #:group-name #:group-gid #:group-passwd
           #:stat-mode #:stat-ino #:stat-dev #:stat-nlink #:stat-uid
           #:stat-gid #:stat-size #:stat-atime #:stat-mtime #:stat-ctime
           #:termios-iflag #:termios-oflag #:termios-cflag
           #:termios-lflag #:termios-cc #:timeval-sec #:timeval-usec
           #:flock-type #:flock-whence #:flock-start #:flock-len
           #:flock-pid))

#+win32
(load-shared-object "msvcrt.dll")
