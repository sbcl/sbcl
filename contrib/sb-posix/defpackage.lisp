(defpackage :sb-posix (:use)
  (:export #:syscall-error #:syscall-errno

	   ;; grovel structure accessors
	   
	   #:dirent-name

	   ;; wrapper class accessors
	   
	   #:stat-mode #:stat-ino #:stat-dev #:stat-nlink #:stat-uid
	   #:stat-gid #:stat-size #:stat-atime #:stat-mtime #:stat-ctime
	   #:termios-iflag #:termios-oflag #:termios-cflag
	   #:termios-lflag #:termios-cc))

(defpackage :sb-posix-internal (:use #:sb-alien #:cl))
