(cl:in-package :sb-posix-internal)

(defun make-alien-slot-name (alien-type slot-name)
  (intern (format nil "~A-~A" alien-type slot-name)
	  (symbol-package slot-name)))

(declaim (inline alien-to-protocol-class))
(defun alien-to-protocol-class (alien alien-type instance protocol-class slots)
  "Copy SLOTS from the alien object ALIEN of type ALIEN-TYPE to INSTANCE, an instance of PROTOCOL-CLASS.
We assume that SLOT names are the same in the alien object and in
the protocol-class."
  (unless instance
    (setf instance (make-instance protocol-class)))
  (loop for slot in slots
	do (setf (slot-value instance slot)
		 (sb-alien:slot alien slot)))
  instance)

(defun protocol-class-to-alien (instance protocol-class alien alien-type slots)
  (loop for slot in slots
	do (setf (sb-alien:slot alien slot) (slot-value instance slot)))
  instance)

(define-condition sb-posix:syscall-error (error)
  ((errno :initarg :errno :reader sb-posix:syscall-errno))
  (:report (lambda (c s)
	     (let ((errno (sb-posix:syscall-errno c)))
	       (format s "System call error ~A (~A)"
		       errno (sb-int:strerror errno))))))

(defun syscall-error ()
  (error 'sb-posix:syscall-error :errno (get-errno)))

(declaim (inline never-fails))
(defun never-fails (&rest args)
  (declare (ignore args))
  nil)

;;; filesystem access

(define-call "access" int minusp (pathname filename) (mode int))
(define-call "chdir" int minusp (pathname filename))
(define-call "chmod" int minusp (pathname filename) (mode sb-posix::mode-t))
(define-call "chown" int minusp (pathname filename)
	     (owner sb-posix::uid-t)  (group sb-posix::gid-t))
(define-call "close" int minusp (fd file-descriptor))
(define-call "creat" int minusp (pathname filename) (mode sb-posix::mode-t))
(define-call "dup" int minusp (oldfd file-descriptor))
(define-call "dup2" int minusp (oldfd file-descriptor) (newfd file-descriptor))
(define-call "fchdir" int minusp (fd file-descriptor))
(define-call "fchmod" int minusp (fd file-descriptor) (mode sb-posix::mode-t))
(define-call "fchown" int minusp (fd file-descriptor)
	     (owner sb-posix::uid-t)  (group sb-posix::gid-t))
(define-call "fdatasync" int minusp (fd file-descriptor))
(define-call "ftruncate" int minusp (fd file-descriptor) (length sb-posix::off-t))
(define-call "fsync" int minusp (fd file-descriptor))
(define-call "lchown" int minusp (pathname filename)
	     (owner sb-posix::uid-t)  (group sb-posix::gid-t))
(define-call "link" int minusp (oldpath filename) (newpath filename))
(define-call "lseek" sb-posix::off-t minusp (fd file-descriptor) (offset sb-posix::off-t) (whence int))
(define-call "mkdir" int minusp (pathname filename) (mode sb-posix::mode-t))
(define-call "mkfifo" int minusp (pathname filename) (mode sb-posix::mode-t))
(define-call-internally open-with-mode "open" int minusp (pathname filename) (flags int) (mode sb-posix::mode-t))
(define-call-internally open-without-mode "open" int minusp (pathname filename) (flags int))
(define-entry-point "open" (pathname flags &optional (mode nil mode-supplied))
  (if mode-supplied
      (open-with-mode pathname flags mode)
      (open-without-mode pathname flags)))
;;(define-call "readlink" int minusp (path filename) (buf (* t)) (len int))
(define-call "rename" int minusp (oldpath filename) (newpath filename))
(define-call "rmdir" int minusp (pathname filename))
(define-call "symlink" int minusp (oldpath filename) (newpath filename))
(define-call "sync" void never-fails)
(define-call "truncate" int minusp (pathname filename) (length sb-posix::off-t))
(define-call "unlink" int minusp (pathname filename))

(define-call-internally ioctl-without-arg "ioctl" int minusp (fd file-descriptor) (cmd int))
(define-call-internally ioctl-with-int-arg "ioctl" int minusp (fd file-descriptor) (cmd int) (arg int))
(define-call-internally ioctl-with-pointer-arg "ioctl" int minusp (fd file-descriptor) (cmd int) (arg alien-pointer-to-anything-or-nil))
(define-entry-point "ioctl" (fd cmd &optional (arg nil argp))
  (if argp
      (etypecase arg
	((alien int) (ioctl-with-int-arg fd cmd arg))
	((or (alien (* t)) null) (ioctl-with-pointer-arg fd cmd arg)))
      (ioctl-without-arg fd cmd)))

(define-call-internally fcntl-without-arg "fcntl" int minusp (fd file-descriptor) (cmd int))
(define-call-internally fcntl-with-int-arg "fcntl" int minusp (fd file-descriptor) (cmd int) (arg int))
(define-call-internally fcntl-with-pointer-arg "fcntl" int minusp (fd file-descriptor) (cmd int) (arg alien-pointer-to-anything-or-nil))
(define-entry-point "fcntl" (fd cmd &optional (arg nil argp))
  (if argp
      (etypecase arg
	((alien int) (fcntl-with-int-arg fd cmd arg))
	((or (alien (* t)) null) (fcntl-with-pointer-arg fd cmd arg)))
      (fcntl-without-arg fd cmd)))

(define-call "opendir" (* t) null-alien (pathname filename))
(define-call "readdir" sb-posix::dirent
  ;; readdir() has the worst error convention in the world.  It's just
  ;; too painful to support.  (return is NULL _and_ errno "unchanged"
  ;; is not an error, it's EOF).
  not
  (dir (* t)))
(define-call "closedir" int minusp (dir (* t)))

(define-call "umask" sb-posix::mode-t never-fails (mode sb-posix::mode-t))

;;; uid, gid

(define-call "geteuid" sb-posix::uid-t never-fails) ; "always successful", it says
(define-call "getresuid" sb-posix::uid-t never-fails)
(define-call "getuid" sb-posix::uid-t never-fails)
(define-call "seteuid" int minusp (uid sb-posix::uid-t))
(define-call "setfsuid" int minusp (uid sb-posix::uid-t))
(define-call "setreuid" int minusp
	     (ruid sb-posix::uid-t) (euid sb-posix::uid-t))
(define-call "setresuid" int minusp
	     (ruid sb-posix::uid-t) (euid sb-posix::uid-t)
	     (suid sb-posix::uid-t))
(define-call "setuid" int minusp (uid sb-posix::uid-t))

(define-call "getegid" sb-posix::gid-t never-fails)
(define-call "getgid" sb-posix::gid-t never-fails)
(define-call "getresgid" sb-posix::gid-t never-fails)
(define-call "setegid" int minusp (gid sb-posix::gid-t))
(define-call "setfsgid" int minusp (gid sb-posix::gid-t))
(define-call "setgid" int minusp (gid sb-posix::gid-t))
(define-call "setregid" int minusp
	     (rgid sb-posix::gid-t) (egid sb-posix::gid-t))
(define-call "setresgid" int minusp
	     (rgid sb-posix::gid-t)
	     (egid sb-posix::gid-t) (sgid sb-posix::gid-t))

;;; processes, signals
(define-call "alarm" int never-fails (seconds unsigned))
(define-call "getpgid" sb-posix::pid-t minusp (pid sb-posix::pid-t))
(define-call "getpid" sb-posix::pid-t never-fails)
(define-call "getppid" sb-posix::pid-t never-fails)
(define-call "getpgrp" sb-posix::pid-t never-fails)
(define-call "getsid" sb-posix::pid-t minusp  (pid sb-posix::pid-t))
(define-call "kill" int minusp (pid sb-posix::pid-t) (signal int))
(define-call "killpg" int minusp (pgrp int) (signal int))
(define-call "pause" int minusp)
(define-call "setpgid" int minusp
	     (pid sb-posix::pid-t) (pgid sb-posix::pid-t))
(define-call "setpgrp" int minusp)

;;; mmap, msync
(define-call "mmap" sb-sys:system-area-pointer
  ;; KLUDGE: #XFFFFFFFF is (void *)-1, which is the charming return
  ;; value of mmap on failure.  Except on 64 bit systems ...
  (lambda (res)
    (= (sb-sys:sap-int res) #-alpha #XFFFFFFFF #+alpha #xffffffffffffffff))
  (addr sap-or-nil) (length unsigned) (prot unsigned)
  (flags unsigned) (fd file-descriptor) (offset sb-posix::off-t))

(define-call "munmap" int minusp
  (start sb-sys:system-area-pointer) (length unsigned))

(define-call "msync" int minusp
  (addr sb-sys:system-area-pointer) (length unsigned) (flags int))

(define-call "getpagesize" int minusp)

(defclass sb-posix::stat ()
     ((sb-posix::mode :initarg :mode :accessor sb-posix:stat-mode)
      (sb-posix::ino :initarg :ino :accessor sb-posix:stat-ino)
      (sb-posix::dev :initarg :dev :accessor sb-posix:stat-dev)
      (sb-posix::nlink :initarg :nlink :accessor sb-posix:stat-nlink)
      (sb-posix::uid :initarg :uid :accessor sb-posix:stat-uid)
      (sb-posix::gid :initarg :gid :accessor sb-posix:stat-gid)
      (sb-posix::size :initarg :size :accessor sb-posix:stat-size)
      (sb-posix::atime :initarg :atime :accessor sb-posix:stat-atime)
      (sb-posix::mtime :initarg :mtime :accessor sb-posix:stat-mtime)
      (sb-posix::ctime :initarg :ctime :accessor sb-posix:stat-ctime)))

(defmacro define-stat-call (name arg designator-fun type)
  ;; FIXME: this isn't the documented way of doing this, surely?
  (let ((lisp-name (intern (string-upcase name) :sb-posix)))
    `(progn
      (export ',lisp-name :sb-posix)
      (declaim (inline ,lisp-name))
      (defun ,lisp-name (,arg &optional stat)
	(declare (type (or null (sb-alien:alien (* sb-posix::alien-stat))) stat))
	(sb-posix::with-alien-stat a-stat ()
	  (let ((r (alien-funcall
		    (extern-alien ,name ,type)
		    (,designator-fun ,arg)
		    a-stat)))
	    (when (minusp r)
	      (syscall-error))
	    (alien-to-protocol-class a-stat 'sb-posix::alien-stat
				     stat 'sb-posix::stat
				     '(sb-posix::mode sb-posix::ino sb-posix::dev
				       sb-posix::nlink sb-posix::uid sb-posix::gid
				       sb-posix::size sb-posix::atime
				       sb-posix::mtime sb-posix::ctime))))))))

(define-stat-call "stat" pathname sb-posix::filename
		  (function int c-string (* sb-posix::alien-stat)))
(define-stat-call "lstat" pathname sb-posix::filename
		  (function int c-string (* sb-posix::alien-stat)))
(define-stat-call "fstat" fd sb-posix::file-descriptor
		  (function int int (* sb-posix::alien-stat)))


;;; mode flags
(define-call "s_isreg" boolean never-fails (mode sb-posix::mode-t))
(define-call "s_isdir" boolean never-fails (mode sb-posix::mode-t))
(define-call "s_ischr" boolean never-fails (mode sb-posix::mode-t))
(define-call "s_isblk" boolean never-fails (mode sb-posix::mode-t))
(define-call "s_isfifo" boolean never-fails (mode sb-posix::mode-t))
(define-call "s_islnk" boolean never-fails (mode sb-posix::mode-t))
(define-call "s_issock" boolean never-fails (mode sb-posix::mode-t))

(export 'sb-posix::pipe :sb-posix)
(declaim (inline sb-posix::pipe))
(defun sb-posix::pipe (&optional filedes2)
  (declare (type (or null (simple-array (signed-byte 32) (2))) filedes2))
  (unless filedes2
    (setq filedes2 (make-array 2 :element-type '(signed-byte 32))))
  (let ((r (alien-funcall
	    ;; FIXME: (* INT)?  (ARRAY INT 2) would be better
	    (extern-alien "pipe" (function int (* int)))
	    (sb-sys:vector-sap filedes2))))
    (when (minusp r)
      (syscall-error)))
  (values (aref filedes2 0) (aref filedes2 1)))

(defclass sb-posix::termios ()
     ((sb-posix::iflag :initarg :iflag :accessor sb-posix:termios-iflag)
      (sb-posix::oflag :initarg :oflag :accessor sb-posix:termios-oflag)
      (sb-posix::cflag :initarg :cflag :accessor sb-posix:termios-cflag)
      (sb-posix::lflag :initarg :lflag :accessor sb-posix:termios-lflag)
      (sb-posix::cc :initarg :cc :accessor sb-posix:termios-cc)))

(export 'sb-posix::tcsetattr :sb-posix)
(declaim (inline sb-posix::tcsetattr))
(defun sb-posix::tcsetattr (fd actions termios)
  (sb-posix::with-alien-termios a-termios ()
    (protocol-class-to-alien termios 'sb-posix::termios
			     a-termios 'sb-posix::alien-termios
			     '(sb-posix::iflag sb-posix::oflag
			       sb-posix::cflag sb-posix::lflag))
    (loop with ccs = (sb-posix::alien-termios-cc a-termios)
	    for i from 0 below sb-posix::nccs
	    do (setf (sb-alien:deref ccs i)
		     (aref (sb-posix::termios-cc termios) i)))
    (let ((fd (sb-posix::file-descriptor fd)))
      (let* ((r (alien-funcall
		 (extern-alien "tcsetattr" (function int int int sb-posix::alien-termios))
		 fd actions termios)))
	(when (minusp r)
	  (syscall-error)))
      (values))))
(export 'sb-posix::tcgetattr :sb-posix)
(declaim (inline sb-posix::tcgetattr))
(defun sb-posix::tcgetattr (fd &optional termios)
  (sb-posix::with-alien-termios a-termios ()
    (let ((r (alien-funcall
	      (extern-alien "tcgetattr" (function int int sb-posix::alien-termios))
	      (sb-posix::file-descriptor fd)
	      a-termios)))
      (when (minusp r)
	(syscall-error))
      (setf termios
	    (alien-to-protocol-class a-termios 'alien-termios
				     termios 'termios
				     '(sb-posix::iflag sb-posix::oflag
				       sb-posix::cflag sb-posix::lflag)))
      (setf (sb-posix::termios-cc termios) (make-array sb-posix::nccs))
      (loop with ccs = (sb-posix::alien-termios-cc a-termios)
	    for i from 0 below sb-posix::nccs
	    do (setf (aref (sb-posix::termios-cc termios) i)
		     (sb-alien:deref ccs i)))))
  termios)
