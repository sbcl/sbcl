(cl:in-package :sb-posix-internal)

(defmacro define-protocol-class 
    (name alien-type superclasses slots &rest options)
  (let ((to-protocol (intern (format nil "ALIEN-TO-~A" name)))
	(to-alien (intern (format nil "~A-TO-ALIEN" name))))
    `(progn
      (defclass ,name ,superclasses 
	,(loop for slotd in slots
	       collect (ldiff slotd (member :array-length slotd)))
	,@options)
      (declaim (inline ,to-alien ,to-protocol))
      (defun ,to-protocol (alien &optional instance)
	(declare (type (sb-alien:alien (* ,alien-type)) alien)
		 (type (or null ,name) instance))
	(unless instance
	  (setf instance (make-instance ',name)))
	,@(loop for slotd in slots
		;; FIXME: slotds in source are more complicated in general
		;;
		;; FIXME: baroque construction of intricate fragility
		for array-length = (getf (cdr slotd) :array-length)
		if array-length
		  collect `(progn
			     (let ((array (make-array ,array-length)))
			       (setf (slot-value instance ',(car slotd))
				     array)
			       (dotimes (i ,array-length)
				 (setf (aref array i)
				       (sb-alien:deref 
					(sb-alien:slot alien ',(car slotd))
					i)))))
		else
		  collect `(setf (slot-value instance ',(car slotd))
			         (sb-alien:slot alien ',(car slotd))))
	instance)
      (defun ,to-alien (instance &optional alien)
	(declare (type (or null (sb-alien:alien (* ,alien-type))) alien)
		 (type ,name instance))
	(unless alien
	  (setf alien (sb-alien:make-alien ,alien-type)))
	,@(loop for slotd in slots
		for array-length = (getf (cdr slotd) :array-length)
		if array-length
		  collect `(progn
			     (let ((array (slot-value instance ',(car slotd))))
			       (dotimes (i ,array-length)
				 (setf (sb-alien:deref 
					(sb-alien:slot alien ',(car slotd))
					i)
				       (aref array i)))))
		else
		  collect `(setf (sb-alien:slot alien ',(car slotd))
			         (slot-value instance ',(car slotd)))))
      (find-class ',name))))

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
(define-call "chroot" int minusp (pathname filename))
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
(define-call "readdir" (* sb-posix::dirent)
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
(define-call "fork" sb-posix::pid-t minusp)
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

(export 'sb-posix::wait :sb-posix)
(declaim (inline sb-posix::wait))
(defun sb-posix::wait (&optional statusptr)
  (declare (type (or null (simple-array (signed-byte 32) (1))) statusptr))
  (let* ((ptr (or statusptr (make-array 1 :element-type '(signed-byte 32))))
	 (pid (alien-funcall
	       (extern-alien "wait" (function sb-posix::pid-t (* int)))
	       (sb-sys:vector-sap ptr))))
    (if (minusp pid)
	(syscall-error)
	(values pid (aref ptr 0)))))

(export 'sb-posix::waitpid :sb-posix)
(declaim (inline sb-posix::waitpid))
(defun sb-posix::waitpid (pid options &optional statusptr)
  (declare (type (sb-alien:alien sb-posix::pid-t) pid)
	   (type (sb-alien:alien int) options)
	   (type (or null (simple-array (signed-byte 32) (1))) statusptr))
  (let* ((ptr (or statusptr (make-array 1 :element-type '(signed-byte 32))))
	 (pid (alien-funcall
	       (extern-alien "waitpid" (function sb-posix::pid-t
						 sb-posix::pid-t (* int) int))
			     pid (sb-sys:vector-sap ptr) options)))
	 (if (minusp pid)
	     (syscall-error)
	     (values pid (aref ptr 0)))))

;; waitpid macros
(define-call "wifexited" boolean never-fails (status int))
(define-call "wexitstatus" int never-fails (status int))
(define-call "wifsignaled" boolean never-fails (status int))
(define-call "wtermsig" int never-fails (status int))
(define-call "wifstopped" boolean never-fails (status int))
(define-call "wstopsig" int never-fails (status int))
#+nil ; see alien/waitpid-macros.c
(define-call "wifcontinued" boolean never-fails (status int))

;;; mmap, msync
(define-call "mmap" sb-sys:system-area-pointer
  (lambda (res)
    (= (sb-sys:sap-int res) #.(1- (expt 2 sb-vm::n-machine-word-bits))))
  (addr sap-or-nil) (length unsigned) (prot unsigned)
  (flags unsigned) (fd file-descriptor) (offset sb-posix::off-t))

(define-call "munmap" int minusp
  (start sb-sys:system-area-pointer) (length unsigned))

(define-call "msync" int minusp
  (addr sb-sys:system-area-pointer) (length unsigned) (flags int))

(define-call "getpagesize" int minusp)

(define-protocol-class sb-posix::stat sb-posix::alien-stat ()
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
	    (alien-to-stat a-stat stat)))))))

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

(define-protocol-class sb-posix::termios sb-posix::alien-termios ()
  ((sb-posix::iflag :initarg :iflag :accessor sb-posix:termios-iflag)
   (sb-posix::oflag :initarg :oflag :accessor sb-posix:termios-oflag)
   (sb-posix::cflag :initarg :cflag :accessor sb-posix:termios-cflag)
   (sb-posix::lflag :initarg :lflag :accessor sb-posix:termios-lflag)
   (sb-posix::cc :initarg :cc :accessor sb-posix:termios-cc :array-length sb-posix::nccs)))

(export 'sb-posix::tcsetattr :sb-posix)
(declaim (inline sb-posix::tcsetattr))
(defun sb-posix::tcsetattr (fd actions termios)
  (sb-posix::with-alien-termios a-termios ()
    (termios-to-alien termios a-termios)
    (let ((fd (sb-posix::file-descriptor fd)))
      (let* ((r (alien-funcall
		 (extern-alien 
		  "tcsetattr" 
		  (function int int int (* sb-posix::alien-termios)))
		 fd actions a-termios)))
	(when (minusp r)
	  (syscall-error)))
      (values))))
(export 'sb-posix::tcgetattr :sb-posix)
(declaim (inline sb-posix::tcgetattr))
(defun sb-posix::tcgetattr (fd &optional termios)
  (sb-posix::with-alien-termios a-termios ()
    (let ((r (alien-funcall
	      (extern-alien "tcgetattr" 
			    (function int int (* sb-posix::alien-termios)))
	      (sb-posix::file-descriptor fd)
	      a-termios)))
      (when (minusp r)
	(syscall-error))
      (setf termios (alien-to-termios a-termios termios))))
  termios)

;;; environment

(export 'sb-posix::getenv :sb-posix)
(defun sb-posix::getenv (name)
  (let ((r (alien-funcall
	    (extern-alien "getenv" (function (* char) c-string))
	    name)))
    (declare (type (alien (* char)) r))
    (unless (null-alien r)
      (cast r c-string))))
(define-call "putenv" int minusp (string c-string))
