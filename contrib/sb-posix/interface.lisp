(cl:in-package :sb-posix-internal)

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
;;; FIXME: MODE arg should be optional?
(define-call "open" int minusp (pathname filename) (flags int) (mode sb-posix::mode-t)) 
;;(define-call "readlink" int minusp (path filename) (buf (* t)) (len int))
(define-call "rename" int minusp (oldpath filename) (newpath filename))
(define-call "rmdir" int minusp (pathname filename))
(define-call "symlink" int minusp (oldpath filename) (newpath filename))
(define-call "sync" void never-fails)
(define-call "truncate" int minusp (pathname filename) (length sb-posix::off-t))
(define-call "unlink" int minusp (pathname filename))

(define-call "opendir" (* t) null-alien (pathname filename))
(define-call "readdir" (* t)
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

(defmacro define-stat-call (name arg designator-fun type)
  ;; FIXME: this isn't the documented way of doing this, surely?
  (let ((lisp-name (intern (string-upcase name) :sb-posix)))
    `(progn
      (export ',lisp-name :sb-posix)
      (declaim (inline ,lisp-name))
      (defun ,lisp-name (,arg &optional stat)
	(declare (type (or null sb-posix::stat) stat))
	(unless stat
	  (setq stat (sb-posix::allocate-stat)))
	;; FIXME: Hmm.  WITH-PINNED-OBJECTS/WITHOUT-GCING or something
	;; is probably needed round here.
	(let* ((s (sb-sys:int-sap
		   ;; FIXME: WILL NOT WORK ON 64-BIT LISP.  VECTOR-SAP
		   ;; would be better if the STAT object were
		   ;; guaranteed to be a vector, but it's not (and may
		   ;; well turn into an alien soon).
		   (+ 8 (logandc2 (sb-kernel:get-lisp-obj-address stat) 7))))
	       (r (alien-funcall
		   (extern-alien ,name ,type)
		   (,designator-fun ,arg)
		   s)))
	  (when (minusp r)
	    (syscall-error)))
	stat))))
(define-stat-call "stat" pathname sb-posix::filename
		  ;; FIXME: (* T)?  Ew.  (* STAT) would be preferable
		  (function int c-string (* t)))
(define-stat-call "lstat" pathname sb-posix::filename
		  (function int c-string (* t)))
(define-stat-call "fstat" fd sb-posix::file-descriptor
		  (function int int (* t)))


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
