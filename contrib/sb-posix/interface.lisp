(cl:in-package :sb-posix-internal)

(define-condition sb-posix::syscall-error (error)
  ((errno :initarg :errno :reader sb-posix::syscall-errno))
  (:report (lambda (c s)
	     (let ((errno (sb-posix::syscall-errno c)))
	       (format s "System call error ~A (~A)"
		       errno (sb-int:strerror errno))))))

(defun syscall-error ()
  (error 'sb-posix::syscall-error :errno (get-errno)))

;;; filesystem access

(define-call "access" int minusp (pathname filename) (mode int))
(define-call "chdir" int minusp (pathname filename))
(define-call "chmod" int minusp (pathname filename) (mode sb-posix::mode-t))
(define-call "chown" int minusp (pathname filename)
	     (owner sb-posix::uid-t)  (group sb-posix::gid-t))
(define-call "close" int minusp (fd file-descriptor))
(define-call "dup" int minusp (oldfd file-descriptor))
(define-call "dup2" int minusp (oldfd file-descriptor) (newfd file-descriptor))
(define-call "fchdir" int minusp (fd file-descriptor))
(define-call "fchmod" int minusp (fd file-descriptor) (mode sb-posix::mode-t))
(define-call "fchown" int minusp (fd file-descriptor)
	     (owner sb-posix::uid-t)  (group sb-posix::gid-t))
(define-call "link" int minusp (oldpath filename) (newpath filename))
(define-call "lchown" int minusp (pathname filename)
	     (owner sb-posix::uid-t)  (group sb-posix::gid-t))
(define-call "mkdir" int minusp (pathname filename) (mode sb-posix::mode-t))
;;(define-call "readlink" int minusp (path filename) (buf (* t)) (len int))
(define-call "rmdir" int minusp (pathname filename))
(define-call "symlink" int minusp (oldpath filename) (newpath filename))
(define-call "unlink" int minusp (pathname filename))


;;; uid, gid

(define-call "geteuid" sb-posix::uid-t not)	;"always successful", it says
(define-call "getresuid" sb-posix::uid-t not)
(define-call "getuid" sb-posix::uid-t not)
(define-call "seteuid" int minusp (uid sb-posix::uid-t))
#+linux (define-call "setfsuid" int minusp (uid sb-posix::uid-t))
(define-call "setreuid" int minusp
	     (ruid sb-posix::uid-t) (euid sb-posix::uid-t))
(define-call "setresuid" int minusp
	     (ruid sb-posix::uid-t) (euid sb-posix::uid-t)
	     (suid sb-posix::uid-t))
(define-call "setuid" int minusp (uid sb-posix::uid-t))

(define-call "getegid" sb-posix::gid-t not)
(define-call "getgid" sb-posix::gid-t not)
(define-call "getresgid" sb-posix::gid-t not)
(define-call "setegid" int minusp (gid sb-posix::gid-t))
#+linux (define-call "setfsgid" int minusp (gid sb-posix::gid-t))
(define-call "setgid" int minusp (gid sb-posix::gid-t))
(define-call "setregid" int minusp
	     (rgid sb-posix::gid-t) (egid sb-posix::gid-t))
(define-call "setresgid" int minusp
	     (rgid sb-posix::gid-t)
	     (egid sb-posix::gid-t) (sgid sb-posix::gid-t))

;;; processes, signals
(define-call "alarm" int not (seconds unsigned))
(define-call "getpgid" sb-posix::pid-t minusp (pid sb-posix::pid-t))
(define-call "getpid" sb-posix::pid-t not)
(define-call "getppid" sb-posix::pid-t not)
(define-call "getpgrp" sb-posix::pid-t not)
(define-call "getsid" sb-posix::pid-t minusp  (pid sb-posix::pid-t))
(define-call "kill" int minusp (pid sb-posix::pid-t) (signal int))
(define-call "killpg" int minusp (pgrp int) (signal int))
(define-call "pause" int minusp)
(define-call "setpgid" int minusp
	     (pid sb-posix::pid-t) (pgid sb-posix::pid-t))
(define-call "setpgrp" int minusp)
