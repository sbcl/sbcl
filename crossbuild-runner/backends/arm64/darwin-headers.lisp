;;;; This is an automatically generated file, please do not hand-edit it.
;;;; See the program "grovel-headers.c".

(in-package "SB-ALIEN")

;;;flags for dlopen()
(defconstant rtld-lazy 1) ; #x1
(defconstant rtld-now 2) ; #x2
(defconstant rtld-global 8) ; #x8

(in-package "SB-UNIX")

(defconstant pthread-min-stack 16384) ; #x4000

;;; select()
(defconstant fd-setsize 1024) ; #x400
;;; poll()
(defconstant pollin 1) ; #x1
(defconstant pollout 4) ; #x4
(defconstant pollpri 2) ; #x2
(defconstant pollhup 16) ; #x10
(defconstant pollnval 32) ; #x20
(defconstant pollerr 8) ; #x8
(define-alien-type nfds-t (unsigned 32))
;;; types, types, types
(define-alien-type clock-t (unsigned 64))
(define-alien-type dev-t (signed 32))
(define-alien-type gid-t (unsigned 32))
(define-alien-type ino-t (unsigned 64))
(define-alien-type mode-t (unsigned 16))
(define-alien-type nlink-t (unsigned 16))
(define-alien-type off-t (signed 64))
(define-alien-type size-t (unsigned 64))
(define-alien-type ssize-t (signed 64))
(define-alien-type time-t (signed 64))
(define-alien-type suseconds-t (signed 32))
(define-alien-type uid-t (unsigned 32))
;; Types in src/runtime/wrap.h. See that file for explantion.
;; Don't use these types for anything other than the stat wrapper.
(define-alien-type wst-ino-t (unsigned 64))
(define-alien-type wst-dev-t (signed 32))
(define-alien-type wst-off-t (signed 64))
(define-alien-type wst-blksize-t (signed 32))
(define-alien-type wst-blkcnt-t (signed 64))
(define-alien-type wst-nlink-t (unsigned 16))
(define-alien-type wst-uid-t (unsigned 32))
(define-alien-type wst-gid-t (unsigned 32))

;;; fcntl.h (or unistd.h on OpenBSD and NetBSD)
(defconstant r_ok 4) ; #x4
(defconstant w_ok 2) ; #x2
(defconstant x_ok 1) ; #x1
(defconstant f_ok 0) ; #x0

;;; fcntlbits.h
(defconstant o_rdonly 0) ; #x0
(defconstant o_wronly 1) ; #x1
(defconstant o_rdwr 2) ; #x2
(defconstant o_accmode 3) ; #x3
(defconstant o_creat 512) ; #x200
(defconstant o_excl 2048) ; #x800
(defconstant o_noctty 131072) ; #x20000
(defconstant o_trunc 1024) ; #x400
(defconstant o_append 8) ; #x8
;;;
(defconstant s-ifmt 61440) ; #xf000
(defconstant s-ififo 4096) ; #x1000
(defconstant s-ifchr 8192) ; #x2000
(defconstant s-ifdir 16384) ; #x4000
(defconstant s-ifblk 24576) ; #x6000
(defconstant s-ifreg 32768) ; #x8000

(defconstant s-iflnk 40960) ; #xa000
(defconstant s-ifsock 49152) ; #xc000

;;; error numbers
(defconstant ebadf 9) ; #x9
(defconstant enoent 2) ; #x2
(defconstant eintr 4) ; #x4
(defconstant eagain 35) ; #x23
(defconstant eio 5) ; #x5
(defconstant eexist 17) ; #x11
(defconstant eloop 62) ; #x3e
(defconstant epipe 32) ; #x20
(defconstant espipe 29) ; #x1d
(defconstant ewouldblock 35) ; #x23

(defconstant sc-nprocessors-onln 58) ; #x3a
;;; for waitpid() in run-program.lisp
(defconstant wcontinued 16) ; #x10
(defconstant wnohang 1) ; #x1
(defconstant wuntraced 2) ; #x2

;;; various ioctl(2) flags
(defconstant tiocgpgrp 1074033783) ; #x40047477

;;; signals
(defconstant sizeof-sigset_t 4) ; #x4
(defconstant sig_block 1) ; #x1
(defconstant sig_unblock 2) ; #x2
(defconstant sig_setmask 3) ; #x3
(defconstant sigalrm 14) ; #xe
(defconstant sigbus 10) ; #xa
(defconstant sigchld 20) ; #x14
(defconstant sigcont 19) ; #x13
(defconstant sigemt 7) ; #x7
(defconstant sigfpe 8) ; #x8
(defconstant sighup 1) ; #x1
(defconstant sigill 4) ; #x4
(defconstant sigint 2) ; #x2
(defconstant sigio 23) ; #x17
(defconstant sigkill 9) ; #x9
(defconstant sigpipe 13) ; #xd
(defconstant sigprof 27) ; #x1b
(defconstant sigquit 3) ; #x3
(defconstant sigsegv 11) ; #xb
(defconstant sigstop 17) ; #x11
(defconstant sigsys 12) ; #xc
(defconstant sigterm 15) ; #xf
(defconstant sigtrap 5) ; #x5
(defconstant sigtstp 18) ; #x12
(defconstant sigttin 21) ; #x15
(defconstant sigttou 22) ; #x16
(defconstant sigurg 16) ; #x10
(defconstant sigusr1 30) ; #x1e
(defconstant sigusr2 31) ; #x1f
(defconstant sigvtalrm 26) ; #x1a
(defconstant sigwinch 28) ; #x1c
(defconstant sigxcpu 24) ; #x18
(defconstant sigxfsz 25) ; #x19
(defconstant itimer-real 0) ; #x0
(defconstant itimer-virtual 1) ; #x1
(defconstant itimer-prof 2) ; #x2
(defconstant fpe-intovf 8) ; #x8
(defconstant fpe-intdiv 7) ; #x7
(defconstant fpe-fltdiv 1) ; #x1
(defconstant fpe-fltovf 2) ; #x2
(defconstant fpe-fltund 3) ; #x3
(defconstant fpe-fltres 4) ; #x4
(defconstant fpe-fltinv 5) ; #x5
(defconstant fpe-fltsub 6) ; #x6

(defconstant clock-realtime 0) ; #x0
(defconstant clock-monotonic 6) ; #x6
(defconstant clock-process-cputime-id 12) ; #xc
;;; structures
(define-alien-type nil
  (struct timeval
          (tv-sec (signed 64))
          (tv-usec (signed 32))))
(define-alien-type nil
  (struct timespec
          (tv-sec (signed 64))
          (tv-nsec (signed 64))))

(defconstant sizeof-timespec 16) ; #x10
(defconstant sizeof-timeval 16) ; #x10

;;; sysctl(3) names
(in-package "SB-IMPL")
(defconstant ctl-kern 1) ; #x1
(defconstant ctl-hw 6) ; #x6
(defconstant ctl-maxname 12) ; #xc
(defconstant kern-ostype 1) ; #x1
(defconstant kern-osrelease 2) ; #x2
(defconstant hw-model 2) ; #x2
(defconstant hw-pagesize 7) ; #x7
(defconstant path-max 1024) ; #x400

(in-package "SB-KERNEL")

;;; GENCGC related
(define-alien-type page-index-t (signed 64))
(define-alien-type generation-index-t (signed 8))

;;; Our runtime types
(define-alien-type os-vm-size-t (unsigned 64))
