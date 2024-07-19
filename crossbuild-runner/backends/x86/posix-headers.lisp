;;;; This is an automatically generated file, please do not hand-edit it.
;;;; See the program "grovel-headers.c".

(in-package "SB-ALIEN")

;;;flags for dlopen()
(defconstant rtld-lazy 1) ; #x1
(defconstant rtld-now 2) ; #x2
(defconstant rtld-global 256) ; #x100

(in-package "SB-UNIX")

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
(define-alien-type clock-t (signed 32))
(define-alien-type dev-t (unsigned 64))
(define-alien-type gid-t (unsigned 32))
(define-alien-type ino-t (unsigned 64))
(define-alien-type mode-t (unsigned 32))
(define-alien-type nlink-t (unsigned 32))
(define-alien-type off-t (signed 64))
(define-alien-type size-t (unsigned 32))
(define-alien-type ssize-t (signed 32))
(define-alien-type time-t (signed 32))
(define-alien-type suseconds-t (signed 32))
(define-alien-type uid-t (unsigned 32))
;; Types in src/runtime/wrap.h. See that file for explantion.
;; Don't use these types for anything other than the stat wrapper.
(define-alien-type wst-ino-t (unsigned 64))
(define-alien-type wst-dev-t (unsigned 64))
(define-alien-type wst-off-t (signed 64))
(define-alien-type wst-blksize-t (signed 32))
(define-alien-type wst-blkcnt-t (signed 64))
(define-alien-type wst-nlink-t (unsigned 32))
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
(defconstant o_creat 64) ; #x40
(defconstant o_excl 128) ; #x80
(defconstant o_noctty 256) ; #x100
(defconstant o_trunc 512) ; #x200
(defconstant o_append 1024) ; #x400
(defconstant o_largefile 32768) ; #x8000
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
(defconstant eagain 11) ; #xb
(defconstant eio 5) ; #x5
(defconstant eexist 17) ; #x11
(defconstant eloop 40) ; #x28
(defconstant epipe 32) ; #x20
(defconstant espipe 29) ; #x1d
(defconstant ewouldblock 11) ; #xb

(defconstant sc-nprocessors-onln 84) ; #x54
;;; for waitpid() in run-program.lisp
(defconstant wcontinued 8) ; #x8
(defconstant wnohang 1) ; #x1
(defconstant wuntraced 2) ; #x2

;;; various ioctl(2) flags
(defconstant tiocgpgrp 21519) ; #x540f

;;; signals
(defconstant sizeof-sigset_t 128) ; #x80
(defconstant sig_block 0) ; #x0
(defconstant sig_unblock 1) ; #x1
(defconstant sig_setmask 2) ; #x2
(defconstant sigalrm 14) ; #xe
(defconstant sigbus 7) ; #x7
(defconstant sigchld 17) ; #x11
(defconstant sigcont 18) ; #x12
(defconstant sigfpe 8) ; #x8
(defconstant sighup 1) ; #x1
(defconstant sigill 4) ; #x4
(defconstant sigint 2) ; #x2
(defconstant sigio 29) ; #x1d
(defconstant sigkill 9) ; #x9
(defconstant sigpipe 13) ; #xd
(defconstant sigprof 27) ; #x1b
(defconstant sigquit 3) ; #x3
(defconstant sigsegv 11) ; #xb
(defconstant sigstkflt 16) ; #x10
(defconstant sigstop 19) ; #x13
(defconstant sigsys 31) ; #x1f
(defconstant sigterm 15) ; #xf
(defconstant sigtrap 5) ; #x5
(defconstant sigtstp 20) ; #x14
(defconstant sigttin 21) ; #x15
(defconstant sigttou 22) ; #x16
(defconstant sigurg 23) ; #x17
(defconstant sigusr1 10) ; #xa
(defconstant sigusr2 12) ; #xc
(defconstant sigvtalrm 26) ; #x1a
(defconstant sigwinch 28) ; #x1c
(defconstant sigxcpu 24) ; #x18
(defconstant sigxfsz 25) ; #x19
(defconstant fpe-intovf 2) ; #x2
(defconstant fpe-intdiv 1) ; #x1
(defconstant fpe-fltdiv 3) ; #x3
(defconstant fpe-fltovf 4) ; #x4
(defconstant fpe-fltund 5) ; #x5
(defconstant fpe-fltres 6) ; #x6
(defconstant fpe-fltinv 7) ; #x7
(defconstant fpe-fltsub 8) ; #x8

(defconstant clock-realtime 0) ; #x0
(defconstant clock-monotonic 1) ; #x1
(defconstant clock-process-cputime-id 2) ; #x2
(defconstant clock-realtime-alarm 8) ; #x8
(defconstant clock-realtime-coarse 5) ; #x5
(defconstant clock-tai 11) ; #xb
(defconstant clock-monotonic-coarse 6) ; #x6
(defconstant clock-monotonic-raw 4) ; #x4
(defconstant clock-boottime 7) ; #x7
(defconstant clock-boottime-alarn 9) ; #x9
(defconstant clock-thread-cputime-id 3) ; #x3
;;; structures
(define-alien-type nil
  (struct timeval
          (tv-sec (signed 32))
          (tv-usec (signed 32))))
(define-alien-type nil
  (struct timespec
          (tv-sec (signed 32))
          (tv-nsec (signed 32))))

(defconstant sizeof-timespec 16) ; #x10
(defconstant sizeof-timeval 16) ; #x10

(in-package "SB-KERNEL")

;;; GENCGC related
(define-alien-type page-index-t (signed 32))
(define-alien-type generation-index-t (signed 8))

;;; Our runtime types
(define-alien-type os-vm-size-t (unsigned 32))
