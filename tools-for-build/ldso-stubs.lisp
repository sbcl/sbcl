;;;; Generate stubs for C-linkage library functions which we need to refer to
;;;; from Lisp.
;;;;
;;;; These stubs exist for the benefit of Lisp code that needs to refer
;;;; to foreign symbols when dlsym() is not available (i.e. when dumping
;;;; cold-sbcl.core, when we may be running in a host that's not SBCL,
;;;; or on platforms that don't have it at all). If the runtime is
;;;; dynamically linked, library functions won't be linked into it, so
;;;; the map file won't show them. So, we need a bunch of stubs that
;;;; nm(1) _can_ see.
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; This is an attempt to follow DB's hint of sbcl-devel
;;; 2001-09-18. -- CSR
;;;
(defun ldso-stubify (index fct stream)
  (declare (ignorable index))
  #+hppa
  (let ((stub (format nil "ldso_stub__~a" fct)))
    (apply #'format stream (list
"    .export ~A
~A:
    .proc
    .callinfo
    b,n ~a
    .procend
    .import ~a,code~%" stub stub fct fct)))

  #-hppa
  (format stream "LDSO_STUBIFY(~A)~%" fct))

(defvar *preludes* (list "
/* This is an automatically generated file, please do not hand-edit it.
 * See the program tools-for-build/ldso-stubs.lisp. */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__
#endif
#include \"sbcl.h\""

#+(and linux alpha) "
#define LDSO_STUBIFY(fct)                       \\
.globl ldso_stub__ ## fct ;                     \\
        .type    ldso_stub__ ## fct,@function ; \\
ldso_stub__ ## fct: ;                           \\
        jmp fct ;                               \\
.L ## fct ## e1: ;                              \\
        .size    ldso_stub__ ## fct,.L ## fct ## e1-ldso_stub__ ## fct ;"

#+hppa "
        .level  2.0
        .text"

))

;;; Most of the #+- in here is just noise, but so is this whole file.
;;; So I don't really care to go through with a comb and remove the fluff.
(defvar *stubs* (append
                 '("_exit"
                   "accept"
                   "access"
                   "acos"
                   "acosh"
                   "asin"
                   "asinh"
                   "atanh"
                   "bind"
                   "chmod"
                   "chown"
                   "close"
                   "closedir"
                   "connect"
                   "cosh"
                   "creat"
                   "dup"
                   "dup2"
                   "execve"
                   "exit"
                   "fchmod"
                   "fchown"
                   "fcntl"
                   "fork"
                   "free"
                   "fstat"
                   #+inode64 "fstat$INODE64"
                   "fsync"
                   "ftruncate"
                   "getcwd"
                   "getdtablesize"
                   "getegid"
                   "getenv"
                   "getgid"
                   "gethostbyaddr"
                   "gethostbyname"
                   "gethostname"
                   "getitimer"
                   "getpeername"
                   "getpgrp"
                   "getpid"
                   "getppid"
                   "getrusage"
                   "getsockname"
                   "gettimeofday"
                   "getuid"
                   "hypot"
                   "ioctl"
                   "isatty"
                   "kill"
                   "killpg"
                   "link"
                   "listen"
                   "log1p"
                   "lseek"
                   "lstat"
                   #+inode64 "lstat$INODE64"
                   "malloc"
                   #+(or x86 x86-64) "memcmp"
                   "memmove"
                   "mkdir"
                   "nanosleep"
                   "open"
                   "opendir"
                   "pipe"
                   "poll"
                   "pow"
                   "read"
                   "readdir"
                   "readlink"
                   "realpath"
                   "recv"
                   "rename"
                   "rmdir"
                   "select"
                   "send"
                   "setitimer"
                   "setpgrp"
                   "setsid"
                   "sinh"
                   "socket"
                   "stat"
                   #+inode64 "stat$INODE64"
                   "strerror"
                   "strlen"
                   "symlink"
                   "sync"
                   "tanh"
                   "truncate"
                   "ttyname"
                   #-hpux "tzname"
                   "unlink"
                   "utimes"
                   "waitpid"
                   "write")
                 ;; These aren't needed on the X86 because they're microcoded into the
                 ;; FPU, so the Lisp VOPs can implement them directly without having to
                 ;; call C code.
                 ;;
                 ;; Note: There might be some other functions in this category as well.
                 ;; E.g. I notice tanh() and acos() in the list above.. -- WHN 2001-06-07
                 #-x86
                 '("sin"
                   "cos"
                   "tan"
                   "atan"
                   "atan2"
                   "exp"
                   "log"
                   "log10"
                   "sqrt")
                 #+alpha
                 '("ieee_get_fp_control"
                   "ieee_set_fp_control")
                 ;; FIXME: After 1.0 this should be made
                 ;; #-linkage-table, as we only need these stubs if
                 ;; we don't have linkage-table. Done this way now to
                 ;; cut down on the number of ports affected.
                 #-(or win32 darwin freebsd netbsd openbsd)
                 '("ptsname"
                   #-android "grantpt"
                   "unlockpt")
                 #+(or openbsd freebsd dragonfly)
                 '("openpty")
                 '("dlclose"
                   "dlerror"
                   "dlopen"
                   "dlsym")
                 #+bsd
                 '("sysctl")
                 #+darwin
                 '("sysctlbyname")
                 #+os-provides-dladdr
                 '("dladdr")
                 #-android
                   '("nl_langinfo"
                     "getpagesize"
                     "cfgetispeed"
                     "cfgetospeed"
                     "cfsetispeed"
                     "cfsetospeed"
                     "tcdrain"
                     "tcflow"
                     "tcflush"
                     "tcgetattr"
                     "tcsendbreak"
                     "tcsetattr")))

(with-open-file (f "src/runtime/ldso-stubs.S" :direction :output :if-exists :supersede)
  (assert (= (length *preludes*) 2))
  (dolist (pre *preludes*)
    (write-line pre f))
  (let ((i -1))
    (dolist (stub *stubs*)
      (check-type stub string)
      (ldso-stubify (incf i) stub f))))
