;;;; Generate stubs for C-linkage library functions which we need to refer to
;;;; from Lisp.
;;;;
;;;; (But note this is only the Linux version, as per the FIXME
;;;; note in the BSD version in undefineds.h.)
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

#!-sparc
(defun ldso-stubify (fct str)
  (format str "LDSO_STUBIFY(~A)~%" fct))

;;; This is an attempt to follow DB's hint of sbcl-devel
;;; 2001-09-18. -- CSR
;;;
;;; And an attempt to work around the Sun toolchain... --ns
#!+sparc
(defun ldso-stubify (fct str)
  (apply #'format str "
.globl ldso_stub__~A ;                          \\
        FUNCDEF(ldso_stub__~A) ;                \\
ldso_stub__~A: ;                                \\
        sethi %hi(~A),%g1       ;               \\
        jmpl %g1+%lo(~A),%g0    ;               \\
        nop /* delay slot*/     ;               \\
.L~Ae1: ;                                       \\
        .size    ldso_stub__~A,.L~Ae1-ldso_stub__~A ;~%"
          (make-list 9 :initial-element fct)))

(defvar *preludes* '("
/* This is an automatically generated file, please do not hand-edit it.
 * See the program tools-for-build/ldso-stubs.lisp. */

#ifndef LANGUAGE_ASSEMBLY
#define LANGUAGE_ASSEMBLY
#endif
#include \"sbcl.h\""

#!+sparc "
#ifdef LISP_FEATURE_SPARC
#include \"sparc-funcdef.h\"
#endif
        .text"

#!+(or x86 x86-64) "
#define LDSO_STUBIFY(fct)                       \\
        .align 16 ;                             \\
.globl ldso_stub__ ## fct ;                     \\
        .type    ldso_stub__ ## fct,@function ; \\
ldso_stub__ ## fct: ;                           \\
        jmp fct ;                               \\
.L ## fct ## e1: ;                              \\
        .size    ldso_stub__ ## fct,.L ## fct ## e1-ldso_stub__ ## fct ;"

;;; osf1 has ancient cpp that doesn't do ##
#!+(and osf1 alpha) "
#define LDSO_STUBIFY(fct)                       \\
.globl ldso_stub__/**/fct ;                     \\
ldso_stub__/**/fct: ;                           \\
        jmp fct ;                               \\
.L/**/fct/**/e1: ;"

;;; but there's no reason we need to put up with that on modern (Linux) OSes
#!+(and linux alpha) "
#define LDSO_STUBIFY(fct)                       \\
.globl ldso_stub__ ## fct ;                     \\
        .type    ldso_stub__ ## fct,@function ; \\
ldso_stub__ ## fct: ;                           \\
        jmp fct ;                               \\
.L ## fct ## e1: ;                              \\
        .size    ldso_stub__ ## fct,.L ## fct ## e1-ldso_stub__ ## fct ;"

#!+(and linux ppc) "
#define LDSO_STUBIFY(fct)                       \\
.globl ldso_stub__ ## fct ;                     \\
        .type    ldso_stub__ ## fct,@function ; \\
ldso_stub__ ## fct: ;                           \\
        b fct ;                                 \\
.L ## fct ## e1: ;                              \\
        .size    ldso_stub__ ## fct,.L ## fct ## e1-ldso_stub__ ## fct ;"

#!+(and darwin ppc) "
#define LDSO_STUBIFY(fct)                       @\\
.text                                           @\\
.globl  ldso_stub___ ## fct                     @\\
ldso_stub___ ## fct:                            @\\
        b ldso_stub__ ## fct ## stub            @\\
.symbol_stub ldso_stub__ ## fct ## stub:        @\\
.indirect_symbol _ ## fct                       @\\
        lis     r11,ha16(ldso_stub__ ## fct ## $lazy_ptr)       @\\
        lwz     r12,lo16(ldso_stub__ ## fct ## $lazy_ptr)(r11)  @\\
        mtctr   r12                             @\\
        addi    r11,r11,lo16(ldso_stub__ ## fct ## $lazy_ptr)   @\\
        bctr                                    @\\
.lazy_symbol_pointer                            @\\
ldso_stub__ ## fct ## $lazy_ptr:                @\\
        .indirect_symbol _ ## fct               @\\
        .long dyld_stub_binding_helper"

;;; KLUDGE: set up the vital fifth argument, passed on the
;;; stack.  Do this unconditionally, even if the stub is for a
;;; function with few arguments: it can't hurt.  We only do this for
;;; the fifth argument, as the first four are passed in registers
;;; and we apparently don't ever need to pass six arguments to a
;;; libc function.  -- CSR, 2003-10-29
;;; Expanded to 8 arguments regardless.  -- ths, 2005-03-24
#!+mips "
#define LDSO_STUBIFY(fct)                      \\
        .globl  ldso_stub__ ## fct ;           \\
        .type   ldso_stub__ ## fct,@function ; \\
        .ent    ldso_stub__ ## fct ;           \\
ldso_stub__ ## fct: ;                  \\
        .set noat ;                    \\
        addiu $29,-48 ;                \\
        sw $28,40($29) ;               \\
        sw $31,44($29) ;               \\
        lw $25,64($29) ;               \\
        lw  $1,68($29) ;               \\
        sw $25,16($29) ;               \\
        sw  $1,20($29) ;               \\
        lw $25,72($29) ;               \\
        lw  $1,76($29) ;               \\
        sw $25,24($29) ;               \\
        sw  $1,28($29) ;               \\
        .set at ;                      \\
        la $25, fct ;                  \\
        jalr $25 ;                     \\
        lw $31,44($29) ;               \\
        lw $28,40($29) ;               \\
        addiu $29,48 ;                 \\
        jr $31 ;                       \\
        .end    ldso_stub__ ## fct ;   \\
        .size   ldso_stub__ ## fct,.-ldso_stub__ ## fct ;"))

(defvar *stubs* (append
                 '("accept"
                   "access"
                   "acos"
                   "acosh"
                   "asin"
                   "asinh"
                   "atanh"
                   "bind"
                   "cfgetispeed"
                   "cfgetospeed"
                   "cfsetispeed"
                   "cfsetospeed"
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
                   "getpagesize"
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
                   "malloc"
                   "memmove"
                   "mkdir"
                   "nanosleep"
                   "nl_langinfo"
                   "open"
                   "opendir"
                   "pipe"
                   "pow"
                   "read"
                   "readdir"
                   "readlink"
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
                   "strerror"
                   "strlen"
                   "symlink"
                   "sync"
                   "tanh"
                   "tcdrain"
                   "tcflow"
                   "tcflush"
                   "tcgetattr"
                   "tcsendbreak"
                   "tcsetattr"
                   "truncate"
                   "ttyname"
                   "tzname"
                   "unlink"
                   "utimes"
                   "wait3"
                   "write")
                 ;; These aren't needed on the X86 because they're microcoded into the
                 ;; FPU, so the Lisp VOPs can implement them directly without having to
                 ;; call C code.
                 ;;
                 ;; Note: There might be some other functions in this category as well.
                 ;; E.g. I notice tanh() and acos() in the list above.. -- WHN 2001-06-07
                 #!-x86
                 '("sin"
                   "cos"
                   "tan"
                   "atan"
                   "atan2"
                   "exp"
                   "log"
                   "log10"
                   "sqrt")
                 #!+alpha
                 '("ieee_get_fp_control"
                   "ieee_set_fp_control")
                 #!-darwin
                 '("dlclose"
                   "dlerror"
                   "dlopen"
                   "dlsym")
                 #!+os-provides-dladdr
                 '("dladdr")
                 #!-sunos ;; !defined(SVR4)
                 '("sigsetmask")))

(with-open-file (f "src/runtime/ldso-stubs.S" :direction :output :if-exists :supersede)
  (assert (= (length *preludes*) 2))
  (dolist (pre *preludes*)
    (write-line pre f))
  (dolist (stub *stubs*)
    (check-type stub string)
    (ldso-stubify stub f)))

