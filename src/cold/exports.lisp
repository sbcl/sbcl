;;;; All the stuff necessary to export various symbols from various packages.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; the specifications of target packages, except for a few things
;;;; which are handled elsewhere by other mechanisms:
;;;;   * some SHADOWing and nickname hackery;
;;;;   * the standard, non-SBCL-specific packages COMMON-LISP,
;;;;     COMMON-LISP-USER, and KEYWORD.
;;;;

;;;; NOTE:
;;;; All uses of #+ and #- reader macros within this file refer to
;;;; the chosen target features, and not CL:*FEATURES*, but
;;;; it is generally not necessary to use reader conditionals
;;;; within this file. If A-NICE-FUNCTION is external in SB-KERNEL
;;;; but not defined for a particular backend, it will not get
;;;; interned during cold load, and hence will not exist in the target.
;;;; This is exactly as desired - it is not necessary for the writer
;;;; of architecture-specific code to clutter up the list of package
;;;; definitions to avoid exporting some stuff. You just export what you
;;;; need, and genesis will do the right thing. Most of the pre-existing
;;;; conditionals are historical baggage, and should be removed.
;;;; EXCEPTION: packages whose symbols are created mainly during
;;;; warm load might have a reason to use reader conditionals.
;;;; For those packages, all symbols listed here are interned during
;;;; genesis, since otherwise the symbols would all disappear,
;;;; and then warm load would intern them as internals, not externals.
;;;; This list of exceptional packages can be found in the
;;;; definition of the FINISH-SYMBOLS function.

(in-package "SB-COLD")

(defpackage* "SB-LOOP"
  (:documentation "private: implementation details of LOOP")
  (:use "CL" "SB-INT" "SB-KERNEL"))

(defpackage* "SB-UNIX"
  (:documentation
   "private: a wrapper layer for SBCL itself to use when talking with
an underlying Unix-y operating system.  This was a public package in
CMU CL, but that was different.  CMU CL's UNIX package tried to
provide a comprehensive, stable Unix interface suitable for the end
user.  This package only tries to implement what happens to be needed
by the current implementation of SBCL, and makes no guarantees of
interface stability.")
  (:use "CL" "SB-ALIEN" "SB-EXT" "SB-INT" "SB-SYS")
  (:reexport "OFF-T"
             "SIZE-T")
  (:export
   ;; wrappers around Unix stuff to give just what Lisp needs
   "NANOSLEEP"
   "UID-USERNAME"
   "UID-HOMEDIR"
   "USER-HOMEDIR"
   "SB-MKSTEMP"
   "UNIX-OFFSET"
   "FD-TYPE"

   ;; Most of this is random detritus worthy of deletion,
   ;; and the ordering is not alphabetical or anything sane.

   ;; stuff with a one-to-one mapping to Unix constructs

   "DEV-T"
   "F_OK" "GID-T"
   "INO-T" "UNIX-ACCESS" "UNIX-SETITIMER" "UNIX-GETITIMER"
   "L_INCR" "L_SET" "L_XTND" "O_APPEND" "O_CREAT" "O_NOCTTY" "O_EXCL"
   "O_RDONLY" "O_RDWR" "O_TRUNC" "O_WRONLY" "POSIX-GETCWD"
   "POSIX-GETCWD/"
   "RU-IDRSS" "RU-INBLOCK" "RU-ISRSS" "RU-IXRSS"
   "RU-MAJFLT" "RU-MAXRSS" "RU-MINFLT" "RU-MSGRCV" "RU-MSGSND"
   "RU-NIVCSW" "RU-NSIGNALS" "RU-NSWAP" "RU-NVCSW" "RU-OUBLOCK"
   "RU-STIME" "RU-UTIME" "RUSAGE_CHILDREN" "RUSAGE_SELF"
   "R_OK" "S-IFDIR" "S-IFLNK" "S-IFMT"
   "S-IFREG"
   "ST-ATIME" "ST-BLKSIZE" "ST-BLOCKS"
   "ST-CTIME" "ST-DEV" "ST-GID" "ST-MODE" "ST-MTIME" "ST-NLINK"
   "ST-RDEV" "ST-SIZE" "ST-UID" "STAT" "TIME-T"
   "TIMEVAL" "TIMEZONE"
   "TIOCGPGRP"
   "TV-SEC" "TV-USEC"
   "TZ-DSTTIME" "TZ-MINUTESWEST" "UID-T" "UNIX-CLOSE"
   "UNIX-CLOSEDIR" "UNIX-DIRENT-NAME" "UNIX-DUP"
   "UNIX-FILE-MODE" "UNIX-FSTAT"
   "UNIX-GETHOSTNAME" "UNIX-GETPID" "UNIX-GETRUSAGE"
   "UNIX-GETTIMEOFDAY" "UNIX-GETUID" "UNIX-GID"
   "UNIX-EXIT"
   "UNIX-IOCTL"
   "UNIX-ISATTY" "UNIX-LSEEK" "UNIX-LSTAT" "UNIX-MKDIR"
   "UNIX-OPEN" "UNIX-OPENDIR" "UNIX-PATHNAME" "UNIX-PID"
   "UNIX-PIPE" "UNIX-POLL" "UNIX-SIMPLE-POLL"
   "UNIX-READ" "UNIX-READDIR" "UNIX-READLINK" "UNIX-REALPATH"
   "UNIX-RENAME" "UNIX-SELECT" "UNIX-STAT" "UNIX-UID"
   "UNIX-UNLINK" "UNIX-WRITE"
   "WCONTINUED" "WNOHANG" "WUNTRACED"
   "W_OK" "X_OK"
   "SC-NPROCESSORS-ONLN"
   "VOID-SYSCALL"
   "CLOCK-THREAD-CPUTIME-ID"
   "CLOCK-PROCESS-CPUTIME-ID"

   ;; signals

   "SIGALRM" "SIGBUS" "SIGCHLD" "SIGCONT" "SIGEMT" "SIGFPE"
   "SIGHUP" "SIGILL" "SIGINT" "SIGIO" "SIGKILL"
   "SIGPIPE" "SIGPROF" "SIGQUIT" "SIGSEGV" "SIGSTOP" "SIGSYS"
   "SIGTERM" "SIGTRAP" "SIGTSTP" "SIGTTIN" "SIGTTOU" "SIGURG"
   "SIGUSR1" "SIGUSR2" "SIGVTALRM" "SIGWINCH"
   "SIGXCPU" "SIGXFSZ"

   ;; errors

   "EAGAIN" "EBADF" "EEXIST" "EINTR" "EIO" "ELOOP" "ENOENT"
   "EPIPE" "ESPIPE" "EWOULDBLOCK"

   "POLLFD" "POLLIN" "POLLOUT" "POLLHUP" "POLLNVAL" "POLLERR"
   "FD" "EVENTS" "REVENTS"
   "FD-ISSET" "FD-SET" "UNIX-FAST-SELECT"
   "PTHREAD-KILL" "RAISE" "UNIX-KILL" "UNIX-KILLPG"
   "FD-ZERO" "FD-CLR"
   "FD-SETSIZE"))

#+win32
(defpackage* "SB-WIN32"
  (:documentation "private: a wrapper layer for Win32 functions needed by
SBCL itself")
  (:use "CL" "SB-ALIEN" "SB-EXT" "SB-INT" "SB-SYS")
  (:export "BOOL"
           "CLOSE-HANDLE"
           "CREATE-FILE"
           "CREATE-FILE-MAPPING"
           "CRYPT-GEN-RANDOM"
           "DWORD"
           "EXCEPTION"
           "EXCEPTION-RECORD"
           "EXCEPTION-CONTEXT"
           "EXCEPTION-CODE"
           "FILE-CREATE-ALWAYS"
           "FILE-CREATE-NEW"
           "FILE-OPEN-ALWAYS"
           "FILE-OPEN-EXISTING"
           "FILE-TRUNCATE-EXISTING"
           "FLUSH-CONSOLE-INPUT-BUFFER"
           "FLUSH-VIEW-OF-FILE"
           "FORMAT-SYSTEM-MESSAGE"
           "GET-FILE-ATTRIBUTES"
           "GET-FILE-SIZE-EX"
           "GET-FILE-TYPE"
           "GET-LAST-ERROR"
           "GET-OSFHANDLE"
           "GET-VERSION-EX"
           "HANDLE"
           "HANDLE-CLEAR-INPUT"
           "HANDLE-LISTEN"
           "INT-PTR"
           "INVALID-HANDLE"
           "LSEEKI64"
           "MAP-VIEW-OF-FILE"
           "MILLISLEEP"
           "PEEK-CONSOLE-INPUT"
           "PEEK-NAMED-PIPE"
           "READ-FILE"
           "UNIXLIKE-CLOSE"
           "UNIXLIKE-OPEN"
           "UNMAP-VIEW-OF-FILE"
           "WAIT-OBJECT-OR-SIGNAL"
           "WRITE-FILE"
           "WITH-PROCESS-TIMES"))

(defpackage* "SB-FORMAT"
  (:documentation "private: implementation of FORMAT and friends")
  (:use "CL" "SB-EXT" "SB-INT" "SB-KERNEL")
  (:export "%COMPILER-WALK-FORMAT-STRING" "FORMAT-ERROR" "TOKENS"))

(defpackage* "SB-BIGNUM"
  (:documentation "private: bignum implementation")
  (:use "CL" "SB-KERNEL" "SB-INT" "SB-EXT" "SB-ALIEN" "SB-SYS")
  (:export "%ADD-WITH-CARRY"
           "%ALLOCATE-BIGNUM" "%ASHL" "%ASHR"
           "%BIGNUM-LENGTH" "%BIGNUM-REF" "%BIGNUM-REF-WITH-OFFSET"
           "%BIGNUM-SET" #+bignum-assertions "%%BIGNUM-SET"
                         "%BIGNUM-SET-LENGTH" "%DIGIT-0-OR-PLUSP"
                         "%DIGIT-LOGICAL-SHIFT-RIGHT"
                         "%FIXNUM-DIGIT-WITH-CORRECT-SIGN" "%FIXNUM-TO-DIGIT"
                         "%BIGFLOOR" "%LOGAND" "%LOGIOR" "%LOGNOT" "%LOGXOR"
                         "%MULTIPLY" "%MULTIPLY-AND-ADD"
                         "%SUBTRACT-WITH-BORROW" "ADD-BIGNUMS"
                         "BIGNUM-ASHIFT-LEFT" "BIGNUM-ASHIFT-LEFT-FIXNUM"
                         "BIGNUM-ASHIFT-RIGHT"
                         "BIGNUM-COMPARE"
                         "BIGNUM-ELEMENT-TYPE" "BIGNUM-GCD" "BIGNUM-INDEX"
                         "BIGNUM-LENGTH"
                         "BIGNUM-INTEGER-LENGTH"
                         "BIGNUM-LOGBITP"
                         "BIGNUM-LOGCOUNT" "BIGNUM-LOGICAL-AND"
                         "BIGNUM-LOGICAL-IOR" "BIGNUM-LOGICAL-NOT"
                         "BIGNUM-LOGICAL-XOR" "BIGNUM-PLUS-P"
                         "BIGNUM-TO-FLOAT" "BIGNUM-TRUNCATE"
                         "MAKE-SMALL-BIGNUM"
                         "MULTIPLY-BIGNUM-AND-FIXNUM" "MULTIPLY-BIGNUMS"
                         "MULTIPLY-FIXNUMS" "NEGATE-BIGNUM"
                         "%RANDOM-BIGNUM" "SUBTRACT-BIGNUM" "SXHASH-BIGNUM"))

;; This package is a grab bag for things which used to be internal
;; symbols in package COMMON-LISP. Lots of these symbols are accessed
;; with explicit SB-IMPL:: prefixes in the code. It would be nice to
;; reduce the use of this practice, so if symbols from here which are
;; accessed that way are found to belong more appropriately in an
;; existing package (e.g. SB-KERNEL or SB-SYS or SB-EXT or SB-FASL),
;; I (WHN 19990223) encourage maintainers to move them there..
(defpackage* "SB-IMPL"
  (:documentation "private: a grab bag of implementation details")
  (:import-from "SB-KERNEL" "*PACKAGE-NAMES*")
  (:export "FORMAT-MICROSECONDS" "FORMAT-MILLISECONDS" ; for ~/fmt/

           "PRINT-TYPE" "PRINT-TYPE-SPECIFIER"
           "PRINT-LAMBDA-LIST"
           ;; protect from tree shaker so we can test this function

           "EXPAND-SYMBOL-CASE"
           ;; symbols used by sb-simple-streams

           "ANSI-STREAM-CLEAR-INPUT"
           "ANSI-STREAM-LISTEN"
           "ANSI-STREAM-READ-BYTE" "ANSI-STREAM-READ-CHAR"
           "ANSI-STREAM-READ-CHAR-NO-HANG" "ANSI-STREAM-READ-LINE"
           "ANSI-STREAM-READ-SEQUENCE" "ANSI-STREAM-PEEK-CHAR"
           "ANSI-STREAM-UNREAD-CHAR"
           "DISPATCH-TABLES" "CHARACTER-MACRO-HASH-TABLE" "CHARACTER-MACRO-ARRAY"
           "TOKEN-DELIMITERP" "WHITESPACE[2]P" "WITH-READ-BUFFER"
           ;; other

           "%MAKUNBOUND")
  (:use "CL" "SB-ALIEN" "SB-BIGNUM" "SB-DEBUG" "SB-EXT"
        "SB-FASL" "SB-GRAY" "SB-INT" "SB-KERNEL" "SB-SYS"))

(defpackage* "SB-SEQUENCE"
  (:documentation "semi-public: implements something which might eventually
be submitted as a CDR")
  (:export "PROTOCOL-UNIMPLEMENTED"
           "PROTOCOL-UNIMPLEMENTED-OPERATION"

           "DOSEQUENCE"

           "MAKE-SEQUENCE-ITERATOR" "MAKE-SIMPLE-SEQUENCE-ITERATOR"

           "ITERATOR-STEP" "ITERATOR-ENDP" "ITERATOR-ELEMENT"
           "ITERATOR-INDEX" "ITERATOR-COPY"

           "WITH-SEQUENCE-ITERATOR" "WITH-SEQUENCE-ITERATOR-FUNCTIONS"

           "CANONIZE-TEST" "CANONIZE-KEY"

           "EMPTYP" "LENGTH" "ELT"
           "MAKE-SEQUENCE-LIKE" "ADJUST-SEQUENCE"

           "MAP"
           "COUNT" "COUNT-IF" "COUNT-IF-NOT"
           "FIND" "FIND-IF" "FIND-IF-NOT"
           "POSITION" "POSITION-IF" "POSITION-IF-NOT"
           "SUBSEQ" "COPY-SEQ" "FILL"
           "NSUBSTITUTE" "NSUBSTITUTE-IF" "NSUBSTITUTE-IF-NOT"
           "SUBSTITUTE" "SUBSTITUTE-IF" "SUBSTITUTE-IF-NOT"
           "REPLACE" "REVERSE" "NREVERSE" "CONCATENATE" "REDUCE"
           "MISMATCH" "SEARCH"
           "DELETE" "DELETE-IF" "DELETE-IF-NOT"
           "REMOVE" "REMOVE-IF" "REMOVE-IF-NOT"
           "DELETE-DUPLICATES" "REMOVE-DUPLICATES"

           "SORT" "STABLE-SORT" "MERGE"))

#+sb-eval
(defpackage* "SB-EVAL"
  (:documentation "internal: the evaluator implementation used to execute code without compiling it.")
  (:use "CL" "SB-KERNEL" "SB-EXT" "SB-INT")
  (:reexport "*EVAL-CALLS*")
  (:export "INTERPRETED-FUNCTION-NAME"
           "INTERPRETED-FUNCTION-DEBUG-NAME"
           "INTERPRETED-FUNCTION-LAMBDA-LIST"
           "INTERPRETED-FUNCTION-DEBUG-LAMBDA-LIST"
           "INTERPRETED-FUNCTION-DECLARATIONS"
           "INTERPRETED-FUNCTION-DOCUMENTATION"
           "INTERPRETED-FUNCTION-BODY"
           "INTERPRETED-FUNCTION-SOURCE-LOCATION"
           "EVAL-IN-ENVIRONMENT"
           "MAKE-NULL-ENVIRONMENT"
           "EVAL-IN-NATIVE-ENVIRONMENT"
           "*EVAL-LEVEL*"))

#+sb-fasteval
(defpackage* "SB-INTERPRETER"
  (:documentation "internal: a new EVAL implementation with semantic preprocessing.")
  (:use "CL" "SB-KERNEL" "SB-EXT" "SB-INT")
  (:import-from "SB-C"
                "PARSE-EVAL-WHEN-SITUATIONS"
                "MAKE-GLOBAL-VAR" "MAKE-LAMBDA-VAR"
                "*LEXENV*")
  (:import-from "SB-VM" "SYMBOL-EXTRA-SLOT-P" "SYMBOL-EXTRA")
  (:import-from "SB-ALIEN" "%HEAP-ALIEN" "ALIEN-VALUE")
  (:import-from "SB-KERNEL" "%%TYPEP")
  (:export "BASIC-ENV"
           "ENV-POLICY"
           "EVAL-IN-ENVIRONMENT"
           "FIND-LEXICAL-FUN"
           "FIND-LEXICAL-VAR"
           "FUN-LAMBDA-EXPRESSION"
           "FUN-LAMBDA-LIST"
           "FUN-PROTO-FN"
           "FUN-SOURCE-LOCATION"
           "%FUN-FTYPE"
           "LEXENV-FROM-ENV"
           "LIST-LOCALS"
           "PROTO-FN-DOCSTRING"
           "PROTO-FN-NAME"
           "PROTO-FN-PRETTY-ARGLIST"
           "TYPE-CHECK"))

(defpackage* "SB-VM"
  (:documentation
   "internal: the default place to hide information about the hardware and data
structure representations")
  (:use "CL" "SB-ALIEN" "SB-ALIEN-INTERNALS" "SB-ASSEM" "SB-C"
        "SB-EXT" "SB-FASL" "SB-INT" "SB-KERNEL" "SB-SYS" "SB-UNIX")
  (:import-from "SB-C" "VOP-ARGS" "VOP-RESULTS")
  (:reexport "WORD")
  (:export "*PRIMITIVE-OBJECTS*"
           "+HIGHEST-NORMAL-GENERATION+"
           "+PSEUDO-STATIC-GENERATION+"
           "+ARRAY-FILL-POINTER-P+"
           "+VECTOR-SHAREABLE+"
           "+VECTOR-SHAREABLE-NONSTD+"
           "%COMPILER-BARRIER" "%DATA-DEPENDENCY-BARRIER"
           "%MEMORY-BARRIER" "%READ-BARRIER" "%WRITE-BARRIER"
           "AFTER-BREAKPOINT-TRAP"
           #+(and gencgc sparc) "ALLOCATION-TRAP"
           "ANY-REG-SC-NUMBER" "ARRAY-DATA-SLOT" "ARRAY-DIMENSIONS-OFFSET"
           "ARRAY-DISPLACED-P-SLOT" "ARRAY-DISPLACEMENT-SLOT"
           "ARRAY-DISPLACED-FROM-SLOT"
           "ARRAY-ELEMENTS-SLOT"
           "ARRAY-FILL-POINTER-SLOT"
           "ARRAY-RANK-POSITION" "ARRAY-FLAGS-POSITION"
           "ARRAY-FLAGS-DATA-POSITION"
           "CHARACTER-REG-SC-NUMBER"
           "CHARACTER-STACK-SC-NUMBER" "CHARACTER-WIDETAG"
           "BIGNUM-DIGITS-OFFSET" "BIGNUM-WIDETAG" "BINDING-SIZE"
           "BINDING-SYMBOL-SLOT" "BINDING-VALUE-SLOT" "BREAKPOINT-TRAP"
           "N-BYTE-BITS"
           "CATCH-BLOCK-CODE-SLOT"
           "CATCH-BLOCK-CFP-SLOT" "CATCH-BLOCK-UWP-SLOT"
           "CATCH-BLOCK-ENTRY-PC-SLOT" "CATCH-BLOCK-PREVIOUS-CATCH-SLOT"
           "CATCH-BLOCK-SC-NUMBER" "CATCH-BLOCK-SIZE"
           "CATCH-BLOCK-TAG-SLOT" "CERROR-TRAP"
           "CLOSURE-FUN-SLOT"
           "CLOSURE-WIDETAG"
           "CLOSURE-INFO-OFFSET"
           "CODE-BOXED-SIZE-SLOT"
           "CODE-CONSTANTS-OFFSET" "CODE-SLOTS-PER-SIMPLE-FUN"
           "CODE-DEBUG-INFO-SLOT"
           "CODE-HEADER-SIZE-SHIFT"
           "CODE-HEADER-WIDETAG" "COMPLEX-ARRAY-WIDETAG"
           "COMPLEX-BIT-VECTOR-WIDETAG" "COMPLEX-DOUBLE-FLOAT-FILLER-SLOT"
           "COMPLEX-DOUBLE-FLOAT-IMAG-SLOT" "COMPLEX-DOUBLE-FLOAT-REAL-SLOT"
           "COMPLEX-DOUBLE-FLOAT-SIZE" "COMPLEX-DOUBLE-FLOAT-WIDETAG"
           "COMPLEX-DOUBLE-REG-SC-NUMBER" "COMPLEX-DOUBLE-STACK-SC-NUMBER"
           "COMPLEX-IMAG-SLOT" "COMPLEX-REAL-SLOT"
           #+long-float "COMPLEX-LONG-FLOAT-IMAG-SLOT"
           #+long-float "COMPLEX-LONG-FLOAT-REAL-SLOT"
           #+long-float "COMPLEX-LONG-FLOAT-SIZE"
           #+long-float "COMPLEX-LONG-FLOAT-WIDETAG"
           #+long-float "COMPLEX-LONG-REG-SC-NUMBER"
           #+long-float "COMPLEX-LONG-STACK-SC-NUMBER"
           #-64-bit "COMPLEX-SINGLE-FLOAT-IMAG-SLOT"
           #-64-bit "COMPLEX-SINGLE-FLOAT-REAL-SLOT"
           #+64-bit "COMPLEX-SINGLE-FLOAT-DATA-SLOT"
           "COMPLEX-SINGLE-FLOAT-SIZE" "COMPLEX-SINGLE-FLOAT-WIDETAG"
           "COMPLEX-SINGLE-REG-SC-NUMBER" "COMPLEX-SINGLE-STACK-SC-NUMBER"
           "COMPLEX-SIZE" "COMPLEX-BASE-STRING-WIDETAG"
           #+sb-unicode "COMPLEX-CHARACTER-STRING-WIDETAG"
           "COMPLEX-WIDETAG"
           "COMPLEX-VECTOR-WIDETAG" "CONS-CAR-SLOT" "CONS-CDR-SLOT"
           "CONS-SIZE" "CONSTANT-SC-NUMBER"
           "CONTEXT-FLOATING-POINT-MODES" "CONTEXT-FLOAT-REGISTER"
           "CONTEXT-PC" "CONTEXT-REGISTER" "BOXED-CONTEXT-REGISTER"
           "CONTROL-STACK-SC-NUMBER"
           #+sb-safepoint "CSP-SAFEPOINT-TRAP"
           "*CURRENT-CATCH-BLOCK*"
           "CURRENT-FLOAT-TRAP"
           "DESCRIPTOR-REG-SC-NUMBER"
           "DO-REFERENCED-OBJECT"
           "DOUBLE-FLOAT-BIAS"
           "DOUBLE-FLOAT-DIGITS" "DOUBLE-FLOAT-EXPONENT-BYTE"
           "DOUBLE-FLOAT-HIDDEN-BIT"
           "DOUBLE-FLOAT-NORMAL-EXPONENT-MAX"
           "DOUBLE-FLOAT-NORMAL-EXPONENT-MIN" "DOUBLE-FLOAT-SIGNIFICAND-BYTE"
           "DOUBLE-FLOAT-SIZE"
           "DOUBLE-FLOAT-WIDETAG" "DOUBLE-FLOAT-VALUE-SLOT"
           "DOUBLE-REG-SC-NUMBER"
           "DOUBLE-STACK-SC-NUMBER"
           "DSD-INDEX-SHIFT"
           "DSD-RAW-TYPE-MASK"
           "EMIT-LONG-NOP" "ERROR-TRAP" "EVEN-FIXNUM-LOWTAG"
           "FDEFN-FUN-SLOT" "FDEFN-NAME-SLOT" "FDEFN-RAW-ADDR-SLOT"
           "FDEFN-SIZE" "FDEFN-WIDETAG"
           "FILLER-WIDETAG"
           "FIXNUMIZE"
           "FIXNUM-TAG-MASK"
           "FIXUP-CODE-OBJECT" "FLOAT-DENORMAL-TRAP-BIT"
           "FLOAT-DIVIDE-BY-ZERO-TRAP-BIT"
           "FLOAT-INVALID-TRAP-BIT"
           "FLOAT-OVERFLOW-TRAP-BIT" "FLOAT-SIGN-SHIFT"
           "FLOAT-UNDERFLOW-TRAP-BIT" "FLOATING-POINT-MODES"
           "FLOAT-STICKY-BITS"
           "FLOAT-TRAPS-BYTE"
           "FP-CONSTANT-SC-NUMBER"
           "FP-DOUBLE-ZERO-SC-NUMBER" "FP-SINGLE-ZERO-SC-NUMBER"
           "FUNCALLABLE-INSTANCE-TRAMPOLINE-SLOT"
           "FUNCALLABLE-INSTANCE-WIDETAG"
           "FUNCALLABLE-INSTANCE-INFO-OFFSET"
           "SIMPLE-FUN-ARGLIST-SLOT" "SIMPLE-FUN-INSTS-OFFSET"
           "FUN-END-BREAKPOINT-TRAP"
           "SIMPLE-FUN-WIDETAG"
           "SIMPLE-FUN-NAME-SLOT"
           "SIMPLE-FUN-ENTRY-SAP"
           "FUN-POINTER-LOWTAG"
           "FUNCTION-LAYOUT"
           "SIMPLE-FUN-INFO-SLOT"
           "SIMPLE-FUN-SELF-SLOT"
           "SIMPLE-FUN-SOURCE-SLOT"
           "GENCGC-CARD-BYTES"
           "GENCGC-PAGE-BYTES"
           "GENCGC-ALLOC-GRANULARITY"
           "GENCGC-RELEASE-GRANULARITY"
           #+(or arm64 ppc ppc64 sparc riscv) "PSEUDO-ATOMIC-INTERRUPTED-FLAG"
           #+(or arm64 ppc ppc64 sparc riscv) "PSEUDO-ATOMIC-FLAG"
           #+sb-safepoint "GLOBAL-SAFEPOINT-TRAP"
           "HALT-TRAP" "IGNORE-ME-SC-NUMBER"
           "IMMEDIATE-SC-NUMBER"
           "CANONICALIZE-INLINE-CONSTANT"
           "INLINE-CONSTANT-VALUE"
           "SORT-INLINE-CONSTANTS"
           "EMIT-INLINE-CONSTANT"
           "HEXDUMP"
           "INSTANCE-DATA-START"
           "INSTANCE-WIDETAG" "INSTANCE-POINTER-LOWTAG"
           "INSTANCE-SLOTS-OFFSET" "INSTANCE-USAGE"
           "INTERIOR-REG-SC-NUMBER" "INTERNAL-ERROR-ARGS"
           "IS-LISP-POINTER"
           #+gencgc "LARGE-OBJECT-SIZE"
           "LAYOUT"
           "LIST-ALLOCATED-OBJECTS" "LIST-POINTER-LOWTAG"
           ;; FIXME: Possibly these other parameters (see
           ;; compiler/{x86,sparc}/parms.lisp) should be defined
           ;; conditionally on #+LONG-FLOAT)

           "LONG-FLOAT-BIAS" "LONG-FLOAT-DIGITS" "LONG-FLOAT-EXPONENT-BYTE"
           "LONG-FLOAT-HIDDEN-BIT" "LONG-FLOAT-NORMAL-EXPONENT-MAX"
           "LONG-FLOAT-NORMAL-EXPONENT-MIN" "LONG-FLOAT-SIGNIFICAND-BYTE"
           #+long-float "LONG-FLOAT-SIZE"
           "LONG-FLOAT-TRAPPING-NAN-BIT"
           #+long-float "LONG-FLOAT-WIDETAG"
           #+long-float "LONG-FLOAT-VALUE-SLOT"
           #+long-float "LONG-REG-SC-NUMBER"
           #+long-float "LONG-STACK-SC-NUMBER"
           "LOWTAG-LIMIT" "LOWTAG-MASK"
           "LRA-SAVE-OFFSET"
           "MAP-ALLOCATED-OBJECTS"
           "MAP-CODE-OBJECTS"
           "MAX-INTERRUPTS"
           #+c-stack-is-control-stack "MEMORY-FAULT-EMULATION-TRAP"
           "UNINITIALIZED-LOAD-TRAP"
           "METASPACE-SLAB-SIZE"
           "MEMORY-USAGE"
           "N-LOWTAG-BITS"
           "N-FIXNUM-TAG-BITS"
           "N-FIXNUM-BITS"
           "N-POSITIVE-FIXNUM-BITS"
           "NIL-VALUE"
           "NFP-SAVE-OFFSET"
           "NON-DESCRIPTOR-REG-SC-NUMBER"
           "NULL-SC-NUMBER"
           "OCFP-SAVE-OFFSET"
           "ODD-FIXNUM-LOWTAG"
           "OTHER-IMMEDIATE-0-LOWTAG"
           "OTHER-IMMEDIATE-1-LOWTAG"
           "OTHER-IMMEDIATE-2-LOWTAG"
           "OTHER-IMMEDIATE-3-LOWTAG"
           "OTHER-POINTER-LOWTAG"
           "PAD0-LOWTAG" "PAD1-LOWTAG" "PAD2-LOWTAG"
           "PAD3-LOWTAG" "PAD4-LOWTAG" "PAD5-LOWTAG"
           "PAD-DATA-BLOCK" "PENDING-INTERRUPT-TRAP"
           "PRIMITIVE-OBJECT" "PRIMITIVE-OBJECT-WIDETAG"
           "PRIMITIVE-OBJECT-LOWTAG" "PRIMITIVE-OBJECT-NAME"
           "PRIMITIVE-OBJECT-P"
           "PRIMITIVE-OBJECT-LENGTH" "PRIMITIVE-OBJECT-SLOTS"
           "PRIMITIVE-OBJECT-VARIABLE-LENGTH-P"
           "PRINT-ALLOCATED-OBJECTS"
           "RATIO-DENOMINATOR-SLOT" "RATIO-NUMERATOR-SLOT"
           "RATIO-SIZE" "RATIO-WIDETAG"
           "*READ-ONLY-SPACE-FREE-POINTER*"
           "RETURN-PC-WIDETAG"
           "RETURN-PC-RETURN-POINT-OFFSET" "RETURN-PC-SAVE-OFFSET"
           "SAETP-CTYPE" "SAETP-INITIAL-ELEMENT-DEFAULT"
           "SAETP-N-BITS" "SAETP-TYPECODE" "SAETP-PRIMITIVE-TYPE-NAME"
           "SAETP-N-PAD-ELEMENTS" "SAETP-N-BITS-SHIFT"
           "SAETP-SPECIFIER"
           "SAETP-COMPLEX-TYPECODE"
           "SAETP-FIXNUM-P"
           "VALID-BIT-BASH-SAETP-P"
           "*SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES*"
           "SANCTIFY-FOR-EXECUTION"
           "SAP-POINTER-SLOT" "SAP-REG-SC-NUMBER" "SAP-SIZE"
           "SAP-STACK-SC-NUMBER" "SAP-WIDETAG"
           "SC-NUMBER-LIMIT" "SC-NUMBER-BITS" ; SC-NUMBER in package "SB-C"

           "SC-OFFSET-LIMIT" "SC-OFFSET-BITS" "SC-OFFSET"
           "FINITE-SC-OFFSET-LIMIT" "FINITE-SC-OFFSET-BITS" "FINITE-SC-OFFSET"
           "FINITE-SC-OFFSET-MAP"
           "SHORT-HEADER-MAX-WORDS"
           "SIGFPE-HANDLER" "SIGNED-REG-SC-NUMBER" "SIGNED-STACK-SC-NUMBER"
           "SIGNED-WORD"
           "SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-WIDETAG"
           #+long-float "SIMPLE-ARRAY-COMPLEX-LONG-FLOAT-WIDETAG"
           "SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-WIDETAG"
           "SIMPLE-ARRAY-DOUBLE-FLOAT-WIDETAG"
           #+long-float "SIMPLE-ARRAY-LONG-FLOAT-WIDETAG"
           "SIMPLE-ARRAY-NIL-WIDETAG"
           "SIMPLE-ARRAY-SINGLE-FLOAT-WIDETAG"
           "SIMPLE-ARRAY-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-15-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-16-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-2-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-FIXNUM-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-31-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-32-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-63-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-64-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-4-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-7-WIDETAG"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-8-WIDETAG"
           "SIMPLE-ARRAY-SIGNED-BYTE-16-WIDETAG"
           "SIMPLE-ARRAY-FIXNUM-WIDETAG"
           "SIMPLE-ARRAY-SIGNED-BYTE-32-WIDETAG"
           "SIMPLE-ARRAY-SIGNED-BYTE-64-WIDETAG"
           "SIMPLE-ARRAY-SIGNED-BYTE-8-WIDETAG"
           "SIMPLE-BIT-VECTOR-WIDETAG"
           "SIMPLE-BASE-STRING-WIDETAG"
           #+sb-unicode "SIMPLE-CHARACTER-STRING-WIDETAG"
           "SIMPLE-VECTOR-WIDETAG" "SINGLE-FLOAT-BIAS"
           "SINGLE-FLOAT-DIGITS" "SINGLE-FLOAT-EXPONENT-BYTE"
           "SINGLE-FLOAT-HIDDEN-BIT" "SINGLE-FLOAT-NORMAL-EXPONENT-MAX"
           "SINGLE-FLOAT-NORMAL-EXPONENT-MIN" "SINGLE-FLOAT-SIGNIFICAND-BYTE"
           "SINGLE-FLOAT-SIZE"
           "SINGLE-FLOAT-WIDETAG"
           #-64-bit "SINGLE-FLOAT-VALUE-SLOT"
           "SINGLE-REG-SC-NUMBER" "SINGLE-STACK-SC-NUMBER"
           "SINGLE-STEP-AROUND-TRAP"
           "SINGLE-STEP-BEFORE-TRAP"
           "SINGLE-STEP-BREAKPOINT-TRAP"
           "INVALID-ARG-COUNT-TRAP"
           "SINGLE-VALUE-RETURN-BYTE-OFFSET"
           "SLOT-NAME" "SLOT-OFFSET"
           "SLOT-SPECIAL"
           "+STATIC-FDEFNS+" "STATIC-FUN-OFFSET"
           "%SPACE-BOUNDS" "SPACE-BYTES"
           "STABLE-HASH-REQUIRED-FLAG"
           "HASH-SLOT-PRESENT-FLAG"
           "STATIC-SYMBOL-OFFSET" "STATIC-SYMBOL-P"
           "+STATIC-SYMBOLS+"
           "SYMBOL-HASH-SLOT" "SYMBOL-WIDETAG" "SYMBOL-NAME-SLOT"
           "SYMBOL-PACKAGE-ID-SLOT" "SYMBOL-INFO-SLOT" "SYMBOL-FDEFN-SLOT"
           "SYMBOL-SIZE" "SYMBOL-VALUE-SLOT" "SYMBOL-TLS-INDEX-SLOT"
           "AUGMENTED-SYMBOL-SIZE"
           "*BINDING-STACK-START*"
           "*CONTROL-STACK-START*" "*CONTROL-STACK-END*"
           "CONTROL-STACK-POINTER-VALID-P"
           "DYNAMIC-SPACE-START" "DYNAMIC-SPACE-END"
           #+gencgc "MAX-DYNAMIC-SPACE-END"
           #+gencgc "PAGE-TABLE"
           #+gencgc "FIND-PAGE-INDEX"
           #+gencgc "NEXT-FREE-PAGE"
           #-gencgc "DYNAMIC-0-SPACE-START"
           #-gencgc "DYNAMIC-0-SPACE-END"
           "READ-ONLY-SPACE-START" "READ-ONLY-SPACE-END"
           "STATIC-SPACE-START" "STATIC-SPACE-END" "*STATIC-SPACE-FREE-POINTER*"
           "STATIC-CODE-SPACE-START" "STATIC-CODE-SPACE-END" "*STATIC-CODE-SPACE-FREE-POINTER*"
           "LINKAGE-TABLE-SPACE-START"
           "LINKAGE-TABLE-SPACE-END"
           "LINKAGE-TABLE-ENTRY-SIZE"
           #+sb-safepoint "GC-SAFEPOINT-PAGE-ADDR"
           #+sb-safepoint "GC-SAFEPOINT-TRAP-OFFSET"
           "THREAD-STATE-WORD-SLOT"
           "THREAD-SPROF-DATA-SLOT"
           "TLS-SIZE" "N-WIDETAG-BITS" "WIDETAG-MASK"
           "INSTANCE-LENGTH-SHIFT"
           "INSTANCE-LENGTH-MASK"
           "UNBOUND-MARKER-WIDETAG"
           "UNDEFINED-FUNCTION-TRAP"
           "NO-TLS-VALUE-MARKER-WIDETAG"
           "UNSIGNED-REG-SC-NUMBER" "UNSIGNED-STACK-SC-NUMBER"
           "UNWIND-BLOCK-CODE-SLOT" "UNWIND-BLOCK-CFP-SLOT"
           "UNWIND-BLOCK-UWP-SLOT" "UNWIND-BLOCK-ENTRY-PC-SLOT"
           "UNWIND-BLOCK-SIZE" "VALUE-CELL-WIDETAG" "VALUE-CELL-SIZE"
           "VALUE-CELL-VALUE-SLOT" "VECTOR-DATA-OFFSET" "VECTOR-LENGTH-SLOT"
           "VECTOR-WEAK-FLAG"
           "VECTOR-WEAK-VISITED-FLAG"
           "VECTOR-HASHING-FLAG"
           "VECTOR-ADDR-HASHING-FLAG"
           #+(and win32 x86-64) "WIN64-SEH-DATA-ADDR"
           "WEAK-POINTER-NEXT-SLOT"
           "WEAK-POINTER-SIZE" "WEAK-POINTER-WIDETAG"
           "WEAK-POINTER-VALUE-SLOT"
           "N-WORD-BITS" "N-WORD-BYTES" "N-MACHINE-WORD-BITS" "N-MACHINE-WORD-BYTES"
           "WORD-REG-SC-NUMBER" "WORD-SHIFT"
           #+win32 "CONTEXT-RESTORE-TRAP"
           "ZERO-SC-NUMBER")
  #+immobile-space
  (:export
   "IMMOBILE-CARD-BYTES"
   "FIXEDOBJ-SPACE-START"
   "FIXEDOBJ-SPACE-SIZE"
   "VARYOBJ-SPACE-START"
   "VARYOBJ-SPACE-SIZE"
   "*FIXEDOBJ-SPACE-FREE-POINTER*"
   "*VARYOBJ-SPACE-FREE-POINTER*")
  #+sb-simd-pack
  (:export
   "SIMD-PACK-TAG-SLOT"
   "SIMD-PACK-HI-VALUE-SLOT"
   "SIMD-PACK-LO-VALUE-SLOT"
   "SIMD-PACK-SIZE"
   "SIMD-PACK-WIDETAG")
  #+sb-simd-pack-256
  (:export
   "SIMD-PACK-256-TAG-SLOT"
   "SIMD-PACK-256-P0-SLOT"
   "SIMD-PACK-256-P1-SLOT"
   "SIMD-PACK-256-P2-SLOT"
   "SIMD-PACK-256-P3-SLOT"
   "SIMD-PACK-256-SIZE"
   "SIMD-PACK-256-WIDETAG"))

(defpackage* "SB-DISASSEM"
  (:documentation "private: stuff related to the implementation of the disassembler")
  (:use "CL" "SB-EXT" "SB-INT" "SB-SYS" "SB-KERNEL" "SB-DI")
  (:export "*DISASSEM-NOTE-COLUMN*" "*DISASSEM-OPCODE-COLUMN-WIDTH*"
           "*DISASSEM-LOCATION-COLUMN-WIDTH*"
           "ALIGN" ;;  prevent it from being removed, older Slime versions are using it

           "ARG-VALUE" "DISASSEM-STATE"
           "DISASSEMBLE-CODE-COMPONENT"
           "DISASSEMBLE-FUN" "DISASSEMBLE-MEMORY"
           "DISASSEMBLE-INSTRUCTION"
           "DISASSEMBLE-SEGMENT" "DISASSEMBLE-SEGMENTS"
           "DSTATE-BYTE-ORDER"
           "DSTATE-GETPROP"
           "DSTATE-SETPROP"
           "DSTATE-SEGMENT-SAP"
           "DSTATE-OPERANDS"
           "FIND-INST"
           "GET-CODE-SEGMENTS" "GET-FUN-SEGMENTS"
           "GET-INST-SPACE" "HANDLE-BREAK-ARGS"
           "LABEL-SEGMENTS"
           "MAYBE-NOTE-ASSEMBLER-ROUTINE"
           "MAYBE-NOTE-ASSOCIATED-STORAGE-REF"
           "MAYBE-NOTE-NIL-INDEXED-OBJECT"
           "MAYBE-NOTE-NIL-INDEXED-SYMBOL-SLOT-REF"
           "MAYBE-NOTE-SINGLE-STORAGE-REF"
           "MAYBE-NOTE-STATIC-SYMBOL"
           "NOTE"
           "NOTE-CODE-CONSTANT"
           "OPERAND"
           "PRIN1-QUOTED-SHORT"
           "PRIN1-SHORT" "PRINT-BYTES"
           "PRINT-CURRENT-ADDRESS" "PRINT-INST"
           "PRINT-NOTES-AND-NEWLINE"
           "SAP-REF-INT"
           "SEG-CODE" "SEG-LENGTH" "SEGMENT"
           "SIGN-EXTEND"
           "MAKE-DSTATE"
           "DEFINE-ARG-TYPE"
           "READ-SIGNED-SUFFIX"
           "MAKE-MEMORY-SEGMENT"
           "MAKE-SEGMENT" "SEG-VIRTUAL-LOCATION"
           "DCHUNK" "DCHUNK-ZERO" "*DEFAULT-DSTATE-HOOKS*"
           "MAKE-CODE-SEGMENT" "MAKE-OFFS-HOOK"
           "DSTATE-SEGMENT" "DSTATE-CUR-OFFS"
           "PRINC16" "INSTRUCTION" "DEFINE-INSTRUCTION-FORMAT"
           "DSTATE-NEXT-OFFS"
           "SEG-SAP-MAKER" "DISASSEMBLE-ASSEM-SEGMENT"
           "READ-SUFFIX"
           "MAP-SEGMENT-INSTRUCTIONS"
           "SET-LOCATION-PRINTING-RANGE" "MAKE-VECTOR-SEGMENT"
           "DSTATE-CUR-ADDR" "DSTATE-NEXT-ADDR"
           "SNARF-ERROR-JUNK"))

(defpackage* "SB-DEBUG"
  (:documentation
   "sorta public: Eventually this should become the debugger interface, with
basic stuff like BACKTRACE and ARG. For now, the actual supported interface
is still mixed indiscriminately with low-level internal implementation stuff
like *STACK-TOP-HINT* and unsupported stuff like *TRACED-FUN-LIST*.")
  (:use "CL" "SB-EXT" "SB-INT" "SB-SYS" "SB-KERNEL" "SB-DI")
  (:reexport "*DEBUG-PRINT-VARIABLE-ALIST*")
  (:export "*BACKTRACE-FRAME-COUNT*"
           "*DEBUG-BEGINNER-HELP-P*"
           "*DEBUG-CONDITION*"
           "*DEBUG-READTABLE*" "*DEBUG-HELP-STRING*"
           "*FLUSH-DEBUG-ERRORS*" "*IN-THE-DEBUGGER*"
           "*METHOD-FRAME-STYLE*"
           "*TRACE-INDENTATION-STEP*" "*MAX-TRACE-INDENTATION*"
           "ARG"
           "INTERNAL-DEBUG" "VAR"
           "*STACK-TOP-HINT*"
           "*TRACE-ENCAPSULATE-DEFAULT*"
           "FRAME-HAS-DEBUG-TAG-P"
           "UNWIND-TO-FRAME-AND-CALL"
           ;; Deprecated

           "BACKTRACE" "BACKTRACE-AS-LIST"
           ;; Replaced by

           "PRINT-BACKTRACE" "LIST-BACKTRACE"))

(defpackage* "SB-EXT"
  (:documentation "public: miscellaneous supported extensions to the ANSI Lisp spec")
  (:use "CL" "SB-ALIEN" "SB-INT" "SB-SYS" "SB-GRAY")
  (:export
   ;; Information about how the program was invoked is
   ;; nonstandard but very useful.
   "*POSIX-ARGV*" "*CORE-PATHNAME*" "*RUNTIME-PATHNAME*"
   "POSIX-GETENV" "POSIX-ENVIRON"

   ;; Customizing initfile locations

   "*USERINIT-PATHNAME-FUNCTION*"
   "*SYSINIT-PATHNAME-FUNCTION*"

   "*DEFAULT-EXTERNAL-FORMAT*"
   "*DEFAULT-C-STRING-EXTERNAL-FORMAT*"

   ;; Compare and Swap support

   "CAS"
   "COMPARE-AND-SWAP"
   "DEFCAS"
   "GET-CAS-EXPANSION"

   ;; Other atomic operations and types related to them

   "ATOMIC-INCF"
   "ATOMIC-DECF"
   "ATOMIC-UPDATE"
   "ATOMIC-PUSH"
   "ATOMIC-POP"
   "WORD"
   "MOST-POSITIVE-WORD"

   ;; Not an atomic operation, but should be used with them

   "SPIN-LOOP-HINT"

   ;; Waiting for arbitrary events.

   "WAIT-FOR"

   ;; Time related things

   "CALL-WITH-TIMING"
   "GET-TIME-OF-DAY"

   ;; People have various good reasons to mess with the GC.

   "HEAP-ALLOCATED-P"
   "STACK-ALLOCATED-P"
   "*AFTER-GC-HOOKS*"
   "BYTES-CONSED-BETWEEN-GCS"
   "GC" "GET-BYTES-CONSED"
   "*GC-RUN-TIME*"
   "PURIFY"
   "DYNAMIC-SPACE-SIZE"
   ;; Gencgc only, but symbols exist for manual building
   ;; convenience on all platforms.

   "GENERATION-AVERAGE-AGE"
   "GENERATION-BYTES-ALLOCATED"
   "GENERATION-BYTES-CONSED-BETWEEN-GCS"
   "GENERATION-MINIMUM-AGE-BEFORE-GC"
   "GENERATION-NUMBER-OF-GCS"
   "GENERATION-NUMBER-OF-GCS-BEFORE-PROMOTION"
   "GC-LOGFILE"

   ;; Stack allocation control

   "*STACK-ALLOCATE-DYNAMIC-EXTENT*"

   ;; Customizing printing of compiler and debugger messages

   "*COMPILER-PRINT-VARIABLE-ALIST*"
   "*DEBUG-PRINT-VARIABLE-ALIST*"
   "COMPILE-FILE-LINE"
   "COMPILE-FILE-POSITION"

   ;; Hooks into init & save sequences

   "*INIT-HOOKS*" "*SAVE-HOOKS*" "*EXIT-HOOKS*"
   "*FORCIBLY-TERMINATE-THREADS-ON-EXIT*"

   ;; Controlling exiting other threads.

   "*EXIT-TIMEOUT*"

   ;; There is no one right way to report progress on
   ;; hairy compiles.

   "*COMPILE-PROGRESS*"

   ;; The default behavior for block compilation.

   "*BLOCK-COMPILE-DEFAULT*"

   ;; It can be handy to be able to evaluate expressions involving
   ;; the thing under examination by CL:INSPECT.

   "*INSPECTED*"

   ;; There is no one right way to do efficiency notes.

   "*EFFICIENCY-NOTE-COST-THRESHOLD*" "*EFFICIENCY-NOTE-LIMIT*"

   ;; There's no one right way to report errors.

   "*ENCLOSING-SOURCE-CUTOFF*"
   "*UNDEFINED-WARNING-LIMIT*"

   ;; and for dedicated users who really want to customize
   ;; error reporting, we have

   "DEFINE-SOURCE-CONTEXT"
   "WITH-CURRENT-SOURCE-FORM"

   ;; and given how many users dislike strict treatment of
   ;; DEFCONSTANT, let's give them enough rope to escape by

   "DEFCONSTANT-UNEQL" "DEFCONSTANT-UNEQL-NAME"
   "DEFCONSTANT-UNEQL-NEW-VALUE" "DEFCONSTANT-UNEQL-OLD-VALUE"

   ;; global lexicals, access to global symbol values

   "DEFGLOBAL"
   "DEFINE-LOAD-TIME-GLOBAL"
   "SYMBOL-GLOBAL-VALUE"

   "FILE-EXISTS"
   "FILE-DOES-NOT-EXIST"
   "DELETE-FILE-ERROR"
   "SUPERSEDE"
   "OVERWRITE"
   "RENAME"
   "CREATE"
   "RETRY"

   ;; package extensions
   ;;
   ;; locks

   "PACKAGE-LOCKED-P"
   "LOCK-PACKAGE"
   "UNLOCK-PACKAGE"
   "PACKAGE-IMPLEMENTED-BY-LIST"
   "PACKAGE-IMPLEMENTS-LIST"
   "ADD-IMPLEMENTATION-PACKAGE"
   "REMOVE-IMPLEMENTATION-PACKAGE"
   "WITH-UNLOCKED-PACKAGES"
   "PACKAGE-LOCK-VIOLATION"
   "PACKAGE-LOCKED-ERROR"
   "SYMBOL-PACKAGE-LOCKED-ERROR"
   "PACKAGE-LOCKED-ERROR-SYMBOL"
   "WITHOUT-PACKAGE-LOCKS"
   "DISABLE-PACKAGE-LOCKS"
   "ENABLE-PACKAGE-LOCKS"
   ;; local nicknames

   "ADD-PACKAGE-LOCAL-NICKNAME"
   "REMOVE-PACKAGE-LOCAL-NICKNAME"
   "PACKAGE-LOCAL-NICKNAMES"
   "PACKAGE-LOCALLY-NICKNAMED-BY-LIST"

   "PACKAGE-DOES-NOT-EXIST" "READER-PACKAGE-DOES-NOT-EXIST"
   ;; behaviour on DEFPACKAGE* variance

   "*ON-PACKAGE-VARIANCE*"

   ;; Custom conditions & condition accessors for users to handle.

   "CODE-DELETION-NOTE"
   "COMPILER-NOTE"
   "IMPLICIT-GENERIC-FUNCTION-NAME"
   "IMPLICIT-GENERIC-FUNCTION-WARNING"
   "INVALID-FASL"

   "NAME-CONFLICT" "NAME-CONFLICT-FUNCTION"
   "NAME-CONFLICT-DATUM" "NAME-CONFLICT-SYMBOLS"
   "RESOLVE-CONFLICT"

   ;; Deprecation stuff

   "DEPRECATED"              ; declaration

   "DEPRECATION-CONDITION"
   "DEPRECATION-CONDITION-NAMESPACE"
   "DEPRECATION-CONDITION-NAME"
   "DEPRECATION-CONDITION-SOFTWARE"
   "DEPRECATION-CONDITION-VERSION"
   "DEPRECATION-CONDITION-REPLACEMENTS"
   "DEPRECATION-CONDITION-RUNTIME-ERROR"
   "EARLY-DEPRECATION-WARNING"
   "LATE-DEPRECATION-WARNING"
   "FINAL-DEPRECATION-WARNING"
   "DEPRECATION-ERROR"       ; condition and function


   "PRINT-UNREADABLY"

   ;; Readtable normalization control

   "READTABLE-BASE-CHAR-PREFERENCE"
   "READTABLE-NORMALIZATION"

   ;; and a mechanism for controlling same at compile time

   "MUFFLE-CONDITIONS" "UNMUFFLE-CONDITIONS"

   ;; and one for controlling same at runtime

   "*MUFFLED-WARNINGS*"

   ;; specification which print errors to ignore ala *break-on-signal*

   "*SUPPRESS-PRINT-ERRORS*"

   ;; extended declarations..

   "ALWAYS-BOUND" "FREEZE-TYPE" "GLOBAL" "INHIBIT-WARNINGS"
   "MAYBE-INLINE" "START-BLOCK" "END-BLOCK"

   ;; ..and variables to control compiler policy

   "*INLINE-EXPANSION-LIMIT*"
   "*DERIVE-FUNCTION-TYPES*"

   ;; ..and inspector of compiler policy

   "DESCRIBE-COMPILER-POLICY"
   "RESTRICT-COMPILER-POLICY"
   "SET-MACRO-POLICY"
   "FOLD-IDENTICAL-CODE" ; this is a verb, not a compiler policy

   ;; a special form for breaking out of our "declarations
   ;; are assertions" default

   "TRULY-THE"

   ;; Misc. array and vector tools.

   "ARRAY-STORAGE-VECTOR"
   "PRIMITIVE-OBJECT-SIZE"

   ;; This is something which must exist inside any Common
   ;; Lisp implementation, and which someone writing a
   ;; customized toplevel might well want. It seems perverse
   ;; to hide it from them..

   "INTERACTIVE-EVAL"

   ;; Used by LOAD and EVAL-WHEN to pass toplevel indexes
   ;; to compiler.

   "EVAL-TLF"

   ;; weak pointers and finalization

   "CANCEL-FINALIZATION"
   "FINALIZE"
   "MAKE-WEAK-POINTER"
   "WEAK-POINTER"
   "WEAK-POINTER-P"
   "WEAK-POINTER-VALUE"
   "MAKE-WEAK-VECTOR"
   "WEAK-VECTOR-P"

   ;; Hash table extensions

   "DEFINE-HASH-TABLE-TEST"
   "HASH-TABLE-SYNCHRONIZED-P"
   "HASH-TABLE-WEAKNESS"
   "WITH-LOCKED-HASH-TABLE"

   ;; If the user knows we're doing IEEE, he might reasonably
   ;; want to do this stuff.

   "FLOAT-DENORMALIZED-P"
   "FLOAT-NAN-P" "FLOAT-TRAPPING-NAN-P"
   "FLOAT-INFINITY-P"
   "SHORT-FLOAT-NEGATIVE-INFINITY"
   "SHORT-FLOAT-POSITIVE-INFINITY"
   "SINGLE-FLOAT-NEGATIVE-INFINITY"
   "SINGLE-FLOAT-POSITIVE-INFINITY"
   "DOUBLE-FLOAT-NEGATIVE-INFINITY"
   "DOUBLE-FLOAT-POSITIVE-INFINITY"
   "LONG-FLOAT-NEGATIVE-INFINITY"
   "LONG-FLOAT-POSITIVE-INFINITY"

   ;; saving Lisp images

   "SAVE-LISP-AND-DIE"

   ;; provided for completeness to make it more convenient
   ;; to use command-line --disable-debugger functionality
   ;; in oddball situations (like building core files using
   ;; scripts which run unattended, when the core files are
   ;; intended for interactive use)

   "DISABLE-DEBUGGER"
   "ENABLE-DEBUGGER"

   ;; the mechanism by which {en,dis}able-debugger works is
   ;; also exported for people writing alternative toplevels
   ;; (Emacs, CLIM interfaces, etc)

   "*INVOKE-DEBUGGER-HOOK*"

   ;; miscellaneous useful supported extensions

   "QUIT" "EXIT"
   "*ED-FUNCTIONS*"
   "*MODULE-PROVIDER-FUNCTIONS*"
   "MAP-DIRECTORY"
   "WITH-TIMEOUT" "TIMEOUT"
   "SEED-RANDOM-STATE"
   "TYPEXPAND-1" "TYPEXPAND" "TYPEXPAND-ALL"
   "DEFINED-TYPE-NAME-P" "VALID-TYPE-SPECIFIER-P"
   "DELETE-DIRECTORY"
   "SET-SBCL-SOURCE-LOCATION"
   "*DISASSEMBLE-ANNOTATE*"
   "PRINT-SYMBOL-WITH-PREFIX"
   "*PRINT-VECTOR-LENGTH*"
   "DECIMAL-WITH-GROUPED-DIGITS-WIDTH"
   ;;"OBJECT-SIZE"

   ;; stepping interface

   "STEP-CONDITION" "STEP-FORM-CONDITION" "STEP-FINISHED-CONDITION"
   "STEP-VALUES-CONDITION"
   "STEP-CONDITION-FORM" "STEP-CONDITION-RESULT"
   "STEP-CONTINUE" "STEP-NEXT" "STEP-INTO"
   "STEP-CONDITION-ARGS" "*STEPPER-HOOK*" "STEP-OUT"

   ;; RUN-PROGRAM is not only useful for users, but also
   ;; useful to implement parts of SBCL itself, so we're
   ;; going to have to implement it anyway, so we might
   ;; as well support it. And then once we're committed
   ;; to implementing RUN-PROGRAM, it's nice to have it
   ;; return a PROCESS object with operations defined on
   ;; that object.

   "RUN-PROGRAM"
   "PROCESS-ALIVE-P" "PROCESS-CLOSE"
   "PROCESS-CORE-DUMPED" "PROCESS-ERROR" "PROCESS-EXIT-CODE"
   "PROCESS-INPUT" "PROCESS-KILL" "PROCESS-OUTPUT" "PROCESS-P"
   "PROCESS-PID" "PROCESS-PLIST" "PROCESS-PTY" "PROCESS-STATUS"
   "PROCESS-STATUS-HOOK" "PROCESS-WAIT"

   ;; pathnames

   "NATIVE-PATHNAME"
   "PARSE-NATIVE-NAMESTRING"
   "NATIVE-NAMESTRING"

   ;; external-format support

   "OCTETS-TO-STRING" "STRING-TO-OCTETS"

   ;; Whether to use the interpreter or the compiler for EVAL

   "*EVALUATOR-MODE*"

   ;; timer

   "TIMER" "MAKE-TIMER" "TIMER-NAME" "TIMER-SCHEDULED-P"
   "SCHEDULE-TIMER" "UNSCHEDULE-TIMER" "LIST-ALL-TIMERS"

   ;; versioning utility

   "ASSERT-VERSION->="
   "UNKNOWN-KEYWORD-ARGUMENT"
   "UNKNOWN-KEYWORD-ARGUMENT-NAME")
  ;; SIMD pack
  #+sb-simd-pack
  (:export
   "SIMD-PACK"
   "SIMD-PACK-P"
   "%MAKE-SIMD-PACK-UB32"
   "%MAKE-SIMD-PACK-UB64"
   "%MAKE-SIMD-PACK-DOUBLE"
   "%MAKE-SIMD-PACK-SINGLE"
   "%SIMD-PACK-UB32S"
   "%SIMD-PACK-UB64S"
   "%SIMD-PACK-DOUBLES"
   "%SIMD-PACK-SINGLES")
  #+sb-simd-pack-256
  (:export
   "SIMD-PACK-256"
   "SIMD-PACK-256-P"
   "%MAKE-SIMD-PACK-256-UB32"
   "%MAKE-SIMD-PACK-256-UB64"
   "%MAKE-SIMD-PACK-256-DOUBLE"
   "%MAKE-SIMD-PACK-256-SINGLE"
   "%SIMD-PACK-256-UB32S"
   "%SIMD-PACK-256-UB64S"
   "%SIMD-PACK-256-DOUBLES"
   "%SIMD-PACK-256-SINGLES"))

(defpackage* "SB-DI"
  (:documentation "private: primitives used to write debuggers")
  (:use "CL" "SB-EXT" "SB-INT" "SB-KERNEL" "SB-SYS" "SB-VM")
  (:import-from "SB-C"
                "DEBUG-SOURCE-NAMESTRING"
                "DEBUG-SOURCE-CREATED"
                "MAKE-DEBUG-SOURCE"
                "DEBUG-SOURCE" "DEBUG-SOURCE-P"
                "CORE-DEBUG-SOURCE" "CORE-DEBUG-SOURCE-P"
                "CORE-DEBUG-SOURCE-FORM")
  (:reexport "DEBUG-SOURCE-NAMESTRING"
             "DEBUG-SOURCE-CREATED"
             "DEBUG-SOURCE" "DEBUG-SOURCE-P")
  (:export "ACTIVATE-BREAKPOINT"
           "AMBIGUOUS-DEBUG-VARS" "AMBIGUOUS-VAR-NAME" "BREAKPOINT"
           "BREAKPOINT-ACTIVE-P" "BREAKPOINT-HOOK-FUN" "BREAKPOINT-INFO"
           "BREAKPOINT-KIND" "BREAKPOINT-P" "BREAKPOINT-WHAT" "CODE-LOCATION"
           "CODE-LOCATION-DEBUG-BLOCK" "CODE-LOCATION-DEBUG-FUN"
           "CODE-LOCATION-DEBUG-SOURCE" "CODE-LOCATION-FORM-NUMBER"
           "CODE-LOCATION-P" "CODE-LOCATION-TOPLEVEL-FORM-OFFSET"
           "CODE-LOCATION-CONTEXT"
           "CODE-LOCATION-UNKNOWN-P" "CODE-LOCATION=" "DEACTIVATE-BREAKPOINT"
           "DEBUG-BLOCK" "DEBUG-BLOCK-ELSEWHERE-P" "DEBUG-BLOCK-P"
           "DEBUG-CONDITION" "DEBUG-ERROR"
           "DEBUG-FUN" "DEBUG-FUN-FUN" "DEBUG-FUN-KIND"
           "DEBUG-FUN-LAMBDA-LIST" "DEBUG-FUN-NAME" "DEBUG-FUN-CLOSURE-NAME"
           "DEBUG-FUN-P" "DEBUG-FUN-START-LOCATION"
           "DEBUG-FUN-SYMBOL-VARS"
           "DEBUG-VAR" "DEBUG-VAR-ID" "DEBUG-VAR-INFO-AVAILABLE"
           "DEBUG-VAR-SYMBOL-NAME" "DEBUG-VAR-P" "DEBUG-VAR-PACKAGE-NAME"
           "DEBUG-VAR-SYMBOL" "DEBUG-VAR-VALID-VALUE"
           "DEBUG-VAR-VALIDITY" "DEBUG-VAR-VALUE"
           "DELETE-BREAKPOINT"
           "DO-DEBUG-BLOCK-LOCATIONS" "DO-DEBUG-FUN-BLOCKS"
           "DO-DEBUG-FUN-VARS"
           "ERROR-CONTEXT"
           "FORM-NUMBER-TRANSLATIONS"
           "FRAME" "FRAME-CATCHES" "FRAME-CODE-LOCATION"
           "FRAME-DEBUG-FUN" "FRAME-DOWN"
           "FRAME-FUN-MISMATCH" "FRAME-NUMBER" "FRAME-P" "FRAME-UP"
           "GET-TOPLEVEL-FORM"
           "REPLACE-FRAME-CATCH-TAG"
           "FUN-DEBUG-FUN" "FUN-END-COOKIE-VALID-P"
           "INVALID-CONTROL-STACK-POINTER" "INVALID-VALUE"
           "LAMBDA-LIST-UNAVAILABLE" "MAKE-BREAKPOINT" "NO-DEBUG-BLOCKS"
           "NO-DEBUG-FUN-RETURNS" "PREPROCESS-FOR-EVAL"
           "EVAL-IN-FRAME" "RETURN-FROM-FRAME" "SOURCE-PATH-CONTEXT"
           "TOP-FRAME" "UNHANDLED-DEBUG-CONDITION"
           "UNKNOWN-DEBUG-VAR"
           "CODE-LOCATION-KIND" "FLUSH-FRAMES-ABOVE"))

#+sb-dyncount
(defpackage* "SB-DYNCOUNT"
  (:documentation "private: some somewhat-stale code for collecting runtime statistics")
  (:use "CL" "SB-ALIEN-INTERNALS" "SB-ALIEN" "SB-BIGNUM"
        "SB-EXT" "SB-INT" "SB-KERNEL" "SB-ASSEM" "SB-SYS")
  (:export "*COLLECT-DYNAMIC-STATISTICS*"
           "COUNT-ME"
           "DYNCOUNT-INFO-COUNTS" "DYNCOUNT-INFO-COSTS"
           "IR2-COMPONENT-DYNCOUNT-INFO"
           "DYNCOUNT-INFO" "DYNCOUNT-INFO-P"))

(defpackage* "SB-FASL"
  (:documentation "private: stuff related to FASL load/dump logic (and GENESIS)")
  (:use "CL" "SB-ALIEN" "SB-ASSEM" "SB-BIGNUM" "SB-C"
        "SB-EXT" "SB-INT" "SB-KERNEL" "SB-SYS")
  (:import-from "SB-VM" "+FIXUP-KINDS+")
  (:export "*ASSEMBLER-ROUTINES*"
           "GET-ASM-ROUTINE"
           "+BACKEND-FASL-FILE-IMPLEMENTATION+"
           "*FASL-FILE-TYPE*"
           "CLOSE-FASL-OUTPUT"
           "DUMP-ASSEMBLER-ROUTINES"
           "DUMP-FOP" "DUMP-OBJECT"
           "FASL-CONSTANT-ALREADY-DUMPED-P"
           "+FASL-FILE-VERSION+"
           "FASL-DUMP-COMPONENT"
           "FASL-DUMP-LOAD-TIME-VALUE-LAMBDA"
           "FASL-DUMP-TOPLEVEL-LAMBDA-CALL"
           "FASL-NOTE-HANDLE-FOR-CONSTANT"
           "FASL-OUTPUT" "FASL-OUTPUT-P"
           "FASL-OUTPUT-ENTRY-TABLE" "FASL-OUTPUT-STREAM"
           "FASL-NOTE-DUMPABLE-INSTANCE"
           "LOAD-FORM-IS-DEFAULT-MLFSS-P"
           "*!LOAD-TIME-VALUES*"
           "OPEN-FASL-OUTPUT"
           "*!COLD-DEFSYMBOLS*"
           "*!COLD-TOPLEVELS*"
           "COLD-CONS" "COLD-INTERN" "COLD-PUSH"))

(defpackage* "SB-C"
  (:documentation "private: implementation of the compiler")
  ;; (It seems strange to have the compiler USE SB-ALIEN-INTERNALS,
  ;; but the point seems to be to be able to express things like
  ;; SB-C:DEFTRANSFORM SB-ALIEN-INTERNALS:MAKE-LOCAL-ALIEN without
  ;; having to use a bunch of package prefixes, by putting them
  ;; in the SB-C package. Maybe it'd be tidier to define an SB-ALIEN-COMP
  ;; package for this? But it seems like a fairly low priority.)
  ;; (Probably the same considerations also explain why BIGNUM is
  ;;in the USE list.)
  (:use "CL" "SB-ALIEN-INTERNALS" "SB-ALIEN" "SB-ASSEM" "SB-BIGNUM"
        #+sb-dyncount "SB-DYNCOUNT" "SB-EXT" "SB-FASL" "SB-INT"
        "SB-KERNEL" "SB-SYS")
  ;; But why do we need SLOT re-exported?
  (:reexport "SLOT" "FLUSHABLE")
  (:export "%ALIEN-FUNCALL"
           "%CATCH-BREAKUP" "%CONTINUE-UNWIND" "%UNWIND"
           "%LISTIFY-REST-ARGS" "%MORE-ARG" "%MORE-ARG-VALUES"
           "%UNWIND-PROTECT-BREAKUP"

           "*BACKEND-BYTE-ORDER*"
           "+BACKEND-INTERNAL-ERRORS+" "+BACKEND-PAGE-BYTES+"
           "*BACKEND-REGISTER-SAVE-PENALTY*"
           "*BACKEND-SBS*"          ; storage bases

           "*BACKEND-SC-NAMES*" "*BACKEND-SC-NUMBERS*"
           "*BACKEND-SUBFEATURES*"
           "*BACKEND-T-PRIMITIVE-TYPE*"

           "*COMPILATION*"
           "*COMPILE-TO-MEMORY-SPACE*"
           "*LEXENV*"
           "*SUPPRESS-VALUES-DECLARATION*"

           #+x86 "SET-FPU-WORD-FOR-C"
           #+x86 "SET-FPU-WORD-FOR-LISP"
           "ALIGN-STACK-POINTER"
           "ALIEN-FUNCALL-SAVES-FP-AND-PC"
           "ALLOC-ALIEN-STACK-SPACE" "ALLOC-NUMBER-STACK-SPACE"
           "ALLOCATE-CODE-OBJECT" "ALLOCATE-FRAME"
           "ALLOCATE-FULL-CALL-FRAME"
           "ALWAYS-TRANSLATABLE"
           "ANCESTOR-FRAME-REF" "ANCESTOR-FRAME-SET"
           "ANY" "ASSEMBLE-FILE"
           "ATTRIBUTES" "ATTRIBUTES-INTERSECTION" "ATTRIBUTES-UNION"
           "ATTRIBUTES="
           "CALL" "CALL-LOCAL" "CALL-NAMED" "CALL-VARIABLE"
           "CALL-OUT" "CALL-OUT-NAMED"
           "CALLEE-NFP-TN" "CALLEE-RETURN-PC-TN"
           "CATCH-BLOCK" "UNWIND-BLOCK"
           "CLOSURE-INIT" "CLOSURE-REF" "CLOSURE-INIT-FROM-FP"
           "COMPARE-AND-SWAP-SLOT"
           "COMPILE-IN-LEXENV"
           "COMPILE-FILES"
           "COMPILED-DEBUG-FUN-FORM-NUMBER"
           "%COMPILER-DEFUN" "COMPILER-ERROR" "FATAL-COMPILER-ERROR"
           "COMPILER-NOTIFY"
           "COMPILER-STYLE-WARN" "COMPILER-WARN"
           "COMPONENT" "COMPONENT-HEADER-LENGTH"
           "COMPONENT-INFO" "COMPONENT-LIVE-TN" "COMPUTE-FUN"
           "COMPUTE-OLD-NFP" "COPY-MORE-ARG"
           "CURRENT-BINDING-POINTER" "CURRENT-NFP-TN"
           "CURRENT-STACK-POINTER"
           "*ALIEN-STACK-POINTER*"
           "CURRENT-NSP" "SET-NSP"
           "DEALLOC-NUMBER-STACK-SPACE"
           "DEBUG-CATCH-TAG"
           "DEF-IR1-TRANSLATOR"
           "!DEF-PRIMITIVE-TYPE" "!DEF-PRIMITIVE-TYPE-ALIAS"
           "DEFINE-SOURCE-TRANSFORM"
           "DEFINITION-SOURCE-LOCATION"
           "DEFINITION-SOURCE-LOCATION-NAMESTRING"
           "DEFINITION-SOURCE-LOCATION-TOPLEVEL-FORM-NUMBER"
           "DEFINITION-SOURCE-LOCATION-FORM-NUMBER"
           "DEFINITION-SOURCE-LOCATION-PLIST"
           "DEFINE-MODULAR-FUN"
           "DEFINE-MOVE-FUN"
           "DEFINE-MOVE-VOP" "!DEFINE-STORAGE-BASES"
           "!DEFINE-STORAGE-CLASS" "DEFINE-VOP"
           "DEFKNOWN" "DEFOPTIMIZER"
           "DEFTRANSFORM" "DERIVE-TYPE"
           "DIS"
           "DO-FORMS-FROM-INFO"
           "EMIT-BLOCK-HEADER"
           "ENVIRONMENT-DEBUG-LIVE-TN" "ENVIRONMENT-LIVE-TN"
           "FAST-SYMBOL-VALUE"
           "FAST-SYMBOL-GLOBAL-VALUE"
           "FIXUP-NOTE-KIND"
           "FIXUP-NOTE-FIXUP"
           "FIXUP-NOTE-POSITION"
           "FOLDABLE"
           "FORCE-TN-TO-STACK"
           "FUN-INFO-DERIVE-TYPE" "FUN-INFO-IR2-CONVERT"
           "FUN-INFO-LTN-ANNOTATE" "FUN-INFO-OPTIMIZER"
           "GET-TOPLEVELISH-FILE-INFO"
           "HALT"
           "IF-EQ"
           "CONSTANT-LVAR-P"
           "CONSTANT-TN-P"
           "INSERT-SAFEPOINTS"
           "COMPILER-MACRO-APPLICATION-MISSED-WARNING"
           "INLINING-DEPENDENCY-FAILURE"
           "INSERT-ARRAY-BOUNDS-CHECKS"
           "INSERT-STEP-CONDITIONS"
           "INSTRUMENT-CONSING"
           "IR2-COMPONENT-CONSTANTS" "IR2-CONVERT"
           "IR2-ENVIRONMENT-NUMBER-STACK-P"
           "KNOWN-CALL-LOCAL" "KNOWN-RETURN"
           "LAMBDA-VAR-IGNOREP"
           "LAMBDA-WITH-LEXENV" "LEXENV-FIND"
           "LOCATION=" "LTN-ANNOTATE"
           "LVAR-VALUE"
           "MACRO-POLICY-DECLS"
           "MAKE-ALIAS-TN" "MAKE-CATCH-BLOCK"
           "MAKE-CLOSURE" "MAKE-CONSTANT-TN"
           "MAKE-FIXUP-NOTE"
           "MAKE-LOAD-TIME-CONSTANT-TN" "MAKE-N-TNS" "MAKE-NORMAL-TN"
           "MAKE-RANDOM-TN"
           "MAKE-REPRESENTATION-TN" "MAKE-RESTRICTED-TN"
           "MAKE-STACK-POINTER-TN" "MAKE-TN-REF" "MAKE-UNWIND-BLOCK"
           "MAKE-WIRED-TN" "MAYBE-COMPILER-NOTIFY"
           "MAYBE-INLINE-SYNTACTIC-CLOSURE"
           "MSAN-UNPOISON"
           "MOVABLE" "MOVE" "MULTIPLE-CALL"
           "MULTIPLE-CALL-LOCAL" "MULTIPLE-CALL-NAMED"
           "MULTIPLE-CALL-VARIABLE"
           "%%NIP-VALUES"
           "NLX-ENTRY" "NLX-ENTRY-MULTIPLE" "NLX-ENTRY-SINGLE"
           "NODE-STACK-ALLOCATE-P"
           "NON-DESCRIPTOR-STACK" "NOTE-ENVIRONMENT-START"
           "NOTE-THIS-LOCATION" "*LOCATION-CONTEXT*"
           "MAKE-RESTART-LOCATION"
           "OPTIMIZER"
           "PACK-CODE-FIXUP-LOCS"
           "PARSE-EVAL-WHEN-SITUATIONS"
           "POLICY"
           "PRIMITIVE-TYPE" "PRIMITIVE-TYPE-OF"
           "PRIMITIVE-TYPE-OR-LOSE"
           "PRIMITIVE-TYPE-NAME"
           "PRIMITIVE-TYPE-INDIRECT-CELL-TYPE"
           "PROCLAIM-FTYPE" "PROCLAIM-TYPE"
           "PUSH-VALUES"
           "READ-PACKED-BIT-VECTOR"
           "READ-VAR-INTEGER" "READ-VAR-INTEGERF"
           "SAP-READ-VAR-INTEGER" "SAP-READ-VAR-INTEGERF"
           "READ-VAR-STRING"
           "REFERENCE-TN" "REFERENCE-TN-LIST"
           "REGISTER-INLINE-CONSTANT"
           "RESET-STACK-POINTER" "RESTORE-DYNAMIC-STATE"
           "RETURN-MULTIPLE" "SAVE-DYNAMIC-STATE" "STORAGE-BASE"
           "SB-ALLOCATED-SIZE" "SB-NAME" "SB-OR-LOSE"
           "STORAGE-CLASS" "SC-CASE" "SC-OPERAND-SIZE"
           "SC-IS" "SC-NAME" "SC-NUMBER" "SC-SB"
           "SC-OR-LOSE" "SC-NUMBER-OR-LOSE"
           "SC+OFFSET" "MAKE-SC+OFFSET" "SC+OFFSET-OFFSET" "SC+OFFSET-SCN"
           "SET-UNWIND-PROTECT"
           "SETUP-CLOSURE-ENVIRONMENT"
           "SOURCE-LOCATION"
           "SPECIFY-SAVE-TN"
           "STATIC-CALL-NAMED" "STATIC-MULTIPLE-CALL-NAMED" "STATIC-TAIL-CALL-NAMED"
           "STORE-COVERAGE-DATA" "STORE-SOURCE-FORM"
           "TAIL-CALL" "TAIL-CALL-NAMED"
           "TAIL-CALL-VARIABLE" "TEMPLATE-OR-LOSE"
           "TN" "TN-OFFSET" "TN-P" "TN-REF" "TN-REF-ACROSS" "TN-REF-LOAD-TN"
           "TN-REF-NEXT" "TN-REF-NEXT-REF" "TN-REF-P" "TN-REF-TARGET"
           "TN-REF-TN" "TN-REF-TYPE" "TN-REF-VOP" "TN-REF-WRITE-P" "TN-SC"
           "TN-REF-MEMORY-ACCESS"
           "TN-KIND" "TN-VALUE"
           "TYPE-CHECK-ERROR" "UNBIND" "UNBIND-N" "UNBIND-TO-HERE"
           "UNSAFE" "UNSAFELY-FLUSHABLE" "UNWIND" "UWP-ENTRY"
           "UNPACK-CODE-FIXUP-LOCS"
           "VERIFY-ARG-COUNT" "WRITE-PACKED-BIT-VECTOR"
           "WRITE-VAR-INTEGER" "WRITE-VAR-STRING" "XEP-ALLOCATE-FRAME"
           "XEP-SETUP-SP"
           "LABEL-ID" "FIXUP" "FIXUP-FLAVOR" "FIXUP-NAME" "FIXUP-OFFSET"
           "FIXUP-P" "MAKE-FIXUP"
           "DEF-ALLOC"
           "VAR-ALLOC"
           "SAFE-FDEFN-FUN"
           "NOTE-FIXUP"
           "DEF-CASSER"
           "DEF-REFFER"
           "EMIT-CONSTANT"
           "EMIT-NOP"
           "DEF-SETTER"
           "FIXED-ALLOC"
           "MAKE-FUNCALLABLE-INSTANCE-TRAMP"
           "RETURN-SINGLE"
           "NOTE-NEXT-INSTRUCTION"
           "SET-SLOT"
           "LOCATION-NUMBER"
           "*COMPONENT-BEING-COMPILED*"
           "BLOCK-NUMBER"
           "IR2-BLOCK-BLOCK"
           "VOP-BLOCK"
           "VOP-NEXT" "NEXT-VOP-IS" "REPLACE-VOPS"
           "VOP-NAME" "VOP-CODEGEN-INFO"

           "IMMEDIATE-CONSTANT-SC"
           "BOXED-IMMEDIATE-SC-P"
           "COMBINATION-IMPLEMENTATION-STYLE"
           "CONVERT-CONDITIONAL-MOVE-P"
           "LOCATION-PRINT-NAME"
           "MAKE-CALL-OUT-TNS"
           "STANDARD-ARG-LOCATION"
           "STANDARD-ARG-LOCATION-SC"
           "ARG-COUNT-SC"
           "CLOSURE-SC"
           "MAKE-RETURN-PC-PASSING-LOCATION"
           "MAKE-OLD-FP-PASSING-LOCATION"
           "MAKE-OLD-FP-SAVE-LOCATION"
           "RETURN-PC-PASSING-OFFSET"
           "OLD-FP-PASSING-OFFSET"
           "MAKE-RETURN-PC-SAVE-LOCATION"
           "MAKE-ARG-COUNT-LOCATION"
           "MAKE-NFP-TN"
           "MAKE-NUMBER-STACK-POINTER-TN"
           "MAKE-UNKNOWN-VALUES-LOCATIONS"
           "MAKE-NLX-SP-TN"
           "MAKE-DYNAMIC-STATE-TNS"
           "MAKE-NLX-ENTRY-ARG-START-LOCATION"
           "GENERATE-CALL-SEQUENCE"
           "GENERATE-RETURN-SEQUENCE"
           "WITH-COMPILER-ERROR-RESIGNALLING"
           "XDEFUN"                 ; extended defun for defstruct


           "BRANCH-IF" "MULTIWAY-BRANCH-IF-EQ"

           ;; for SB-COVER

           "*CODE-COVERAGE-INFO*" "CODE-COVERAGE-RECORD-MARKED"
           "CLEAR-CODE-COVERAGE" "RESET-CODE-COVERAGE"
           "+CODE-COVERAGE-UNMARKED+"
           ;; for SB-INTROSPECT

           "MAP-PACKED-XREF-DATA" "MAP-SIMPLE-FUNS"

           "DO-BLOCKS" "DO-BLOCKS-BACKWARDS"
           "DO-NODES" "DO-NODES-BACKWARDS"
           "DO-IR2-BLOCKS"))

(defpackage* "SB-REGALLOC"
  (:documentation "private: implementation of the compiler's register allocator")
  (:use "CL" "SB-ALIEN-INTERNALS" "SB-ALIEN" "SB-ASSEM" "SB-BIGNUM"
        #+sb-dyncount "SB-DYNCOUNT" "SB-EXT" "SB-FASL" "SB-INT"
        "SB-KERNEL" "SB-SYS"
        "SB-C")
  (:import-from "SB-C"
                "*LOOP-ANALYZE*"
                "BLOCK-INFO" "BLOCK-LAST" "BLOCK-LOOP" "BLOCK-NEXT"
                "CLEAR-BIT-VECTOR" "COMPONENT-HEAD" "COMPONENT-TAIL"
                "DEFEVENT" "DELETE-VOP" "DO-IR2-BLOCKS" "DO-LIVE-TNS"
                "EMIT-LOAD-TEMPLATE" "EVENT" "FIND-IN"
                "FINITE-SB" "FINITE-SB-ALWAYS-LIVE"
                "FINITE-SB-WIRED-MAP" "FINITE-SB-CONFLICTS"
                "FINITE-SB-CURRENT-SIZE" "FINITE-SB-LAST-BLOCK-COUNT"
                "FINITE-SB-LAST-OFFSET" "FINITE-SB-LIVE-TNS"
                "FINITE-SB-SIZE-ALIGNMENT" "FINITE-SB-SIZE-INCREMENT"
                "GET-OPERAND-INFO" "GLOBAL-CONFLICTS-BLOCK"
                "GLOBAL-CONFLICTS-CONFLICTS" "GLOBAL-CONFLICTS-KIND"
                "GLOBAL-CONFLICTS-NEXT-TNWISE" "GLOBAL-CONFLICTS-NUMBER"
                "GLOBAL-CONFLICTS-NEXT-BLOCKWISE" "GLOBAL-CONFLICTS-TN"
                "IR2-BLOCK" "IR2-BLOCK-COUNT" "IR2-BLOCK-GLOBAL-TNS"
                "IR2-BLOCK-LAST-VOP" "IR2-BLOCK-LIVE-IN"
                "IR2-BLOCK-LOCAL-TN-COUNT" "IR2-BLOCK-LOCAL-TNS"
                "IR2-BLOCK-NEXT" "IR2-BLOCK-NUMBER" "IR2-BLOCK-PREV"
                "IR2-BLOCK-START-VOP" "IR2-COMPONENT"
                "IR2-COMPONENT-GLOBAL-TN-COUNTER"
                "IR2-COMPONENT-NORMAL-TNS" "IR2-COMPONENT-RESTRICTED-TNS"
                "IR2-COMPONENT-SPILLED-TNS" "IR2-COMPONENT-SPILLED-VOPS"
                "IR2-COMPONENT-WIRED-TNS"
                "LAMBDA-PARENT" "LEXENV-LAMBDA" "LISTIFY-RESTRICTIONS"
                "LOCAL-TN-BIT-VECTOR" "LOCAL-TN-COUNT" "LOCAL-TN-LIMIT"
                "LOCAL-TN-NUMBER" "LOCAL-TN-VECTOR"
                "LOOP-DEPTH" "MAKE-TN" "MOVE-OPERAND" "NODE" "NODE-LEXENV"
                "OPERAND-PARSE-NAME" "POSITION-IN"
                "PRIMITIVE-TYPE-SCS" "PRINT-TN-GUTS"
                "SB-KIND" "SB-SIZE"
                "SC-LOCATIONS" "MAKE-SC-LOCATIONS" "SC-OFFSET-TO-SC-LOCATIONS"
                "SC-LOCATIONS-COUNT" "SC-LOCATIONS-FIRST" "SC-LOCATIONS-MEMBER"
                "DO-SC-LOCATIONS"
                "SC-ALIGNMENT" "SC-ALLOWED-BY-PRIMITIVE-TYPE"
                "SC-ALTERNATE-SCS" "SC-CONSTANT-SCS" "SC-ELEMENT-SIZE"
                "SC-LOCATIONS" "SC-MOVE-FUNS" "SC-RESERVE-LOCATIONS"
                "SC-SAVE-P" "SC-VECTOR"
                "SET-BIT-VECTOR"
                "TEMPLATE-NAME"
                "TN" "TN-COST" "TN-GLOBAL-CONFLICTS" "TN-KIND" "TN-LEAF"
                "TN-LOCAL" "TN-LOCAL-CONFLICTS" "TN-LOCAL-NUMBER"
                "TN-NEXT" "TN-NUMBER" "TN-PRIMITIVE-TYPE"
                "TN-READS" "TN-SAVE-TN" "TN-VERTEX" "TN-WRITES"
                "TNS-CONFLICT" "TNS-CONFLICT-GLOBAL-GLOBAL"
                "TNS-CONFLICT-LOCAL-GLOBAL"
                "VOP" "VOP-ARGS" "VOP-INFO" "VOP-INFO-ARG-LOAD-SCS"
                "VOP-INFO-MOVE-ARGS" "VOP-NAME"
                "VOP-INFO-RESULT-LOAD-SCS" "VOP-INFO-SAVE-P"
                "VOP-NODE"
                "VOP-PARSE-OR-LOSE" "VOP-PARSE-TEMPS" "VOP-PREV"
                "VOP-REFS" "VOP-RESULTS" "VOP-SAVE-SET" "VOP-TEMPS")
  (:export "PACK" "TARGET-IF-DESIRABLE" "*REGISTER-ALLOCATION-METHOD*"
           "*PACK-ITERATIONS*"
           "*PACK-ASSIGN-COSTS*" "*PACK-OPTIMIZE-SAVES*"
           "*TN-WRITE-COST*" "*TN-LOOP-DEPTH-MULTIPLIER*"))

(defpackage* "SB-PRETTY"
  (:documentation "private: implementation of pretty-printing")
  (:use "CL" "SB-EXT" "SB-INT" "SB-KERNEL")
  (:export "OUTPUT-PRETTY-OBJECT"
           "PRETTY-STREAM" "PRETTY-STREAM-P"
           "PPRINT-DISPATCH-TABLE"
           "*PPRINT-QUOTE-WITH-SYNTACTIC-SUGAR*"
           "!PPRINT-COLD-INIT"))

(defpackage* "SB-SYS"
  (:documentation
   "private: In theory, this \"contains functions and information
necessary for system interfacing\" (said cmu-user.tex at the time
of the SBCL code fork). That probably was and is a good idea, but in
practice, the distinctions between this package and SB-KERNEL
and even SB-VM seem to have become somewhat blurred over the years.
Some anomalies (e.g. FIND-IF-IN-CLOSURE being in SB-SYS instead of
SB-KERNEL) have been undone, but probably more remain.")
  (:use "CL" "SB-EXT" "SB-INT")
  (:export
   ;; FIXME: %PRIMITIVE shouldn't be here. (I now know that %SYS
   ;; is for OS-dependent stuff. %PRIMITIVE should probably be in
   ;; SB-KERNEL.)
   "%PRIMITIVE"
   "%STANDARD-CHAR-P"
   "*EXIT-ERROR-HANDLER*"
   "*EXIT-IN-PROGRESS*"
   "*ALLOW-WITH-INTERRUPTS*"
   "*INTERRUPTS-ENABLED*"
   "*INTERRUPT-PENDING*"
   #+sb-safepoint "*THRUPTION-PENDING*"
   "*LINKAGE-INFO*"
   "*LONG-SITE-NAME*" "*SHORT-SITE-NAME*"
   "*MACHINE-VERSION*"
   "*PERIODIC-POLLING-FUNCTION*"
   "*PERIODIC-POLLING-PERIOD*"
   "*RUNTIME-DLHANDLE*"
   "*SHARED-OBJECTS*"
   "*STDERR*" "*STDIN*"
   "*STDOUT*"
   "*TTY*"
   "ADD-FD-HANDLER"
   "ALLOCATE-SYSTEM-MEMORY"
   "ALLOW-WITH-INTERRUPTS"
   "BEEP"
   "BREAKPOINT-ERROR"
   "CANCEL-DEADLINE"
   "CLOSE-SHARED-OBJECTS"
   "DEADLINE-TIMEOUT"
   "DEALLOCATE-SYSTEM-MEMORY"
   "DECODE-TIMEOUT"
   "DECODE-INTERNAL-TIME"
   "DEFER-DEADLINE"
   "DLOPEN-OR-LOSE"
   "ENABLE-INTERRUPT"
   "EXTERN-ALIEN-NAME"
   "EXIT-CODE"
   "FD-STREAM" "FD-STREAM-FD" "FD-STREAM-P"
   "FIND-DYNAMIC-FOREIGN-SYMBOL-ADDRESS"
   "FIND-FOREIGN-SYMBOL-ADDRESS"
   "FIND-FOREIGN-SYMBOL-IN-TABLE"
   #+(and win32 x86-64) "FOREIGN-HEAP-CORRUPTION"
   "FOREIGN-SYMBOL-SAP"
   "FOREIGN-SYMBOL-ADDRESS"
   "FOREIGN-SYMBOL-DATAREF-SAP"
   "GET-MACHINE-VERSION" "GET-SYSTEM-INFO"
   "IN-INTERRUPTION"
   "INTERACTIVE-INTERRUPT"
   "INT-SAP"
   "INVALIDATE-DESCRIPTOR"
   "INVOKE-INTERRUPTION"
   "IO-TIMEOUT"
   "MACRO" "MAKE-FD-STREAM"
   "MEMORY-FAULT-ERROR"
   "MEMMOVE"
   "NLX-PROTECT"
   "OS-EXIT"
   "OS-COLD-INIT-OR-REINIT" "OS-DEINIT"
   "OS-CONTEXT-T" "OUTPUT-RAW-BYTES"
   "READ-CYCLE-COUNTER" "ELAPSED-CYCLES"
   "READ-N-BYTES"
   "REMOVE-FD-HANDLER"
   "REOPEN-SHARED-OBJECTS"
   "SAP+" "SAP-"
   "SAP-FOREIGN-SYMBOL"
   "SAP-INT"
   "SAP-REF-16" "SAP-REF-32" "SAP-REF-64" "SAP-REF-WORD"
   "SAP-REF-8"
   "SAP-REF-DOUBLE" "SAP-REF-LISPOBJ" "SAP-REF-LONG"
   "SAP-REF-SAP" "SAP-REF-SINGLE"
   "SAP<" "SAP<=" "SAP=" "SAP>" "SAP>="
   "SCRUB-CONTROL-STACK" "SERVE-ALL-EVENTS"
   "SIGNAL-DEADLINE"
   "SERVE-EVENT"
   "SIGNED-SAP-REF-16" "SIGNED-SAP-REF-32"
   "SIGNED-SAP-REF-64" "SIGNED-SAP-REF-WORD" "SIGNED-SAP-REF-8"
   "STRUCTURE!OBJECT"
   "SYSTEM-AREA-POINTER" "SYSTEM-AREA-POINTER-P"
   "SYSTEM-CONDITION" "SYSTEM-CONDITION-ADDRESS"
   "SYSTEM-CONDITION-CONTEXT"
   "REINIT-INTERNAL-REAL-TIME"
   "SYSTEM-INTERNAL-RUN-TIME"
   "UPDATE-LINKAGE-TABLE" "VECTOR-SAP"
   "WAIT-UNTIL-FD-USABLE"
   "WITH-CODE-PAGES-PINNED"
   "WITH-DEADLINE"
   "WITH-FD-HANDLER"
   "WITH-INTERRUPTS" "WITH-LOCAL-INTERRUPTS"
   "WITH-PINNED-OBJECTS" "WITHOUT-GCING"
   "WITHOUT-INTERRUPTS"
   "WITH-INTERRUPT-BINDINGS"))

(defpackage* "SB-ALIEN"
  (:documentation "public: the ALIEN foreign function interface (If you're
porting CMU CL code, note that this package corresponds roughly to a union
of the packages ALIEN and C-CALL at the time of the SBCL fork. SB-C-CALL
is a deprecated nickname to help ease the transition from older versions
of SBCL which maintained the CMU-CL-style split into two packages.)")
  (:use "CL" "SB-EXT" "SB-INT" "SB-SYS" "SB-ALIEN-INTERNALS")
  (:reexport "ARRAY"
             "BOOLEAN" "CHAR" "DOUBLE-FLOAT"
             "FLOAT" "FUNCTION" "INTEGER" "LONG-FLOAT"
             "SINGLE-FLOAT"
             ;; FIXME: Do we really want to reexport
             ;; SYSTEM-AREA-POINTER here? Why?
             "SYSTEM-AREA-POINTER"
             "UNION"  "VALUES" "*")
  (:export "ADDR"
           "ALIEN"
           "ALIEN-FUNCALL" "ALIEN-SAP" "ALIEN-SIZE"
           "CAST" "C-STRING"
           "DEFINE-ALIEN-ROUTINE" "DEFINE-ALIEN-TYPE" "DEFINE-ALIEN-VARIABLE"
           "DEREF" "DOUBLE"
           "ENUM" "EXTERN-ALIEN"
           "FREE-ALIEN"
           "GET-ERRNO"
           "INT"
           "LOAD-1-FOREIGN" "LOAD-FOREIGN" "LOAD-SHARED-OBJECT" "LONG" "LONG-LONG"
           "MAKE-ALIEN"
           "MAKE-ALIEN-STRING"
           "NULL-ALIEN"
           "OFF-T"
           "SAP-ALIEN" "SHORT" "SIGNED"
           "SIZE-T" "SSIZE-T"
           "SLOT" "STRUCT"
           "UNDEFINED-ALIEN-ERROR"
           "UNLOAD-SHARED-OBJECT"
           "UNSIGNED"
           "UNSIGNED-CHAR" "UNSIGNED-INT" "UNSIGNED-LONG" "UNSIGNED-LONG-LONG" "UNSIGNED-SHORT"
           "UTF8-STRING"
           "VOID"
           "WITH-ALIEN"))

(defpackage* "SB-ALIEN-INTERNALS"
  (:documentation "private: stuff for implementing ALIENs and friends")
  (:use "CL")
  (:export "%ALIEN-VALUE"
           "%CAST"
           "%DEREF-ADDR" "%HEAP-ALIEN" "%HEAP-ALIEN-ADDR"
           "%LOCAL-ALIEN-ADDR" "%LOCAL-ALIEN-FORCED-TO-MEMORY-P" "%SAP-ALIEN"
           "%SET-DEREF" "%SET-HEAP-ALIEN" "%SET-LOCAL-ALIEN" "%SET-SLOT"
           "%SLOT-ADDR" "*SAVED-FP*" "*VALUES-TYPE-OKAY*"
           "ALIEN-ARRAY-TYPE"
           "ALIEN-ARRAY-TYPE-DIMENSIONS" "ALIEN-ARRAY-TYPE-ELEMENT-TYPE"
           "ALIEN-ARRAY-TYPE-P" "ALIEN-BOOLEAN-TYPE" "ALIEN-BOOLEAN-TYPE-P"
           "ALIEN-CALLBACK"
           "ALIEN-CALLBACK-ACCESSOR-FORM"
           "ALIEN-CALLBACK-ASSEMBLER-WRAPPER"
           "ALIEN-DOUBLE-FLOAT-TYPE" "ALIEN-DOUBLE-FLOAT-TYPE-P"
           "ALIEN-ENUM-TYPE" "ALIEN-ENUM-TYPE-P" "ALIEN-FLOAT-TYPE"
           "ALIEN-FLOAT-TYPE-P" "ALIEN-FUN-TYPE"
           "ALIEN-FUN-TYPE-ARG-TYPES" "ALIEN-FUN-TYPE-P"
           "ALIEN-FUN-TYPE-RESULT-TYPE" "ALIEN-INTEGER-TYPE"
           "ALIEN-INTEGER-TYPE-P" "ALIEN-INTEGER-TYPE-SIGNED"
           "ALIEN-LONG-FLOAT-TYPE" "ALIEN-LONG-FLOAT-TYPE-P"
           "ALIEN-POINTER-TYPE" "ALIEN-POINTER-TYPE-P"
           "ALIEN-POINTER-TYPE-TO" "ALIEN-RECORD-FIELD"
           "ALIEN-RECORD-FIELD-NAME" "ALIEN-RECORD-FIELD-OFFSET"
           "ALIEN-RECORD-FIELD-P" "ALIEN-RECORD-FIELD-TYPE"
           "ALIEN-RECORD-TYPE" "ALIEN-RECORD-TYPE-FIELDS"
           "ALIEN-RECORD-TYPE-P" "ALIEN-SINGLE-FLOAT-TYPE"
           "ALIEN-SINGLE-FLOAT-TYPE-P" "ALIEN-SUBTYPE-P" "ALIEN-TYPE"
           "ALIEN-TYPE-=" "ALIEN-TYPE-ALIGNMENT" "ALIEN-TYPE-BITS"
           "ALIEN-TYPE-P" "ALIEN-TYPEP"
           "ALIEN-VALUE"
           "ALIEN-VALUE-TYPE"
           "ALIEN-VALUE-TYPEP"
           "ALIEN-VALUE-SAP" "ALIEN-VALUE-P"
           "ALIEN-VALUES-TYPE" "ALIEN-VALUES-TYPE-P"
           "ALIEN-VALUES-TYPE-VALUES" "ALIGN-OFFSET" "ALIEN-VOID-TYPE-P"
           "COMPUTE-ALIEN-REP-TYPE" "COMPUTE-DEPORT-ALLOC-LAMBDA"
           "COMPUTE-DEPORT-LAMBDA" "COMPUTE-DEPOSIT-LAMBDA"
           "COMPUTE-EXTRACT-LAMBDA" "COMPUTE-LISP-REP-TYPE"
           "COMPUTE-NATURALIZE-LAMBDA" "DEFINE-ALIEN-TYPE-CLASS"
           "DEFINE-ALIEN-TYPE-METHOD" "DEFINE-ALIEN-TYPE-TRANSLATOR"
           "DEPORT" "DEPORT-ALLOC"
           "ENTER-ALIEN-CALLBACK"
           "HEAP-ALIEN-INFO" "HEAP-ALIEN-INFO-P" "HEAP-ALIEN-INFO-SAP-FORM"
           "HEAP-ALIEN-INFO-TYPE" "INVOKE-ALIEN-TYPE-METHOD"
           "INVOKE-WITH-SAVED-FP" "LOCAL-ALIEN"
           "LOCAL-ALIEN-INFO" "LOCAL-ALIEN-INFO-FORCE-TO-MEMORY-P"
           "LOCAL-ALIEN-INFO-P" "LOCAL-ALIEN-INFO-TYPE"
           "MAKE-ALIEN-FUN-TYPE" "MAKE-ALIEN-POINTER-TYPE"
           "MAYBE-WITH-PINNED-OBJECTS"
           "MAKE-LOCAL-ALIEN" "NATURALIZE"
           "NOTE-LOCAL-ALIEN-TYPE"
           "PARSE-ALIEN-TYPE" "UNPARSE-ALIEN-TYPE"))

(defpackage* "SB-APROF"
  (:documentation "public: the interface to the deterministic consing profiler")
  (:use "CL" "SB-EXT" "SB-INT" "SB-KERNEL" "SB-ALIEN" "SB-SYS"))

(defpackage* "SB-PROFILE"
  (:documentation "public: the interface to the profiler")
  (:use "CL" "SB-EXT" "SB-INT" "SB-KERNEL")
  (:export "PROFILE" "REPORT" "RESET" "UNPROFILE"))

;;; FIXME: This package is awfully huge. It'd probably be good to
;;; split it. There's at least one natural way to split it: the
;;; implementation of the Lisp type system (e.g. TYPE-INTERSECTION and
;;; SPECIFIER-TYPE) could move to a separate package SB-TYPE. (There's
;;; lots of stuff which currently uses the SB-KERNEL package which
;;; doesn't actually use the type system stuff.) And maybe other
;;; possible splits too:
;;;   * Pull GC stuff (*GC-INHIBIT*, *GC-PENDING*, etc.)
;;;     out into SB-GC.
;;;   * Pull special case implementations of sequence functions (e.g.
;;;     %MAP-TO-LIST-ARITY-1 and %FIND-POSITION-IF-NOT) and
;;;     other sequence function implementation grot into SB-SEQ.
;;;   * Pull all the math stuff (%ACOS, %COSH, WORD-LOGICAL-AND...)
;;;     into SB-MATH.
;;;   * Pull all the array stuff (%ARRAY-DATA,
;;;     WITH-ARRAY-DATA, ALLOCATE-VECTOR, HAIRY-DATA-VECTOR-REF...)
;;;     into SB-ARRAY.
;;;   * Pull all the streams stuff out into SB-STREAM.
;;;   * Pull all the OBJECT-NOT-FOO symbols out. Maybe we could even
;;;     figure out a way to stop exporting them? Failing that,
;;;     they could be in SB-INTERR.
;;;     Or better still, since error names are basically meaningless except
;;;     for the ~18 errors that are _not_ object-not-<x>, stop naming them.
;;;     C just needs a map from number -> string-to-show.
(defpackage* "SB-KERNEL"
  (:documentation
   "private: Theoretically this 'hides state and types used for package
integration' (said CMU CL architecture.tex) and that probably was and
is a good idea, but see SB-SYS re. blurring of boundaries.")
  (:use "CL" "SB-ALIEN" "SB-ALIEN-INTERNALS" "SB-BIGNUM"
        "SB-EXT" "SB-FASL" "SB-INT" "SB-SYS" "SB-GRAY")
  #+sb-simd-pack
  (:reexport "SIMD-PACK"
             "SIMD-PACK-P"
             "%MAKE-SIMD-PACK-UB32"
             "%MAKE-SIMD-PACK-UB64"
             "%MAKE-SIMD-PACK-DOUBLE"
             "%MAKE-SIMD-PACK-SINGLE"
             "%SIMD-PACK-UB32S"
             "%SIMD-PACK-UB64S"
             "%SIMD-PACK-DOUBLES"
             "%SIMD-PACK-SINGLES")
  (:export "%%DATA-VECTOR-REFFERS%%"
           "%%DATA-VECTOR-SETTERS%%"
           "%ACOS"
           "%ACOSH"
           "%ADJOIN"
           "%ADJOIN-EQ"
           "%ADJOIN-KEY"
           "%ADJOIN-KEY-EQ"
           "%ADJOIN-KEY-TEST"
           "%ADJOIN-KEY-TEST-NOT"
           "%ADJOIN-TEST"
           "%ADJOIN-TEST-NOT"
           "%ARRAY-AVAILABLE-ELEMENTS"
           "%ARRAY-DATA"
           "%ARRAY-DATA-VECTOR"     ; DO NOT USE !!!

           "%ARRAY-DIMENSION" "%ARRAY-DISPLACED-P"
           "%ARRAY-DISPLACED-FROM"
           "%ARRAY-DISPLACEMENT" "%ARRAY-FILL-POINTER"
           "%ARRAY-RANK" "%ARRAY-RANK="
           "SIMPLE-ARRAY-HEADER-OF-RANK-P"
           "%ARRAY-ATOMIC-INCF/WORD"
           "%ASH/RIGHT"
           "%ASSOC"
           "%ASSOC-EQ"
           "%ASSOC-IF"
           "%ASSOC-IF-KEY"
           "%ASSOC-IF-NOT"
           "%ASSOC-IF-NOT-KEY"
           "%ASSOC-KEY"
           "%ASSOC-KEY-EQ"
           "%ASSOC-KEY-TEST"
           "%ASSOC-KEY-TEST-NOT"
           "%ASSOC-TEST"
           "%ASSOC-TEST-NOT"
           "%ASIN" "%ASINH"
           "%ATAN" "%ATAN2" "%ATANH"
           "%ATOMIC-DEC-CAR" "%ATOMIC-INC-CAR"
           "%ATOMIC-DEC-CDR" "%ATOMIC-INC-CDR"
           "%ATOMIC-DEC-SYMBOL-GLOBAL-VALUE"
           "%ATOMIC-INC-SYMBOL-GLOBAL-VALUE"
           "%CALLER-FRAME"
           "%CALLER-PC"
           "%CHECK-BOUND" "CHECK-BOUND"
           "%CHECK-GENERIC-SEQUENCE-BOUNDS"
           "%CHECK-VECTOR-SEQUENCE-BOUNDS"
           "%CAS-SYMBOL-GLOBAL-VALUE"
           "%COPY-INSTANCE" "%COPY-INSTANCE-SLOTS"
           "%COMPARE-AND-SWAP-CAR"
           "%COMPARE-AND-SWAP-CDR"
           "%COMPARE-AND-SWAP-SVREF"
           "%COMPARE-AND-SWAP-SYMBOL-VALUE"
           "%CONCATENATE-TO-BASE-STRING"
           "%CONCATENATE-TO-STRING"
           "%CONCATENATE-TO-SIMPLE-VECTOR"
           "%CONCATENATE-TO-LIST" "%CONCATENATE-TO-VECTOR"
           "CONTAINS-UNKNOWN-TYPE-P"
           "%COS" "%COS-QUICK"
           "%COSH" "%DATA-VECTOR-AND-INDEX" "%DEPOSIT-FIELD"
           "%DOUBLE-FLOAT" "%DPB" "%EQL" "%EQL/INTEGER"
           "%EXIT"
           "%EXP" "%EXPM1"
           "%FIND-POSITION"
           "%FIND-POSITION-VECTOR-MACRO" "%FIND-POSITION-IF"
           "%FIND-POSITION-IF-VECTOR-MACRO" "%FIND-POSITION-IF-NOT"
           "%FIND-POSITION-IF-NOT-VECTOR-MACRO"
           "FIXNUM-MOD-P"
           "%HYPOT"
           "%INSTANCE-CAS"
           "%LDB" "%LOG" "%LOGB" "%LOG10"
           "%LAST0"
           "%LAST1"
           "%LASTN/FIXNUM"
           "%LASTN/BIGNUM"
           "%LOG1P"
           #+long-float "%LONG-FLOAT"
           "%MAKE-ARRAY"
           "%MAKE-COMPLEX" "%MAKE-FUNCALLABLE-INSTANCE"
           "%MAKE-FUNCALLABLE-STRUCTURE-INSTANCE-ALLOCATOR"
           "%MAKE-INSTANCE"
           "%MAKE-LISP-OBJ"
           "%MAKE-LIST"
           "%MAKE-RATIO"
           #+sb-simd-pack "%MAKE-SIMD-PACK"
           #+sb-simd-pack-256 "%MAKE-SIMD-PACK-256"
           "%MAKE-STRUCTURE-INSTANCE"
           "%MAKE-STRUCTURE-INSTANCE-ALLOCATOR"
           "%MAP" "%MAP-FOR-EFFECT-ARITY-1"
           "%MAP-TO-LIST-ARITY-1" "%MAP-TO-SIMPLE-VECTOR-ARITY-1"
           "%MASK-FIELD"
           "%MEMBER"
           "%MEMBER-EQ"
           "%MEMBER-IF"
           "%MEMBER-IF-KEY"
           "%MEMBER-IF-NOT"
           "%MEMBER-IF-NOT-KEY"
           "%MEMBER-KEY"
           "%MEMBER-KEY-EQ"
           "%MEMBER-KEY-TEST"
           "%MEMBER-KEY-TEST-NOT"
           "%MEMBER-TEST"
           "%MEMBER-TEST-NOT"
           "%MULTIPLY-HIGH" "%SIGNED-MULTIPLY-HIGH"
           "%NEGATE" "%POW"
           "%OTHER-POINTER-WIDETAG"
           "%PCL-INSTANCE-P"
           "%PUTHASH"
           "%RASSOC"
           "%RASSOC-EQ"
           "%RASSOC-IF"
           "%RASSOC-IF-KEY"
           "%RASSOC-IF-NOT"
           "%RASSOC-IF-NOT-KEY"
           "%RASSOC-KEY"
           "%RASSOC-KEY-EQ"
           "%RASSOC-KEY-TEST"
           "%RASSOC-KEY-TEST-NOT"
           "%RASSOC-TEST"
           "%RASSOC-TEST-NOT"
           "%VECTOR-RAW-BITS"
           "%SCALB" "%SCALBN"
           "%RAW-INSTANCE-ATOMIC-INCF/WORD"
           "%RAW-INSTANCE-CAS/WORD" "%RAW-INSTANCE-XCHG/WORD"
           "%RAW-INSTANCE-REF/WORD" "%RAW-INSTANCE-SET/WORD"
           "%RAW-INSTANCE-CAS/SIGNED-WORD"
           "%RAW-INSTANCE-REF/SIGNED-WORD" "%RAW-INSTANCE-SET/SIGNED-WORD"
           "%RAW-INSTANCE-REF/SINGLE" "%RAW-INSTANCE-SET/SINGLE"
           "%RAW-INSTANCE-REF/DOUBLE" "%RAW-INSTANCE-SET/DOUBLE"
           "%RAW-INSTANCE-REF/COMPLEX-SINGLE"
           "%RAW-INSTANCE-SET/COMPLEX-SINGLE"
           "%RAW-INSTANCE-REF/COMPLEX-DOUBLE"
           "%RAW-INSTANCE-SET/COMPLEX-DOUBLE"
           "ROUND-DOUBLE" "ROUND-SINGLE"
           "%SET-ARRAY-DIMENSION" "%SET-FUNCALLABLE-INSTANCE-INFO"
           "%SET-VECTOR-RAW-BITS"
           "%SET-SAP-REF-16" "%SET-SAP-REF-32" "%SET-SAP-REF-64"
           "%SET-SAP-REF-WORD" "%SET-SAP-REF-8" "%SET-SAP-REF-DOUBLE"
           "%SET-SAP-REF-LISPOBJ" "%SET-SAP-REF-LONG" "%SET-SAP-REF-SAP"
           "%SET-SAP-REF-SINGLE" "%SET-SIGNED-SAP-REF-16"
           "%SET-SIGNED-SAP-REF-32" "%SET-SIGNED-SAP-REF-64"
           "%SET-SIGNED-SAP-REF-WORD"
           "%SET-SIGNED-SAP-REF-8" "%SET-STACK-REF"
           "%SET-SYMBOL-HASH"
           "%SIN" "%SIN-QUICK" "%SINGLE-FLOAT"
           "%SINH" "%SQRT"
           "%SXHASH-BIT-VECTOR" "%SXHASH-SIMPLE-BIT-VECTOR"
           "%SXHASH-STRING" "%SXHASH-SIMPLE-STRING"
           "%SXHASH-SIMPLE-SUBSTRING" "%TAN" "%TAN-QUICK" "%TANH"
           "THE*"
           "%UNARY-ROUND"
           "%UNARY-TRUNCATE"
           "%UNARY-TRUNCATE/SINGLE-FLOAT"
           "%UNARY-TRUNCATE/DOUBLE-FLOAT"
           "%UNARY-FTRUNCATE"
           "%WITH-ARRAY-DATA"
           "%WITH-ARRAY-DATA/FP"
           "%WITH-ARRAY-DATA-MACRO"
           "*APPROXIMATE-NUMERIC-UNIONS*"
           "*CURRENT-INTERNAL-ERROR-CONTEXT*"
           "*CURRENT-LEVEL-IN-PRINT*"
           "*EMPTY-TYPE*"
           "*EVAL-CALLS*"
           "*GC-INHIBIT*" "*GC-PENDING*"
           "*GC-PIN-CODE-PAGES*"
           #+sb-thread "*STOP-FOR-GC-PENDING*"
           "*IN-WITHOUT-GCING*"
           "*UNIVERSAL-TYPE*"
           "*UNIVERSAL-FUN-TYPE*" "*UNPARSE-FUN-TYPE-SIMPLIFY*"
           "*WILD-TYPE*" "WORD-LOGICAL-AND" "WORD-LOGICAL-ANDC1"
           "WORD-LOGICAL-ANDC2" "WORD-LOGICAL-EQV"
           "WORD-LOGICAL-NAND" "WORD-LOGICAL-NOR" "WORD-LOGICAL-NOT"
           "WORD-LOGICAL-OR" "WORD-LOGICAL-ORC1" "WORD-LOGICAL-ORC2"
           "WORD-LOGICAL-XOR"
           "ALLOW-NON-RETURNING-TAIL-CALL"
           "ALIEN-TYPE-TYPE" "ALIEN-TYPE-TYPE-ALIEN-TYPE" "ALIEN-TYPE-TYPE-P"
           "ALLOCATE-VECTOR" "ALLOCATE-STATIC-VECTOR"
           "ASSERT-SYMBOL-HOME-PACKAGE-UNLOCKED"
           "BUILD-RATIO"
           "PROGRAM-ASSERT-SYMBOL-HOME-PACKAGE-UNLOCKED"
           "DISABLED-PACKAGE-LOCKS"
           "WITH-SINGLE-PACKAGE-LOCKED-ERROR"
           "ARGS-TYPE" "ARGS-TYPE-ALLOWP" "ARGS-TYPE-KEYP"
           "ARGS-TYPE-KEYWORDS" "ARGS-TYPE-OPTIONAL" "ARGS-TYPE-P"
           "ARGS-TYPE-REQUIRED" "ARGS-TYPE-REST"
           "ARRAY-HEADER-P" "SIMPLE-ARRAY-HEADER-P"
           "ARRAY-TYPE" "ARRAY-TYPE-COMPLEXP"
           "ARRAY-TYPE-DIMENSIONS" "ARRAY-TYPE-ELEMENT-TYPE"
           "ARRAY-TYPE-P" "ARRAY-TYPE-SPECIALIZED-ELEMENT-TYPE"
           "ARRAY-UNDERLYING-WIDETAG"
           "ASSERT-ERROR"
           #+sb-unicode "BASE-CHAR-P"
           "BASE-STRING-P"
           "BIND" "BINDING-STACK-POINTER-SAP" "BIT-INDEX"
           "BOGUS-ARG-TO-VALUES-LIST-ERROR" "BOOLE-CODE"
           "BOUNDING-INDICES-BAD-ERROR" "BYTE-SPECIFIER" "%BYTE-BLT"
           "FUNCTION-DESIGNATOR"
           "CASE-BODY-ERROR"
           "CHARACTER-SET" "CHARACTER-SET-TYPE"
           "CHARACTER-SET-TYPE-PAIRS"
           #+sb-unicode "CHARACTER-STRING-P"
           "CHARPOS"
           "CHECK-FOR-CIRCULARITY" "CHECK-TYPE-ERROR" "CLOSED-FLAME"
           "CLASS-CLASSOID"
           "CODE-COMPONENT" "CODE-COMPONENT-P"
           "CODE-HEADER-REF" "CODE-HEADER-SET" "CODE-HEADER-WORDS"
           "CODE-JUMP-TABLE-WORDS"
           "CODE-INSTRUCTIONS" "CODE-N-UNBOXED-DATA-BYTES"
           "CODE-OBJECT-SIZE" "CODE-TRAILER-REF"
           "COERCE-SYMBOL-TO-FUN"
           "COERCE-TO-FUN" "COERCE-TO-LEXENV" "COERCE-TO-LIST"
           "COERCE-TO-VALUES" "COERCE-TO-VECTOR"
           "COMPLEX-DOUBLE-FLOAT" "COMPLEX-DOUBLE-FLOAT-P"
           "COMPLEX-FLOAT-P"
           #+long-float ("COMPLEX-LONG-FLOAT" "COMPLEX-LONG-FLOAT-P")
           "COMPLEX-RATIONAL-P"
           "COMPLEX-SINGLE-FLOAT" "COMPLEX-SINGLE-FLOAT-P"
           "COMPLEX-VECTOR-P" "COMPOUND-TYPE" "COMPOUND-TYPE-P"
           "COMPOUND-TYPE-TYPES"
           "CONDITION-DESIGNATOR-HEAD"
           "CONS-TYPE" "CONS-TYPE-CAR-TYPE"
           "CONS-TYPE-CDR-TYPE" "CONS-TYPE-P" "CONSED-SEQUENCE"
           "CONSTANT" "CONSTANT-TYPE" "CONSTANT-TYPE-P"
           "CONSTANT-TYPE-TYPE" "CONTAINING-INTEGER-TYPE"
           "CONTROL-STACK-POINTER-SAP"
           "CSUBTYPEP" "CTYPE" "TYPE-HASH-VALUE" "CTYPE-OF"
           "CTYPE-P" "CTYPEP"
           "CTYPE-ARRAY-DIMENSIONS" "CTYPE-ARRAY-SPECIALIZED-ELEMENT-TYPES"
           "CURRENT-FP" "CURRENT-SP" "CURRENT-DYNAMIC-SPACE-START"
           "DATA-NIL-VECTOR-REF"
           "DATA-VECTOR-REF" "DATA-VECTOR-REF-WITH-OFFSET"
           "DATA-VECTOR-SET" "DATA-VECTOR-SET-WITH-OFFSET"
           "DECLARATION-TYPE-CONFLICT-ERROR"
           "DECODE-DOUBLE-FLOAT"
           #+long-float "DECODE-LONG-FLOAT"
           "DECODE-SINGLE-FLOAT"
           "DEFINE-STRUCTURE-SLOT-ADDRESSOR"
           "DEFINED-FTYPE-MATCHES-DECLARED-FTYPE-P"
           "!DEFSTRUCT-WITH-ALTERNATE-METACLASS" "DESCEND-INTO"
           "DIVISION-BY-ZERO-ERROR"
           "DO-REST-ARG"
           "DO-INSTANCE-TAGGED-SLOT"
           "DOUBLE-FLOAT-EXPONENT"
           "DOUBLE-FLOAT-BITS"
           "DOUBLE-FLOAT-HIGH-BITS" "DOUBLE-FLOAT-INT-EXPONENT"
           "DOUBLE-FLOAT-LOW-BITS" "DOUBLE-FLOAT-SIGNIFICAND"
           "DSD-ACCESSOR-NAME" "DSD-ALWAYS-BOUNDP" "DSD-DEFAULT" "DSD-INDEX"
           "DSD-NAME" "DSD-RAW-TYPE" "DSD-READ-ONLY" "DSD-TYPE"
           "DYNBIND"
           "FLOAT-WAIT" "DYNAMIC-SPACE-FREE-POINTER" "DYNAMIC-USAGE"
           "EFFECTIVE-FIND-POSITION-TEST"
           "EFFECTIVE-FIND-POSITION-KEY"
           "ENSURE-SYMBOL-HASH"
           "ENSURE-SYMBOL-TLS-INDEX"
           "ERROR-NUMBER-OR-LOSE"
           "EXTERNAL-FORMAT-DESIGNATOR"
           "FAST-&REST-NTH"
           "FILENAME"
           "FILL-ARRAY"
           "FILL-DATA-VECTOR"
           "FIND-DEFSTRUCT-DESCRIPTION"
           "FIND-OR-CREATE-FDEFN"
           "FLOAT-EXPONENT"
           "FLOAT-FORMAT-DIGITS" "FLOAT-FORMAT-NAME"
           "FLOAT-FORMAT-MAX" "FLOAT-INT-EXPONENT"
           "FLOAT-INFINITY-OR-NAN-P"
           "FLOAT-SIGN-BIT"
           "FLOATING-POINT-EXCEPTION" "FORM" "FORMAT-CONTROL"
           "*FREE-INTERRUPT-CONTEXT-INDEX*" "FUNCALLABLE-INSTANCE-P"
           "FSC-INSTANCE-HASH"
           "%FUN-CODE-OFFSET" "FUN-CODE-HEADER"
           "FUN-DESIGNATOR-TYPE" "FUN-DESIGNATOR-TYPE-P"
           "FUN-TYPE" "FUN-TYPE-ALLOWP"
           "FUN-TYPE-KEYP" "FUN-TYPE-KEYWORDS" "FUN-TYPE-NARGS"
           "FUN-TYPE-OPTIONAL" "FUN-TYPE-P" "FUN-TYPE-REQUIRED"
           "FUN-TYPE-REST" "FUN-TYPE-RETURNS" "FUN-TYPE-WILD-ARGS"
           "GENERALIZED-BOOLEAN"
           "GENERIC-ABSTRACT-TYPE-FUNCTION"
           "GET-CLOSURE-LENGTH" "GET-HEADER-DATA"
           "FUNCTION-HEADER-WORD"
           "INSTANCE-HEADER-WORD"
           "GET-LISP-OBJ-ADDRESS" "GENERATION-OF" "OBJECT-CARD-MARKS"
           "LOWTAG-OF" "WIDETAG-OF" "WIDETAG="
           "HAIRY-DATA-VECTOR-REF"
           "HAIRY-DATA-VECTOR-REF/CHECK-BOUNDS"  "HAIRY-DATA-VECTOR-SET"
           "HAIRY-DATA-VECTOR-SET/CHECK-BOUNDS"
           "HAIRY-TYPE" "HAIRY-TYPE-P" "HAIRY-TYPE-SPECIFIER"
           "HANDLE-CIRCULARITY" "HOST" "ILL-BIN"
           "ILL-BOUT" "ILL-IN" "ILL-OUT" "INDEX-OR-MINUS-1"
           "INDEX-TOO-LARGE-ERROR" "*!INITIAL-ASSEMBLER-ROUTINES*"
           "*!INITIAL-SYMBOLS*"
           "INTEGER-DECODE-DOUBLE-FLOAT"
           #+long-float "INTEGER-DECODE-LONG-FLOAT"
           "INTEGER-DECODE-SINGLE-FLOAT" "INTERNAL-ERROR"
           #+win32 "HANDLE-WIN32-EXCEPTION"
           "INTERNAL-TIME" "INTERNAL-SECONDS"
           "INTERNAL-SECONDS-LIMIT" "SAFE-INTERNAL-SECONDS-LIMIT"
           "INTERPRETED-FUNCTION"
           "INTERSECTION-TYPE" "INTERSECTION-TYPE-P"
           "INTERSECTION-TYPE-TYPES" "INVALID-ARG-COUNT-ERROR"
           "LOCAL-INVALID-ARG-COUNT-ERROR"
           "INVALID-UNWIND-ERROR"
           "IRRATIONAL" "KEY-INFO"
           "KEY-INFO-NAME" "KEY-INFO-P" "KEY-INFO-TYPE"
           "BITMAP-NWORDS"
           "LAYOUT-DEPTHOID"
           "LAYOUT-ID"
           "LAYOUT-FOR-PCL-OBJ-P"
           "WRAPPER-BITMAP"
           "WRAPPER-CLASSOID-NAME"
           "WRAPPER-DEPTHOID" "WRAPPER-EQUALP-IMPL"
           "WRAPPER-SLOT-TABLE"
           "WRAPPER-FRIEND" "LAYOUT-FRIEND"
           #+(or x86-64 x86) "%LEA"
           "LEXENV" "LEXENV-DESIGNATOR" "LINE-LENGTH"
           "LIST-ABSTRACT-TYPE-FUNCTION"
           "LIST-COPY-SEQ*"
           "LIST-FILL*"
           "LIST-SUBSEQ*"
           "ANSI-STREAM"
           "ANSI-STREAM-BIN" "ANSI-STREAM-BOUT"
           "ANSI-STREAM-IN"
           "ANSI-STREAM-IN-BUFFER" "ANSI-STREAM-IN-INDEX"
           "ANSI-STREAM-MISC"
           "ANSI-STREAM-N-BIN"
           "ANSI-STREAM-OUT" "ANSI-STREAM-SOUT"
           "COMPLEX-VECTOR"
           "LIST-TO-VECTOR*"
           "LOGICAL-HOST" "LOGICAL-HOST-DESIGNATOR"
           "LRA" "LRA-CODE-HEADER" "LRA-P"
           "MAKE-ALIEN-TYPE-TYPE"
           "MAKE-ARRAY-HEADER" "MAKE-ARRAY-TYPE" "MAKE-CONS-TYPE"
           "%MAKE-DOUBLE-FLOAT"
           "MAKE-DOUBLE-FLOAT" "MAKE-FUN-TYPE" "MAKE-KEY-INFO"
           "MAKE-LISP-OBJ"
           #+long-float "MAKE-LONG-FLOAT"
           "MAKE-MEMBER-TYPE" "MAKE-NULL-LEXENV"
           "MAKE-EQL-TYPE" "MEMBER-TYPE-FROM-LIST"
           "MAKE-NEGATION-TYPE" "TYPE-NEGATION"
           "MAKE-NUMERIC-TYPE"
           "MAKE-SINGLE-FLOAT"
           "MAKE-UNBOUND-MARKER"
           "MAKE-SHORT-VALUES-TYPE" "MAKE-SINGLE-VALUE-TYPE"
           "MAKE-VALUE-CELL" "MAKE-VALUES-TYPE"
           "MAPC-MEMBER-TYPE-MEMBERS" "MAPCAR-MEMBER-TYPE-MEMBERS"
           "MAXIMUM-BIGNUM-LENGTH"
           "MEMBER-TYPE" "MEMBER-TYPE-MEMBERS" "MEMBER-TYPE-P"
           "MEMBER-TYPE-SIZE"
           "MODIFIED-NUMERIC-TYPE"
           "MOST-NEGATIVE-EXACTLY-DOUBLE-FLOAT-FIXNUM"
           "MOST-NEGATIVE-EXACTLY-SINGLE-FLOAT-FIXNUM"
           "MOST-POSITIVE-EXACTLY-DOUBLE-FLOAT-FIXNUM"
           "MOST-POSITIVE-EXACTLY-SINGLE-FLOAT-FIXNUM"
           "NAMED-TYPE" "NAMED-TYPE-NAME" "NAMED-TYPE-P"
           "NEGATE" "NEGATION-TYPE" "NEGATION-TYPE-TYPE"
           "NIL-ARRAY-ACCESSED-ERROR"
           "NIL-FUN-RETURNED-ERROR"
           "NON-NULL-SYMBOL-P"
           "NUMERIC-CONTAGION" "NUMERIC-TYPE"
           "NUMERIC-TYPE-CLASS" "NUMERIC-TYPE-COMPLEXP"
           "NUMERIC-TYPE-EQUAL" "NUMERIC-TYPE-FORMAT"
           "NUMERIC-TYPE-HIGH" "NUMERIC-TYPE-LOW" "NUMERIC-TYPE-P"
           "OBJECT-NOT-ARRAY-ERROR" "OBJECT-NOT-CHARACTER-ERROR"
           "OBJECT-NOT-BASE-STRING-ERROR" "OBJECT-NOT-BIGNUM-ERROR"
           "OBJECT-NOT-BIT-VECTOR-ERROR"
           #+sb-unicode "OBJECT-NOT-CHARACTER-STRING-ERROR"
           "OBJECT-NOT-COMPLEX-ERROR"
           "OBJECT-NOT-COMPLEX-FLOAT-ERROR"
           "OBJECT-NOT-COMPLEX-SINGLE-FLOAT-ERROR"
           #+long-float "OBJECT-NOT-COMPLEX-LONG-FLOAT-ERROR"
           "OBJECT-NOT-COMPLEX-DOUBLE-FLOAT-ERROR"
           "OBJECT-NOT-COMPLEX-RATIONAL-ERROR"
           ;; FIXME: It's confusing using "complex" to mean both
           ;; "not on the real number line" and "not of a
           ;; SIMPLE-ARRAY nature". Perhaps we could rename all the
           ;; uses in the second sense as "hairy" instead?

           "OBJECT-NOT-COMPLEX-VECTOR-ERROR" "OBJECT-NOT-CONS-ERROR"
           "OBJECT-NOT-DOUBLE-FLOAT-ERROR" "OBJECT-NOT-FIXNUM-ERROR"
           "OBJECT-NOT-FLOAT-ERROR" "OBJECT-NOT-FUNCTION-ERROR"
           "OBJECT-NOT-INSTANCE-ERROR" "OBJECT-NOT-INTEGER-ERROR"
           "OBJECT-NOT-LIST-ERROR"
           #+long-float "OBJECT-NOT-LONG-FLOAT-ERROR"
           "OBJECT-NOT-NUMBER-ERROR" "OBJECT-NOT-RATIO-ERROR"
           "OBJECT-NOT-RATIONAL-ERROR" "OBJECT-NOT-REAL-ERROR"
           "OBJECT-NOT-SAP-ERROR" "OBJECT-NOT-SIGNED-BYTE-32-ERROR"
           "OBJECT-NOT-SIGNED-BYTE-64-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-ERROR"
           #+long-float
           "OBJECT-NOT-SIMPLE-ARRAY-COMPLEX-LONG-FLOAT-ERROR"
           #+sb-simd-pack
           "OBJECT-NOT-SIMD-PACK-ERROR"
           #+sb-simd-pack-256
           "OBJECT-NOT-SIMD-PACK-256-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-DOUBLE-FLOAT-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-ERROR"
           #+long-float "OBJECT-NOT-SIMPLE-ARRAY-LONG-FLOAT-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-NIL-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-SINGLE-FLOAT-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-15-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-16-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-2-ERROR"
           ;; KLUDGE: 32-bit and 64-bit ports implement a different
           ;; set of specialized array types.  Various bits of code
           ;; in SBCL assume that symbols connected to the
           ;; specialized array types are exported.  But there's not
           ;; a good way at this point to know whether the port for
           ;; which we're building is 32-bit or 64-bit.  Granted, we
           ;; could hardcode the particulars (or even come up with a
           ;; special :64BIT feature), but that seems a little
           ;; inelegant.  For now, we brute-force the issue by
           ;; always exporting all the names required for both
           ;; 32-bit and 64-bit ports.  Other bits connected to the
           ;; same issue are noted throughout the code below with
           ;; the tag "32/64-bit issues".  --njf, 2004-08-09

           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-FIXNUM-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-31-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-32-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-4-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-63-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-64-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-7-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-8-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-SIGNED-BYTE-16-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-FIXNUM-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-SIGNED-BYTE-32-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-SIGNED-BYTE-64-ERROR"
           "OBJECT-NOT-SIMPLE-ARRAY-SIGNED-BYTE-8-ERROR"
           "OBJECT-NOT-SIMPLE-BIT-VECTOR-ERROR"
           "OBJECT-NOT-SIMPLE-BASE-STRING-ERROR"
           #+sb-unicode "OBJECT-NOT-SIMPLE-CHARACTER-STRING-ERROR"
           "OBJECT-NOT-SIMPLE-STRING-ERROR"
           "OBJECT-NOT-SIMPLE-VECTOR-ERROR"
           "OBJECT-NOT-SINGLE-FLOAT-ERROR" "OBJECT-NOT-STRING-ERROR"
           "OBJECT-NOT-SYMBOL-ERROR"
           "OBJECT-NOT-TYPE-ERROR"
           "OBJECT-NOT-UNSIGNED-BYTE-32-ERROR"
           "OBJECT-NOT-UNSIGNED-BYTE-64-ERROR"
           "OBJECT-NOT-VECTOR-ERROR"
           "OBJECT-NOT-VECTOR-NIL-ERROR"
           "OBJECT-NOT-WEAK-POINTER-ERROR"
           "ODD-KEY-ARGS-ERROR" "OUTPUT-OBJECT" "OUTPUT-UGLY-OBJECT"
           "PACKAGE-DESIGNATOR" "PACKAGE-DOC-STRING"
           "PACKAGE-HASHTABLE-SIZE" "PACKAGE-HASHTABLE-FREE"
           "PACKAGE-INTERNAL-SYMBOLS" "PACKAGE-EXTERNAL-SYMBOLS"
           "PARSE-UNKNOWN-TYPE"
           "PARSE-UNKNOWN-TYPE-SPECIFIER"
           "PATHNAME-DESIGNATOR" "PATHNAME-COMPONENT-CASE"
           "POINTER-HASH"
           "POINTERP"
           #+(or x86 x86-64) "*PSEUDO-ATOMIC-BITS*"
           "PUNT-PRINT-IF-TOO-LONG"
           "RANDOM-DOCUMENTATION"
           "READER-IMPOSSIBLE-NUMBER-ERROR"
           "READER-EOF-ERROR"
           "RESTART-DESIGNATOR"
           "RUN-PENDING-FINALIZERS"
           "SCALE-DOUBLE-FLOAT"
           #+long-float "SCALE-LONG-FLOAT"
           "SCALE-SINGLE-FLOAT"
           "SCHWARTZIAN-STABLE-SORT-LIST" "SCHWARTZIAN-STABLE-SORT-VECTOR"
           "SCRUB-POWER-CACHE"
           "SEQUENCEP" "SEQUENCE-COUNT" "SEQUENCE-END"
           "SEQUENCE-OF-CHECKED-LENGTH-GIVEN-TYPE"
           "SET-ARRAY-HEADER" "SET-HEADER-DATA"
           "ASSIGN-VECTOR-FLAGS" "LOGIOR-HEADER-BITS" "RESET-HEADER-BITS"
           "LOGIOR-ARRAY-FLAGS" "RESET-ARRAY-FLAGS"
           "TEST-HEADER-DATA-BIT"
           "SHIFT-TOWARDS-END"
           "SHIFT-TOWARDS-START" "SHRINK-VECTOR" "%SHRINK-VECTOR"
           "SIGNED-BYTE-32-P" "SIGNED-BYTE-64-P"
           "SIMPLE-RANK-1-ARRAY-*-P"
           "SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT-P"
           #+long-float "SIMPLE-ARRAY-COMPLEX-LONG-FLOAT-P"
           "SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT-P"
           "SIMPLE-ARRAY-DOUBLE-FLOAT-P"
           #+long-float "SIMPLE-ARRAY-LONG-FLOAT-P"
           "SIMPLE-ARRAY-NIL-P" "SIMPLE-ARRAY-P"
           "SIMPLE-ARRAY-SINGLE-FLOAT-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-15-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-16-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-2-P"
           "SIMPLE-ARRAY-UNSIGNED-FIXNUM-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-31-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-32-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-4-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-63-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-64-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-7-P"
           "SIMPLE-ARRAY-UNSIGNED-BYTE-8-P"
           "SIMPLE-ARRAY-SIGNED-BYTE-16-P"
           "SIMPLE-ARRAY-FIXNUM-P"
           "SIMPLE-ARRAY-SIGNED-BYTE-32-P"
           "SIMPLE-ARRAY-SIGNED-BYTE-64-P"
           "SIMPLE-ARRAY-SIGNED-BYTE-8-P" "SIMPLE-BASE-STRING-P"
           "SIMPLE-CHARACTER-STRING"
           #+sb-unicode "SIMPLE-CHARACTER-STRING-P"
           "SIMPLE-PACKAGE-ERROR" "SIMPLE-UNBOXED-ARRAY"
           "SINGLE-FLOAT-BITS" "SINGLE-FLOAT-EXPONENT"
           "SINGLE-FLOAT-INT-EXPONENT" "SINGLE-FLOAT-SIGNIFICAND"
           "SINGLE-FLOAT-SIGN" "SINGLE-FLOAT-COPYSIGN"
           "SINGLE-VALUE-TYPE" "SINGLE-VALUE-SPECIFIER-TYPE"
           "SPECIFIER-TYPE"
           "STACK-REF" "STREAM-DESIGNATOR" "STRING-DESIGNATOR"
           "STRING-FILL*"
           "STRUCT-SLOT-SAP"
           "STRUCTURE-CTOR-LAMBDA-PARTS"
           "STRUCTURE-INSTANCE-ACCESSOR-P"
           "SUB-GC"
           "SYMBOL-TLS-INDEX"
           "SYMBOLS-DESIGNATOR"
           "SYMEVAL"
           "SYM-GLOBAL-VAL"
           "%INSTANCE-LENGTH"
           "%INSTANCE-REF" "%INSTANCE-REF-EQ"
           "%INSTANCE-SET"
           "TESTABLE-TYPE-P"
           "TLS-EXHAUSTED-ERROR"
           "*TOP-LEVEL-FORM-P*"
           "TWO-ARG-*" "TWO-ARG-+" "TWO-ARG--" "TWO-ARG-/"
           "TWO-ARG-/=" "TWO-ARG-<" "TWO-ARG-<=" "TWO-ARG-="
           "TWO-ARG->" "TWO-ARG->=" "TWO-ARG-AND" "TWO-ARG-EQV"
           "TWO-ARG-GCD" "TWO-ARG-IOR" "TWO-ARG-LCM" "TWO-ARG-XOR"
           "TWO-ARG-STRING=" "TWO-ARG-STRING-EQUAL" "TWO-ARG-STRING<"
           "TWO-ARG-STRING>" "TWO-ARG-STRING<=" "TWO-ARG-STRING>="
           "TWO-ARG-STRING/=" "TWO-ARG-STRING-LESSP" "TWO-ARG-STRING-GREATERP"
           "TWO-ARG-STRING-NOT-LESSP" "TWO-ARG-STRING-NOT-GREATERP" "TWO-ARG-STRING-NOT-EQUAL"
           "TYPE-*-TO-T"
           "TYPE-DIFFERENCE" "TYPE-INTERSECTION"
           "TYPE-INTERSECTION2" "TYPE-APPROX-INTERSECTION2"
           "TYPE-SINGLETON-P"
           "TYPE-SINGLE-VALUE-P" "TYPE-SPECIFIER" "TYPE-UNION"
           "TYPE/=" "TYPE=" "TYPES-EQUAL-OR-INTERSECT"
           "TYPE-OR-NIL-IF-UNKNOWN"
           "TYPE-ERROR-DATUM-STORED-TYPE"
           "UNBOUND-SYMBOL-ERROR" "UNBOXED-ARRAY"
           "UNDEFINED-FUN-ERROR" "UNDEFINED-ALIEN-FUN-ERROR"
           "UNION-TYPE" "UNION-TYPE-P"
           "UNION-TYPE-TYPES" "UNKNOWN-ERROR"
           "UNINITIALIZED-ELEMENT-ERROR" ; for Lisp arrays

           "UNINITIALIZED-MEMORY-ERROR"  ; for SAPs

           "UNKNOWN-KEY-ARG-ERROR" "UNKNOWN-TYPE" "UNKNOWN-TYPE-P"
           "UNKNOWN-TYPE-SPECIFIER" "UNSEEN-THROW-TAG-ERROR"
           "UNSIGNED-BYTE-32-P" "UNSIGNED-BYTE-64-P"
           "UPDATE-OBJECT-LAYOUT"
           "VALID-MACROEXPAND-HOOK"
           "VALUE-CELL-REF" "VALUE-CELL-SET" "VALUES-SPECIFIER-TYPE"
           "VALUES-SPECIFIER-TYPE-CACHE-CLEAR" "VALUES-SUBTYPEP"
           "VALUES-TYPE" "VALUES-TYPE-IN"
           "VALUES-TYPE-INTERSECTION"
           "VALUES-TYPE-MIN-VALUE-COUNT" "VALUES-TYPE-MAX-VALUE-COUNT"
           "VALUES-TYPE-MAY-BE-SINGLE-VALUE-P" "VALUES-TYPE-OPTIONAL"
           "VALUES-TYPE-OUT" "VALUES-TYPE-P" "VALUES-TYPE-REQUIRED"
           "VALUES-TYPE-REST" "VALUES-TYPE-UNION"
           "VALUES-TYPE-TYPES" "VALUES-TYPES"
           "VALUES-TYPES-EQUAL-OR-INTERSECT"

           "*VECTOR-WITHOUT-COMPLEX-TYPECODE-INFOS*"
           "VECTOR-SINGLE-FLOAT-P" "VECTOR-DOUBLE-FLOAT-P"
           "VECTOR-UNSIGNED-BYTE-2-P" "VECTOR-UNSIGNED-BYTE-4-P"
           "VECTOR-UNSIGNED-BYTE-7-P" "VECTOR-UNSIGNED-BYTE-8-P"
           "VECTOR-UNSIGNED-BYTE-15-P" "VECTOR-UNSIGNED-BYTE-16-P"
           "VECTOR-UNSIGNED-BYTE-31-P" "VECTOR-UNSIGNED-BYTE-32-P"
           "VECTOR-UNSIGNED-BYTE-63-P" "VECTOR-UNSIGNED-BYTE-64-P"
           "VECTOR-SIGNED-BYTE-8-P" "VECTOR-SIGNED-BYTE-16-P"
           "VECTOR-FIXNUM-P" "VECTOR-SIGNED-BYTE-32-P"
           "VECTOR-SIGNED-BYTE-64-P" "VECTOR-COMPLEX-SINGLE-FLOAT-P"
           "VECTOR-COMPLEX-DOUBLE-FLOAT-P" "VECTOR-T-P"

           "VECTOR-NIL-P"
           "VECTOR-FILL*" "VECTOR-FILL/T"
           "VECTOR-SUBSEQ*"
           "VECTOR-TO-VECTOR*"
           "VECTOR-OF-CHECKED-LENGTH-GIVEN-LENGTH" "WITH-ARRAY-DATA"
           "WITH-CIRCULARITY-DETECTION"
           "WITH-WORLD-LOCK"

           ;; bit bash fillers

           "UB1-BASH-FILL"
           "UB2-BASH-FILL"
           "UB4-BASH-FILL"
           "UB8-BASH-FILL"
           "UB16-BASH-FILL"
           "UB32-BASH-FILL"
           "UB64-BASH-FILL"

           "UB1-BASH-FILL-WITH-UB1" "UB1-BASH-FILL-WITH-SB1"
           "UB2-BASH-FILL-WITH-UB2" "UB2-BASH-FILL-WITH-SB2"
           "UB4-BASH-FILL-WITH-UB4" "UB4-BASH-FILL-WITH-SB4"
           "UB8-BASH-FILL-WITH-UB8" "UB8-BASH-FILL-WITH-SB8"
           "UB16-BASH-FILL-WITH-UB16" "UB16-BASH-FILL-WITH-SB16"
           "UB32-BASH-FILL-WITH-UB32" "UB32-BASH-FILL-WITH-SB32"
           "UB64-BASH-FILL-WITH-SB64"
           "UB32-BASH-FILL-WITH-FIXNUM" "UB64-BASH-FILL-WITH-FIXNUM"
           "UB32-BASH-FILL-WITH-SINGLE-FLOAT"
           "UB64-BASH-FILL-WITH-DOUBLE-FLOAT"
           "UB64-BASH-FILL-WITH-COMPLEX-SINGLE-FLOAT"

           ;; bit bash copiers

           "SYSTEM-AREA-UB8-COPY"
           "COPY-UB8-TO-SYSTEM-AREA" "COPY-UB8-FROM-SYSTEM-AREA"
           "UB1-BASH-COPY"
           "UB2-BASH-COPY"
           "UB4-BASH-COPY"
           "UB8-BASH-COPY"
           "UB16-BASH-COPY"
           "UB32-BASH-COPY"
           "UB64-BASH-COPY"

           ;; Bit bashing position for bit-vectors

           "%BIT-POSITION" "%BIT-POS-FWD" "%BIT-POS-REV"
           "%BIT-POSITION/0" "%BIT-POS-FWD/0" "%BIT-POS-REV/0"
           "%BIT-POSITION/1" "%BIT-POS-FWD/1" "%BIT-POS-REV/1"

           ;; SIMPLE-FUN type and accessors

           "SIMPLE-FUN"
           "SIMPLE-FUN-P"
           "%SIMPLE-FUN-ARGLIST"
           "%SIMPLE-FUN-DOC"
           "%SIMPLE-FUN-INFO"
           "%SIMPLE-FUN-LEXPR"
           "%SIMPLE-FUN-NAME"
           "%SIMPLE-FUN-TEXT-LEN"
           "%SIMPLE-FUN-SOURCE"
           "%SIMPLE-FUN-TYPE"
           "%SIMPLE-FUN-XREFS"

           ;; CLOSURE type and accessors

           "CLOSURE"
           "CLOSUREP"
           "DO-CLOSURE-VALUES"
           "%CLOSURE-FUN"
           "%CLOSURE-INDEX-REF"
           "%CLOSURE-INDEX-SET"
           "%CLOSURE-VALUES"

           ;; Abstract function accessors

           "%FUN-FUN"
           "%FUN-LAMBDA-LIST"
           "%FUN-NAME"

           "FDEFN" "MAKE-FDEFN" "FDEFN-P" "FDEFN-NAME" "FDEFN-FUN"
           "FDEFN-MAKUNBOUND"
           "%COERCE-CALLABLE-TO-FUN"
           "%COERCE-CALLABLE-FOR-CALL"
           "%FUN-POINTER-WIDETAG"
           "*MAXIMUM-ERROR-DEPTH*" "%SET-SYMBOL-PLIST"
           "INFINITE-ERROR-PROTECT"
           "FIND-CALLER-OF-NAMED-FRAME"
           "FIND-CALLER-FRAME"
           "FIND-INTERRUPTED-FRAME"
           "%SET-SYMBOL-VALUE" "%SET-SYMBOL-GLOBAL-VALUE" "%SET-SYMBOL-PACKAGE"
           "SET-SYMBOL-GLOBAL-VALUE"
           "OUTPUT-SYMBOL" "%COERCE-NAME-TO-FUN"
           "DEFAULT-STRUCTURE-PRINT"
           "WRAPPER" "WRAPPER-LENGTH"
           "DEFSTRUCT-DESCRIPTION" "UNDECLARE-STRUCTURE"
           "UNDEFINE-FUN-NAME" "DD-TYPE" "CLASSOID-STATE" "INSTANCE"
           "*TYPE-SYSTEM-INITIALIZED*" "FIND-LAYOUT"
           "%TYPEP" "%%TYPEP" "DD-NAME" "CLASSOID-SUBCLASSES"
           "CLASSOID-LAYOUT" "CLASSOID-NAME" "CLASSOID-P" "CLASSOID-WRAPPER"
           "NOTE-NAME-DEFINED"
           "%CODE-CODE-SIZE" "%CODE-TEXT-SIZE"
           "%CODE-SERIALNO"
           "DD-SLOTS" "DD-INCLUDE" "SLOT-SETTER-LAMBDA-FORM"
           "SLOT-ACCESS-TRANSFORM"
           "%IMAGPART" "%CODE-DEBUG-INFO"
           "WRAPPER-CLASSOID" "WRAPPER-INVALID"
           "WRAPPER-FLAGS"
           "LAYOUT-FLAGS"
           "%INSTANCEP" "DEFSTRUCT-SLOT-DESCRIPTION"
           "DD-PREDICATE-NAME"
           "CLASSOID-PROPER-NAME" "%NOTE-TYPE-DEFINED" "WRAPPER-INFO"
           "WRAPPER-DD"
           "%SET-INSTANCE-LAYOUT"
           "DD-CONSTRUCTORS" "DD-DEFAULT-CONSTRUCTOR"
           "WRAPPER-OF"
           "%REALPART"
           "STRUCTURE-CLASSOID" "STRUCTURE-CLASSOID-P"
           "GET-DSD-INDEX"
           "%INSTANCE-LAYOUT" "LAYOUT-CLOS-HASH" "%INSTANCEOID-LAYOUT"
           "WRAPPER-CLOS-HASH"
           "%INSTANCE-WRAPPER" "%FUN-WRAPPER"
           "PROCLAIM-AS-FUN-NAME" "BECOME-DEFINED-FUN-NAME"
           "%NUMERATOR" "CLASSOID-TYPEP"
           "WRAPPER-INHERITS" "DD-LENGTH"
           "SET-LAYOUT-INHERITS"
           "%CODE-ENTRY-POINT"
           "%CODE-FUN-OFFSET"
           "CODE-N-ENTRIES"
           "CODE-N-NAMED-CALLS"
           "CODE-OBJ-IS-FILLER-P"
           "%DENOMINATOR"
           "%OTHER-POINTER-P"
           "%OTHER-POINTER-SUBTYPE-P"
           "IMMOBILE-SPACE-ADDR-P"
           "IMMOBILE-SPACE-OBJ-P"

           "STANDARD-CLASSOID" "CLASSOID-OF"
           "MAKE-STANDARD-CLASSOID"
           "CLASSOID-CELL-CLASSOID"
           "CLASSOID-CELL-NAME"
           "CLASSOID-CELL-PCL-CLASS"
           "CLASSOID-CELL-TYPEP"
           "%CLEAR-CLASSOID"
           "FIND-CLASSOID-CELL"
           "%RANDOM-DOUBLE-FLOAT"
           #+long-float "%RANDOM-LONG-FLOAT"
           "%RANDOM-SINGLE-FLOAT" "STATIC-CLASSOID"
           "%FUNCALLABLE-INSTANCE-INFO" "RANDOM-CHUNK" "BIG-RANDOM-CHUNK"
           "N-RANDOM-CHUNK-BITS"
           "BUILT-IN-CLASSOID-DIRECT-SUPERCLASSES"
           "BUILT-IN-CLASSOID-TRANSLATION" "HASH-LAYOUT-NAME"
           "CLASSOID-PCL-CLASS" "FUNCALLABLE-STRUCTURE"
           "FUNCTION-WITH-LAYOUT-P"
           "%FUN-LAYOUT" "%SET-FUN-LAYOUT"
           "REGISTER-LAYOUT"
           "FUNCALLABLE-INSTANCE"
           "MAKE-STATIC-CLASSOID"
           "%MAKE-SYMBOL"
           "%FUNCALLABLE-INSTANCE-FUN" "SYMBOL-HASH" "SYMBOL-HASH*"
           "SYMBOL-%INFO" "SYMBOL-DBINFO" "%INFO-REF"

           "EXTENDED-SEQUENCE" "*EXTENDED-SEQUENCE-TYPE*"
           "EXTENDED-SEQUENCE-P"

           "BUILT-IN-CLASSOID" "CONDITION-CLASSOID-P"
           "CONDITION-CLASSOID-SLOTS" "MAKE-UNDEFINED-CLASSOID"
           "FIND-CLASSOID" "CLASSOID" "CLASSOID-DIRECT-SUPERCLASSES"
           "MAKE-LAYOUT"
           "INSURED-FIND-CLASSOID" "ORDER-LAYOUT-INHERITS"
           "STD-COMPUTE-CLASS-PRECEDENCE-LIST"
           "+STRUCTURE-LAYOUT-FLAG+"
           "+PCL-OBJECT-LAYOUT-FLAG+"
           "+CONDITION-LAYOUT-FLAG+"
           "+PATHNAME-LAYOUT-FLAG+"
           "+STREAM-LAYOUT-FLAG+"
           "+SIMPLE-STREAM-LAYOUT-FLAG+"
           "+FILE-STREAM-LAYOUT-FLAG+"
           "+STRING-STREAM-LAYOUT-FLAG+"
           "+SEQUENCE-LAYOUT-FLAG+"
           "+STRICTLY-BOXED-FLAG+"
           "+LAYOUT-ALL-TAGGED+"
           "**PRIMITIVE-OBJECT-LAYOUTS**"

           "*HANDLER-CLUSTERS*" "*RESTART-CLUSTERS*"
           "WITH-SIMPLE-CONDITION-RESTARTS"
           "CASE-FAILURE" "ECASE-FAILURE" "ETYPECASE-FAILURE"
           "NAMESTRING-PARSE-ERROR"
           "NAMESTRING-PARSE-ERROR-OFFSET"
           "NO-NAMESTRING-ERROR" "NO-NATIVE-NAMESTRING-ERROR"
           "MAKE-RESTART" "RESTART-ASSOCIATED-CONDITIONS"
           "COERCE-TO-CONDITION"

           "ALLOCATE-CONDITION"

           "CONDITION-SLOT-VALUE"
           "SET-CONDITION-SLOT-VALUE"

           "CONDITION-SLOT-ALLOCATION"
           "CONDITION-SLOT-DOCUMENTATION"
           "CONDITION-SLOT-INITARGS"
           "CONDITION-SLOT-INITFORM"
           "CONDITION-SLOT-INITFORM-P"
           "CONDITION-SLOT-INITFUNCTION"
           "CONDITION-SLOT-NAME" "CONDITION-SLOT-READERS"
           "CONDITION-SLOT-WRITERS"

           "REDEFINITION-WARNING"
           "REDEFINITION-WITH-DEFUN"
           "REDEFINITION-WITH-DEFMACRO"
           "REDEFINITION-WITH-DEFGENERIC"
           "REDEFINITION-WITH-DEFMETHOD"
           "UNINTERESTING-ORDINARY-FUNCTION-REDEFINITION-P"
           "UNINTERESTING-GENERIC-FUNCTION-REDEFINITION-P"
           "UNINTERESTING-METHOD-REDEFINITION-P"
           "UNINTERESTING-REDEFINITION"
           "REDEFINITION-WITH-DEFTRANSFORM"

           "DUBIOUS-ASTERISKS-AROUND-VARIABLE-NAME"
           "ASTERISKS-AROUND-LEXICAL-VARIABLE-NAME"
           "ASTERISKS-AROUND-CONSTANT-VARIABLE-NAME"
           "&OPTIONAL-AND-&KEY-IN-LAMBDA-LIST"
           "UNDEFINED-ALIEN-STYLE-WARNING"
           "LEXICAL-ENVIRONMENT-TOO-COMPLEX"
           "CHARACTER-DECODING-ERROR-IN-COMMENT"
           "DEPRECATED-EVAL-WHEN-SITUATIONS"
           "PROCLAMATION-MISMATCH"
           "PROCLAMATION-MISMATCH-NAME"
           "PROCLAMATION-MISMATCH-NEW"
           "PROCLAMATION-MISMATCH-OLD"
           "TYPE-PROCLAMATION-MISMATCH"
           "TYPE-PROCLAMATION-MISMATCH-WARNING"
           "TYPE-PROCLAMATION-MISMATCH-WARN"
           "FTYPE-PROCLAMATION-MISMATCH"
           "FTYPE-PROCLAMATION-MISMATCH-WARNING"
           "FTYPE-PROCLAMATION-MISMATCH-WARN"
           "FTYPE-PROCLAMATION-MISMATCH-ERROR"

           "!COLD-INIT"
           "!TYPE-CLASS-COLD-INIT"
           "!CLASSES-COLD-INIT"
           "!TYPE-COLD-INIT"
           "!FIXUP-TYPE-COLD-INIT"
           "!RANDOM-COLD-INIT"
           "!LOADER-COLD-INIT"
           "!POLICY-COLD-INIT-OR-RESANIFY"
           "!LATE-PROCLAIM-COLD-INIT" "!CLASS-FINALIZE"

           "FLOAT-COLD-INIT-OR-REINIT"
           "GC-REINIT"
           "TIME-REINIT"
           "SIGNAL-COLD-INIT-OR-REINIT"
           "STREAM-COLD-INIT-OR-RESET"

           ;; Cleanups to run before saving a core

           "FLOAT-DEINIT"
           "FOREIGN-DEINIT"
           "PROFILE-DEINIT"

           ;; Note: These are out of lexicographical order
           ;; because in CMU CL they were defined as
           ;; internal symbols in package "CL" imported
           ;; into package "C", as opposed to what we're
           ;; doing here, defining them as external symbols
           ;; in a package which is used by both "SB-C" and
           ;; "SB-IMPL". (SBCL's "SB-C" is directly
           ;; analogous to CMU CL's "C"; and for this
           ;; purpose, SBCL's "SB-IMPL" is analogous to CMU
           ;; CL's "CL".) As far as I know there's nothing
           ;; special about them, so they could be merged
           ;; into the same order as everything else in the
           ;; in this package. -- WHN 19990911

           "STRING>=*" "STRING>*" "STRING=*" "STRING<=*"
           "STRING<*" "STRING/=*" "%SVSET"
           "%SP-STRING-COMPARE" "%SETNTH" "%SETELT"
           "%SET-ROW-MAJOR-AREF" "%SET-FILL-POINTER"
           "%SET-FDEFINITION" "%SCHARSET"
           "%RPLACD" "%RPLACA" "%PUT" "%CHARSET"
           "%WITH-OUTPUT-TO-STRING")
  #+sb-simd-pack
  (:export "%SIMD-PACK-TAG"
           "%SIMD-PACK-LOW"
           "%SIMD-PACK-HIGH")
  #+sb-simd-pack-256
  (:export "%SIMD-PACK-256-TAG"
           "%SIMD-PACK-256-0" "%SIMD-PACK-256-1"
           "%SIMD-PACK-256-2" "%SIMD-PACK-256-3")
  #+sb-simd-pack
  (:export "SIMD-PACK-SINGLE"
           "SIMD-PACK-DOUBLE"
           "SIMD-PACK-INT"
           "SIMD-PACK-TYPE"
           "SIMD-PACK-TYPE-ELEMENT-TYPE"
           "*SIMD-PACK-ELEMENT-TYPES*")
  #+sb-simd-pack-256
  (:export "SIMD-PACK-256-SINGLE"
           "SIMD-PACK-256-DOUBLE"
           "SIMD-PACK-256-INT"
           "SIMD-PACK-256-TYPE"
           "SIMD-PACK-256-TYPE-ELEMENT-TYPE")
  #+long-float
  (:export "LONG-FLOAT-EXPONENT" "LONG-FLOAT-EXP-BITS"
           "LONG-FLOAT-HIGH-BITS" "LONG-FLOAT-LOW-BITS"
           "LONG-FLOAT-MID-BITS"))

(defpackage* "SB-ASSEM"
  (:documentation "private: the assembler, used by the compiler")
  (:use "CL" "SB-EXT" "SB-INT")
  (:export "ASSEMBLY-UNIT"
           "ASSEMBLY-UNIT-BITS"
           "+INST-ALIGNMENT-BYTES+"

           "ASSEM-SCHEDULER-P"
           "+ASSEM-MAX-LOCATIONS+"

           "*ASMSTREAM*" "MAKE-ASMSTREAM"
           "ASMSTREAM-DATA-SECTION"
           "ASMSTREAM-CODE-SECTION"
           "ASMSTREAM-ELSEWHERE-SECTION"
           "ASMSTREAM-ELSEWHERE-LABEL"
           "ASMSTREAM-CONSTANT-TABLE"
           "ASMSTREAM-CONSTANT-VECTOR"
           "APPEND-SECTIONS" "ASSEMBLE-SECTIONS"
           "EMIT" ".ALIGN" ".BYTE" ".LISPWORD" ".SKIP"
           ".COVERAGE-MARK" ".COMMENT"
           "EMIT-ALIGNMENT" "EMIT-BYTE" "EMIT-BACK-PATCH"
           "EMIT-CHOOSER" "DEFINE-BITFIELD-EMITTER"
           "DEFINE-INSTRUCTION" "DEFINE-INSTRUCTION-MACRO"
           "EMIT-POSTIT"
           "ANY-ALIGNMENT-BETWEEN-P"

           "MAKE-SEGMENT" "SEGMENT-ORIGIN" "ASSEMBLE"
           "SEGMENT-BUFFER"
           "SEGMENT-ENCODER-STATE"
           "SEGMENT-HEADER-SKEW"
           "INST" "INST*" "LABEL" "LABEL-P" "GEN-LABEL"
           "EMIT-LABEL" "LABEL-POSITION" "LABEL-USEDP"
           "FINALIZE-SEGMENT"
           "SEGMENT-CONTENTS-AS-VECTOR" "WRITE-SEGMENT-CONTENTS"
           "READS" "WRITES" "SEGMENT"
           "WITHOUT-SCHEDULING"
           "VARIABLE-LENGTH"
           "SEGMENT-COLLECT-DYNAMIC-STATISTICS"
           "SECTION-START"
           "STMT-LABELS" "STMT-MNEMONIC" "STMT-OPERANDS"
           "STMT-PLIST"
           "STMT-PREV" "STMT-NEXT"
           "ADD-STMT-LABELS" "DELETE-STMT"
           "LABELED-STATEMENT-P"
           "DEFPATTERN"

           ;; FIXME: These are in the SB-ASSEM package now, but
           ;; (left over from CMU CL) are defined in files which
           ;; are IN-PACKAGE SB-C. It would probably be cleaner
           ;; to move at least most of them to files which are
           ;; IN-PACKAGE SB-ASSEM.

           "BRANCH"  "FLUSHABLE"))

(defpackage* "SB-THREAD"
  (:documentation "public (but low-level): native thread support")
  (:use "CL" "SB-ALIEN" "SB-INT" "SB-SYS" "SB-KERNEL")
  (:export "*CURRENT-THREAD*"
           "DESTROY-THREAD"
           "INTERRUPT-THREAD"
           "INTERRUPT-THREAD-ERROR"
           "INTERRUPT-THREAD-ERROR-THREAD"
           "RETURN-FROM-THREAD"
           "ABORT-THREAD"
           "MAIN-THREAD-P"
           "MAIN-THREAD"
           "JOIN-THREAD"
           "JOIN-THREAD-ERROR"
           "JOIN-THREAD-ERROR-THREAD"
           "LIST-ALL-THREADS"
           "MAKE-THREAD"
           "SYMBOL-VALUE-IN-THREAD"
           "SYMBOL-VALUE-IN-THREAD-ERROR"
           "TERMINATE-THREAD"
           "THREAD"
           "THREAD-DEADLOCK"
           "THREAD-DEADLOCK-CYCLE"
           "THREAD-ERROR"
           "THREAD-ERROR-THREAD"
           "THREAD-ALIVE-P"
           "THREAD-EPHEMERAL-P"
           "THREAD-NAME"
           "THREAD-OS-TID"
           "THREAD-YIELD"
           "FOREIGN-THREAD"
           ;; Memory barrier

           "BARRIER"
           ;; Mutexes

           "GET-MUTEX"
           "GRAB-MUTEX"
           "HOLDING-MUTEX-P"
           "MAKE-MUTEX"
           "MUTEX"
           "MUTEX-NAME"
           "MUTEX-OWNER"
           "MUTEX-VALUE"
           "RELEASE-MUTEX"
           "WITH-MUTEX"
           "WITH-RECURSIVE-LOCK"
           ;; Condition variables

           "CONDITION-BROADCAST"
           "CONDITION-NOTIFY"
           "CONDITION-WAIT"
           "MAKE-WAITQUEUE"
           "WAITQUEUE"
           "WAITQUEUE-NAME"
           ;; Sessions

           "MAKE-LISTENER-THREAD"
           "RELEASE-FOREGROUND"
           "WITH-NEW-SESSION"
           ;; Semaphores

           "MAKE-SEMAPHORE"
           "SEMAPHORE"
           "SEMAPHORE-NAME"
           "SEMAPHORE-COUNT"
           "SIGNAL-SEMAPHORE"
           "TRY-SEMAPHORE"
           "WAIT-ON-SEMAPHORE"
           ;; Semaphore notification objects

           "CLEAR-SEMAPHORE-NOTIFICATION"
           "MAKE-SEMAPHORE-NOTIFICATION"
           "SEMAPHORE-NOTIFICATION"
           "SEMAPHORE-NOTIFICATION-STATUS"))

(defpackage* "SB-WALKER"
  (:documentation "internal: a code walker used by PCL")
  (:use "CL" "SB-INT" "SB-EXT")
  (:export "DEFINE-WALKER-TEMPLATE"
           "WALK-FORM"
           "*WALK-FORM-EXPAND-MACROS-P*"
           "VAR-LEXICAL-P" "VAR-SPECIAL-P"
           "VAR-GLOBALLY-SPECIAL-P"
           "VAR-DECLARATION"))

(defpackage* "SB-UNICODE"
  (:documentation "public: algorithms for Unicode data")
  (:use "CL" "SB-INT")
  (:export "GENERAL-CATEGORY"
           "BIDI-CLASS"
           "COMBINING-CLASS"
           "DECIMAL-VALUE"
           "DIGIT-VALUE"
           "NUMERIC-VALUE"
           "MIRRORED-P"
           "BIDI-MIRRORING-GLYPH"
           "AGE"
           "HANGUL-SYLLABLE-TYPE"
           "EAST-ASIAN-WIDTH"
           "SCRIPT"
           "CHAR-BLOCK"
           "UNICODE-1-NAME"
           "LINE-BREAK-CLASS"
           "PROPLIST-P"
           "UPPERCASE-P"
           "LOWERCASE-P"
           "CASED-P"
           "CASE-IGNORABLE-P"
           "ALPHABETIC-P"
           "IDEOGRAPHIC-P"
           "MATH-P"
           "WHITESPACE-P"
           "HEX-DIGIT-P"
           "SOFT-DOTTED-P"
           "DEFAULT-IGNORABLE-P"
           "NORMALIZE-STRING"
           "NORMALIZED-P"
           "UPPERCASE"
           "LOWERCASE"
           "TITLECASE"
           "CASEFOLD"
           "GRAPHEME-BREAK-CLASS"
           "WORD-BREAK-CLASS"
           "SENTENCE-BREAK-CLASS"
           "GRAPHEMES"
           "WORDS"
           "SENTENCES"
           "LINES"
           "UNICODE="
           "UNICODE-EQUAL"
           "UNICODE<"
           "UNICODE<="
           "UNICODE>"
           "UNICODE>="
           "CONFUSABLE-P"))

(defpackage* "SB-GRAY"
  (:documentation
   "public: an implementation of the stream-definition-by-user
Lisp extension proposal by David N. Gray")
  (:use "CL" "SB-EXT" "SB-INT" "SB-KERNEL")
  (:export "FUNDAMENTAL-BINARY-STREAM"
           "FUNDAMENTAL-BINARY-INPUT-STREAM"
           "FUNDAMENTAL-BINARY-OUTPUT-STREAM" "FUNDAMENTAL-CHARACTER-STREAM"
           "FUNDAMENTAL-CHARACTER-INPUT-STREAM"
           "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
           "FUNDAMENTAL-INPUT-STREAM" "FUNDAMENTAL-OUTPUT-STREAM"
           "FUNDAMENTAL-STREAM"
           "STREAM-ADVANCE-TO-COLUMN" "STREAM-CLEAR-INPUT"
           "STREAM-CLEAR-OUTPUT" "STREAM-FILE-POSITION" "STREAM-FINISH-OUTPUT" "STREAM-FORCE-OUTPUT"
           "STREAM-FRESH-LINE" "STREAM-LINE-COLUMN" "STREAM-LINE-LENGTH"
           "STREAM-LISTEN" "STREAM-PEEK-CHAR" "STREAM-READ-BYTE"
           "STREAM-READ-CHAR" "STREAM-READ-CHAR-NO-HANG" "STREAM-READ-LINE"
           "STREAM-READ-SEQUENCE" "STREAM-START-LINE-P" "STREAM-TERPRI"
           "STREAM-UNREAD-CHAR"
           "STREAM-WRITE-BYTE" "STREAM-WRITE-CHAR" "STREAM-WRITE-SEQUENCE"
           "STREAM-WRITE-STRING"))

(defpackage* "SB-INT"
  (:documentation
   "private: miscellaneous unsupported extensions to the ANSI spec. Much of
the stuff in here originated in CMU CL's EXTENSIONS package and is retained,
possibly temporarily, because it might be used internally.")
  (:use "CL" "SB-ALIEN" "SB-GRAY" "SB-FASL" "SB-SYS")
  (:export ;; lambda list keyword extensions
           "&MORE"

            ;; utilities for floating point zero handling

           "FP-ZERO-P"

            ;; Advice to the compiler that it doesn't need to assert types.

           "EXPLICIT-CHECK"
            ;; Stack allocation without any questions asked

           "TRULY-DYNAMIC-EXTENT"

           "WITH-SYSTEM-MUTEX"
           "HASH-TABLE-LOCK"

            ;; generic set implementation backed by a list that upgrades
            ;; to a hashtable if a certain size is exceeded.

           "ADD-TO-XSET"
           "ALLOC-XSET"
           "MAP-XSET"
           "XSET"
           "XSET-COUNT"
           "XSET-EMPTY-P"
           "XSET-INTERSECTION"
           "XSET-MEMBER-P"
           "XSET-MEMBERS"
           "XSET-SUBSET-P"
           "XSET-UNION"

            ;; sparse set implementation backed by a lightweight hashtable

           "COPY-SSET"
           "DO-SSET-ELEMENTS"
           "MAKE-SSET"
           "SSET" "SSET-ELEMENT"
           "SSET-ADJOIN" "SSET-DELETE" "SSET-EMPTY" "SSET-COUNT"
           "SSET-MEMBER"

            ;; communication between the runtime and Lisp

           "*CORE-STRING*"

            ;; INFO stuff doesn't belong in a user-visible package, we
            ;; should be able to change it without apology.

           "*INFO-ENVIRONMENT*"
           "*RECOGNIZED-DECLARATIONS*"
           "+INFOS-PER-WORD+"
           "+FDEFN-INFO-NUM+"
           "PACKED-INFO"
           "+NIL-PACKED-INFOS+"
           "ATOMIC-SET-INFO-VALUE"
           "CALL-WITH-EACH-GLOBALDB-NAME"
           "CLEAR-INFO"
           "CLEAR-INFO-VALUES"
           "DEFINE-INFO-TYPE"
           "FIND-FDEFN"
           "SYMBOL-FDEFN"
           "GET-INFO-VALUE-INITIALIZING"
           "GLOBALDB-SXHASHOID"
           "GLOBAL-FTYPE"
           "INFO"
           "INFO-FIND-AUX-KEY/PACKED"
           "INFO-GETHASH"
           "INFO-MAPHASH"
           "INFO-NUMBER"
           "INFO-NUMBER-BITS"
           "PACKED-INFO-FDEFN"
           "MAKE-INFO-HASHTABLE"
           "META-INFO"
           "META-INFO-NUMBER"
           "PACKED-INFO-FIELD"
           "PACKED-INFO-INSERT"
           "PCL-METHODFN-NAME-P"
           "SET-INFO-VALUE"
           "SHOW-INFO"
           "UPDATE-SYMBOL-INFO"
           "WITH-GLOBALDB-NAME"
           "%BOUNDP"

            ;; Calling a list of hook functions, plus error handling.

           "CALL-HOOKS"
            ;; Constant form evaluation

           "CONSTANT-FORM-VALUE"

            ;; stepping control

           "*STEPPING*"

            ;; packages grabbed once and for all

           "*KEYWORD-PACKAGE*" "*CL-PACKAGE*"

            ;; hash mixing operations

           "MIX" "MIXF" "WORD-MIX"

            ;; Macroexpansion that doesn't touch special forms

           "%MACROEXPAND"
           "%MACROEXPAND-1"

           "*SETF-FDEFINITION-HOOK*"

            ;; error-reporting facilities

           "ARGUMENTS-OUT-OF-DOMAIN-ERROR"
           "CLOSED-STREAM-ERROR" "CLOSED-SAVED-STREAM-ERROR"
           "COMPILED-PROGRAM-ERROR"
           "COMPILER-MACRO-KEYWORD-PROBLEM"
           "ENCAPSULATED-CONDITION"
           "INVALID-ARRAY-ERROR"
           "INVALID-ARRAY-INDEX-ERROR"
           "INVALID-ARRAY-P"
           "SIMPLE-CONTROL-ERROR" "SIMPLE-FILE-ERROR"
           "SIMPLE-PARSE-ERROR"
           "SIMPLE-PROGRAM-ERROR" "%PROGRAM-ERROR"
           "SIMPLE-READER-ERROR"
           "SIMPLE-READER-PACKAGE-ERROR"
           "SIMPLE-REFERENCE-ERROR"
           "SIMPLE-REFERENCE-WARNING"
           "SIMPLE-STREAM-ERROR"
           "SIMPLE-STORAGE-CONDITION"
           "SIMPLE-STYLE-WARNING"
           "STREAM-ERROR-POSITION-INFO"
           "TRY-RESTART"

           "BROKEN-PIPE"

            ;; error-signalling facilities

           "STANDARD-READTABLE-MODIFIED-ERROR"
           "STANDARD-PPRINT-DISPATCH-TABLE-MODIFIED-ERROR"
           "ARRAY-BOUNDING-INDICES-BAD-ERROR"
           "CIRCULAR-LIST-ERROR"
           "SEQUENCE-BOUNDING-INDICES-BAD-ERROR"
           "SPECIAL-FORM-FUNCTION"
           "STYLE-WARN" "SIMPLE-COMPILER-NOTE"
           "TWO-ARG-CHAR-EQUAL" "TWO-ARG-CHAR-NOT-EQUAL"
           "TWO-ARG-CHAR-LESSP" "TWO-ARG-CHAR-NOT-LESSP"
           "TWO-ARG-CHAR-GREATERP" "TWO-ARG-CHAR-NOT-GREATERP"
           "CHAR-CASE-INFO"
            ;; FIXME: potential SB-EXT exports

           "CHARACTER-CODING-ERROR"
           "CHARACTER-DECODING-ERROR" "CHARACTER-DECODING-ERROR-OCTETS"
           "CHARACTER-ENCODING-ERROR" "CHARACTER-ENCODING-ERROR-CODE"
           "STREAM-DECODING-ERROR" "STREAM-ENCODING-ERROR"
           "C-STRING-ENCODING-ERROR"
           "C-STRING-DECODING-ERROR"
           "ATTEMPT-RESYNC" "FORCE-END-OF-FILE"

            ;; bootstrapping magic, to make things happen both in
            ;; the cross-compilation host compiler's environment and
            ;; in the cross-compiler's environment

           "DEF!STRUCT" "DEF!TYPE"
           "*!REMOVABLE-SYMBOLS*"

            ;; bootstrapping macro whose effect is to delay until warm load.

           "!SET-LOAD-FORM-METHOD"

            ;; stuff for hinting to the compiler

           "NAMED-DS-BIND"
           "NAMED-LAMBDA"

            ;; other variations on DEFFOO stuff useful for bootstrapping
            ;; and cross-compiling

           "DEFCONSTANT-EQX"
           "DEFINE-UNSUPPORTED-FUN"

            ;; messing with PATHNAMEs

           "MAKE-TRIVIAL-DEFAULT-PATHNAME"
           "MAKE-TRIVIAL-DEFAULT-LOGICAL-PATHNAME"
           "PHYSICALIZE-PATHNAME"
           "SANE-DEFAULT-PATHNAME-DEFAULTS"
           "SBCL-HOMEDIR-PATHNAME"
           "SIMPLIFY-NAMESTRING"
           "DEBUG-SOURCE-NAMESTRING"
           "DEBUG-SOURCE-CREATED"

           "*N-BYTES-FREED-OR-PURIFIED*"

            ;; Deprecating stuff

           "DEFINE-DEPRECATED-FUNCTION"
           "DEFINE-DEPRECATED-VARIABLE"
           "DEPRECATION-STATE"
           "DEPRECATION-INFO" "MAKE-DEPRECATION-INFO"
           "DEPRECATION-INFO-STATE"
           "DEPRECATION-INFO-SOFTWARE"
           "DEPRECATION-INFO-VERSION"
           "DEPRECATION-INFO-REPLACEMENTS"
           "PRINT-DEPRECATION-MESSAGE"
           "CHECK-DEPRECATED-THING" "CHECK-DEPRECATED-TYPE"
           "DEPRECATED-THING-P"
           "DEPRECATION-WARN"
           "LOADER-DEPRECATION-WARN"

            ;; miscellaneous non-standard but handy user-level functions..

           "ASSQ" "DELQ" "DELQ1" "MEMQ" "POSQ" "NEQ"
           "ADJUST-LIST"
           "ALIGN-UP"
           "%FIND-PACKAGE-OR-LOSE" "FIND-UNDELETED-PACKAGE-OR-LOSE"
           "SANE-PACKAGE"
           "COALESCE-TREE-P"
           "COMPOUND-OBJECT-P"
           "SWAPPED-ARGS-FUN"
           "AND/TYPE" "NOT/TYPE"
           "ANY/TYPE" "EVERY/TYPE"
           "EQUAL-BUT-NO-CAR-RECURSION"
           "TYPE-BOUND-NUMBER" "COERCE-NUMERIC-BOUND"
           "CONSTANTLY-T" "CONSTANTLY-NIL" "CONSTANTLY-0"
           "PSXHASH"
           "%BREAK"
           "BIT-VECTOR-="
           "PATHNAME="
           "%HASH-TABLE-ALIST"
           "HASH-TABLE-EQUALP"
           "READ-EVALUATED-FORM" "READ-EVALUATED-FORM-OF-TYPE"
           "PICK-BEST-SXHASH-BITS"

           "MAKE-UNPRINTABLE-OBJECT"
           "POSSIBLY-BASE-STRINGIZE"
           "POWER-OF-TWO-CEILING"
           "PRINT-NOT-READABLE-ERROR"
           "HASH-TABLE-REPLACE"
           "RECONS"
           "SET-CLOSURE-NAME"

            ;; ..and macros..

           "COLLECT"
           "COPY-LIST-MACRO"
           "DO-ANONYMOUS" "DOVECTOR" "DOHASH" "DOPLIST"
           "ENSURE-GETHASH"
           "GET-SIMILAR"
           "NAMED-LET"
           "ONCE-ONLY"
           "DEFENUM"
           "DEFPRINTER"
           "*PRINT-IR-NODES-PRETTY*"
           "AVER"
           "DX-FLET" "DX-LET"
           "AWHEN" "ACOND" "IT"
           "BINDING*" "EXTRACT-VAR-DECLS"
           "!DEF-BOOLEAN-ATTRIBUTE"
           "FUNARG-BIND/CALL-FORMS"
           "QUASIQUOTE"
           "COMMA-P"
           "COMMA-EXPR"
           "COMMA-KIND"
           "UNQUOTE"
           "PACKAGE-HASHTABLE"
           "PACKAGE-ITER-STEP"
           "WITH-REBOUND-IO-SYNTAX"
           "WITH-SANE-IO-SYNTAX"
           "WITH-PROGRESSIVE-TIMEOUT"

            ;; ..and CONDITIONs..

           "BUG"
           "UNSUPPORTED-OPERATOR"

           "BOOTSTRAP-PACKAGE-NAME"
           "BOOTSTRAP-PACKAGE-NOT-FOUND"
           "DEBOOTSTRAP-PACKAGE"
           "SYSTEM-PACKAGE-P"

           "REFERENCE-CONDITION" "REFERENCE-CONDITION-REFERENCES"
           "*PRINT-CONDITION-REFERENCES*"

           "DUPLICATE-DEFINITION" "DUPLICATE-DEFINITION-NAME"
           "SAME-FILE-REDEFINITION-WARNING"
           "PACKAGE-AT-VARIANCE"
           "PACKAGE-AT-VARIANCE-ERROR"
           "ARRAY-INITIAL-ELEMENT-MISMATCH"
           "INITIAL-ELEMENT-MISMATCH-STYLE-WARNING"
           "TYPE-WARNING" "TYPE-STYLE-WARNING"
           "SLOT-INITFORM-TYPE-STYLE-WARNING"
           "LOCAL-ARGUMENT-MISMATCH"
           "FORMAT-ARGS-MISMATCH" "FORMAT-TOO-FEW-ARGS-WARNING"
           "FORMAT-TOO-MANY-ARGS-WARNING" "EXTENSION-FAILURE"
           "STRUCTURE-INITARG-NOT-KEYWORD" "CONSTANT-MODIFIED"

            ;; ..and DEFTYPEs..

           "HASH-CODE"
           "INDEX" "LOAD/STORE-INDEX"
           "SIGNED-BYTE-WITH-A-BITE-OUT"
           "UNSIGNED-BYTE-WITH-A-BITE-OUT"
           "SFUNCTION" "UNSIGNED-BYTE*"
           "CONSTANT-DISPLACEMENT"
           "EXTENDED-FUNCTION-DESIGNATOR"
           "EXTENDED-FUNCTION-DESIGNATOR-P"
            ;; ..and type predicates

           "DOUBLE-FLOAT-P"
           "LOGICAL-PATHNAME-P"
           "LONG-FLOAT-P"
           "SHORT-FLOAT-P"
           "SINGLE-FLOAT-P"
           "FIXNUMP"
           "BIGNUMP"
           "RATIOP"
           "UNBOUND-MARKER-P"

            ;; encapsulation

           "ENCAPSULATE" "ENCAPSULATED-P"
           "UNENCAPSULATE"
           "ENCAPSULATE-FUNOBJ"

            ;; various CHAR-CODEs

           "BACKSPACE-CHAR-CODE" "BELL-CHAR-CODE" "ESCAPE-CHAR-CODE"
           "FORM-FEED-CHAR-CODE" "LINE-FEED-CHAR-CODE"
           "RETURN-CHAR-CODE" "RUBOUT-CHAR-CODE" "TAB-CHAR-CODE"

            ;; symbol-hacking idioms

           "GENSYMIFY" "GENSYMIFY*" "KEYWORDICATE" "SYMBOLICATE"
           "INTERNED-SYMBOL-P" "PACKAGE-SYMBOLICATE"
           "LOGICALLY-READONLYIZE"

            ;; certainly doesn't belong in public extensions
            ;; FIXME: maybe belongs in %KERNEL with other typesystem stuff?

           "CONSTANT-ARG"

            ;; various internal defaults

           "*LOAD-SOURCE-DEFAULT-TYPE*" "BASE-CHAR-CODE-LIMIT"

            ;; hash caches

           "DEFUN-CACHED"
           "DROP-ALL-HASH-CACHES"

            ;; time

           "FORMAT-DECODED-TIME"
           "FORMAT-UNIVERSAL-TIME"

            ;; used for FORMAT tilde paren

           "MAKE-CASE-FROB-STREAM"

            ;; helpers for C library calls

           "STRERROR" "SIMPLE-PERROR"

            ;; debuggers' little helpers

           #+sb-show "*/SHOW*"
           #+sb-show "HEXSTR"
           "/SHOW"  "/NOSHOW"
           "/SHOW0" "/NOSHOW0"

            ;; cross-compilation bootstrap hacks which turn into
            ;; placeholders in a target system

           "UNCROSS"
           "!UNCROSS-FORMAT-CONTROL"

            ;; might as well be shared among the various files which
            ;; need it:

           "*EOF-OBJECT*"

            ;; allocation to static space

           "MAKE-STATIC-VECTOR"

            ;; alien interface utilities

           "C-STRINGS->STRING-LIST"

            ;; misc. utilities used internally

           "ADDRESS-BASED-COUNTER-VAL"
           "DEFINE-FUNCTION-NAME-SYNTAX" "VALID-FUNCTION-NAME-P" ; should be SB-EXT?

           "LEGAL-VARIABLE-NAME-P"
           "LEGAL-FUN-NAME-P" "LEGAL-FUN-NAME-OR-TYPE-ERROR"
           "LEGAL-CLASS-NAME-P"
           "FUN-NAME-BLOCK-NAME"
           "FUN-NAME-INLINE-EXPANSION"
           "LISTEN-SKIP-WHITESPACE"
           "PACKAGE-INTERNAL-SYMBOL-COUNT" "PACKAGE-EXTERNAL-SYMBOL-COUNT"
           "PARSE-BODY" "PARSE-LAMBDA-LIST" "MAKE-LAMBDA-LIST"
           "CHECK-DESIGNATOR"
           "CHECK-LAMBDA-LIST-NAMES"
           "LAMBDA-LIST-KEYWORD-MASK"
           "PARSE-KEY-ARG-SPEC" "PARSE-OPTIONAL-ARG-SPEC"
           "LL-KWDS-RESTP" "LL-KWDS-KEYP" "LL-KWDS-ALLOWP"
           "MAKE-MACRO-LAMBDA"
           "PROPER-LIST-OF-LENGTH-P" "PROPER-LIST-P"
           "LIST-OF-LENGTH-AT-LEAST-P" "SEQUENCE-OF-LENGTH-AT-LEAST-P"
           "SINGLETON-P" "ENSURE-LIST"
           "MISSING-ARG"
           "FEATUREP"
           "FLUSH-STANDARD-OUTPUT-STREAMS"
           "WITH-UNIQUE-NAMES" "MAKE-GENSYM-LIST"
           "ABOUT-TO-MODIFY-SYMBOL-VALUE"
           "SELF-EVALUATING-P"
           "PRINT-PRETTY-ON-STREAM-P"
           "ARRAY-READABLY-PRINTABLE-P"
           "LOOKS-LIKE-NAME-OF-SPECIAL-VAR-P"
           "POSITIVE-PRIMEP"
           "EVAL-IN-LEXENV"
           "SIMPLE-EVAL-IN-LEXENV"
           "FORCE" "DELAY" "PROMISE-READY-P"
           "FIND-RESTART-OR-CONTROL-ERROR"
           "LOAD-AS-SOURCE"
           "DESCRIPTOR-SAP"
           "DO-PACKED-VARINTS"

           "CLOSURE-EXTRA-VALUES"
           "PACK-CLOSURE-EXTRA-VALUES"
           "SET-CLOSURE-EXTRA-VALUES"
           "+CLOSURE-NAME-INDEX+"

            ;; These could be moved back into SB-EXT if someone has
            ;; compelling reasons, but hopefully we can get by
            ;; without supporting them, at least not as publicly
            ;; accessible things with fixed interfaces.

           "GET-FLOATING-POINT-MODES"
           "SET-FLOATING-POINT-MODES"
           "WITH-FLOAT-TRAPS-MASKED"

            ;; a sort of quasi-unbound tag for use in hash tables

           "+EMPTY-HT-SLOT+"

            ;; low-level i/o stuff

           "DONE-WITH-FAST-READ-CHAR"
           "FAST-READ-BYTE"
           "FAST-READ-BYTE-REFILL"
           "FAST-READ-CHAR"
           "FAST-READ-CHAR-REFILL"
           "FAST-READ-S-INTEGER"
           "FAST-READ-U-INTEGER"
           "FAST-READ-VAR-U-INTEGER"
           "FILE-NAME"
           "FORM-TRACKING-STREAM"
           "FORM-TRACKING-STREAM-OBSERVER"
           "FORM-TRACKING-STREAM-P"
           "FORM-TRACKING-STREAM-FORM-START-BYTE-POS"
           "FORM-TRACKING-STREAM-FORM-START-CHAR-POS"
           "LINE/COL-FROM-CHARPOS"
           "%INTERN"
           "WITH-FAST-READ-BYTE"
           "PREPARE-FOR-FAST-READ-CHAR"
           "OUT-STREAM-FROM-DESIGNATOR"
           "STRINGIFY-OBJECT"
           "%WRITE"

            ;; hackery to help set up for cold init

           "!BEGIN-COLLECTING-COLD-INIT-FORMS"
           "!COLD-INIT-FORMS"
           "!DEFUN-FROM-COLLECTED-COLD-INIT-FORMS"

            ;; catch tags

           "TOPLEVEL-CATCHER"

            ;; hooks for contrib/ stuff we're insufficiently sure
            ;; about to add to SB-EXT

           "*REPL-PROMPT-FUN*"
           "*REPL-READ-FORM-FUN*"

            ;; Character database

           "**CHARACTER-PRIMARY-COMPOSITIONS**"
           "**CHARACTER-COLLATIONS**"

            ;; Character database access

           "MISC-INDEX"
           "CLEAR-FLAG"
           "PACK-3-CODEPOINTS"

            ;; Huffman trees

           "HUFFMAN-ENCODE"
           "HUFFMAN-DECODE"
           "BINARY-SEARCH"
           "DOUBLE-VECTOR-BINARY-SEARCH"))

(defpackage* "SB-MOP"
  (:documentation
   "public: the MetaObject Protocol interface, as defined by
The Art of the Metaobject Protocol, by Kiczales, des Rivieres and Bobrow:
ISBN 0-262-61074-4, with exceptions as noted in the User Manual.")
  (:use "CL")
  (:reexport "ADD-METHOD"
             "ALLOCATE-INSTANCE"
             "CLASS-NAME" "COMPUTE-APPLICABLE-METHODS"
             "ENSURE-GENERIC-FUNCTION" "MAKE-INSTANCE"
             "METHOD-QUALIFIERS" "REMOVE-METHOD"
             "BUILT-IN-CLASS" "CLASS"
             "FUNCTION" "GENERIC-FUNCTION"
             "METHOD" "METHOD-COMBINATION"
             "STANDARD-CLASS" "STANDARD-GENERIC-FUNCTION"
             "STANDARD-METHOD" "STANDARD-OBJECT" "T")
  (:export "ADD-DEPENDENT"
           "ADD-DIRECT-METHOD"
           "ADD-DIRECT-SUBCLASS"
           "CLASS-DEFAULT-INITARGS"
           "CLASS-DIRECT-DEFAULT-INITARGS"
           "CLASS-DIRECT-SLOTS"
           "CLASS-DIRECT-SUBCLASSES"
           "CLASS-DIRECT-SUPERCLASSES"
           "CLASS-FINALIZED-P"
           "CLASS-PRECEDENCE-LIST"
           "CLASS-PROTOTYPE"
           "CLASS-SLOTS"
           "COMPUTE-APPLICABLE-METHODS-USING-CLASSES"
           "COMPUTE-CLASS-PRECEDENCE-LIST"
           "COMPUTE-DEFAULT-INITARGS"
           "COMPUTE-DISCRIMINATING-FUNCTION"
           "COMPUTE-EFFECTIVE-METHOD"
           "COMPUTE-EFFECTIVE-SLOT-DEFINITION"
           "COMPUTE-SLOTS"
           "DIRECT-SLOT-DEFINITION"
           "DIRECT-SLOT-DEFINITION-CLASS"
           "EFFECTIVE-SLOT-DEFINITION"
           "EFFECTIVE-SLOT-DEFINITION-CLASS"
           "ENSURE-CLASS"
           "ENSURE-CLASS-USING-CLASS"
           "ENSURE-GENERIC-FUNCTION-USING-CLASS"
           "EQL-SPECIALIZER"
           "EQL-SPECIALIZER-OBJECT"
           "EXTRACT-LAMBDA-LIST"
           "EXTRACT-SPECIALIZER-NAMES"
           "FINALIZE-INHERITANCE"
           "FIND-METHOD-COMBINATION"
           "FORWARD-REFERENCED-CLASS"
           "FUNCALLABLE-STANDARD-CLASS"
           "FUNCALLABLE-STANDARD-INSTANCE-ACCESS"
           "FUNCALLABLE-STANDARD-OBJECT"
           "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"
           "GENERIC-FUNCTION-DECLARATIONS"
           "GENERIC-FUNCTION-LAMBDA-LIST"
           "GENERIC-FUNCTION-METHOD-CLASS"
           "GENERIC-FUNCTION-METHOD-COMBINATION"
           "GENERIC-FUNCTION-METHODS"
           "GENERIC-FUNCTION-NAME"
           "INTERN-EQL-SPECIALIZER"
           "MAKE-METHOD-LAMBDA"
           "MAP-DEPENDENTS"
           "METAOBJECT"
           "METHOD-FUNCTION"
           "METHOD-GENERIC-FUNCTION"
           "METHOD-LAMBDA-LIST"
           "METHOD-SPECIALIZERS"
           "ACCESSOR-METHOD-SLOT-DEFINITION"
           "READER-METHOD-CLASS"
           "REMOVE-DEPENDENT"
           "REMOVE-DIRECT-METHOD"
           "REMOVE-DIRECT-SUBCLASS"
           "SET-FUNCALLABLE-INSTANCE-FUNCTION"
           "SLOT-BOUNDP-USING-CLASS"
           "SLOT-DEFINITION"
           "SLOT-DEFINITION-ALLOCATION"
           "SLOT-DEFINITION-INITARGS"
           "SLOT-DEFINITION-INITFORM"
           "SLOT-DEFINITION-INITFUNCTION"
           "SLOT-DEFINITION-LOCATION"
           "SLOT-DEFINITION-NAME"
           "SLOT-DEFINITION-READERS"
           "SLOT-DEFINITION-WRITERS"
           "SLOT-DEFINITION-TYPE"
           "SLOT-MAKUNBOUND-USING-CLASS"
           "SLOT-VALUE-USING-CLASS"
           "SPECIALIZER"
           "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
           "SPECIALIZER-DIRECT-METHODS"
           "STANDARD-ACCESSOR-METHOD"
           "STANDARD-DIRECT-SLOT-DEFINITION"
           "STANDARD-EFFECTIVE-SLOT-DEFINITION"
           "STANDARD-INSTANCE-ACCESS"
           "STANDARD-READER-METHOD"
           "STANDARD-SLOT-DEFINITION"
           "STANDARD-WRITER-METHOD"
           "UPDATE-DEPENDENT"
           "VALIDATE-SUPERCLASS"
           "WRITER-METHOD-CLASS"))

(defpackage* "SB-PCL"
  (:documentation
   "semi-public: This package includes useful meta-object protocol
extensions, but even they are not guaranteed to be present in later
versions of SBCL, and the other stuff in here is definitely not
guaranteed to be present in later versions of SBCL.  Use of this
package is deprecated in favour of SB-MOP.")
  (:use "CL" "SB-MOP" "SB-INT" "SB-EXT" "SB-WALKER" "SB-KERNEL")
  ;; experimental SBCL-only (for now) symbols
  (:export "SYSTEM-CLASS"

           "MAKE-METHOD-LAMBDA-USING-SPECIALIZERS"
           "MAKE-METHOD-SPECIALIZERS-FORM"

           "SPECIALIZER-TYPE-SPECIFIER"
           "MAKE-SPECIALIZER-FORM-USING-CLASS"
           "PARSE-SPECIALIZER-USING-CLASS"
           "UNPARSE-SPECIALIZER-USING-CLASS"

           "+SLOT-UNBOUND+"

           "ENSURE-CLASS-FINALIZED"

           "ILLEGAL-CLASS-NAME-ERROR"
           "CLASS-NOT-FOUND-ERROR"
           "SPECIALIZER-NAME-SYNTAX-ERROR"))

(defpackage* "SB-RBTREE"
  (:documentation "internal: red/black tree")
  (:use "CL" "SB-INT" "SB-EXT")
  (:shadow "DELETE")
  (:export "INSERT" "DELETE" "FIND=" "FIND<="))

(defpackage* "SB-LOCKLESS"
  (:documentation "internal: lockfree lists")
  (:use "CL" "SB-INT" "SB-EXT" "SB-SYS" "SB-KERNEL")
  (:shadow "ENDP"))
