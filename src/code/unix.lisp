;;;; This file contains Unix support that SBCL needs to implement
;;;; itself. It's derived from Peter Van Eynde's unix-glibc2.lisp for
;;;; CMU CL, which was derived from CMU CL unix.lisp 1.56. But those
;;;; files aspired to be complete Unix interfaces exported to the end
;;;; user, while this file aims to be as simple as possible and is not
;;;; intended for the end user.
;;;;
;;;; FIXME: The old CMU CL unix.lisp code was implemented as hand
;;;; transcriptions from Unix headers into Lisp. It appears that this was as
;;;; unmaintainable in practice as you'd expect in theory, so I really really
;;;; don't want to do that. It'd be good to implement the various system calls
;;;; as C code implemented using the Unix header files, and have their
;;;; interface back to SBCL code be characterized by things like "32-bit-wide
;;;; int" which are already in the interface between the runtime
;;;; executable and the SBCL lisp code.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!UNIX")

(/show0 "unix.lisp 21")

;;; Given a C-level zero-terminated array of C strings, return a
;;; corresponding Lisp-level list of SIMPLE-STRINGs.
(defun c-strings->string-list (c-strings)
  (declare (type (alien (* c-string)) c-strings))
  (let ((reversed-result nil))
    (dotimes (i most-positive-fixnum (error "argh! can't happen"))
      (declare (type index i))
      (let ((c-string (deref c-strings i)))
        (if c-string
            (push c-string reversed-result)
            (return (nreverse reversed-result)))))))

;;;; Lisp types used by syscalls

(deftype unix-pathname () 'simple-string)
(deftype unix-fd () `(integer 0 ,sb!xc:most-positive-fixnum))

(deftype unix-file-mode () '(unsigned-byte 32))
(deftype unix-pid () '(unsigned-byte 32))
(deftype unix-uid () '(unsigned-byte 32))
(deftype unix-gid () '(unsigned-byte 32))

;;;; system calls

(/show0 "unix.lisp 74")

;;; FIXME: The various FOO-SYSCALL-BAR macros, and perhaps some other
;;; macros in this file, are only used in this file, and could be
;;; implemented using SB!XC:DEFMACRO wrapped in EVAL-WHEN.
;;;
;;; SB-EXECUTABLE, at least, uses one of these macros; other libraries
;;; and programs have been known to use them as well.  Perhaps they
;;; should live in SB-SYS or even SB-EXT?

(defmacro syscall ((name &rest arg-types) success-form &rest args)
  (when (eql 3 (mismatch "[_]" name))
    (setf name
          (concatenate 'string #!+win32 "_" (subseq name 3))))
  `(locally
    (declare (optimize (sb!c::float-accuracy 0)))
    (let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
                                ,@args)))
      (if (minusp result)
          (values nil (get-errno))
          ,success-form))))

;;; This is like SYSCALL, but if it fails, signal an error instead of
;;; returning error codes. Should only be used for syscalls that will
;;; never really get an error.
(defmacro syscall* ((name &rest arg-types) success-form &rest args)
  `(locally
    (declare (optimize (sb!c::float-accuracy 0)))
    (let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
                                 ,@args)))
      (if (minusp result)
          (error "Syscall ~A failed: ~A" ,name (strerror))
          ,success-form))))

(defmacro int-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values result 0) ,@args))

(defmacro with-restarted-syscall ((&optional (value (gensym))
                                             (errno (gensym)))
                                  syscall-form &rest body)
  #!+sb-doc
  "Evaluate BODY with VALUE and ERRNO bound to the return values of
SYSCALL-FORM. Repeat evaluation of SYSCALL-FORM if it is interrupted."
  `(let (,value ,errno)
     (loop (multiple-value-setq (,value ,errno)
             ,syscall-form)
        (unless #!-win32 (eql ,errno eintr) #!+win32 nil
          (return (values ,value ,errno))))
     ,@body))

(defmacro void-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values t 0) ,@args))

#!+win32
(progn
  (defconstant espipe 29))

;;;; hacking the Unix environment

#!-win32
(define-alien-routine ("getenv" posix-getenv) c-string
  #!+sb-doc
  "Return the \"value\" part of the environment string \"name=value\" which
corresponds to NAME, or NIL if there is none."
  (name (c-string :not-null t)))

;;; from stdio.h

;;; Rename the file with string NAME1 to the string NAME2. NIL and an
;;; error code is returned if an error occurs.
#!-win32
(defun unix-rename (name1 name2)
  (declare (type unix-pathname name1 name2))
  (void-syscall ("rename" (c-string :not-null t)
                          (c-string :not-null t))
                name1 name2))

;;; from sys/types.h and gnu/types.h

(/show0 "unix.lisp 220")

;;; FIXME: We shouldn't hand-copy types from header files into Lisp
;;; like this unless we have extreme provocation. Reading directories
;;; is not extreme enough, since it doesn't need to be blindingly
;;; fast: we can just implement those functions in C as a wrapper
;;; layer.
(define-alien-type fd-mask unsigned)

(define-alien-type nil
  (struct fd-set
          (fds-bits (array fd-mask #.(/ fd-setsize
                                        sb!vm:n-machine-word-bits)))))

(/show0 "unix.lisp 304")


;;;; fcntl.h
;;;;
;;;; POSIX Standard: 6.5 File Control Operations        <fcntl.h>

;;; Open the file whose pathname is specified by PATH for reading
;;; and/or writing as specified by the FLAGS argument. Various FLAGS
;;; masks (O_RDONLY etc.) are defined in fcntlbits.h.
;;;
;;; If the O_CREAT flag is specified, then the file is created with a
;;; permission of argument MODE if the file doesn't exist. An integer
;;; file descriptor is returned by UNIX-OPEN.
(defun unix-open (path flags mode)
  (declare (type unix-pathname path)
           (type fixnum flags)
           (type unix-file-mode mode))
  #!+win32 (sb!win32:unixlike-open path flags mode)
  #!-win32
  (with-restarted-syscall (value errno)
    (int-syscall ("open" c-string int int)
                 path
                 (logior #!+win32 o_binary
                         #!+largefile o_largefile
                         flags)
                 mode)))

;;; UNIX-CLOSE accepts a file descriptor and attempts to close the file
;;; associated with it.
(/show0 "unix.lisp 391")
(defun unix-close (fd)
  #!+win32 (sb!win32:unixlike-close fd)
  #!-win32 (declare (type unix-fd fd))
  #!-win32 (void-syscall ("close" int) fd))

;;;; stdlib.h

;;; There are good reasons to implement some OPEN options with an
;;; mkstemp(3)-like routine, but we don't do that yet.  Instead, this
;;; function is used only to make a temporary file for RUN-PROGRAM.
;;; sb_mkstemp() is a wrapper that lives in src/runtime/wrap.c.  Since
;;; SUSv3 mkstemp() doesn't specify the mode of the created file and
;;; since we have to implement most of this ourselves for Windows
;;; anyway, it seems worthwhile to depart from the mkstemp()
;;; specification by taking a mode to use when creating the new file.
(defun sb-mkstemp (template-string mode)
  (declare (type string template-string)
           (type unix-file-mode mode))
  (let ((template-buffer (string-to-octets template-string :null-terminate t)))
    (with-pinned-objects (template-buffer)
      (let ((fd (alien-funcall (extern-alien "sb_mkstemp"
                                             (function int (* char) int))
                               (vector-sap template-buffer)
                               mode)))
        (if (minusp fd)
            (values nil (get-errno))
            (values #!-win32 fd #!+win32 (sb!win32::duplicate-and-unwrap-fd fd)
                    (octets-to-string template-buffer)))))))

;;;; resourcebits.h

(defconstant rusage_self 0) ; the calling process
(defconstant rusage_children -1) ; terminated child processes
(defconstant rusage_both -2)

(define-alien-type nil
  (struct rusage
    (ru-utime (struct timeval))     ; user time used
    (ru-stime (struct timeval))     ; system time used.
    (ru-maxrss long)                ; maximum resident set size (in kilobytes)
    (ru-ixrss long)                 ; integral shared memory size
    (ru-idrss long)                 ; integral unshared data size
    (ru-isrss long)                 ; integral unshared stack size
    (ru-minflt long)                ; page reclaims
    (ru-majflt long)                ; page faults
    (ru-nswap long)                 ; swaps
    (ru-inblock long)               ; block input operations
    (ru-oublock long)               ; block output operations
    (ru-msgsnd long)                ; messages sent
    (ru-msgrcv long)                ; messages received
    (ru-nsignals long)              ; signals received
    (ru-nvcsw long)                 ; voluntary context switches
    (ru-nivcsw long)))              ; involuntary context switches

;;;; unistd.h

;;; Given a file path (a string) and one of four constant modes,
;;; return T if the file is accessible with that mode and NIL if not.
;;; When NIL, also return an errno value with NIL which tells why the
;;; file was not accessible.
;;;
;;; The access modes are:
;;;   r_ok     Read permission.
;;;   w_ok     Write permission.
;;;   x_ok     Execute permission.
;;;   f_ok     Presence of file.

;;; In Windows, the MODE argument to access is defined in terms of
;;; literal magic numbers---there are no constants to grovel.  X_OK
;;; is not defined.
#!+win32
(progn
  (defconstant f_ok 0)
  (defconstant w_ok 2)
  (defconstant r_ok 4))

(defun unix-access (path mode)
  (declare (type unix-pathname path)
           (type (mod 8) mode))
  (void-syscall ("[_]access" c-string int) path mode))

;;; values for the second argument to UNIX-LSEEK
;;; Note that nowadays these are called SEEK_SET, SEEK_CUR, and SEEK_END
(defconstant l_set 0) ; to set the file pointer
(defconstant l_incr 1) ; to increment the file pointer
(defconstant l_xtnd 2) ; to extend the file size

;; off_t is 32 bit on Windows, yet our functions support 64 bit seeks.
(define-alien-type unix-offset
  #!-win32 off-t
  #!+win32 (signed 64))

;;; Is a stream interactive?
(defun unix-isatty (fd)
  (declare (type unix-fd fd))
  #!-win32 (int-syscall ("isatty" int) fd)
  #!+win32 (sb!win32::windows-isatty fd))

(defun unix-lseek (fd offset whence)
  #!+sb-doc
  "Unix-lseek accepts a file descriptor and moves the file pointer by
   OFFSET octets.  Whence can be any of the following:

   L_SET        Set the file pointer.
   L_INCR       Increment the file pointer.
   L_XTND       Extend the file size.
  "
  (declare (type unix-fd fd)
           (type (integer 0 2) whence))
  (let ((result
         #!-win32
          (alien-funcall (extern-alien #!-largefile "lseek"
                                             #!+largefile "lseek_largefile"
                                             (function off-t int off-t int))
                        fd offset whence)
          #!+win32 (sb!win32:lseeki64 fd offset whence)))
    (if (minusp result)
        (values nil (get-errno))
      (values result 0))))

;;; UNIX-READ accepts a file descriptor, a buffer, and the length to read.
;;; It attempts to read len bytes from the device associated with fd
;;; and store them into the buffer. It returns the actual number of
;;; bytes read.

#!-sb!fluid
(declaim (maybe-inline unix-read))

(defun unix-read (fd buf len)
  (declare (type unix-fd fd)
           (type (unsigned-byte 32) len))
  (int-syscall (#!-win32 "read" #!+win32 "win32_unix_read"
                int (* char) int) fd buf len))

;;; UNIX-WRITE accepts a file descriptor, a buffer, an offset, and the
;;; length to write. It attempts to write len bytes to the device
;;; associated with fd from the buffer starting at offset. It returns
;;; the actual number of bytes written.
(defun unix-write (fd buf offset len)
  (declare (type unix-fd fd)
           (type (unsigned-byte 32) offset len))
  (flet ((%write (sap)
           (declare (system-area-pointer sap))
           (int-syscall (#!-win32 "write" #!+win32 "win32_unix_write"
                         int (* char) int)
                        fd
                        (with-alien ((ptr (* char) sap))
                          (addr (deref ptr offset)))
                        len)))
    (etypecase buf
      ((simple-array * (*))
       (with-pinned-objects (buf)
         (%write (vector-sap buf))))
      (system-area-pointer
       (%write buf)))))

;;; Set up a unix-piping mechanism consisting of an input pipe and an
;;; output pipe. Return two values: if no error occurred the first
;;; value is the pipe to be read from and the second is can be written
;;; to. If an error occurred the first value is NIL and the second the
;;; unix error code.
#!-win32
(defun unix-pipe ()
  (with-alien ((fds (array int 2)))
    (syscall ("pipe" (* int))
             (values (deref fds 0) (deref fds 1))
             (cast fds (* int)))))

#!+win32
(defun unix-pipe ()
  (sb!win32::windows-pipe))

;; Windows mkdir() doesn't take the mode argument. It's cdecl, so we could
;; actually call it passing the mode argument, but some sharp-eyed reader
;; would put five and twenty-seven together and ask us about it, so...
;;    -- AB, 2005-12-27
#!-win32
(defun unix-mkdir (name mode)
  (declare (type unix-pathname name)
           (type unix-file-mode mode)
           #!+win32 (ignore mode))
  (void-syscall ("mkdir" c-string #!-win32 int) name #!-win32 mode))

;;; Given a C char* pointer allocated by malloc(), free it and return a
;;; corresponding Lisp string (or return NIL if the pointer is a C NULL).
(defun newcharstar-string (newcharstar)
  (declare (type (alien (* char)) newcharstar))
  (if (null-alien newcharstar)
      nil
      (prog1
          (cast newcharstar c-string)
        (free-alien newcharstar))))

;;; Return the Unix current directory as a SIMPLE-STRING, in the
;;; style returned by getcwd() (no trailing slash character).
#!-win32
(defun posix-getcwd ()
  ;; This implementation relies on a BSD/Linux extension to getcwd()
  ;; behavior, automatically allocating memory when a null buffer
  ;; pointer is used. On a system which doesn't support that
  ;; extension, it'll have to be rewritten somehow.
  ;;
  ;; SunOS and OSF/1 provide almost as useful an extension: if given a null
  ;; buffer pointer, it will automatically allocate size space. The
  ;; KLUDGE in this solution arises because we have just read off
  ;; PATH_MAX+1 from the Solaris header files and stuck it in here as
  ;; a constant. Going the grovel_headers route doesn't seem to be
  ;; helpful, either, as Solaris doesn't export PATH_MAX from
  ;; unistd.h.
  ;;
  ;; Signal an error at compile-time, since it's needed for the
  ;; runtime to start up
  #!-(or android linux openbsd freebsd netbsd sunos osf1 darwin hpux win32 dragonfly)
  #.(error "POSIX-GETCWD is not implemented.")
  (or
   #!+(or linux openbsd freebsd netbsd sunos osf1 darwin hpux win32 dragonfly)
   (newcharstar-string (alien-funcall (extern-alien "getcwd"
                                                    (function (* char)
                                                              (* char)
                                                              size-t))
                                      nil
                                      #!+(or linux openbsd freebsd netbsd darwin win32 dragonfly) 0
                                      #!+(or sunos osf1 hpux) 1025))
   #!+android
   (with-alien ((ptr (array char #.path-max)))
     ;; Older bionic versions do not have the above feature.
     (alien-funcall
      (extern-alien "getcwd"
                    (function c-string (array char #.path-max) int))
      ptr path-max))
   (simple-perror "getcwd")))

;;; Return the Unix current directory as a SIMPLE-STRING terminated
;;; by a slash character.
(defun posix-getcwd/ ()
  (concatenate 'string (posix-getcwd) "/"))

;;; Duplicate an existing file descriptor (given as the argument) and
;;; return it. If FD is not a valid file descriptor, NIL and an error
;;; number are returned.
#!-win32
(defun unix-dup (fd)
  (declare (type unix-fd fd))
  (int-syscall ("dup" int) fd))

;;; Terminate the current process with an optional error code. If
;;; successful, the call doesn't return. If unsuccessful, the call
;;; returns NIL and an error number.
(deftype exit-code ()
  `(signed-byte 32))
(defun os-exit (code &key abort)
  #!+sb-doc
  "Exit the process with CODE. If ABORT is true, exit is performed using _exit(2),
avoiding atexit(3) hooks, etc. Otherwise exit(2) is called."
  (unless (typep code 'exit-code)
    (setf code (if abort 1 0)))
  (if abort
      (void-syscall ("_exit" int) code)
      (void-syscall ("exit" int) code)))

(define-deprecated-function :early "1.0.56.55" unix-exit os-exit (code)
  (os-exit code))

;;; Return the process id of the current process.
(define-alien-routine (#!+win32 "_getpid" #!-win32 "getpid" unix-getpid) int)

;;; Return the real user id associated with the current process.
#!-win32
(define-alien-routine ("getuid" unix-getuid) int)

;;; Translate a user id into a login name.
#!-win32
(defun uid-username (uid)
  (or (newcharstar-string (alien-funcall (extern-alien "uid_username"
                                                       (function (* char) int))
                                         uid))
      (error "found no match for Unix uid=~S" uid)))

;;; Return the namestring of the home directory, being careful to
;;; include a trailing #\/
#!-win32
(progn
  (defun uid-homedir (uid)
    (or (newcharstar-string (alien-funcall (extern-alien "uid_homedir"
                                                         (function (* char) int))
                                           uid))
        (error "failed to resolve home directory for Unix uid=~S" uid)))

  (defun user-homedir (uid)
    (or (newcharstar-string (alien-funcall (extern-alien "user_homedir"
                                                         (function (* char) c-string))
                                           uid))
        (error "failed to resolve home directory for Unix uid=~S" uid))))

;;; Invoke readlink(2) on the file name specified by PATH. Return
;;; (VALUES LINKSTRING NIL) on success, or (VALUES NIL ERRNO) on
;;; failure.
#!-win32
(defun unix-readlink (path)
  (declare (type unix-pathname path))
  (with-alien ((ptr (* char)
                    (alien-funcall (extern-alien
                                    "wrapped_readlink"
                                    (function (* char) c-string))
                                   path)))
    (if (null-alien ptr)
        (values nil (get-errno))
        (multiple-value-prog1
            (values (with-alien ((c-string c-string ptr)) c-string)
                    nil)
          (free-alien ptr)))))
#!+win32
;; Win32 doesn't do links, but something likes to call this anyway.
;; Something in this file, no less. But it only takes one result, so...
(defun unix-readlink (path)
  (declare (ignore path))
  nil)

(defun unix-realpath (path)
  (declare (type unix-pathname path))
  (with-alien ((ptr (* char)
                    (alien-funcall (extern-alien
                                    "sb_realpath"
                                    (function (* char) c-string))
                                   path)))
    (if (null-alien ptr)
        (values nil (get-errno))
        (multiple-value-prog1
            (values (with-alien ((c-string c-string ptr)) c-string)
                    nil)
          (free-alien ptr)))))

;;; UNIX-UNLINK accepts a name and deletes the directory entry for that
;;; name and the file if this is the last link.
(defun unix-unlink (name)
  (declare (type unix-pathname name))
  (void-syscall ("[_]unlink" c-string) name))

;;; Return the name of the host machine as a string.
#!-win32
(defun unix-gethostname ()
  (with-alien ((buf (array char 256)))
    (syscall ("gethostname" (* char) int)
             (cast buf c-string)
             (cast buf (* char)) 256)))

#!-win32
(defun unix-setsid ()
  (int-syscall ("setsid")))

;;;; sys/ioctl.h

;;; UNIX-IOCTL performs a variety of operations on open i/o
;;; descriptors. See the UNIX Programmer's Manual for more
;;; information.
#!-win32
(defun unix-ioctl (fd cmd arg)
  (declare (type unix-fd fd)
           (type (signed-byte 32) cmd))
  (void-syscall ("ioctl" int int (* char)) fd cmd arg))

;;;; sys/resource.h

;;; FIXME: All we seem to need is the RUSAGE_SELF version of this.
;;;
;;; This is like getrusage(2), except it returns only the system and
;;; user time, and returns the seconds and microseconds as separate
;;; values.
#!-sb-fluid (declaim (inline unix-fast-getrusage))
#!-win32
(defun unix-fast-getrusage (who)
  (declare (values (member t)
                   unsigned-byte fixnum
                   unsigned-byte fixnum))
  (with-alien ((usage (struct rusage)))
    (syscall* ("sb_getrusage" int (* (struct rusage)))
              (values t
                      (slot (slot usage 'ru-utime) 'tv-sec)
                      (slot (slot usage 'ru-utime) 'tv-usec)
                      (slot (slot usage 'ru-stime) 'tv-sec)
                      (slot (slot usage 'ru-stime) 'tv-usec))
              who (addr usage))))

;;; Return information about the resource usage of the process
;;; specified by WHO. WHO can be either the current process
;;; (rusage_self) or all of the terminated child processes
;;; (rusage_children). NIL and an error number is returned if the call
;;; fails.
#!-win32
(defun unix-getrusage (who)
  (with-alien ((usage (struct rusage)))
    (syscall ("sb_getrusage" int (* (struct rusage)))
              (values t
                      (+ (* (slot (slot usage 'ru-utime) 'tv-sec) 1000000)
                         (slot (slot usage 'ru-utime) 'tv-usec))
                      (+ (* (slot (slot usage 'ru-stime) 'tv-sec) 1000000)
                         (slot (slot usage 'ru-stime) 'tv-usec))
                      (slot usage 'ru-maxrss)
                      (slot usage 'ru-ixrss)
                      (slot usage 'ru-idrss)
                      (slot usage 'ru-isrss)
                      (slot usage 'ru-minflt)
                      (slot usage 'ru-majflt)
                      (slot usage 'ru-nswap)
                      (slot usage 'ru-inblock)
                      (slot usage 'ru-oublock)
                      (slot usage 'ru-msgsnd)
                      (slot usage 'ru-msgrcv)
                      (slot usage 'ru-nsignals)
                      (slot usage 'ru-nvcsw)
                      (slot usage 'ru-nivcsw))
              who (addr usage))))

(defvar *on-dangerous-wait* :warn)

;;; Calling select in a bad place can hang in a nasty manner, so it's better
;;; to have some way to detect these.
(defun note-dangerous-wait (type)
  (let ((action *on-dangerous-wait*)
        (*on-dangerous-wait* nil))
    (case action
      (:warn
       (warn "Starting a ~A without a timeout while interrupts are ~
             disabled."
             type))
      (:error
       (error "Starting a ~A without a timeout while interrupts are ~
              disabled."
              type))
      (:backtrace
       (format *debug-io*
               "~&=== Starting a ~A without a timeout while interrupts are disabled. ===~%"
               type)
       (sb!debug:print-backtrace)))
    nil))

;;;; poll.h
#!+os-provides-poll
(progn
  (define-alien-type nil
      (struct pollfd
              (fd      int)
              (events  short)           ; requested events
              (revents short)))         ; returned events

  (defun unix-poll (pollfds nfds to-msec)
    (declare (fixnum nfds to-msec))
    (when (and (minusp to-msec) (not *interrupts-enabled*))
      (note-dangerous-wait "poll(2)"))
    ;; FAST-SELECT doesn't use WITH-RESTARTED-SYSCALL so this doesn't either
    (int-syscall ("poll" (* (struct pollfd)) int int)
                 (alien-sap pollfds) nfds to-msec))

  ;; "simple" poll operates on a single descriptor only
  (defun unix-simple-poll (fd direction to-msec)
    (declare (fixnum fd to-msec))
    (when (and (minusp to-msec) (not *interrupts-enabled*))
      (note-dangerous-wait "poll(2)"))
    (let ((events (ecase direction
                    (:input (logior pollin pollpri))
                    (:output pollout))))
      (with-alien ((fds (struct pollfd)))
        (with-restarted-syscall (count errno)
          (progn
            (setf (slot fds 'fd) fd
                  (slot fds 'events) events
                  (slot fds 'revents) 0)
            (int-syscall ("poll" (* (struct pollfd)) int int)
                         (addr fds) 1 to-msec))
          (if (zerop errno)
              (let ((revents (slot fds 'revents)))
                (or (and (eql 1 count) (logtest events revents))
                    (logtest pollhup revents)))
              (error "Syscall poll(2) failed: ~A" (strerror))))))))

;;;; sys/select.h

(defmacro with-fd-setsize ((n) &body body)
  `(let ((,n (if (< 0 ,n fd-setsize)
                 ,n
                 (error "Cannot select(2) on ~D: above FD_SETSIZE limit."
                        (1- ,n)))))
     (declare (type (integer 0 #.fd-setsize) ,n))
     ,@body))

;;;; FIXME: Why have both UNIX-SELECT and UNIX-FAST-SELECT?

;;; Perform the UNIX select(2) system call.
(declaim (inline unix-fast-select))
(defun unix-fast-select (num-descriptors
                         read-fds write-fds exception-fds
                         timeout-secs timeout-usecs)
  (declare (type integer num-descriptors)
           (type (or (alien (* (struct fd-set))) null)
                 read-fds write-fds exception-fds)
           (type (or null (unsigned-byte 31)) timeout-secs timeout-usecs))
  (with-fd-setsize (num-descriptors)
    (flet ((select (tv-sap)
             (int-syscall ("sb_select" int (* (struct fd-set)) (* (struct fd-set))
                                    (* (struct fd-set)) (* (struct timeval)))
                          num-descriptors read-fds write-fds exception-fds
                          tv-sap)))
      (cond ((or timeout-secs timeout-usecs)
             (with-alien ((tv (struct timeval)))
               (setf (slot tv 'tv-sec) (or timeout-secs 0))
               (setf (slot tv 'tv-usec) (or timeout-usecs 0))
               (select (alien-sap (addr tv)))))
            (t
             (unless *interrupts-enabled*
               (note-dangerous-wait "select(2)"))
             (select (int-sap 0)))))))

;;; UNIX-SELECT accepts sets of file descriptors and waits for an event
;;; to happen on one of them or to time out.
(declaim (inline num-to-fd-set fd-set-to-num))
(defun num-to-fd-set (fdset num)
  (typecase num
    (fixnum
     (setf (deref (slot fdset 'fds-bits) 0) num)
     (loop for index from 1 below (/ fd-setsize
                                     sb!vm:n-machine-word-bits)
           do (setf (deref (slot fdset 'fds-bits) index) 0)))
    (t
     (loop for index from 0 below (/ fd-setsize
                                     sb!vm:n-machine-word-bits)
           do (setf (deref (slot fdset 'fds-bits) index)
                    (ldb (byte sb!vm:n-machine-word-bits
                               (* index sb!vm:n-machine-word-bits))
                         num))))))

(defun fd-set-to-num (nfds fdset)
  (if (<= nfds sb!vm:n-machine-word-bits)
      (deref (slot fdset 'fds-bits) 0)
      (loop for index below (/ fd-setsize
                               sb!vm:n-machine-word-bits)
            sum (ash (deref (slot fdset 'fds-bits) index)
                     (* index sb!vm:n-machine-word-bits)))))

;;; Examine the sets of descriptors passed as arguments to see whether
;;; they are ready for reading and writing. See the UNIX Programmer's
;;; Manual for more information.
(defun unix-select (nfds rdfds wrfds xpfds to-secs &optional (to-usecs 0))
  (declare (type integer nfds)
           (type unsigned-byte rdfds wrfds xpfds)
           (type (or (unsigned-byte 31) null) to-secs)
           (type (unsigned-byte 31) to-usecs)
           (optimize (speed 3) (safety 0)))
  (with-fd-setsize (nfds)
    (with-alien ((tv (struct timeval))
                 (rdf (struct fd-set))
                 (wrf (struct fd-set))
                 (xpf (struct fd-set)))
      (cond (to-secs
             (setf (slot tv 'tv-sec) to-secs
                   (slot tv 'tv-usec) to-usecs))
            ((not *interrupts-enabled*)
             (note-dangerous-wait "select(2)")))
      (num-to-fd-set rdf rdfds)
      (num-to-fd-set wrf wrfds)
      (num-to-fd-set xpf xpfds)
      (macrolet ((frob (lispvar alienvar)
                   `(if (zerop ,lispvar)
                        (int-sap 0)
                        (alien-sap (addr ,alienvar)))))
        (syscall ("sb_select" int (* (struct fd-set)) (* (struct fd-set))
                           (* (struct fd-set)) (* (struct timeval)))
                 (values result
                         (fd-set-to-num nfds rdf)
                         (fd-set-to-num nfds wrf)
                         (fd-set-to-num nfds xpf))
                 nfds (frob rdfds rdf) (frob wrfds wrf) (frob xpfds xpf)
                 (if to-secs (alien-sap (addr tv)) (int-sap 0)))))))

;;; Lisp-side implmentations of FD_FOO macros.
(declaim (inline fd-set fd-clr fd-isset fd-zero))
(defun fd-set (offset fd-set)
  (multiple-value-bind (word bit) (floor offset
                                            sb!vm:n-machine-word-bits)
     (setf (deref (slot fd-set 'fds-bits) word)
           (logior (truly-the (unsigned-byte #.sb!vm:n-machine-word-bits)
                              (ash 1 bit))
                   (deref (slot fd-set 'fds-bits) word)))))

(defun fd-clr (offset fd-set)
  (multiple-value-bind (word bit) (floor offset
                                         sb!vm:n-machine-word-bits)
    (setf (deref (slot fd-set 'fds-bits) word)
          (logand (deref (slot fd-set 'fds-bits) word)
                  (sb!kernel:word-logical-not
                   (truly-the (unsigned-byte #.sb!vm:n-machine-word-bits)
                              (ash 1 bit)))))))

(defun fd-isset (offset fd-set)
  (multiple-value-bind (word bit) (floor offset
                                         sb!vm:n-machine-word-bits)
     (logbitp bit (deref (slot fd-set 'fds-bits) word))))

(defun fd-zero (fd-set)
  (loop for index below (/ fd-setsize sb!vm:n-machine-word-bits)
        do (setf (deref (slot fd-set 'fds-bits) index) 0)))

#!-os-provides-poll
(defun unix-simple-poll (fd direction to-msec)
  (multiple-value-bind (to-sec to-usec)
      (if (minusp to-msec)
          (values nil nil)
          (multiple-value-bind (to-sec to-msec2) (truncate to-msec 1000)
            (values to-sec (* to-msec2 1000))))
    (with-restarted-syscall (count errno)
      (with-alien ((fds (struct fd-set)))
        (fd-zero fds)
        (fd-set fd fds)
        (multiple-value-bind (read-fds write-fds)
            (ecase direction
              (:input
               (values (addr fds) nil))
              (:output
               (values nil (addr fds))))
          (unix-fast-select (1+ fd)
                                    read-fds write-fds nil
                                    to-sec to-usec)))
      (case count
        ((1) t)
        ((0) nil)
        (otherwise
         (error "Syscall select(2) failed on fd ~D: ~A" fd (strerror)))))))

;;;; sys/stat.h

;;; This is a structure defined in src/runtime/wrap.c, to look
;;; basically like "struct stat" according to stat(2). It may not
;;; actually correspond to the real in-memory stat structure that the
;;; syscall uses, and that's OK. Linux in particular is packed full of
;;; stat macros, and trying to keep Lisp code in correspondence with
;;; it is more pain than it's worth, so we just let our C runtime
;;; synthesize a nice consistent structure for us.
;;;
;;; Note that st-dev is a long, not a dev-t. This is because dev-t on
;;; linux 32 bit archs is a 64 bit quantity, but alien doesn't support
;;; those. We don't actually access that field anywhere, though, so
;;; until we can get 64 bit alien support it'll do. Also note that
;;; st_size is a long, not an off-t, because off-t is a 64-bit
;;; quantity on Alpha. And FIXME: "No one would want a file length
;;; longer than 32 bits anyway, right?":-|
;;;
;;; The comment about alien and 64-bit quantities has not been kept in
;;; sync with the comment now in wrap.h (formerly wrap.c), but it's
;;; not clear whether either comment is correct.  -- RMK 2007-11-14.
(define-alien-type nil
  (struct wrapped_stat
    (st-dev wst-dev-t)
    (st-ino wst-ino-t)
    (st-mode mode-t)
    (st-nlink wst-nlink-t)
    (st-uid wst-uid-t)
    (st-gid wst-gid-t)
    (st-rdev wst-dev-t)
    (st-size wst-off-t)
    (st-blksize wst-blksize-t)
    (st-blocks wst-blkcnt-t)
    (st-atime time-t)
    (st-mtime time-t)
    (st-ctime time-t)))

;;; shared C-struct-to-multiple-VALUES conversion for the stat(2)
;;; family of Unix system calls
;;;
;;; FIXME: I think this should probably not be INLINE. However, when
;;; this was not inline, it seemed to cause memory corruption
;;; problems. My first guess is that it's a bug in the FFI code, where
;;; the WITH-ALIEN expansion doesn't deal well with being wrapped
;;; around a call to a function returning >10 values. But I didn't try
;;; to figure it out, just inlined it as a quick fix. Perhaps someone
;;; who's motivated to debug the FFI code can go over the DISASSEMBLE
;;; output in the not-inlined case and see whether there's a problem,
;;; and maybe even find a fix..
(declaim (inline %extract-stat-results))
(defun %extract-stat-results (wrapped-stat)
  (declare (type (alien (* (struct wrapped_stat))) wrapped-stat))
  (values t
          (slot wrapped-stat 'st-dev)
          (slot wrapped-stat 'st-ino)
          (slot wrapped-stat 'st-mode)
          (slot wrapped-stat 'st-nlink)
          (slot wrapped-stat 'st-uid)
          (slot wrapped-stat 'st-gid)
          (slot wrapped-stat 'st-rdev)
          (slot wrapped-stat 'st-size)
          (slot wrapped-stat 'st-atime)
          (slot wrapped-stat 'st-mtime)
          (slot wrapped-stat 'st-ctime)
          (slot wrapped-stat 'st-blksize)
          (slot wrapped-stat 'st-blocks)))

;;; Unix system calls in the stat(2) family are handled by calls to
;;; C-level wrapper functions which copy all the raw "struct stat"
;;; slots into the system-independent wrapped_stat format.
;;;    stat(2) <->  stat_wrapper()
;;;   fstat(2) <-> fstat_wrapper()
;;;   lstat(2) <-> lstat_wrapper()
(defun unix-stat (name)
  (declare (type unix-pathname name))
  (with-alien ((buf (struct wrapped_stat)))
    (syscall ("stat_wrapper" c-string (* (struct wrapped_stat)))
             (%extract-stat-results (addr buf))
             name (addr buf))))
(defun unix-lstat (name)
  (declare (type unix-pathname name))
  (with-alien ((buf (struct wrapped_stat)))
    (syscall ("lstat_wrapper" c-string (* (struct wrapped_stat)))
             (%extract-stat-results (addr buf))
             name (addr buf))))
(defun unix-fstat (fd)
  #!-win32
  (declare (type unix-fd fd))
  (#!-win32 funcall #!+win32 sb!win32::call-with-crt-fd
   (lambda (fd)
     (with-alien ((buf (struct wrapped_stat)))
       (syscall ("fstat_wrapper" int (* (struct wrapped_stat)))
                (%extract-stat-results (addr buf))
                fd (addr buf))))
   fd))

#!-win32
(defun fd-type (fd)
  (declare (type unix-fd fd))
  (let ((fmt (logand
              s-ifmt
              (or (with-alien ((buf (struct wrapped_stat)))
                    (syscall ("fstat_wrapper" int (* (struct wrapped_stat)))
                             (slot buf 'st-mode)
                             fd (addr buf)))
                  0))))
    (cond ((logtest s-ififo fmt)
           :fifo)
          ((logtest s-ifchr fmt)
           :character)
          ((logtest s-ifdir fmt)
           :directory)
          ((logtest s-ifblk fmt)
           :block)
          ((logtest s-ifreg fmt)
           :regular)
          ((logtest s-ifsock fmt)
           :socket)
          (t
           :unknown))))

;;;; time.h

;; used by other time functions
(define-alien-type nil
    (struct tm
            (tm-sec int)   ; Seconds.   [0-60] (1 leap second)
            (tm-min int)   ; Minutes.   [0-59]
            (tm-hour int)  ; Hours.     [0-23]
            (tm-mday int)  ; Day.       [1-31]
            (tm-mon int)   ; Month.     [0-11]
            (tm-year int)  ; Year - 1900.
            (tm-wday int)  ; Day of week. [0-6]
            (tm-yday int)  ; Days in year. [0-365]
            (tm-isdst int) ; DST.       [-1/0/1]
            (tm-gmtoff long) ;  Seconds east of UTC.
            (tm-zone c-string))) ; Timezone abbreviation.

(define-alien-routine get-timezone sb!alien:void
  (when time-t :in)
  (seconds-west sb!alien:int :out)
  (daylight-savings-p sb!alien:boolean :out))

#!-win32
(defun nanosleep (secs nsecs)
  (declare (optimize (sb!c:alien-funcall-saves-fp-and-pc 0)))
  (with-alien ((req (struct timespec))
               (rem (struct timespec)))
    (setf (slot req 'tv-sec) secs
          (slot req 'tv-nsec) nsecs)
    (loop while (and (eql eintr
                          (nth-value 1
                                     (int-syscall ("sb_nanosleep" (* (struct timespec))
                                                                  (* (struct timespec)))
                                                  (addr req) (addr rem))))
                     ;; KLUDGE: On Darwin, if an interrupt cases nanosleep to
                     ;; take longer than the requested time, the call will
                     ;; return with EINT and (unsigned)-1 seconds in the
                     ;; remainder timespec, which would cause us to enter
                     ;; nanosleep again for ~136 years. So, we check that the
                     ;; remainder time is actually decreasing.
                     ;;
                     ;; It would be neat to do this bit of defensive
                     ;; programming on all platforms, but unfortunately on
                     ;; Linux, REM can be a little higher than REQ if the
                     ;; nanosleep() call is interrupted quickly enough,
                     ;; probably due to the request being rounded up to the
                     ;; nearest HZ. This would cause the sleep to return way
                     ;; too early.
                     #!+darwin
                     (let ((rem-sec (slot rem 'tv-sec))
                           (rem-nsec (slot rem 'tv-nsec)))
                       (when (or (> secs rem-sec)
                                 (and (= secs rem-sec) (>= nsecs rem-nsec)))
                         ;; Update for next round.
                         (setf secs  rem-sec
                               nsecs rem-nsec)
                         t)))
          do (setf (slot req 'tv-sec) (slot rem 'tv-sec)
                   (slot req 'tv-nsec) (slot rem 'tv-nsec)))))

(defun unix-get-seconds-west (secs)
  (multiple-value-bind (ignore seconds dst) (get-timezone secs)
    (declare (ignore ignore) (ignore dst))
    (values seconds)))

;;;; sys/time.h

;;; Structure crudely representing a timezone. KLUDGE: This is
;;; obsolete and should never be used.
(define-alien-type nil
  (struct timezone
    (tz-minuteswest int)                ; minutes west of Greenwich
    (tz-dsttime int)))                  ; type of dst correction


;; Type of the second argument to `getitimer' and
;; the second and third arguments `setitimer'.
(define-alien-type nil
  (struct itimerval
    (it-interval (struct timeval))      ; timer interval
    (it-value (struct timeval))))       ; current value

(defconstant itimer-real 0)
(defconstant itimer-virtual 1)
(defconstant itimer-prof 2)

#!-win32
(defun unix-getitimer (which)
  #!+sb-doc
  "UNIX-GETITIMER returns the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). On success,
   unix-getitimer returns 5 values,
   T, it-interval-secs, it-interval-usec, it-value-secs, it-value-usec."
  (declare (type (member :real :virtual :profile) which)
           (values t
                   unsigned-byte (mod 1000000)
                   unsigned-byte (mod 1000000)))
  (let ((which (ecase which
                 (:real itimer-real)
                 (:virtual itimer-virtual)
                 (:profile itimer-prof))))
    (with-alien ((itv (struct itimerval)))
      (syscall* ("sb_getitimer" int (* (struct itimerval)))
                (values t
                        (slot (slot itv 'it-interval) 'tv-sec)
                        (slot (slot itv 'it-interval) 'tv-usec)
                        (slot (slot itv 'it-value) 'tv-sec)
                        (slot (slot itv 'it-value) 'tv-usec))
                which (alien-sap (addr itv))))))

#!-win32
(defun unix-setitimer (which int-secs int-usec val-secs val-usec)
  #!+sb-doc
  "UNIX-SETITIMER sets the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). A SIGALRM signal
   will be delivered VALUE <seconds+microseconds> from now. INTERVAL,
   when non-zero, is <seconds+microseconds> to be loaded each time
   the timer expires. Setting INTERVAL and VALUE to zero disables
   the timer. See the Unix man page for more details. On success,
   unix-setitimer returns the old contents of the INTERVAL and VALUE
   slots as in unix-getitimer."
  (declare (type (member :real :virtual :profile) which)
           (type unsigned-byte int-secs val-secs)
           (type (integer 0 (1000000)) int-usec val-usec)
           (values t
                   unsigned-byte (mod 1000000)
                   unsigned-byte (mod 1000000)))
  (let ((which (ecase which
                 (:real itimer-real)
                 (:virtual itimer-virtual)
                 (:profile itimer-prof))))
    (with-alien ((itvn (struct itimerval))
                 (itvo (struct itimerval)))
      (setf (slot (slot itvn 'it-interval) 'tv-sec ) int-secs
            (slot (slot itvn 'it-interval) 'tv-usec) int-usec
            (slot (slot itvn 'it-value   ) 'tv-sec ) val-secs
            (slot (slot itvn 'it-value   ) 'tv-usec) val-usec)
      (syscall* ("sb_setitimer" int (* (struct timeval))(* (struct timeval)))
                (values t
                        (slot (slot itvo 'it-interval) 'tv-sec)
                        (slot (slot itvo 'it-interval) 'tv-usec)
                        (slot (slot itvo 'it-value) 'tv-sec)
                        (slot (slot itvo 'it-value) 'tv-usec))
                which (alien-sap (addr itvn))(alien-sap (addr itvo))))))


;;; FIXME: Many Unix error code definitions were deleted from the old
;;; CMU CL source code here, but not in the exports of SB-UNIX. I
;;; (WHN) hope that someday I'll figure out an automatic way to detect
;;; unused symbols in package exports, but if I don't, there are
;;; enough of them all in one place here that they should probably be
;;; removed by hand.

(defconstant micro-seconds-per-internal-time-unit
  (/ 1000000 sb!xc:internal-time-units-per-second))

;;; UNIX specific code, that has been cleanly separated from the
;;; Windows build.
#!-win32
(progn

  #!-sb-fluid (declaim (inline get-time-of-day))
  (defun get-time-of-day ()
    #!+sb-doc
    "Return the number of seconds and microseconds since the beginning of
the UNIX epoch (January 1st 1970.)"
    #!+(or darwin netbsd)
    (with-alien ((tv (struct timeval)))
      ;; CLH: FIXME! This seems to be a MacOS bug, but on x86-64/darwin,
      ;; gettimeofday occasionally fails. passing in a null pointer for the
      ;; timezone struct seems to work around the problem. NS notes: Darwin
      ;; manpage says the timezone is not used anymore in their implementation
      ;; at all.
      (syscall* ("sb_gettimeofday" (* (struct timeval))
                                (* (struct timezone)))
                (values (slot tv 'tv-sec)
                        (slot tv 'tv-usec))
                (addr tv)
                nil))
    #!-(or darwin netbsd)
    (with-alien ((tv (struct timeval))
                 (tz (struct timezone)))
      (syscall* ("sb_gettimeofday" (* (struct timeval))
                                (* (struct timezone)))
                (values (slot tv 'tv-sec)
                        (slot tv 'tv-usec))
                (addr tv)
                (addr tz))))

  (declaim (inline system-internal-run-time
                   system-real-time-values))

  (defun system-real-time-values ()
    (multiple-value-bind (sec usec) (get-time-of-day)
      (declare (type unsigned-byte sec) (type (unsigned-byte 31) usec))
      (values sec (truncate usec micro-seconds-per-internal-time-unit))))

  ;; There are two optimizations here that actually matter (on 32-bit
  ;; systems): substract the epoch from seconds and milliseconds
  ;; separately, as those should remain fixnums for the first 17 years
  ;; or so of runtime. Also, avoid doing consing a new bignum if the
  ;; result would be = to the last result given.
  ;;
  ;; Note: the next trick would be to spin a separate thread to update
  ;; a global value once per internal tick, so each individual call to
  ;; get-internal-real-time would be just a memory read... but that is
  ;; probably best left for user-level code. ;)
  ;;
  ;; Thanks to James Anderson for the optimization hint.
  ;;
  ;; Yes, it is possible to a computation to be GET-INTERNAL-REAL-TIME
  ;; bound.
  ;;
  ;; --NS 2007-04-05
  (let ((e-sec 0)
        (e-msec 0)
        (c-sec 0)
        (c-msec 0)
        (now 0))
    (declare (type sb!kernel:internal-seconds e-sec c-sec)
             (type sb!kernel:internal-seconds e-msec c-msec)
             (type sb!kernel:internal-time now))
    (defun reinit-internal-real-time ()
      (setf (values e-sec e-msec) (system-real-time-values)
            c-sec 0
            c-msec 0))
    ;; If two threads call this at the same time, we're still safe, I
    ;; believe, as long as NOW is updated before either of C-MSEC or
    ;; C-SEC. Same applies to interrupts. --NS
    ;;
    ;; I believe this is almost correct with x86/x86-64 cache
    ;; coherency, but if the new value of C-SEC, C-MSEC can become
    ;; visible to another CPU without NOW doing the same then it's
    ;; unsafe. It's `almost' correct on x86 because writes by other
    ;; processors may become visible in any order provided transitity
    ;; holds. With at least three cpus, C-MSEC and C-SEC may be from
    ;; different threads and an incorrect value may be returned.
    ;; Considering that this failure is not detectable by the caller -
    ;; it looks like time passes a bit slowly - and that it should be
    ;; an extremely rare occurance I'm inclinded to leave it as it is.
    ;; --MG
    (defun get-internal-real-time ()
      (multiple-value-bind (sec msec) (system-real-time-values)
        (unless (and (= msec c-msec) (= sec c-sec))
          (setf now (+ (* (- sec e-sec)
                          sb!xc:internal-time-units-per-second)
                       (- msec e-msec))
                c-msec msec
                c-sec sec))
        now)))

  (defun system-internal-run-time ()
    (multiple-value-bind (ignore utime-sec utime-usec stime-sec stime-usec)
        (unix-fast-getrusage rusage_self)
      (declare (ignore ignore)
               (type unsigned-byte utime-sec stime-sec)
               ;; (Classic CMU CL had these (MOD 1000000) instead, but
               ;; at least in Linux 2.2.12, the type doesn't seem to
               ;; be documented anywhere and the observed behavior is
               ;; to sometimes return 1000000 exactly.)
               (type fixnum utime-usec stime-usec))
      (let ((result (+ (* (+ utime-sec stime-sec)
                          sb!xc:internal-time-units-per-second)
                       (floor (+ utime-usec
                                 stime-usec
                                 (floor micro-seconds-per-internal-time-unit 2))
                              micro-seconds-per-internal-time-unit))))
        result))))

;;; FIXME, KLUDGE: GET-TIME-OF-DAY used to be UNIX-GETTIMEOFDAY, and had a
;;; primary return value indicating sucess, and also returned timezone
;;; information -- though the timezone data was not there on Darwin.
;;; Now we have GET-TIME-OF-DAY, but it turns out that despite SB-UNIX being
;;; an implementation package UNIX-GETTIMEOFDAY has users in the wild.
;;; So we're stuck with it for a while -- maybe delete it towards the end
;;; of 2009.
(defun unix-gettimeofday ()
  (multiple-value-bind (sec usec) (get-time-of-day)
    (values t sec usec nil nil)))

;;;; opendir, readdir, closedir, and dirent-name

(declaim (inline unix-opendir))
(defun unix-opendir (namestring &optional (errorp t))
  (let ((dir (alien-funcall
              (extern-alien "sb_opendir"
                            (function system-area-pointer c-string))
              namestring)))
    (if (zerop (sap-int dir))
        (when errorp (simple-perror
                      (format nil "Error opening directory ~S"
                              namestring)))
        dir)))

(declaim (inline unix-readdir))
(defun unix-readdir (dir &optional (errorp t) namestring)
  (let ((ent (alien-funcall
              (extern-alien "sb_readdir"
                            (function system-area-pointer system-area-pointer))
              dir))
        errno)
    (if (zerop (sap-int ent))
        (when (and errorp
                   (not (zerop (setf errno (get-errno)))))
          (simple-perror
           (format nil "Error reading directory entry~@[ from ~S~]"
                   namestring)
           :errno errno))
        ent)))

(declaim (inline unix-closedir))
(defun unix-closedir (dir &optional (errorp t) namestring)
  (let ((r (alien-funcall
            (extern-alien "sb_closedir" (function int system-area-pointer))
            dir)))
    (if (minusp r)
        (when errorp (simple-perror
                      (format nil "Error closing directory~@[ ~S~]"
                              namestring)))
        r)))

(declaim (inline unix-dirent-name))
(defun unix-dirent-name (ent)
  (alien-funcall
   (extern-alien "sb_dirent_name" (function c-string system-area-pointer))
   ent))

;;;; A magic constant for wait3().
;;;;
;;;; FIXME: This used to be defined in run-program.lisp as
;;;; (defconstant wait-wstopped #-svr4 #o177 #+svr4 wait-wuntraced)
;;;; According to some of the man pages, the #o177 is part of the API
;;;; for wait3(); that said, under SunOS there is a WSTOPPED thing in
;;;; the headers that may or may not be the same thing. To be
;;;; investigated. -- CSR, 2002-03-25
(defconstant wstopped #o177)
