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

(in-package "SB-UNIX")

(/show0 "unix.lisp 21")

;;; Given a C-level zero-terminated array of C strings, return a
;;; corresponding Lisp-level list of SIMPLE-STRINGs.
(defun c-strings->string-list (c-strings)
  (declare (type (alien (* c-string)) c-strings))
  (let ((reversed-result nil))
    (dotimes (i most-positive-fixnum)
      (declare (type index i))
      (let ((c-string (deref c-strings i)))
        (if c-string
            (push c-string reversed-result)
            (return (nreverse reversed-result)))))))

;;;; Lisp types used by syscalls

(deftype unix-pathname () 'simple-string)
(deftype unix-fd () '(unsigned-byte #-win32 31 #+win32 #.sb-vm:n-word-bits))

(deftype unix-file-mode () '(unsigned-byte 32))
(deftype unix-pid () '(unsigned-byte 32))
(deftype unix-uid () '(unsigned-byte 32))
(deftype unix-gid () '(unsigned-byte 32))

;;;; system calls

(/show0 "unix.lisp 74")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun libc-name-for (x)
    (assert (stringp x))
    ;; This function takes a possibly-wrapped C name and strips off "sb_"
    ;; if it doesn't need a wrapper. The list of functions that can be
    ;; called directly is listed explicitly, because there are also others
    ;; that might want to be wrapped even if they don't need to be,
    ;; like sb_opendir and sb_closedir. Why are those wrapped in fact?
    #+(or netbsd (not 64-bit)) x
    #-(or netbsd (not 64-bit))
    (if (member x '("sb_getrusage"      ; syscall*
                    "sb_gettimeofday"   ;syscall*
                    "sb_clock_gettime"  ; alien-funcall
                    "sb_select"         ; int-syscall
                    "sb_getitimer"      ; syscall*
                    "sb_setitimer"      ; syscall*
                    "sb_utimes")        ; posix
                :test #'string=)
        (subseq x 3)
        x)))

(defmacro syscall-type ((name return-type &rest arg-types) success-form &rest args)
  (when (eql 3 (mismatch "[_]" name))
    (setf name
          (concatenate 'string #+win32 "_" (subseq name 3))))
  `(locally
       (declare (optimize (sb-c::float-accuracy 0)))
     (let ((result (alien-funcall (extern-alien ,(libc-name-for name)
                                                (function ,return-type ,@arg-types))
                                  ,@args)))
       (if (minusp result)
           (values nil (get-errno))
           ,success-form))))

(defmacro syscall ((name &rest arg-types) success-form &rest args)
  `(syscall-type (,name int ,@arg-types) ,success-form ,@args))

;;; This is like SYSCALL, but if it fails, signal an error instead of
;;; returning error codes. Should only be used for syscalls that will
;;; never really get an error.
(defmacro syscall* ((name &rest arg-types) success-form &rest args)
  `(locally
    (declare (optimize (sb-c::float-accuracy 0)))
    (let ((result (alien-funcall (extern-alien ,(libc-name-for name)
                                               (function int ,@arg-types))
                                 ,@args)))
      (if (minusp result)
          (error "Syscall ~A failed: ~A" ,name (strerror))
          ,success-form))))

(defmacro int-syscall ((name &rest arg-types) &rest args)
  `(syscall (,(libc-name-for name) ,@arg-types) (values result 0) ,@args))

(defmacro type-syscall ((name return-type &rest arg-types) &rest args)
  `(syscall-type (,(libc-name-for name) ,return-type ,@arg-types) (values result 0) ,@args))

(defmacro with-restarted-syscall ((&optional (value (gensym))
                                             (errno (gensym)))
                                  syscall-form &rest body)
  "Evaluate BODY with VALUE and ERRNO bound to the return values of
SYSCALL-FORM. Repeat evaluation of SYSCALL-FORM if it is interrupted."
  `(let (,value ,errno)
     (loop (multiple-value-setq (,value ,errno)
             ,syscall-form)
        (unless #-win32 (eql ,errno eintr) #+win32 nil
          (return (values ,value ,errno))))
     ,@body))

(defmacro void-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values t 0) ,@args))

#+win32
(progn
  (defconstant espipe 29))

;;;; hacking the Unix environment

#-win32
(define-alien-routine ("getenv" posix-getenv) c-string
  "Return the \"value\" part of the environment string \"name=value\" which
corresponds to NAME, or NIL if there is none."
  (name (c-string :not-null t)))

;;; from stdio.h

;;; Rename the file with string NAME1 to the string NAME2. NIL and an
;;; error code is returned if an error occurs.
#-win32
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
                                        sb-vm:n-machine-word-bits)))))

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
(defun unix-open (path flags mode &key #+win32 overlapped)
  (declare (type unix-pathname path)
           (type fixnum flags)
           (type unix-file-mode mode)
           #+win32
           (ignore mode))
  #+win32 (sb-win32:unixlike-open path flags :overlapped overlapped)
  #-win32
  (with-restarted-syscall (value errno)
    (locally
        (declare (optimize (sb-c::float-accuracy 0)))
      (let ((result (alien-funcall (extern-alien "open" (function int c-string int &optional int))
                                   path (logior flags
                                                #+largefile o_largefile)
                                   mode)))
        (if (minusp result)
            (values nil (get-errno))
            (values result 0))))))

;;; UNIX-CLOSE accepts a file descriptor and attempts to close the file
;;; associated with it.
(/show0 "unix.lisp 391")
(defun unix-close (fd)
  (declare (type unix-fd fd))
  #+win32 (sb-win32:unixlike-close fd)
  #-win32 (void-syscall ("close" int) fd))

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
            (values #-win32 fd #+win32 (sb-win32::duplicate-and-unwrap-fd fd)
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
#+win32
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
  #-win32 off-t
  #+win32 (signed 64))

;;; Is a stream interactive?
(defun unix-isatty (fd)
  (declare (type unix-fd fd))
  #-win32 (int-syscall ("isatty" int) fd)
  #+win32 (sb-win32::windows-isatty fd))

(defun unix-lseek (fd offset whence)
  "Unix-lseek accepts a file descriptor and moves the file pointer by
   OFFSET octets.  Whence can be any of the following:

   L_SET        Set the file pointer.
   L_INCR       Increment the file pointer.
   L_XTND       Extend the file size.
  "
  (declare (type unix-fd fd)
           (type (integer 0 2) whence))
  (let ((result
         #-win32
          (alien-funcall (extern-alien #-largefile "lseek"
                                             #+largefile "lseek_largefile"
                                             (function off-t int off-t int))
                        fd offset whence)
          #+win32 (sb-win32:lseeki64 fd offset whence)))
    (if (minusp result)
        (values nil (get-errno))
      (values result 0))))

;;; UNIX-READ accepts a file descriptor, a buffer, and the length to read.
;;; It attempts to read len bytes from the device associated with fd
;;; and store them into the buffer. It returns the actual number of
;;; bytes read.

(declaim (maybe-inline unix-read))

(defun unix-read (fd buf len)
  (declare (type unix-fd fd)
           (type index len))
  (type-syscall (#-win32 "read" #+win32 "win32_unix_read"
                 ssize-t
                 int (* char) size-t)
                fd buf
                (min len
                     #+(or darwin freebsd)
                     (1- (expt 2 31)))))

;;; UNIX-WRITE accepts a file descriptor, a buffer, an offset, and the
;;; length to write. It attempts to write len bytes to the device
;;; associated with fd from the buffer starting at offset. It returns
;;; the actual number of bytes written.
(defun unix-write (fd buf offset len)
  ;; KLUDGE: change 60fa88b187e438cc made this function unusable in cold-init
  ;; if compiled with #+sb-show (which increases DEBUG to 2) because of
  ;; full calls to SB-ALIEN-INTERNALS:DEPORT-ALLOC and DEPORT.
  (declare (optimize (debug 1)))
  (declare (type unix-fd fd)
           (type index offset len))
  (flet ((%write (sap)
           (declare (system-area-pointer sap))
           (type-syscall (#-win32 "write" #+win32 "win32_unix_write"
                          ssize-t int (* char) size-t)
                         fd
                         (with-alien ((ptr (* char) sap))
                           (addr (deref ptr offset)))
                         (min len
                              #+(or darwin freebsd)
                              (1- (expt 2 31))))))
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
#-win32
(defun unix-pipe ()
  (with-alien ((fds (array int 2)))
    (syscall ("pipe" (* int))
             (values (deref fds 0) (deref fds 1))
             (cast fds (* int)))))

#+win32
(defun unix-pipe ()
  (sb-win32::windows-pipe))

;; Windows mkdir() doesn't take the mode argument. It's cdecl, so we could
;; actually call it passing the mode argument, but some sharp-eyed reader
;; would put five and twenty-seven together and ask us about it, so...
;;    -- AB, 2005-12-27
#-win32
(defun unix-mkdir (name mode)
  (declare (type unix-pathname name)
           (type unix-file-mode mode))
  (void-syscall ("mkdir" c-string int) name mode))

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
#-win32
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
  #-(or android linux openbsd freebsd netbsd sunos darwin dragonfly haiku)
  #.(error "POSIX-GETCWD is not implemented.")
  (or
   #+(or linux openbsd freebsd netbsd sunos darwin dragonfly haiku)
   (newcharstar-string (alien-funcall (extern-alien "getcwd"
                                                    (function (* char)
                                                              (* char)
                                                              size-t))
                                      nil
                                      #+(or linux openbsd freebsd netbsd darwin dragonfly haiku) 0
                                      #+(or sunos) 1025))
   #+android
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
#-win32
(defun unix-dup (fd)
  (declare (type unix-fd fd))
  (int-syscall ("dup" int) fd))

;;; Terminate the current process with an optional error code. If
;;; successful, the call doesn't return. If unsuccessful, the call
;;; returns NIL and an error number.
(deftype exit-code ()
  `(signed-byte 32))
(defun os-exit (code &key abort)
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
(define-alien-routine (#+win32 "_getpid" #-win32 "getpid" unix-getpid) int)

;;; Return the real user id associated with the current process.
#-win32
(define-alien-routine ("getuid" unix-getuid) int)

;;; Translate a user id into a login name.
#-win32
(defun uid-username (uid)
  (or (newcharstar-string (alien-funcall (extern-alien "uid_username"
                                                       (function (* char) int))
                                         uid))
      (error "found no match for Unix uid=~S" uid)))

;;; Return the namestring of the home directory, being careful to
;;; include a trailing #\/
#-win32
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
#-win32
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
#+win32
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
#-win32
(defun unix-gethostname ()
  (with-alien ((buf (array char 256)))
    (syscall ("gethostname" (* char) int)
             (cast buf c-string)
             (cast buf (* char)) 256)))

#-win32
(defun unix-setsid ()
  (int-syscall ("setsid")))

;;;; sys/ioctl.h

;;; UNIX-IOCTL performs a variety of operations on open i/o
;;; descriptors. See the UNIX Programmer's Manual for more
;;; information.
#-win32
(defun unix-ioctl (fd cmd arg)
  (declare (type unix-fd fd)
           (type word cmd))
  (void-syscall ("ioctl" int unsigned-long &optional (* char)) fd cmd arg))

;;;; sys/resource.h

;;; Return information about the resource usage of the process
;;; specified by WHO. WHO can be either the current process
;;; (rusage_self) or all of the terminated child processes
;;; (rusage_children). NIL and an error number is returned if the call
;;; fails.
#-win32
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
       (sb-debug:print-backtrace)))
    nil))

;;;; poll.h
#+os-provides-poll
(progn
  (define-alien-type nil
      (struct pollfd
              (fd      int)
              (events  short)           ; requested events
              (revents short)))         ; returned events

  (declaim (inline unix-poll))
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
                    (:output pollout)))
          (deadline (if (minusp to-msec)
                        to-msec
                        (+ (* (get-universal-time) 1000) to-msec))))
      (with-alien ((fds (struct pollfd)))
        (with-restarted-syscall (count errno)
          (let ((timeout (if (minusp to-msec)
                             -1
                             (max 0 (- deadline (* 1000 (get-universal-time)))))))
            (declare (fixnum timeout))
            (setf (slot fds 'fd) fd
                  (slot fds 'events) events
                  (slot fds 'revents) 0)
            (int-syscall ("poll" (* (struct pollfd)) int int)
                         (addr fds) 1 timeout))
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

;;; Lisp-side implementations of FD_FOO macros.
(declaim (inline fd-set fd-clr fd-isset fd-zero))
(defun fd-set (offset fd-set)
  (multiple-value-bind (word bit) (floor offset
                                            sb-vm:n-machine-word-bits)
     (setf (deref (slot fd-set 'fds-bits) word)
           (logior (truly-the (unsigned-byte #.sb-vm:n-machine-word-bits)
                              (ash 1 bit))
                   (deref (slot fd-set 'fds-bits) word)))))

(defun fd-clr (offset fd-set)
  (multiple-value-bind (word bit) (floor offset
                                         sb-vm:n-machine-word-bits)
    (setf (deref (slot fd-set 'fds-bits) word)
          (logand (deref (slot fd-set 'fds-bits) word)
                  (sb-kernel:word-logical-not
                   (truly-the (unsigned-byte #.sb-vm:n-machine-word-bits)
                              (ash 1 bit)))))))

(defun fd-isset (offset fd-set)
  (multiple-value-bind (word bit) (floor offset
                                         sb-vm:n-machine-word-bits)
     (logbitp bit (deref (slot fd-set 'fds-bits) word))))

(defun fd-zero (fd-set)
  (loop for index below (/ fd-setsize sb-vm:n-machine-word-bits)
        do (setf (deref (slot fd-set 'fds-bits) index) 0)))

#-os-provides-poll
(defun unix-simple-poll (fd direction to-msec)
  (flet ((msec-to-sec-usec (msec)
           (multiple-value-bind (sec msec2) (truncate msec 1000)
             (values sec (* msec2 1000)))))
    (let ((deadline (if (minusp to-msec)
                        to-msec
                        (+ (* (get-universal-time) 1000) to-msec))))
      (with-restarted-syscall (count errno)
        (with-alien ((fds (struct fd-set)))
          (fd-zero fds)
          (fd-set fd fds)
          (multiple-value-bind (to-sec to-usec)
              (if (minusp to-msec)
                  (values nil nil)
                  (msec-to-sec-usec (max 0 (- deadline (* 1000 (get-universal-time))))))
            (multiple-value-bind (read-fds write-fds)
                (ecase direction
                  (:input
                   (values (addr fds) nil))
                  (:output
                   (values nil (addr fds))))
              (unix-fast-select (1+ fd)
                                read-fds write-fds nil
                                to-sec to-usec))))
        (case count
          ((1) t)
          ((0) nil)
          (otherwise
           (error "Syscall select(2) failed on fd ~D: ~A" fd (strerror))))))))

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
  (declare (type unix-fd fd))
  (#-win32 funcall #+win32 sb-win32::call-with-crt-fd
   (lambda (fd)
     (with-alien ((buf (struct wrapped_stat)))
       (syscall ("fstat_wrapper" int (* (struct wrapped_stat)))
                (%extract-stat-results (addr buf))
                fd (addr buf))))
   fd))

#-win32
(defun fd-type (fd)
  (declare (type unix-fd fd))
  (let ((mode (or (with-alien ((buf (struct wrapped_stat)))
                    (syscall ("fstat_wrapper" int (* (struct wrapped_stat)))
                             (slot buf 'st-mode)
                             fd (addr buf)))
                  0)))
    (case (logand mode s-ifmt)
      (#.s-ifchr :character)
      (#.s-ifdir :directory)
      (#.s-ifblk :block)
      (#.s-ifreg :regular)
      (#.s-ifsock :socket)
      (#.s-iflnk :link)
      (#.s-ififo :fifo)
      (t :unknown))))

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

(define-alien-routine get-timezone int
  (when time-t)
  ;; BOOLEAN is N-WORD-BITS normally. Reduce it to an unsigned int in size.
  ;; But we can't just put UNSIGNED-INT here because the clients of this function
  ;; want to receive a T or NIL, not a 1 or 0.
  (daylight-savings-p (boolean 32) :out))
#-win32
(defun nanosleep (secs nsecs)
  (alien-funcall (extern-alien "sb_nanosleep" (function int time-t int))
                 secs nsecs)
  nil)

#-win32
(defun nanosleep-double (seconds)
  (alien-funcall (extern-alien "sb_nanosleep_double" (function (values) double))
                 seconds)
  nil)

#-win32
(defun nanosleep-float (seconds)
  (alien-funcall (extern-alien "sb_nanosleep_float" (function (values) float))
                 seconds)
  nil)


;; Type of the second argument to `getitimer' and
;; the second and third arguments `setitimer'.
(define-alien-type nil
  (struct itimerval
    (it-interval (struct timeval))      ; timer interval
    (it-value (struct timeval))))       ; current value

#-win32
(defun unix-getitimer (which)
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

#-win32
(defun unix-setitimer (which int-secs int-usec val-secs val-usec)
  "UNIX-SETITIMER sets the INTERVAL and VALUE slots of one of three system
   timers (:real :virtual or :profile). A SIGALRM, SIGVTALRM, or SIGPROF
   respectively will be delivered in VALUE <seconds+microseconds> from now.
   INTERVAL, when non-zero, is reloaded into the timer on each expiration.
   Setting VALUE to zero disables the timer. See the Unix man page for more
   details. On success, unix-setitimer returns the
   old contents of the INTERVAL and VALUE slots as in unix-getitimer."
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
      (syscall* ("sb_setitimer" int (* (struct timeval)) (* (struct timeval)))
                (values t
                        (slot (slot itvo 'it-interval) 'tv-sec)
                        (slot (slot itvo 'it-interval) 'tv-usec)
                        (slot (slot itvo 'it-value) 'tv-sec)
                        (slot (slot itvo 'it-value) 'tv-usec))
                which (alien-sap (addr itvn)) (alien-sap (addr itvo))))))


;;; FIXME: Many Unix error code definitions were deleted from the old
;;; CMU CL source code here, but not in the exports of SB-UNIX. I
;;; (WHN) hope that someday I'll figure out an automatic way to detect
;;; unused symbols in package exports, but if I don't, there are
;;; enough of them all in one place here that they should probably be
;;; removed by hand.

(defconstant microseconds-per-internal-time-unit
  (/ 1000000 internal-time-units-per-second))
(defconstant nanoseconds-per-internal-time-unit
  (* microseconds-per-internal-time-unit 1000))

;;; UNIX specific code, that has been cleanly separated from the
;;; Windows build.
#-win32
(progn

  #-avoid-clock-gettime
  (declaim (inline clock-gettime))
  #-avoid-clock-gettime
  (defun clock-gettime (clockid)
    (declare (type (signed-byte 32) clockid))
    (with-alien ((ts (struct timespec)))
      (alien-funcall (extern-alien #.(libc-name-for "sb_clock_gettime")
                                   (function int int (* (struct timespec))))
                     clockid (addr ts))
      ;; 'seconds' is definitely a fixnum for 64-bit, because most-positive-fixnum
      ;; can express 1E11 years in seconds.
      (values #+64-bit (truly-the fixnum (slot ts 'tv-sec))
              #-64-bit (slot ts 'tv-sec)
              (truly-the (integer 0 #.(expt 10 9)) (slot ts 'tv-nsec)))))

  (declaim (inline get-time-of-day))
  (defun get-time-of-day ()
    "Return the number of seconds and microseconds since the beginning of
the UNIX epoch (January 1st 1970.)"
    (with-alien ((tv (struct timeval)))
      (syscall* ("sb_gettimeofday" (* (struct timeval)) system-area-pointer)
                (values (slot tv 'tv-sec)
                        (slot tv 'tv-usec))
                (addr tv) (int-sap 0))))

  ;; The "optimizations that actually matter" don't actually matter for 64-bit.
  ;; Microseconds can express at least 1E5 years of uptime:
  ;; (float (/ most-positive-fixnum (* 1000000 60 60 24 (+ 365 1/4))))
  ;;   = microseconds-per-second * seconds-per-minute * minutes-per-hour
  ;;     * hours-per-day * days-per-year

  (defun get-internal-real-time ()
    (with-alien ((base (struct timespec) :extern "lisp_init_time"))
      (multiple-value-bind (c-sec c-nsec)
          ;; By scaling down we end up with far less resolution than clock-realtime
          ;; offers, and COARSE is about twice as fast, so use that, but only for linux.
          ;; BSD has something similar.
          #-avoid-clock-gettime
        (clock-gettime #+linux clock-monotonic-coarse #-linux clock-monotonic)
        #+avoid-clock-gettime
        (multiple-value-bind (c-sec c-usec) (get-time-of-day) (values c-sec (* c-usec 1000)))

        #+64-bit ;; I know that my math is valid for 64-bit.
        (declare (optimize (sb-c::type-check 0)))
        #+64-bit
        (let ((delta-sec (the fixnum (- c-sec (the fixnum (slot base 'tv-sec)))))
              (delta-nsec (the fixnum (- c-nsec (the fixnum (slot base 'tv-nsec))))))
          (the sb-kernel:internal-time
               (+ (the fixnum (* delta-sec internal-time-units-per-second))
                  (truncate delta-nsec nanoseconds-per-internal-time-unit))))

        ;; There are two optimizations here that actually matter on 32-bit systems:
        ;;  (1) subtract the epoch from seconds and milliseconds separately,
        ;;  (2) avoid consing a new bignum if the result is unchanged.
        ;;
        ;; Thanks to James Anderson for the optimization hint.
        ;;
        ;; Yes, it is possible to a computation to be GET-INTERNAL-REAL-TIME
        ;; bound.
        ;;
        ;; --NS 2007-04-05
        #-64-bit
        (symbol-macrolet ((observed-sec
                            (sb-thread::thread-observed-internal-real-time-delta-sec thr))
                          (observed-msec
                            (sb-thread::thread-observed-internal-real-time-delta-millisec thr))
                          (time (sb-thread::thread-internal-real-time thr)))
          (let* ((delta-sec (- c-sec (slot base 'tv-sec)))
                 ;; I inadvertently had too many THE casts in here, so I'd prefer
                 ;; to err on the side of caution rather than cause GC lossage
                 ;; (which I accidentally did). So assert that nanoseconds are <= 10^9
                 ;; and the compiler will do as best it can with that information.
                 (delta-nsec (- c-nsec (the (integer 0 #.(expt 10 9))
                                            (slot base 'tv-nsec))))
                 ;; ROUND, FLOOR? Who cares, it's a number that's going to change.
                 ;; More math = more self-induced jitter.
                 (delta-millisec (floor delta-nsec 1000000))
                 (thr sb-thread:*current-thread*))
            (if (and (= delta-sec observed-sec) (= delta-millisec observed-msec))
                time
                (let ((current (+ (* delta-sec internal-time-units-per-second)
                                  ;; ASSUMPTION: delta-millisec = delta-itu
                                  delta-millisec)))
                  (setf time current
                        observed-msec delta-millisec
                        observed-sec delta-sec)
                  current)))))))

  (declaim (inline system-internal-run-time))

  ;; SunOS defines CLOCK_PROCESS_CPUTIME_ID but you get EINVAL if you try to use it,
  ;; also use the same trick when clock_gettime should be avoided.
  #-(or sunos avoid-clock-gettime)
  (defun system-internal-run-time ()
    (multiple-value-bind (sec nsec) (clock-gettime clock-process-cputime-id)
      (+ (* sec internal-time-units-per-second)
         (floor (+ nsec (floor nanoseconds-per-internal-time-unit 2))
                nanoseconds-per-internal-time-unit))))
  #+(or sunos avoid-clock-gettime)
  (defun system-internal-run-time ()
    (multiple-value-bind (utime-sec utime-usec stime-sec stime-usec)
        (with-alien ((usage (struct sb-unix::rusage)))
          (syscall* ("sb_getrusage" int (* (struct sb-unix::rusage)))
                    (values (slot (slot usage 'sb-unix::ru-utime) 'sb-unix::tv-sec)
                            (slot (slot usage 'sb-unix::ru-utime) 'sb-unix::tv-usec)
                            (slot (slot usage 'sb-unix::ru-stime) 'sb-unix::tv-sec)
                            (slot (slot usage 'sb-unix::ru-stime) 'sb-unix::tv-usec))
                    rusage_self (addr usage)))
      (+ (* (+ utime-sec stime-sec) internal-time-units-per-second)
         (floor (+ utime-usec stime-usec
                   (floor microseconds-per-internal-time-unit 2))
                microseconds-per-internal-time-unit)))))

;;; FIXME, KLUDGE: GET-TIME-OF-DAY used to be UNIX-GETTIMEOFDAY, and had a
;;; primary return value indicating sucess, and also returned timezone
;;; information -- though the timezone data was not there on Darwin.
;;; Now we have GET-TIME-OF-DAY, but it turns out that despite SB-UNIX being
;;; an implementation package UNIX-GETTIMEOFDAY has users in the wild.
;;; So we're stuck with it for a while -- maybe delete it towards the end
;;; of 2009.
(defun unix-gettimeofday ()
  #+win32 (declare (notinline get-time-of-day)) ; forward ref
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
