;;;; This file contains Unix support that SBCL needs to implement itself. It's
;;;; derived from Peter Van Eynde's unix-glibc2.lisp for CMU CL, which was
;;;; derived from CMU CL unix.lisp 1.56. But those files aspired to be complete
;;;; Unix interfaces exported to the end user, while this file aims to be as
;;;; simple as possible and is not intended for the end user.
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

(file-comment
  "$Header$")

(/show0 "unix.lisp 21")

;;;; common machine-independent structures

(eval-when (:compile-toplevel :execute)

(defparameter *compiler-unix-errors* nil)

(/show0 "unix.lisp 29")

(sb!xc:defmacro def-unix-error (name number description)
  `(progn
     (eval-when (:compile-toplevel :execute)
       (push (cons ,number ,description) *compiler-unix-errors*))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defconstant ,name ,number ,description))))

(sb!xc:defmacro emit-unix-errors ()
  (let* ((max (apply #'max (mapcar #'car *compiler-unix-errors*)))
	 (array (make-array (1+ max) :initial-element nil)))
    (dolist (error *compiler-unix-errors*)
      (setf (svref array (car error)) (cdr error)))
    `(progn
       (defvar *unix-errors* ',array)
       (proclaim '(simple-vector *unix-errors*)))))

) ; EVAL-WHEN

(defvar *unix-errors*)

(/show0 "unix.lisp 52")

(defmacro def-enum (inc cur &rest names)
  (flet ((defform (name)
	   (prog1 (when name `(defconstant ,name ,cur))
	     (setf cur (funcall inc cur 1)))))
    `(progn ,@(mapcar #'defform names))))

;;;; Lisp types used by syscalls

(deftype unix-pathname () 'simple-string)
(deftype unix-fd () `(integer 0 ,most-positive-fixnum))

(deftype unix-file-mode () '(unsigned-byte 32))
(deftype unix-pid () '(unsigned-byte 32))
(deftype unix-uid () '(unsigned-byte 32))
(deftype unix-gid () '(unsigned-byte 32))

;;;; system calls

(def-alien-routine ("os_get_errno" get-errno) integer
  "Return the value of the C library pseudo-variable named \"errno\".")

(/show0 "unix.lisp 74")

(defun get-unix-error-msg (&optional (error-number (get-errno)))
  #!+sb-doc
  "Returns a string describing the error number which was returned by a
  UNIX system call."
  (declare (type integer error-number))
  (if (array-in-bounds-p *unix-errors* error-number)
      (svref *unix-errors* error-number)
      (format nil "unknown error [~D]" error-number)))

;;; FIXME: The various FOO-SYSCALL-BAR macros, and perhaps some other
;;; macros in this file, are only used in this file, and could be
;;; implemented using SB!XC:DEFMACRO wrapped in EVAL-WHEN.

(defmacro syscall ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (minusp result)
	 (values nil (get-errno))
	 ,success-form)))

;;; Like SYSCALL, but if it fails, signal an error instead of returning error
;;; codes. Should only be used for syscalls that will never really get an
;;; error.
(defmacro syscall* ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (minusp result)
	 (error "Syscall ~A failed: ~A" ,name (get-unix-error-msg))
	 ,success-form)))

(/show0 "unix.lisp 109")

(defmacro void-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values t 0) ,@args))

(defmacro int-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values result 0) ,@args))

;;; from stdio.h

(/show0 "unix.lisp 124")

(defun unix-rename (name1 name2)
  #!+sb-doc
  "Unix-rename renames the file with string name1 to the string
   name2. NIL and an error code is returned if an error occurs."
  (declare (type unix-pathname name1 name2))
  (void-syscall ("rename" c-string c-string) name1 name2))

;;; from stdlib.h

(def-alien-routine ("getenv" posix-getenv) c-string
  "Return the environment string \"name=value\" which corresponds to NAME, or
   NIL if there is none."
  (name c-string))

;;; from sys/types.h and gnu/types.h

(/show0 "unix.lisp 144")

(defconstant +max-s-long+ 2147483647)
(defconstant +max-u-long+ 4294967295)

;;; FIXME: Isn't there some way to use a C wrapper to avoid this hand-copying?
(def-alien-type quad-t #+nil long-long #-nil (array long 2))
(def-alien-type uquad-t #+nil unsigned-long-long
		#-nil (array unsigned-long 2))
(def-alien-type qaddr-t (* quad-t))
(def-alien-type daddr-t int)
(def-alien-type caddr-t (* char))
(def-alien-type swblk-t long)
(def-alien-type size-t unsigned-int)
(def-alien-type time-t long)
(def-alien-type clock-t
  #!+linux long
  #!+bsd   unsigned-long)
(def-alien-type uid-t unsigned-int)
(def-alien-type ssize-t int)

(/show0 "unix.lisp 163")

;;; FIXME: We shouldn't hand-copy types from header files into Lisp like this
;;; unless we have extreme provocation. Reading directories is not extreme
;;; enough, since it doesn't need to be blindingly fast: we can just implement
;;; those functions in C as a wrapper layer.
(def-alien-type fd-mask unsigned-long)
(/show0 "unix.lisp 171")

;;; FIXME: Isn't there some way to use a C wrapper to avoid this hand-copying?
(def-alien-type dev-t
  #!+linux uquad-t
  #!+bsd   unsigned-int)
(def-alien-type uid-t unsigned-int)
(def-alien-type gid-t unsigned-int)
(def-alien-type ino-t
  #!+linux unsigned-long
  #!+bsd   unsigned-int)
(def-alien-type mode-t
  #!+linux unsigned-int
  #!+bsd   unsigned-short)
(def-alien-type nlink-t
  #!+linux unsigned-int
  #!+bsd   unsigned-short)
(/show0 "unix.lisp 190")

;;; FIXME: We shouldn't hand-copy types from header files into Lisp like this
;;; unless we have extreme provocation. Reading directories is not extreme
;;; enough, since it doesn't need to be blindingly fast: we can just implement
;;; those functions in C as a wrapper layer.

(def-alien-type off-t
  #!+linux long
  #!+bsd   quad-t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (/show0 "unix.lisp 215")
  (defconstant fd-setsize 1024))
(/show0 "unix.lisp 217")

(def-alien-type nil
  (struct fd-set
	  (fds-bits (array fd-mask #.(/ fd-setsize 32)))))

(/show0 "unix.lisp 223")

;;;; direntry.h

(def-alien-type nil
  (struct direct
    (d-ino long); inode number of entry
    (d-off off-t)			; offset of next disk directory entry
    (d-reclen unsigned-short)		; length of this record
    (d_type unsigned-char)
    (d-name (array char 256))))		; name must be no longer than this
(/show0 "unix.lisp 241")

;;;; dirent.h

;;; operations on Unix directories

;;;; FIXME: It might be really nice to implement these in C, so that
;;;; we don't need to do horrible things like hand-copying the
;;;; direntry struct slot types into an alien struct.

;;; FIXME: DIRECTORY is an external symbol of package CL, so we should use some
;;; other name for this low-level implementation type.
(defstruct directory
  name
  (dir-struct (required-argument) :type system-area-pointer))
(/show0 "unix.lisp 258")

(def!method print-object ((dir directory) stream)
  (print-unreadable-object (dir stream :type t)
    (prin1 (directory-name dir) stream)))

(/show0 "unix.lisp 264")
(defun open-dir (pathname)
  (declare (type unix-pathname pathname))
  (when (string= pathname "")
    (setf pathname "."))
  (let ((kind (unix-file-kind pathname)))
    (case kind
      (:directory
       (let ((dir-struct
	      (alien-funcall (extern-alien "opendir"
					   (function system-area-pointer
						     c-string))
			     pathname)))
	 (if (zerop (sap-int dir-struct))
	     (values nil (get-errno))
	     (make-directory :name pathname :dir-struct dir-struct))))
      ((nil)
       (values nil enoent))
      (t
       (values nil enotdir)))))
(/show0 "unix.lisp 286")

(defun read-dir (dir)
  (declare (type directory dir))
  (let ((daddr (alien-funcall (extern-alien "readdir"
					    (function system-area-pointer
						      system-area-pointer))
			      (directory-dir-struct dir))))
    (declare (type system-area-pointer daddr))
    (if (zerop (sap-int daddr))
	nil
	(with-alien ((direct (* (struct direct)) daddr))
	  (values (cast (slot direct 'd-name) c-string)
		  (slot direct 'd-ino))))))

(/show0 "unix.lisp 301")
(defun close-dir (dir)
  (declare (type directory dir))
  (alien-funcall (extern-alien "closedir"
			       (function void system-area-pointer))
		 (directory-dir-struct dir))
  nil)

;;; dlfcn.h -> in foreign.lisp

;;; fcntl.h
;;;
;;; POSIX Standard: 6.5 File Control Operations	<fcntl.h>

(/show0 "unix.lisp 318")
(defconstant r_ok 4 #!+sb-doc "Test for read permission")
(defconstant w_ok 2 #!+sb-doc "Test for write permission")
(defconstant x_ok 1 #!+sb-doc "Test for execute permission")
(defconstant f_ok 0 #!+sb-doc "Test for presence of file")

(/show0 "unix.lisp 352")
(defun unix-open (path flags mode)
  #!+sb-doc
  "Unix-open opens the file whose pathname is specified by path
   for reading and/or writing as specified by the flags argument.
   The flags argument can be:

     o_rdonly	Read-only flag.
     o_wronly	Write-only flag.
     o_rdwr	  Read-and-write flag.
     o_append	Append flag.
     o_creat	 Create-if-nonexistent flag.
     o_trunc	 Truncate-to-size-0 flag.
     o_excl	  Error if the file allready exists
     o_noctty	Don't assign controlling tty
     o_ndelay	Non-blocking I/O
     o_sync	  Synchronous I/O
     o_async	 Asynchronous I/O

   If the o_creat flag is specified, then the file is created with
   a permission of argument mode if the file doesn't exist. An
   integer file descriptor is returned by unix-open."
  (declare (type unix-pathname path)
	   (type fixnum flags)
	   (type unix-file-mode mode))
  (int-syscall ("open" c-string int int) path flags mode))

;;; UNIX-CLOSE accepts a file descriptor and attempts to close the file
;;; associated with it.
(/show0 "unix.lisp 391")
(defun unix-close (fd)
  #!+sb-doc
  "Unix-close takes an integer file descriptor as an argument and
   closes the file associated with it. T is returned upon successful
   completion, otherwise NIL and an error number."
  (declare (type unix-fd fd))
  (void-syscall ("close" int) fd))

;;; fcntlbits.h
(eval-when (:compile-toplevel :load-toplevel :execute)

(/show0 "unix.lisp 337")
(defconstant o_rdonly  0) ; read-only flag
(defconstant o_wronly  1) ; write-only flag
(defconstant o_rdwr    2) ; read/write flag
(defconstant o_accmode 3) ; access mode mask
(defconstant o_creat ; create-if-nonexistent flag (not fcntl)
  #!+linux #o100
  #!+bsd   #x0200)
(/show0 "unix.lisp 345")
(defconstant o_excl ; error if already exists (not fcntl)
  #!+linux #o200
  #!+bsd   #x0800)
(defconstant o_noctty ; Don't assign controlling tty. (not fcntl)
  #!+linux #o400
  #!+bsd   #x8000)
(defconstant o_trunc ; truncation flag (not fcntl)
  #!+linux #o1000
  #!+bsd   #x0400)
(defconstant o_append ; append flag
  #!+linux #o2000
  #!+bsd   #x0008)
(/show0 "unix.lisp 361")
) ; EVAL-WHEN

;;;; timebits.h

;; A time value that is accurate to the nearest
;; microsecond but also has a range of years.
(def-alien-type nil
  (struct timeval
	  (tv-sec time-t)		; seconds
	  (tv-usec time-t)))		; and microseconds

;;;; resourcebits.h

(defconstant rusage_self 0 #!+sb-doc "The calling process.")
(defconstant rusage_children -1 #!+sb-doc "Terminated child processes.")
(defconstant rusage_both -2)

(def-alien-type nil
  (struct rusage
    (ru-utime (struct timeval))		; user time used
    (ru-stime (struct timeval))		; system time used.
    (ru-maxrss long)		    ; Maximum resident set size (in kilobytes)
    (ru-ixrss long)			; integral shared memory size
    (ru-idrss long)			; integral unshared data size
    (ru-isrss long)			; integral unshared stack size
    (ru-minflt long)			; page reclaims
    (ru-majflt long)			; page faults
    (ru-nswap long)			; swaps
    (ru-inblock long)			; block input operations
    (ru-oublock long)			; block output operations
    (ru-msgsnd long)			; messages sent
    (ru-msgrcv long)			; messages received
    (ru-nsignals long)			; signals received
    (ru-nvcsw long)			; voluntary context switches
    (ru-nivcsw long)))			; involuntary context switches

;;;; statbuf.h

;;; FIXME: This should go into C code so that we don't need to hand-copy
;;; it from header files.
#!+Linux
(def-alien-type nil
  (struct stat
    (st-dev dev-t)
    (st-pad1 unsigned-short)
    (st-ino ino-t)
    (st-mode mode-t)
    (st-nlink  nlink-t)
    (st-uid  uid-t)
    (st-gid  gid-t)
    (st-rdev dev-t)
    (st-pad2  unsigned-short)
    (st-size off-t)
    (st-blksize unsigned-long)
    (st-blocks unsigned-long)
    (st-atime time-t)
    (unused-1 unsigned-long)
    (st-mtime time-t)
    (unused-2 unsigned-long)
    (st-ctime time-t)
    (unused-3 unsigned-long)
    (unused-4 unsigned-long)
    (unused-5 unsigned-long)))

#!+bsd
(def-alien-type nil
  (struct timespec-t
    (tv-sec long)
    (tv-nsec long)))

#!+bsd
(def-alien-type nil
  (struct stat
    (st-dev dev-t)
    (st-ino ino-t)
    (st-mode mode-t)
    (st-nlink nlink-t)
    (st-uid uid-t)
    (st-gid gid-t)
    (st-rdev dev-t)
    (st-atime (struct timespec-t))
    (st-mtime (struct timespec-t))
    (st-ctime (struct timespec-t))
    (st-size    unsigned-long)		; really quad
    (st-sizeh   unsigned-long)		;
    (st-blocks  unsigned-long)		; really quad
    (st-blocksh unsigned-long)
    (st-blksize unsigned-long)
    (st-flags   unsigned-long)
    (st-gen     unsigned-long)
    (st-lspare  long)
    (st-qspare (array long 4))
    ))

;; encoding of the file mode

(defconstant s-ifmt   #o0170000 #!+sb-doc "These bits determine file type.")

;; file types
(defconstant s-ififo  #o0010000 #!+sb-doc "FIFO")
(defconstant s-ifchr  #o0020000 #!+sb-doc "Character device")
(defconstant s-ifdir  #o0040000 #!+sb-doc "Directory")
(defconstant s-ifblk  #o0060000 #!+sb-doc "Block device")
(defconstant s-ifreg  #o0100000 #!+sb-doc "Regular file")

;; These don't actually exist on System V, but having them doesn't hurt.
(defconstant s-iflnk  #o0120000 #!+sb-doc "Symbolic link.")
(defconstant s-ifsock #o0140000 #!+sb-doc "Socket.")

;;;; unistd.h

;;; values for the second argument to access
(defun unix-access (path mode)
  #!+sb-doc
  "Given a file path (a string) and one of four constant modes,
   UNIX-ACCESS returns T if the file is accessible with that
   mode and NIL if not. It also returns an errno value with
   NIL which determines why the file was not accessible.

   The access modes are:
	r_ok     Read permission.
	w_ok     Write permission.
	x_ok     Execute permission.
	f_ok     Presence of file."
  (declare (type unix-pathname path)
	   (type (mod 8) mode))
  (void-syscall ("access" c-string int) path mode))

(defconstant l_set 0 #!+sb-doc "set the file pointer")
(defconstant l_incr 1 #!+sb-doc "increment the file pointer")
(defconstant l_xtnd 2 #!+sb-doc "extend the file size")

(defun unix-lseek (fd offset whence)
  #!+sb-doc
  "Unix-lseek accepts a file descriptor and moves the file pointer ahead
   a certain offset for that file. Whence can be any of the following:

   l_set	Set the file pointer.
   l_incr       Increment the file pointer.
   l_xtnd       Extend the file size.
  "
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) offset)
	   (type (integer 0 2) whence))
  #!-(and x86 bsd)
  (int-syscall ("lseek" int off-t int) fd offset whence)
  ;; Need a 64-bit return value type for this. TBD. For now,
  ;; don't use this with any 2G+ partitions.
  #!+(and x86 bsd)
  (int-syscall ("lseek" int unsigned-long unsigned-long int)
	       fd offset 0 whence))

;;; UNIX-READ accepts a file descriptor, a buffer, and the length to read.
;;; It attempts to read len bytes from the device associated with fd
;;; and store them into the buffer. It returns the actual number of
;;; bytes read.
(defun unix-read (fd buf len)
  #!+sb-doc
  "Unix-read attempts to read from the file described by fd into
   the buffer buf until it is full. Len is the length of the buffer.
   The number of bytes actually read is returned or NIL and an error
   number if an error occurred."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) len))

  (int-syscall ("read" int (* char) int) fd buf len))

;;; UNIX-WRITE accepts a file descriptor, a buffer, an offset, and the
;;; length to write. It attempts to write len bytes to the device
;;; associated with fd from the the buffer starting at offset. It returns
;;; the actual number of bytes written.
(defun unix-write (fd buf offset len)
  #!+sb-doc
  "Unix-write attempts to write a character buffer (buf) of length
   len to the file described by the file descriptor fd. NIL and an
   error is returned if the call is unsuccessful."
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) offset len))
  (int-syscall ("write" int (* char) int)
	       fd
	       (with-alien ((ptr (* char) (etypecase buf
					    ((simple-array * (*))
					     (vector-sap buf))
					    (system-area-pointer
					     buf))))
		 (addr (deref ptr offset)))
	       len))

;;; UNIX-CHDIR accepts a directory name and makes that the
;;; current working directory.
(defun unix-chdir (path)
  #!+sb-doc
  "Given a file path string, unix-chdir changes the current working
   directory to the one specified."
  (declare (type unix-pathname path))
  (void-syscall ("chdir" c-string) path))

(defun unix-current-directory ()
  #!+sb-doc
  "Return the current directory as a SIMPLE-STRING."
  ;; FIXME: Gcc justifiably complains that getwd is dangerous and should
  ;; not be used; especially with a hardwired 1024 buffer size, yecch.
  ;; This should be rewritten to use getcwd(3), perhaps by writing
  ;; a C service routine to do the actual call to getcwd(3) and check
  ;; of return values.
  (with-alien ((buf (array char 1024)))
    (values (not (zerop (alien-funcall (extern-alien "getwd"
						     (function int (* char)))
				       (cast buf (* char)))))
	    (cast buf c-string))))

;;; UNIX-EXIT terminates a program.
(defun unix-exit (&optional (code 0))
  #!+sb-doc
  "Unix-exit terminates the current process with an optional
   error code. If successful, the call doesn't return. If
   unsuccessful, the call returns NIL and an error number."
  (declare (type (signed-byte 32) code))
  (void-syscall ("exit" int) code))

(def-alien-routine ("getpid" unix-getpid) int
  #!+sb-doc
  "Unix-getpid returns the process-id of the current process.")

(def-alien-routine ("getuid" unix-getuid) int
  #!+sb-doc
  "Unix-getuid returns the real user-id associated with the
   current process.")

(defun unix-readlink (path)
  #!+sb-doc
  "Unix-readlink invokes the readlink system call on the file name
  specified by the simple string path. It returns up to two values:
  the contents of the symbolic link if the call is successful, or
  NIL and the Unix error number."
  (declare (type unix-pathname path))
  (with-alien ((buf (array char 1024)))
    (syscall ("readlink" c-string (* char) int)
	     (let ((string (make-string result)))
	       (sb!kernel:copy-from-system-area
		(alien-sap buf) 0
		string (* sb!vm:vector-data-offset sb!vm:word-bits)
		(* result sb!vm:byte-bits))
	       string)
	     path (cast buf (* char)) 1024)))

;;; UNIX-UNLINK accepts a name and deletes the directory entry for that
;;; name and the file if this is the last link.
(defun unix-unlink (name)
  #!+sb-doc
  "Unix-unlink removes the directory entry for the named file.
   NIL and an error code is returned if the call fails."
  (declare (type unix-pathname name))
  (void-syscall ("unlink" c-string) name))

(defun %set-tty-process-group (pgrp &optional fd)
  #!+sb-doc
  "Set the tty-process-group for the unix file-descriptor FD to PGRP. If not
  supplied, FD defaults to /dev/tty."
  (let ((old-sigs (unix-sigblock (sigmask :sigttou
					  :sigttin
					  :sigtstp
					  :sigchld))))
    (declare (type (unsigned-byte 32) old-sigs))
    (unwind-protect
	(if fd
	    (tcsetpgrp fd pgrp)
	    (multiple-value-bind (tty-fd errno) (unix-open "/dev/tty" o_rdwr 0)
	      (cond (tty-fd
		     (multiple-value-prog1
			 (tcsetpgrp tty-fd pgrp)
		       (unix-close tty-fd)))
		    (t
		     (values nil errno)))))
      (unix-sigsetmask old-sigs))))

(defun unix-gethostname ()
  #!+sb-doc
  "Unix-gethostname returns the name of the host machine as a string."
  (with-alien ((buf (array char 256)))
    (syscall ("gethostname" (* char) int)
	     (cast buf c-string)
	     (cast buf (* char)) 256)))

;;; Unix-fsync writes the core-image of the file described by "fd" to
;;; permanent storage (i.e. disk).

(defun unix-fsync (fd)
  #!+sb-doc
  "Unix-fsync writes the core image of the file described by
   fd to disk."
  (declare (type unix-fd fd))
  (void-syscall ("fsync" int) fd))

;;;; sys/resource.h

;;; FIXME: All we seem to need is the RUSAGE_SELF version of this.
#!-sb-fluid (declaim (inline unix-fast-getrusage))
(defun unix-fast-getrusage (who)
  #!+sb-doc
  "Like call getrusage, but return only the system and user time, and returns
   the seconds and microseconds as separate values."
  (declare (values (member t)
		   (unsigned-byte 31) (mod 1000000)
		   (unsigned-byte 31) (mod 1000000)))
  (with-alien ((usage (struct rusage)))
    (syscall* ("getrusage" int (* (struct rusage)))
	      (values t
		      (slot (slot usage 'ru-utime) 'tv-sec)
		      (slot (slot usage 'ru-utime) 'tv-usec)
		      (slot (slot usage 'ru-stime) 'tv-sec)
		      (slot (slot usage 'ru-stime) 'tv-usec))
	      who (addr usage))))

(defun unix-getrusage (who)
  #!+sb-doc
  "Unix-getrusage returns information about the resource usage
   of the process specified by who. Who can be either the
   current process (rusage_self) or all of the terminated
   child processes (rusage_children). NIL and an error number
   is returned if the call fails."
  (with-alien ((usage (struct rusage)))
    (syscall ("getrusage" int (* (struct rusage)))
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


;;;; sys/select.h

(defmacro unix-fast-select (num-descriptors
			    read-fds write-fds exception-fds
			    timeout-secs &optional (timeout-usecs 0))
  #!+sb-doc
  "Perform the UNIX select(2) system call."
  (declare (type (integer 0 #.FD-SETSIZE) num-descriptors)
	   (type (or (alien (* (struct fd-set))) null)
		 read-fds write-fds exception-fds)
	   (type (or null (unsigned-byte 31)) timeout-secs)
	   (type (unsigned-byte 31) timeout-usecs) )
  ;; FIXME: CMU CL had
  ;;   (optimize (speed 3) (safety 0) (inhibit-warnings 3))
  ;; in the declarations above. If they're important, they should
  ;; be in a declaration inside the LET expansion, not in the
  ;; macro compile-time code.
  `(let ((timeout-secs ,timeout-secs))
     (with-alien ((tv (struct timeval)))
       (when timeout-secs
	 (setf (slot tv 'tv-sec) timeout-secs)
	 (setf (slot tv 'tv-usec) ,timeout-usecs))
       (int-syscall ("select" int (* (struct fd-set)) (* (struct fd-set))
		     (* (struct fd-set)) (* (struct timeval)))
		    ,num-descriptors ,read-fds ,write-fds ,exception-fds
		    (if timeout-secs (alien-sap (addr tv)) (int-sap 0))))))

;;; Unix-select accepts sets of file descriptors and waits for an event
;;; to happen on one of them or to time out.

(defmacro num-to-fd-set (fdset num)
  `(if (fixnump ,num)
       (progn
	 (setf (deref (slot ,fdset 'fds-bits) 0) ,num)
	 ,@(loop for index upfrom 1 below (/ fd-setsize 32)
	     collect `(setf (deref (slot ,fdset 'fds-bits) ,index) 0)))
       (progn
	 ,@(loop for index upfrom 0 below (/ fd-setsize 32)
	     collect `(setf (deref (slot ,fdset 'fds-bits) ,index)
			    (ldb (byte 32 ,(* index 32)) ,num))))))

(defmacro fd-set-to-num (nfds fdset)
  `(if (<= ,nfds 32)
       (deref (slot ,fdset 'fds-bits) 0)
       (+ ,@(loop for index upfrom 0 below (/ fd-setsize 32)
	      collect `(ash (deref (slot ,fdset 'fds-bits) ,index)
			    ,(* index 32))))))

(defun unix-select (nfds rdfds wrfds xpfds to-secs &optional (to-usecs 0))
  #!+sb-doc
  "Unix-select examines the sets of descriptors passed as arguments
   to see whether they are ready for reading and writing. See the UNIX
   Programmers Manual for more information."
  (declare (type (integer 0 #.FD-SETSIZE) nfds)
	   (type unsigned-byte rdfds wrfds xpfds)
	   (type (or (unsigned-byte 31) null) to-secs)
	   (type (unsigned-byte 31) to-usecs)
	   (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  (with-alien ((tv (struct timeval))
	       (rdf (struct fd-set))
	       (wrf (struct fd-set))
	       (xpf (struct fd-set)))
    (when to-secs
      (setf (slot tv 'tv-sec) to-secs)
      (setf (slot tv 'tv-usec) to-usecs))
    (num-to-fd-set rdf rdfds)
    (num-to-fd-set wrf wrfds)
    (num-to-fd-set xpf xpfds)
    (macrolet ((frob (lispvar alienvar)
		 `(if (zerop ,lispvar)
		      (int-sap 0)
		      (alien-sap (addr ,alienvar)))))
      (syscall ("select" int (* (struct fd-set)) (* (struct fd-set))
		(* (struct fd-set)) (* (struct timeval)))
	       (values result
		       (fd-set-to-num nfds rdf)
		       (fd-set-to-num nfds wrf)
		       (fd-set-to-num nfds xpf))
	       nfds (frob rdfds rdf) (frob wrfds wrf) (frob xpfds xpf)
	       (if to-secs (alien-sap (addr tv)) (int-sap 0))))))

;;;; sys/stat.h

;;; FIXME: This is only used in this file, and needn't be in target Lisp
;;; runtime. It's also unclear why it needs to be a macro instead of a
;;; function. Perhaps it should become a FLET.
(defmacro extract-stat-results (buf)
  `(values T
	   #!+bsd
	   (slot ,buf 'st-dev)
	   #!+linux
	   (+ (deref (slot ,buf 'st-dev) 0)
	      (* (+ +max-u-long+  1)
		 (deref (slot ,buf 'st-dev) 1)))   ;;; let's hope this works..
	   (slot ,buf 'st-ino)
	   (slot ,buf 'st-mode)
	   (slot ,buf 'st-nlink)
	   (slot ,buf 'st-uid)
	   (slot ,buf 'st-gid)
	   #!+bsd
	   (slot ,buf 'st-rdev)
	   #!+linux
	   (+ (deref (slot ,buf 'st-rdev) 0)
	      (* (+ +max-u-long+  1)
		 (deref (slot ,buf 'st-rdev) 1)))   ;;; let's hope this works..
	   #!+linux (slot ,buf 'st-size)
	   #!+bsd
	   (+ (slot ,buf 'st-size)
	      (* (+ +max-u-long+ 1)
		 (slot ,buf 'st-sizeh)))
	   #!+linux (slot ,buf 'st-atime)
	   #!+bsd   (slot (slot ,buf 'st-atime) 'tv-sec)
	   #!+linux (slot ,buf 'st-mtime)
	   #!+bsd   (slot (slot ,buf 'st-mtime) 'tv-sec)
	   #!+linux (slot ,buf 'st-ctime)
	   #!+bsd   (slot (slot ,buf 'st-ctime) 'tv-sec)
	   (slot ,buf 'st-blksize)
	   #!+linux (slot ,buf 'st-blocks)
	   #!+bsd
	   (+ (slot ,buf 'st-blocks)
	      (* (+ +max-u-long+ 1)
		 (slot ,buf 'st-blocksh)))
	   ))

(defun unix-stat (name)
  #!+sb-doc
  "Unix-stat retrieves information about the specified
   file returning them in the form of multiple values.
   See the UNIX Programmer's Manual for a description
   of the values returned. If the call fails, then NIL
   and an error number is returned instead."
  (declare (type unix-pathname name))
  (when (string= name "")
    (setf name "."))
  (with-alien ((buf (struct stat)))
    (syscall ("stat" c-string (* (struct stat)))
	     (extract-stat-results buf)
	     name (addr buf))))

(defun unix-fstat (fd)
  #!+sb-doc
  "Unix-fstat is similar to unix-stat except the file is specified
   by the file descriptor fd."
  (declare (type unix-fd fd))
  (with-alien ((buf (struct stat)))
    (syscall ("fstat" int (* (struct stat)))
	     (extract-stat-results buf)
	     fd (addr buf))))

(defun unix-lstat (name)
  #!+sb-doc
  "Unix-lstat is similar to unix-stat except the specified
   file must be a symbolic link."
  (declare (type unix-pathname name))
  (with-alien ((buf (struct stat)))
    (syscall ("lstat" c-string (* (struct stat)))
	     (extract-stat-results buf)
	     name (addr buf))))

;;; UNIX-MKDIR accepts a name and a mode and attempts to create the
;;; corresponding directory with mode mode.
(defun unix-mkdir (name mode)
  #!+sb-doc
  "Unix-mkdir creates a new directory with the specified name and mode.
   (Same as those for unix-fchmod.)  It returns T upon success, otherwise
   NIL and an error number."
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (void-syscall ("mkdir" c-string int) name mode))

;;;; time.h

;; POSIX.4 structure for a time value. This is like a `struct timeval' but
;; has nanoseconds instead of microseconds.

(def-alien-type nil
    (struct timespec
	    (tv-sec long)   ;Seconds
	    (tv-nsec long))) ;Nanoseconds

;; Used by other time functions.
(def-alien-type nil
    (struct tm
	    (tm-sec int)   ; Seconds.	[0-60] (1 leap second)
	    (tm-min int)   ; Minutes.	[0-59]
	    (tm-hour int)  ; Hours.	[0-23]
	    (tm-mday int)  ; Day.		[1-31]
	    (tm-mon int)   ;  Month.	[0-11]
	    (tm-year int)  ; Year	- 1900.
	    (tm-wday int)  ; Day of week.	[0-6]
	    (tm-yday int)  ; Days in year.[0-365]
	    (tm-isdst int) ;  DST.		[-1/0/1]
	    (tm-gmtoff long)	;  Seconds east of UTC.
	    (tm-zone c-string)))	; Timezone abbreviation.

(def-alien-variable ("tzname" unix-tzname) (array c-string 2))

(def-alien-routine get-timezone sb!c-call:void
  (when sb!c-call:long :in)
  (minutes-west sb!c-call:int :out)
  (daylight-savings-p sb!alien:boolean :out))

(defun unix-get-minutes-west (secs)
  (multiple-value-bind (ignore minutes dst) (get-timezone secs)
    (declare (ignore ignore) (ignore dst))
    (values minutes)))

(defun unix-get-timezone (secs)
  (multiple-value-bind (ignore minutes dst) (get-timezone secs)
    (declare (ignore ignore) (ignore minutes))
    (values (deref unix-tzname (if dst 1 0)))))

;;;; sys/time.h

;;; Structure crudely representing a timezone. KLUDGE: This is
;;; obsolete and should never be used.
(def-alien-type nil
  (struct timezone
    (tz-minuteswest int)		; minutes west of Greenwich
    (tz-dsttime	int)))			; type of dst correction

#!-sb-fluid (declaim (inline unix-gettimeofday))
(defun unix-gettimeofday ()
  #!+sb-doc
  "If it works, unix-gettimeofday returns 5 values: T, the seconds and
   microseconds of the current time of day, the timezone (in minutes west
   of Greenwich), and a daylight-savings flag. If it doesn't work, it
   returns NIL and the errno."
  (with-alien ((tv (struct timeval))
	       (tz (struct timezone)))
    (syscall* ("gettimeofday" (* (struct timeval))
			      (* (struct timezone)))
	      (values T
		      (slot tv 'tv-sec)
		      (slot tv 'tv-usec)
		      (slot tz 'tz-minuteswest)
		      (slot tz 'tz-dsttime))
	      (addr tv)
	      (addr tz))))

;;;; asm/errno.h

#|
(def-unix-error ESUCCESS 0 "Successful")
(def-unix-error EPERM 1 "Operation not permitted")
|#
(def-unix-error ENOENT 2 "No such file or directory")
#|
(def-unix-error ESRCH 3 "No such process")
(def-unix-error EINTR 4 "Interrupted system call")
(def-unix-error EIO 5 "I/O error")
(def-unix-error ENXIO 6 "No such device or address")
(def-unix-error E2BIG 7 "Arg list too long")
(def-unix-error ENOEXEC 8 "Exec format error")
(def-unix-error EBADF 9 "Bad file number")
(def-unix-error ECHILD 10 "No children")
(def-unix-error EAGAIN 11 "Try again")
(def-unix-error ENOMEM 12 "Out of memory")
|#
(def-unix-error EACCES 13 "Permission denied")
#|
(def-unix-error EFAULT 14 "Bad address")
(def-unix-error ENOTBLK 15 "Block device required")
(def-unix-error EBUSY 16 "Device or resource busy")
|#
(def-unix-error EEXIST 17 "File exists")
#|
(def-unix-error EXDEV 18 "Cross-device link")
(def-unix-error ENODEV 19 "No such device")
|#
(def-unix-error ENOTDIR 20 "Not a directory")
#|
(def-unix-error EISDIR 21 "Is a directory")
(def-unix-error EINVAL 22 "Invalid argument")
(def-unix-error ENFILE 23 "File table overflow")
(def-unix-error EMFILE 24 "Too many open files")
(def-unix-error ENOTTY 25 "Not a typewriter")
(def-unix-error ETXTBSY 26 "Text file busy")
(def-unix-error EFBIG 27 "File too large")
(def-unix-error ENOSPC 28 "No space left on device")
|#
(def-unix-error ESPIPE 29 "Illegal seek")
#|
(def-unix-error EROFS 30 "Read-only file system")
(def-unix-error EMLINK 31 "Too many links")
(def-unix-error EPIPE 32 "Broken pipe")
|#

#|
;;; Math
(def-unix-error EDOM 33 "Math argument out of domain")
(def-unix-error ERANGE 34 "Math result not representable")
(def-unix-error  EDEADLK	 35     "Resource deadlock would occur")
(def-unix-error  ENAMETOOLONG    36     "File name too long")
(def-unix-error  ENOLCK	  37     "No record locks available")
(def-unix-error  ENOSYS	  38     "Function not implemented")
(def-unix-error  ENOTEMPTY       39     "Directory not empty")
(def-unix-error  ELOOP	   40     "Too many symbolic links encountered")
|#
(def-unix-error  EWOULDBLOCK     11     "Operation would block")
(/show0 "unix.lisp 3192")
#|
(def-unix-error  ENOMSG	  42     "No message of desired type")
(def-unix-error  EIDRM	   43     "Identifier removed")
(def-unix-error  ECHRNG	  44     "Channel number out of range")
(def-unix-error  EL2NSYNC	45     "Level 2 not synchronized")
(def-unix-error  EL3HLT	  46     "Level 3 halted")
(def-unix-error  EL3RST	  47     "Level 3 reset")
(def-unix-error  ELNRNG	  48     "Link number out of range")
(def-unix-error  EUNATCH	 49     "Protocol driver not attached")
(def-unix-error  ENOCSI	  50     "No CSI structure available")
(def-unix-error  EL2HLT	  51     "Level 2 halted")
(def-unix-error  EBADE	   52     "Invalid exchange")
(def-unix-error  EBADR	   53     "Invalid request descriptor")
(def-unix-error  EXFULL	  54     "Exchange full")
(def-unix-error  ENOANO	  55     "No anode")
(def-unix-error  EBADRQC	 56     "Invalid request code")
(def-unix-error  EBADSLT	 57     "Invalid slot")
(def-unix-error  EDEADLOCK       EDEADLK     "File locking deadlock error")
(def-unix-error  EBFONT	  59     "Bad font file format")
(def-unix-error  ENOSTR	  60     "Device not a stream")
(def-unix-error  ENODATA	 61     "No data available")
(def-unix-error  ETIME	   62     "Timer expired")
(def-unix-error  ENOSR	   63     "Out of streams resources")
(def-unix-error  ENONET	  64     "Machine is not on the network")
(def-unix-error  ENOPKG	  65     "Package not installed")
(def-unix-error  EREMOTE	 66     "Object is remote")
(def-unix-error  ENOLINK	 67     "Link has been severed")
(def-unix-error  EADV	    68     "Advertise error")
(def-unix-error  ESRMNT	  69     "Srmount error")
(def-unix-error  ECOMM	   70     "Communication error on send")
(def-unix-error  EPROTO	  71     "Protocol error")
(def-unix-error  EMULTIHOP       72     "Multihop attempted")
(def-unix-error  EDOTDOT	 73     "RFS specific error")
(def-unix-error  EBADMSG	 74     "Not a data message")
(def-unix-error  EOVERFLOW       75     "Value too large for defined data type")
(def-unix-error  ENOTUNIQ	76     "Name not unique on network")
(def-unix-error  EBADFD	  77     "File descriptor in bad state")
(def-unix-error  EREMCHG	 78     "Remote address changed")
(def-unix-error  ELIBACC	 79     "Can not access a needed shared library")
(def-unix-error  ELIBBAD	 80     "Accessing a corrupted shared library")
(def-unix-error  ELIBSCN	 81     ".lib section in a.out corrupted")
(def-unix-error  ELIBMAX	 82     "Attempting to link in too many shared libraries")
(def-unix-error  ELIBEXEC	83     "Cannot exec a shared library directly")
(def-unix-error  EILSEQ	  84     "Illegal byte sequence")
(def-unix-error  ERESTART	85     "Interrupted system call should be restarted ")
(def-unix-error  ESTRPIPE	86     "Streams pipe error")
(def-unix-error  EUSERS	  87     "Too many users")
(def-unix-error  ENOTSOCK	88     "Socket operation on non-socket")
(def-unix-error  EDESTADDRREQ    89     "Destination address required")
(def-unix-error  EMSGSIZE	90     "Message too long")
(def-unix-error  EPROTOTYPE      91     "Protocol wrong type for socket")
(def-unix-error  ENOPROTOOPT     92     "Protocol not available")
(def-unix-error  EPROTONOSUPPORT 93     "Protocol not supported")
(def-unix-error  ESOCKTNOSUPPORT 94     "Socket type not supported")
(def-unix-error  EOPNOTSUPP      95     "Operation not supported on transport endpoint")
(def-unix-error  EPFNOSUPPORT    96     "Protocol family not supported")
(def-unix-error  EAFNOSUPPORT    97     "Address family not supported by protocol")
(def-unix-error  EADDRINUSE      98     "Address already in use")
(def-unix-error  EADDRNOTAVAIL   99     "Cannot assign requested address")
(def-unix-error  ENETDOWN	100    "Network is down")
(def-unix-error  ENETUNREACH     101    "Network is unreachable")
(def-unix-error  ENETRESET       102    "Network dropped connection because of reset")
(def-unix-error  ECONNABORTED    103    "Software caused connection abort")
(def-unix-error  ECONNRESET      104    "Connection reset by peer")
(def-unix-error  ENOBUFS	 105    "No buffer space available")
(def-unix-error  EISCONN	 106    "Transport endpoint is already connected")
(def-unix-error  ENOTCONN	107    "Transport endpoint is not connected")
(def-unix-error  ESHUTDOWN       108    "Cannot send after transport endpoint shutdown")
(def-unix-error  ETOOMANYREFS    109    "Too many references: cannot splice")
(def-unix-error  ETIMEDOUT       110    "Connection timed out")
(def-unix-error  ECONNREFUSED    111    "Connection refused")
(def-unix-error  EHOSTDOWN       112    "Host is down")
(def-unix-error  EHOSTUNREACH    113    "No route to host")
(def-unix-error  EALREADY	114    "Operation already in progress")
(def-unix-error  EINPROGRESS     115    "Operation now in progress")
(def-unix-error  ESTALE	  116    "Stale NFS file handle")
(def-unix-error  EUCLEAN	 117    "Structure needs cleaning")
(def-unix-error  ENOTNAM	 118    "Not a XENIX named type file")
(def-unix-error  ENAVAIL	 119    "No XENIX semaphores available")
(def-unix-error  EISNAM	  120    "Is a named type file")
(def-unix-error  EREMOTEIO       121    "Remote I/O error")
(def-unix-error  EDQUOT	  122    "Quota exceeded")
|#

;;; And now for something completely different ...
(emit-unix-errors)

;;;; support routines for dealing with unix pathnames

(defun unix-file-kind (name &optional check-for-links)
  #!+sb-doc
  "Return either :FILE, :DIRECTORY, :LINK, :SPECIAL, or NIL."
  (declare (simple-string name))
  (multiple-value-bind (res dev ino mode)
      (if check-for-links (unix-lstat name) (unix-stat name))
    (declare (type (or fixnum null) mode)
	     (ignore dev ino))
    (when res
      (let ((kind (logand mode s-ifmt)))
	(cond ((eql kind s-ifdir) :directory)
	      ((eql kind s-ifreg) :file)
	      ((eql kind s-iflnk) :link)
	      (t :special))))))

(defun unix-maybe-prepend-current-directory (name)
  (declare (simple-string name))
  (if (and (> (length name) 0) (char= (schar name 0) #\/))
      name
      (multiple-value-bind (win dir) (unix-current-directory)
	(if win
	    (concatenate 'simple-string dir "/" name)
	    name))))

(defun unix-resolve-links (pathname)
  #!+sb-doc
  "Returns the pathname with all symbolic links resolved."
  (declare (simple-string pathname))
  (let ((len (length pathname))
	(pending pathname))
    (declare (fixnum len) (simple-string pending))
    (if (zerop len)
	pathname
	(let ((result (make-string 1024 :initial-element (code-char 0)))
	      (fill-ptr 0)
	      (name-start 0))
	  (loop
	    (let* ((name-end (or (position #\/ pending :start name-start) len))
		   (new-fill-ptr (+ fill-ptr (- name-end name-start))))
	      (replace result pending
		       :start1 fill-ptr
		       :end1 new-fill-ptr
		       :start2 name-start
		       :end2 name-end)
	      (let ((kind (unix-file-kind (if (zerop name-end) "/" result) t)))
		(unless kind (return nil))
		(cond ((eq kind :link)
		       (multiple-value-bind (link err) (unix-readlink result)
			 (unless link
			   (error "error reading link ~S: ~S"
				  (subseq result 0 fill-ptr)
				  (get-unix-error-msg err)))
			 (cond ((or (zerop (length link))
				    (char/= (schar link 0) #\/))
				;; It's a relative link.
				(fill result (code-char 0)
				      :start fill-ptr
				      :end new-fill-ptr))
			       ((string= result "/../" :end1 4)
				;; It's across the super-root.
				(let ((slash (or (position #\/ result :start 4)
						 0)))
				  (fill result (code-char 0)
					:start slash
					:end new-fill-ptr)
				  (setf fill-ptr slash)))
			       (t
				;; It's absolute.
				(and (> (length link) 0)
				     (char= (schar link 0) #\/))
				(fill result (code-char 0) :end new-fill-ptr)
				(setf fill-ptr 0)))
			 (setf pending
			       (if (= name-end len)
				   link
				   (concatenate 'simple-string
						link
						(subseq pending name-end))))
			 (setf len (length pending))
			 (setf name-start 0)))
		      ((= name-end len)
		       (return (subseq result 0 new-fill-ptr)))
		      ((eq kind :directory)
		       (setf (schar result new-fill-ptr) #\/)
		       (setf fill-ptr (1+ new-fill-ptr))
		       (setf name-start (1+ name-end)))
		      (t
		       (return nil))))))))))

(defun unix-simplify-pathname (src)
  (declare (simple-string src))
  (let* ((src-len (length src))
	 (dst (make-string src-len))
	 (dst-len 0)
	 (dots 0)
	 (last-slash nil))
    (macrolet ((deposit (char)
			`(progn
			   (setf (schar dst dst-len) ,char)
			   (incf dst-len))))
      (dotimes (src-index src-len)
	(let ((char (schar src src-index)))
	  (cond ((char= char #\.)
		 (when dots
		   (incf dots))
		 (deposit char))
		((char= char #\/)
		 (case dots
		   (0
		    ;; Either ``/...' or ``...//...'
		    (unless last-slash
		      (setf last-slash dst-len)
		      (deposit char)))
		   (1
		    ;; Either ``./...'' or ``..././...''
		    (decf dst-len))
		   (2
		    ;; We've found ..
		    (cond
		     ((and last-slash (not (zerop last-slash)))
		      ;; There is something before this ..
		      (let ((prev-prev-slash
			     (position #\/ dst :end last-slash :from-end t)))
			(cond ((and (= (+ (or prev-prev-slash 0) 2)
				       last-slash)
				    (char= (schar dst (- last-slash 2)) #\.)
				    (char= (schar dst (1- last-slash)) #\.))
			       ;; The something before this .. is another ..
			       (deposit char)
			       (setf last-slash dst-len))
			      (t
			       ;; The something is some directory or other.
			       (setf dst-len
				     (if prev-prev-slash
					 (1+ prev-prev-slash)
					 0))
			       (setf last-slash prev-prev-slash)))))
		     (t
		      ;; There is nothing before this .., so we need to keep it
		      (setf last-slash dst-len)
		      (deposit char))))
		   (t
		    ;; Something other than a dot between slashes.
		    (setf last-slash dst-len)
		    (deposit char)))
		 (setf dots 0))
		(t
		 (setf dots nil)
		 (setf (schar dst dst-len) char)
		 (incf dst-len))))))
    (when (and last-slash (not (zerop last-slash)))
      (case dots
	(1
	 ;; We've got  ``foobar/.''
	 (decf dst-len))
	(2
	 ;; We've got ``foobar/..''
	 (unless (and (>= last-slash 2)
		      (char= (schar dst (1- last-slash)) #\.)
		      (char= (schar dst (- last-slash 2)) #\.)
		      (or (= last-slash 2)
			  (char= (schar dst (- last-slash 3)) #\/)))
	   (let ((prev-prev-slash
		  (position #\/ dst :end last-slash :from-end t)))
	     (if prev-prev-slash
		 (setf dst-len (1+ prev-prev-slash))
		 (return-from unix-simplify-pathname "./")))))))
    (cond ((zerop dst-len)
	   "./")
	  ((= dst-len src-len)
	   dst)
	  (t
	   (subseq dst 0 dst-len)))))

;;;; stuff not yet found in the header files
;;;;
;;;; Abandon all hope who enters here...

;;; not checked for linux...
(defmacro fd-set (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (setf (deref (slot ,fd-set 'fds-bits) ,word)
	     (logior (truly-the (unsigned-byte 32) (ash 1 ,bit))
		     (deref (slot ,fd-set 'fds-bits) ,word))))))

;;; not checked for linux...
(defmacro fd-clr (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (setf (deref (slot ,fd-set 'fds-bits) ,word)
	     (logand (deref (slot ,fd-set 'fds-bits) ,word)
		     (sb!kernel:32bit-logical-not
		      (truly-the (unsigned-byte 32) (ash 1 ,bit))))))))

;;; not checked for linux...
(defmacro fd-isset (offset fd-set)
  (let ((word (gensym))
	(bit (gensym)))
    `(multiple-value-bind (,word ,bit) (floor ,offset 32)
       (logbitp ,bit (deref (slot ,fd-set 'fds-bits) ,word)))))

;;; not checked for linux...
(defmacro fd-zero (fd-set)
  `(progn
     ,@(loop for index upfrom 0 below (/ fd-setsize 32)
	 collect `(setf (deref (slot ,fd-set 'fds-bits) ,index) 0))))

(/show0 "unix.lisp 3555")
