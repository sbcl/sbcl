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

(defmacro def-enum (inc cur &rest names)
  (flet ((defform (name)
	   (prog1 (when name `(defconstant ,name ,cur))
	     (setf cur (funcall inc cur 1)))))
    `(progn ,@(mapcar #'defform names))))

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
(deftype unix-fd () `(integer 0 ,most-positive-fixnum))

(deftype unix-file-mode () '(unsigned-byte 32))
(deftype unix-pid () '(unsigned-byte 32))
(deftype unix-uid () '(unsigned-byte 32))
(deftype unix-gid () '(unsigned-byte 32))

;;;; system calls

(/show0 "unix.lisp 74")

;;; FIXME: The various FOO-SYSCALL-BAR macros, and perhaps some other
;;; macros in this file, are only used in this file, and could be
;;; implemented using SB!XC:DEFMACRO wrapped in EVAL-WHEN.

(defmacro syscall ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (minusp result)
	 (values nil (get-errno))
	 ,success-form)))

;;; This is like SYSCALL, but if it fails, signal an error instead of
;;; returning error codes. Should only be used for syscalls that will
;;; never really get an error.
(defmacro syscall* ((name &rest arg-types) success-form &rest args)
  `(let ((result (alien-funcall (extern-alien ,name (function int ,@arg-types))
				,@args)))
     (if (minusp result)
	 (error "Syscall ~A failed: ~A" ,name (strerror))
	 ,success-form)))

(/show0 "unix.lisp 109")

(defmacro void-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values t 0) ,@args))

(defmacro int-syscall ((name &rest arg-types) &rest args)
  `(syscall (,name ,@arg-types) (values result 0) ,@args))

;;;; hacking the Unix environment

(def-alien-routine ("getenv" posix-getenv) c-string
  "Return the environment string \"name=value\" which corresponds to NAME, or
   NIL if there is none."
  (name c-string))

;;; from stdio.h

;;; Rename the file with string NAME1 to the string NAME2. NIL and an
;;; error code is returned if an error occurs.
(defun unix-rename (name1 name2)
  (declare (type unix-pathname name1 name2))
  (void-syscall ("rename" c-string c-string) name1 name2))

;;; from sys/types.h and gnu/types.h

(/show0 "unix.lisp 220")

;;; FIXME: Isn't there some way to use a C wrapper to avoid this hand-copying?
(defconstant +max-s-long+ 2147483647)
(defconstant +max-u-long+ 4294967295)
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

;;; FIXME: We shouldn't hand-copy types from header files into Lisp like this
;;; unless we have extreme provocation. Reading directories is not extreme
;;; enough, since it doesn't need to be blindingly fast: we can just implement
;;; those functions in C as a wrapper layer.
(def-alien-type fd-mask unsigned-long)

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
(/show0 "unix.lisp 263")

;;; FIXME: We shouldn't hand-copy types from header files into Lisp like this
;;; unless we have extreme provocation. Reading directories is not extreme
;;; enough, since it doesn't need to be blindingly fast: we can just implement
;;; those functions in C as a wrapper layer.

(def-alien-type off-t
  #!+linux long
  #!+bsd   quad-t)

(defconstant fd-setsize 1024)

(def-alien-type nil
  (struct fd-set
	  (fds-bits (array fd-mask #.(/ fd-setsize 32)))))

(/show0 "unix.lisp 304")

;;;; direntry.h
;;;; dirent.h
;;;; 
;;;; (CMU CL copied stuff out of these, but as of 0.6.11.41, SBCL
;;;; doesn't need to, instead calling C-level wrapper code to handle
;;;; all the opendir/readdir/closedir stuff.)

;;;; fcntl.h
;;;;
;;;; POSIX Standard: 6.5 File Control Operations	<fcntl.h>

(/show0 "unix.lisp 356")
(defconstant r_ok 4 #!+sb-doc "Test for read permission")
(defconstant w_ok 2 #!+sb-doc "Test for write permission")
(defconstant x_ok 1 #!+sb-doc "Test for execute permission")
(defconstant f_ok 0 #!+sb-doc "Test for presence of file")

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
  (int-syscall ("open" c-string int int) path flags mode))

;;; UNIX-CLOSE accepts a file descriptor and attempts to close the file
;;; associated with it.
(/show0 "unix.lisp 391")
(defun unix-close (fd)
  (declare (type unix-fd fd))
  (void-syscall ("close" int) fd))

;;;; fcntlbits.h

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

;;;; timebits.h

;; A time value that is accurate to the nearest
;; microsecond but also has a range of years.
(def-alien-type nil
  (struct timeval
	  (tv-sec time-t)		; seconds
	  (tv-usec time-t)))		; and microseconds

;;;; resourcebits.h

(defconstant rusage_self 0) ; the calling process
(defconstant rusage_children -1) ; terminated child processes
(defconstant rusage_both -2)

(def-alien-type nil
  (struct rusage
    (ru-utime (struct timeval))	    ; user time used
    (ru-stime (struct timeval))	    ; system time used.
    (ru-maxrss long)		    ; maximum resident set size (in kilobytes)
    (ru-ixrss long)		    ; integral shared memory size
    (ru-idrss long)		    ; integral unshared data size
    (ru-isrss long)		    ; integral unshared stack size
    (ru-minflt long)		    ; page reclaims
    (ru-majflt long)		    ; page faults
    (ru-nswap long)		    ; swaps
    (ru-inblock long)		    ; block input operations
    (ru-oublock long)		    ; block output operations
    (ru-msgsnd long)		    ; messages sent
    (ru-msgrcv long)		    ; messages received
    (ru-nsignals long)		    ; signals received
    (ru-nvcsw long)		    ; voluntary context switches
    (ru-nivcsw long)))		    ; involuntary context switches

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

;;; These bits determine file type.
(defconstant s-ifmt   #o0170000)

;; basic file types, exist even on System V
(defconstant s-ififo  #o0010000) ; FIFO
(defconstant s-ifchr  #o0020000) ; Character device
(defconstant s-ifdir  #o0040000) ; Directory
(defconstant s-ifblk  #o0060000) ; Block device
(defconstant s-ifreg  #o0100000) ; Regular file

;; more file types: These don't actually exist on System V, but having
;; them doesn't hurt.
(defconstant s-iflnk  #o0120000) ; Symbolic link
(defconstant s-ifsock #o0140000) ; Socket

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
(defun unix-access (path mode)
  (declare (type unix-pathname path)
	   (type (mod 8) mode))
  (void-syscall ("access" c-string int) path mode))

;;; values for the second argument to UNIX-LSEEK
(defconstant l_set 0) ; to set the file pointer
(defconstant l_incr 1) ; to increment the file pointer
(defconstant l_xtnd 2) ; to extend the file size

;;; Accept a file descriptor and move the file pointer ahead
;;; a certain offset for that file. WHENCE can be any of the following:
;;;  L_SET     Set the file pointer.
;;;  L_INCR    Increment the file pointer.
;;;  L_XTND    Extend the file size.
(defun unix-lseek (fd offset whence)
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
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) len))

  (int-syscall ("read" int (* char) int) fd buf len))

;;; UNIX-WRITE accepts a file descriptor, a buffer, an offset, and the
;;; length to write. It attempts to write len bytes to the device
;;; associated with fd from the the buffer starting at offset. It returns
;;; the actual number of bytes written.
(defun unix-write (fd buf offset len)
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

;;; Set up a unix-piping mechanism consisting of an input pipe and an
;;; output pipe. Return two values: if no error occurred the first
;;; value is the pipe to be read from and the second is can be written
;;; to. If an error occurred the first value is NIL and the second the
;;; unix error code.
(defun unix-pipe ()
  (with-alien ((fds (array int 2)))
    (syscall ("pipe" (* int))
	     (values (deref fds 0) (deref fds 1))
	     (cast fds (* int)))))

;;; UNIX-CHDIR accepts a directory name and makes that the
;;; current working directory.
(defun unix-chdir (path)
  (declare (type unix-pathname path))
  (void-syscall ("chdir" c-string) path))

;;; Return the current directory as a SIMPLE-STRING.
(defun unix-current-directory ()
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

;;; Duplicate an existing file descriptor (given as the argument) and
;;; return it. If FD is not a valid file descriptor, NIL and an error
;;; number are returned.
(defun unix-dup (fd)
  (declare (type unix-fd fd))
  (int-syscall ("dup" int) fd))

;;; Terminate the current process with an optional error code. If
;;; successful, the call doesn't return. If unsuccessful, the call
;;; returns NIL and an error number.
(defun unix-exit (&optional (code 0))
  (declare (type (signed-byte 32) code))
  (void-syscall ("exit" int) code))

;;; Return the process id of the current process.
(def-alien-routine ("getpid" unix-getpid) int)

;;; Return the real user-id associated with the current process.
(def-alien-routine ("getuid" unix-getuid) int)

;;; Invoke readlink(2) on the file name specified by the simple string
;;; PATH. Return up to two values: the contents of the symbolic link
;;; if the call is successful, or NIL and the Unix error number.
(defun unix-readlink (path)
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
  (declare (type unix-pathname name))
  (void-syscall ("unlink" c-string) name))

;;; Set the tty-process-group for the unix file-descriptor FD to PGRP.
;;; If not supplied, FD defaults to "/dev/tty".
(defun %set-tty-process-group (pgrp &optional fd)
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

;;; Return the name of the host machine as a string.
(defun unix-gethostname ()
  (with-alien ((buf (array char 256)))
    (syscall ("gethostname" (* char) int)
	     (cast buf c-string)
	     (cast buf (* char)) 256)))

;;; Write the core image of the file described by FD to disk.
(defun unix-fsync (fd)
  (declare (type unix-fd fd))
  (void-syscall ("fsync" int) fd))

;;;; sys/ioctl.h

;;; UNIX-IOCTL performs a variety of operations on open i/o
;;; descriptors. See the UNIX Programmer's Manual for more
;;; information.
(defun unix-ioctl (fd cmd arg)
  (declare (type unix-fd fd)
	   (type (unsigned-byte 32) cmd))
  (void-syscall ("ioctl" int unsigned-int (* char)) fd cmd arg))

;;;; sys/resource.h

;;; FIXME: All we seem to need is the RUSAGE_SELF version of this.
;;;
;;; Like getrusage(2), but return only the system and user time,
;;; and return the seconds and microseconds as separate values.
#!-sb-fluid (declaim (inline unix-fast-getrusage))
(defun unix-fast-getrusage (who)
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

;;; Return information about the resource usage of the process
;;; specified by WHO. WHO can be either the current process
;;; (rusage_self) or all of the terminated child processes
;;; (rusage_children). NIL and an error number is returned if the call
;;; fails.
(defun unix-getrusage (who)
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

;;; UNIX-SELECT accepts sets of file descriptors and waits for an event
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

;;; Examine the sets of descriptors passed as arguments to see whether
;;; they are ready for reading and writing. See the UNIX Programmer's
;;; Manual for more information.
(defun unix-select (nfds rdfds wrfds xpfds to-secs &optional (to-usecs 0))
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

;;; Retrieve information about the specified file returning them in
;;; the form of multiple values. See the UNIX Programmer's Manual for
;;; a description of the values returned. If the call fails, then NIL
;;; and an error number is returned instead.
(defun unix-stat (name)
  (declare (type unix-pathname name))
  (when (string= name "")
    (setf name "."))
  (with-alien ((buf (struct stat)))
    (syscall ("stat" c-string (* (struct stat)))
	     (extract-stat-results buf)
	     name (addr buf))))

;;; like UNIX-STAT except the file is specified by the file descriptor FD
(defun unix-fstat (fd)
  (declare (type unix-fd fd))
  (with-alien ((buf (struct stat)))
    (syscall ("fstat" int (* (struct stat)))
	     (extract-stat-results buf)
	     fd (addr buf))))

;;; like UNIX-STAT except the specified file must be a symbolic link
(defun unix-lstat (name)
  (declare (type unix-pathname name))
  (with-alien ((buf (struct stat)))
    (syscall ("lstat" c-string (* (struct stat)))
	     (extract-stat-results buf)
	     name (addr buf))))

;;; UNIX-MKDIR accepts a name and a mode and attempts to create the
;;; corresponding directory with mode mode.
(defun unix-mkdir (name mode)
  (declare (type unix-pathname name)
	   (type unix-file-mode mode))
  (void-syscall ("mkdir" c-string int) name mode))

;;;; time.h

;; the POSIX.4 structure for a time value. This is like a `struct
;; timeval' but has nanoseconds instead of microseconds.
(def-alien-type nil
    (struct timespec
	    (tv-sec long)   ;Seconds
	    (tv-nsec long))) ;Nanoseconds

;; used by other time functions
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

;;; If it works, UNIX-GETTIMEOFDAY returns 5 values: T, the seconds
;;; and microseconds of the current time of day, the timezone (in
;;; minutes west of Greenwich), and a daylight-savings flag. If it
;;; doesn't work, it returns NIL and the errno.
#!-sb-fluid (declaim (inline unix-gettimeofday))
(defun unix-gettimeofday ()
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

(defconstant ENOENT 2) ; Unix error code, "No such file or directory"
(defconstant EINTR 4) ; Unix error code, "Interrupted system call"
(defconstant EIO 5) ; Unix error code, "I/O error"
(defconstant EEXIST 17) ; Unix error code, "File exists"
(defconstant ESPIPE 29) ; Unix error code, "Illegal seek"
(defconstant EWOULDBLOCK 11) ; Unix error code, "Operation would block"
;;; FIXME: Many Unix error code definitions were deleted from the old
;;; CMU CL source code here, but not in the exports of SB-UNIX. I
;;; (WHN) hope that someday I'll figure out an automatic way to detect
;;; unused symbols in package exports, but if I don't, there are
;;; enough of them all in one place here that they should probably be
;;; removed by hand.

;;;; support routines for dealing with Unix pathnames

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

;;; Return the pathname with all symbolic links resolved.
;;;
;;; FIXME: Could we just use Unix readlink(2) instead?
(defun unix-resolve-links (pathname)
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
			   (error 'simple-file-error
				  :pathname pathname
				  :format-control
				  "~@<error reading link ~S: ~2I~_~A~:>"
				  :format-arguments (list (subseq
							   result 0 fill-ptr)
							  (strerror err))))
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
