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

(deftype unix-pathname () 'simple-base-string)
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

(define-alien-routine ("getenv" posix-getenv) c-string
  "Return the \"value\" part of the environment string \"name=value\" which
   corresponds to NAME, or NIL if there is none."
  (name c-string))

;;; from stdio.h

;;; Rename the file with string NAME1 to the string NAME2. NIL and an
;;; error code is returned if an error occurs.
(defun unix-rename (name1 name2)
  (declare (type unix-pathname name1 name2))
  (void-syscall ("rename" c-string c-string) name1 name2))

;;; from sys/types.h and gnu/types.h

(/show0 "unix.lisp 220")

;;; FIXME: We shouldn't hand-copy types from header files into Lisp
;;; like this unless we have extreme provocation. Reading directories
;;; is not extreme enough, since it doesn't need to be blindingly
;;; fast: we can just implement those functions in C as a wrapper
;;; layer.
(define-alien-type fd-mask unsigned-long)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant fd-setsize 1024))

(define-alien-type nil
  (struct fd-set
	  (fds-bits (array fd-mask #.(/ fd-setsize 32)))))

(/show0 "unix.lisp 304")


;;;; fcntl.h
;;;;
;;;; POSIX Standard: 6.5 File Control Operations	<fcntl.h>

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

;;;; timebits.h

;; A time value that is accurate to the nearest
;; microsecond but also has a range of years.
(define-alien-type nil
  (struct timeval
	  (tv-sec time-t)		; seconds
	  (tv-usec time-t)))		; and microseconds

;;;; resourcebits.h

(defconstant rusage_self 0) ; the calling process
(defconstant rusage_children -1) ; terminated child processes
(defconstant rusage_both -2)

(define-alien-type nil
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

;;; Is a stream interactive?
(defun unix-isatty (fd)
  (declare (type unix-fd fd))
  (int-syscall ("isatty" int) fd))

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
  #!-(or linux openbsd freebsd sunos osf1) (,stub,)
  #!+(or linux openbsd freebsd sunos osf1)
  (or (newcharstar-string (alien-funcall (extern-alien "getcwd"
						       (function (* char)
								 (* char)
								 size-t))
					 nil 
					 #!+(or linux openbsd freebsd) 0
					 #!+(or sunos osf1) 1025))
      (simple-perror "getcwd")))

;;; Return the Unix current directory as a SIMPLE-STRING terminated
;;; by a slash character.
(defun posix-getcwd/ ()
  (concatenate 'string (posix-getcwd) "/"))

;;; Convert at the UNIX level from a possibly relative filename to
;;; an absolute filename.
;;;
;;; FIXME: Do we still need this even as we switch to
;;; *DEFAULT-PATHNAME-DEFAULTS*? I think maybe we do, since it seems
;;; to be valid for the user to set *DEFAULT-PATHNAME-DEFAULTS* to
;;; have a NIL directory component, and then this'd be the only way to
;;; interpret a relative directory specification. But I don't find the
;;; ANSI pathname documentation to be a model of clarity. Maybe
;;; someone who understands it better can take a look at this.. -- WHN
(defun unix-maybe-prepend-current-directory (name)
  (declare (simple-string name))
  (if (and (> (length name) 0) (char= (schar name 0) #\/))
      name
      (concatenate 'simple-string (posix-getcwd/) name)))

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
(define-alien-routine ("getpid" unix-getpid) int)

;;; Return the real user id associated with the current process.
(define-alien-routine ("getuid" unix-getuid) int)

;;; Translate a user id into a login name.
(defun uid-username (uid)
  (or (newcharstar-string (alien-funcall (extern-alien "uid_username"
						       (function (* char) int))
					 uid))
      (error "found no match for Unix uid=~S" uid)))

;;; Return the namestring of the home directory, being careful to
;;; include a trailing #\/
(defun uid-homedir (uid)
  (or (newcharstar-string (alien-funcall (extern-alien "uid_homedir"
						       (function (* char) int))
					 uid))
      (error "failed to resolve home directory for Unix uid=~S" uid)))

;;; Invoke readlink(2) on the file name specified by PATH. Return
;;; (VALUES LINKSTRING NIL) on success, or (VALUES NIL ERRNO) on
;;; failure.
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

;;; UNIX-UNLINK accepts a name and deletes the directory entry for that
;;; name and the file if this is the last link. 
(defun unix-unlink (name)
  (declare (type unix-pathname name))
  (void-syscall ("unlink" c-string) name))

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


(defun unix-setsid ()
  (int-syscall ("setsid")))

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
;;; This is like getrusage(2), except it returns only the system and
;;; user time, and returns the seconds and microseconds as separate
;;; values.
#!-sb-fluid (declaim (inline unix-fast-getrusage))
(defun unix-fast-getrusage (who)
  (declare (values (member t)
		   (unsigned-byte 31) (integer 0 1000000)
		   (unsigned-byte 31) (integer 0 1000000)))
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

;;;; FIXME: Why have both UNIX-SELECT and UNIX-FAST-SELECT?

;;; Perform the UNIX select(2) system call.
(declaim (inline unix-fast-select)) ; (used to be a macro in CMU CL)
(defun unix-fast-select (num-descriptors
			 read-fds write-fds exception-fds
			 timeout-secs &optional (timeout-usecs 0))
  (declare (type (integer 0 #.fd-setsize) num-descriptors)
	   (type (or (alien (* (struct fd-set))) null)
		 read-fds write-fds exception-fds)
	   (type (or null (unsigned-byte 31)) timeout-secs)
	   (type (unsigned-byte 31) timeout-usecs))
  ;; FIXME: CMU CL had
  ;;   (declare (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  ;; here. Is that important for SBCL? If so, why? Profiling might tell us..
  (with-alien ((tv (struct timeval)))
    (when timeout-secs
      (setf (slot tv 'tv-sec) timeout-secs)
      (setf (slot tv 'tv-usec) timeout-usecs))
    (int-syscall ("select" int (* (struct fd-set)) (* (struct fd-set))
		  (* (struct fd-set)) (* (struct timeval)))
		 num-descriptors read-fds write-fds exception-fds
		 (if timeout-secs (alien-sap (addr tv)) (int-sap 0)))))

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

;;; This is a structure defined in src/runtime/wrap.c, to look
;;; basically like "struct stat" according to stat(2). It may not
;;; actually correspond to the real in-memory stat structure that the
;;; syscall uses, and that's OK. Linux in particular is packed full of
;;; stat macros, and trying to keep Lisp code in correspondence with
;;; it is more pain than it's worth, so we just let our C runtime
;;; synthesize a nice consistent structure for us.
;;;
;;; Note that st-dev is a long, not a dev-t. This is because dev-t on
;;; linux 32 bit archs is a 64 bit quantity, but alien doesn's support
;;; those. We don't actually access that field anywhere, though, so
;;; until we can get 64 bit alien support it'll do. Also note that
;;; st_size is a long, not an off-t, because off-t is a 64-bit
;;; quantity on Alpha. And FIXME: "No one would want a file length
;;; longer than 32 bits anyway, right?":-|
(define-alien-type nil
  (struct wrapped_stat
    (st-dev unsigned-long)              ; would be dev-t in a real stat
    (st-ino ino-t)
    (st-mode mode-t)
    (st-nlink  nlink-t)
    (st-uid  uid-t)
    (st-gid  gid-t)
    (st-rdev unsigned-long)             ; would be dev-t in a real stat
    (st-size unsigned-long)		; would be off-t in a real stat
    (st-blksize unsigned-long)
    (st-blocks unsigned-long)
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
  (with-alien ((buf (struct wrapped_stat)))
    (syscall ("fstat_wrapper" int (* (struct wrapped_stat)))
	     (%extract-stat-results (addr buf))
	     fd (addr buf))))

;;;; time.h

;; the POSIX.4 structure for a time value. This is like a "struct
;; timeval" but has nanoseconds instead of microseconds.
(define-alien-type nil
    (struct timespec
	    (tv-sec long)   ; seconds
	    (tv-nsec long))) ; nanoseconds

;; used by other time functions
(define-alien-type nil
    (struct tm
	    (tm-sec int)   ; Seconds.	[0-60] (1 leap second)
	    (tm-min int)   ; Minutes.	[0-59]
	    (tm-hour int)  ; Hours.	[0-23]
	    (tm-mday int)  ; Day.       [1-31]
	    (tm-mon int)   ; Month.	[0-11]
	    (tm-year int)  ; Year - 1900.
	    (tm-wday int)  ; Day of week. [0-6]
	    (tm-yday int)  ; Days in year. [0-365]
	    (tm-isdst int) ; DST.       [-1/0/1]
	    (tm-gmtoff long) ;  Seconds east of UTC.
	    (tm-zone c-string))) ; Timezone abbreviation.

(define-alien-routine get-timezone sb!alien:void
  (when sb!alien:long :in)
  (minutes-west sb!alien:int :out)
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
(define-alien-type nil
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


;; Type of the second argument to `getitimer' and
;; the second and third arguments `setitimer'. 
(define-alien-type nil
  (struct itimerval
    (it-interval (struct timeval))	; timer interval
    (it-value (struct timeval))))	; current value

(defconstant ITIMER-REAL 0)
(defconstant ITIMER-VIRTUAL 1)
(defconstant ITIMER-PROF 2)

(defun unix-getitimer(which)
  "Unix-getitimer returns the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). On success,
   unix-getitimer returns 5 values,
   T, it-interval-secs, it-interval-usec, it-value-secs, it-value-usec."
  (declare (type (member :real :virtual :profile) which)
	   (values t
		   (unsigned-byte 29) (mod 1000000)
		   (unsigned-byte 29) (mod 1000000)))
  (let ((which (ecase which
		 (:real ITIMER-REAL)
		 (:virtual ITIMER-VIRTUAL)
		 (:profile ITIMER-PROF))))
    (with-alien ((itv (struct itimerval)))
      (syscall* ("getitimer" int (* (struct itimerval)))
		(values T
			(slot (slot itv 'it-interval) 'tv-sec)
			(slot (slot itv 'it-interval) 'tv-usec)
			(slot (slot itv 'it-value) 'tv-sec)
			(slot (slot itv 'it-value) 'tv-usec))
		which (alien-sap (addr itv))))))

(defun unix-setitimer (which int-secs int-usec val-secs val-usec)
  " Unix-setitimer sets the INTERVAL and VALUE slots of one of
   three system timers (:real :virtual or :profile). A SIGALRM signal
   will be delivered VALUE <seconds+microseconds> from now. INTERVAL,
   when non-zero, is <seconds+microseconds> to be loaded each time
   the timer expires. Setting INTERVAL and VALUE to zero disables
   the timer. See the Unix man page for more details. On success,
   unix-setitimer returns the old contents of the INTERVAL and VALUE
   slots as in unix-getitimer."
  (declare (type (member :real :virtual :profile) which)
	   (type (unsigned-byte 29) int-secs val-secs)
	   (type (integer 0 (1000000)) int-usec val-usec)
	   (values t
		   (unsigned-byte 29) (mod 1000000)
		   (unsigned-byte 29) (mod 1000000)))
  (let ((which (ecase which
		 (:real ITIMER-REAL)
		 (:virtual ITIMER-VIRTUAL)
		 (:profile ITIMER-PROF))))
    (with-alien ((itvn (struct itimerval))
		 (itvo (struct itimerval)))
      (setf (slot (slot itvn 'it-interval) 'tv-sec ) int-secs
	    (slot (slot itvn 'it-interval) 'tv-usec) int-usec
	    (slot (slot itvn 'it-value   ) 'tv-sec ) val-secs
	    (slot (slot itvn 'it-value   ) 'tv-usec) val-usec)
      (syscall* ("setitimer" int (* (struct timeval))(* (struct timeval)))
		(values T
			(slot (slot itvo 'it-interval) 'tv-sec)
			(slot (slot itvo 'it-interval) 'tv-usec)
			(slot (slot itvo 'it-value) 'tv-sec)
			(slot (slot itvo 'it-value) 'tv-usec))
		which (alien-sap (addr itvn))(alien-sap (addr itvo))))))

(defmacro with-timeout (expires &body body)
  "Execute the body, interrupting it with a SIGALRM after at least
EXPIRES seconds have passed.  Uses Unix setitimer(), restoring any
previous timer after the body has finished executing"
  (let ((saved-seconds (gensym "SAVED-SECONDS"))
	(saved-useconds (gensym "SAVED-USECONDS"))
	(s (gensym "S")) (u (gensym "U")))
    `(let (- ,saved-seconds ,saved-useconds)
      (multiple-value-setq (- - - ,saved-seconds ,saved-useconds)
	(unix-getitimer :real))
      (multiple-value-bind (,s ,u) (floor ,expires)
	(setf ,u (floor (* ,u 1000000)))
	(if (and (> ,expires 0)
		 (or (and (zerop ,saved-seconds) (zerop ,saved-useconds))
		     (> ,saved-seconds ,s)
		     (and (= ,saved-seconds ,s)
			  (> ,saved-useconds ,u))))
	    (unwind-protect
		 (progn
		   (unix-setitimer :real 0 0 ,s ,u)
		   ,@body)
	      (unix-setitimer :real 0 0 ,saved-seconds ,saved-useconds))
	    ,@body)))))



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
  (declare (simple-base-string name))
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

;;; Is the Unix pathname PATHNAME relative, instead of absolute? (E.g.
;;; "passwd" or "etc/passwd" instead of "/etc/passwd"?)
(defun relative-unix-pathname? (pathname)
  (declare (type simple-string pathname))
  (or (zerop (length pathname))
      (char/= (schar pathname 0) #\/)))

;;; Return PATHNAME with all symbolic links resolved. PATHNAME should
;;; already be a complete absolute Unix pathname, since at least in
;;; sbcl-0.6.12.36 we're called only from TRUENAME, and only after
;;; paths have been converted to absolute paths, so we don't need to
;;; try to handle any more generality than that.
(defun unix-resolve-links (pathname)
  (declare (type simple-string pathname))
  (aver (not (relative-unix-pathname? pathname)))
  (/noshow "entering UNIX-RESOLVE-LINKS")
  (loop with previous-pathnames = nil do
	(/noshow pathname previous-pathnames)
	(let ((link (unix-readlink pathname)))
	  (/noshow link)
	  ;; Unlike the old CMU CL code, we handle a broken symlink by
	  ;; returning the link itself. That way, CL:TRUENAME on a
	  ;; broken link returns the link itself, so that CL:DIRECTORY
	  ;; can return broken links, so that even without
	  ;; Unix-specific extensions to do interesting things with
	  ;; them, at least Lisp programs can see them and, if
	  ;; necessary, delete them. (This is handy e.g. when your
	  ;; managed-by-Lisp directories are visited by Emacs, which
	  ;; creates broken links as notes to itself.)
	  (if (null link)
	      (return pathname)
	      (let ((new-pathname 
		     (unix-simplify-pathname
		      (if (relative-unix-pathname? link)
			  (let* ((dir-len (1+ (position #\/
							pathname
							:from-end t)))
				 (dir (subseq pathname 0 dir-len)))
			    (/noshow dir)
			    (concatenate 'string dir link))
			  link))))
		(if (unix-file-kind new-pathname)
		    (setf pathname new-pathname)
		    (return pathname)))))
	;; To generalize the principle that even if portable Lisp code
	;; can't do anything interesting with a broken symlink, at
	;; least it should be able to see and delete it, when we
	;; detect a cyclic link, we return the link itself. (So even
	;; though portable Lisp code can't do anything interesting
	;; with a cyclic link, at least it can see it and delete it.)
	(if (member pathname previous-pathnames :test #'string=)
	    (return pathname)
	    (push pathname previous-pathnames))))

(defun unix-simplify-pathname (src)
  (declare (type simple-string src))
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
		    ;; either ``/...' or ``...//...'
		    (unless last-slash
		      (setf last-slash dst-len)
		      (deposit char)))
		   (1
		    ;; either ``./...'' or ``..././...''
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
		    ;; something other than a dot between slashes
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

;;;; A magic constant for wait3().
;;;;
;;;; FIXME: This used to be defined in run-program.lisp as
;;;; (defconstant wait-wstopped #-svr4 #o177 #+svr4 wait-wuntraced)
;;;; According to some of the man pages, the #o177 is part of the API
;;;; for wait3(); that said, under SunOS there is a WSTOPPED thing in
;;;; the headers that may or may not be the same thing. To be
;;;; investigated. -- CSR, 2002-03-25
(defconstant wstopped #o177)


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


