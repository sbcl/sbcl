;;;; RUN-PROGRAM and friends, a facility for running Unix programs
;;;; from inside SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL") ;(SB-IMPL, not SB!IMPL, since we're built in warm load.)

;;;; hacking the Unix environment
;;;;
;;;; In the original CMU CL code that LOAD-FOREIGN is derived from, the
;;;; Unix environment (as in "man environ") was represented as an
;;;; alist from keywords to strings, so that e.g. the Unix environment
;;;;   "SHELL=/bin/bash" "HOME=/root" "PAGER=less"
;;;; was represented as
;;;;   ((:SHELL . "/bin/bash") (:HOME . "/root") (:PAGER "less"))
;;;; This had a few problems in principle: the mapping into
;;;; keyword symbols smashed the case of environment
;;;; variables, and the whole mapping depended on the presence of
;;;; #\= characters in the environment strings. In practice these
;;;; problems weren't hugely important, since conventionally environment
;;;; variables are uppercase strings followed by #\= followed by
;;;; arbitrary data. However, since it's so manifestly not The Right
;;;; Thing to make code which breaks unnecessarily on input which
;;;; doesn't follow what is, after all, only a tradition, we've switched
;;;; formats in SBCL, so that the fundamental environment list
;;;; is just a list of strings, with a one-to-one-correspondence
;;;; to the C-level representation. I.e., in the example above,
;;;; the SBCL representation is
;;;;   '("SHELL=/bin/bash" "HOME=/root" "PAGER=less")
;;;; CMU CL's implementation is currently supported to help with porting.
;;;;
;;;; It's not obvious that this code belongs here (instead of e.g. in
;;;; unix.lisp), since it has only a weak logical connection with
;;;; RUN-PROGRAM. However, physically it's convenient to put it here.
;;;; It's not needed at cold init, so we *can* put it in this
;;;; warm-loaded file. And by putting it in this warm-loaded file, we
;;;; make it easy for it to get to the C-level 'environ' variable.
;;;; which (at least in sbcl-0.6.10 on Red Hat Linux 6.2) is not
;;;; visible at GENESIS time.

(def-alien-variable "environ" (* c-string))
(push (lambda ()
	;; We redo this here to protect ourselves from this scenario:
	;;   * Build under one version of shared lib, save a core.
	;;   * Load core under another version of shared lib. ("Now
	;;     where was environ again?" SIGSEGV, etc.)
	;; Obviously it's a KLUDGE to do this hack for every alien
	;; variable, but as it happens, as of sbcl-0.7.0 this is the
	;; only alien variable used to implement SBCL, so it's not
	;; worth coming up with a general solution. (A general
	;; solution would be nice for users who want to have their
	;; alien code be preserved across a save/load cycle, but this
	;; problem with alien variables is only one of several
	;; problems which'd need to be solved before that can happen.)
        (def-alien-variable "environ" (* c-string)))
      *after-save-initializations*)

(defun posix-environ ()
  "Return the Unix environment (\"man environ\") as a list of SIMPLE-STRINGs."
  (c-strings->string-list environ))

;;; Convert as best we can from a SBCL representation of a Unix
;;; environment to a CMU CL representation.
;;;
;;; * (UNIX-ENVIRONMENT-CMUCL-FROM-SBCL '("Bletch=fub" "Noggin" "YES=No!"))
;;; WARNING:
;;;   smashing case of "Bletch=fub" in conversion to CMU-CL-style
;;;     environment alist
;;; WARNING:
;;;   no #\= in "Noggin", eliding it in CMU-CL-style environment alist
;;; ((:BLETCH . "fub") (:YES . "No!"))
(defun unix-environment-cmucl-from-sbcl (sbcl)
  (mapcan
   (lambda (string)
     (declare (type simple-string string))
     (let ((=-pos (position #\= string :test #'equal)))
       (if =-pos
	   (list
	    (let* ((key-as-string (subseq string 0 =-pos))
		   (key-as-upcase-string (string-upcase key-as-string))
		   (key (keywordicate key-as-upcase-string))
		   (val (subseq string (1+ =-pos))))
	      (unless (string= key-as-string key-as-upcase-string)
		(warn "smashing case of ~S in conversion to CMU-CL-style ~
                      environment alist"
		      string))
	      (cons key val)))
	   (warn "no #\\= in ~S, eliding it in CMU-CL-style environment alist"
		 string))))
   sbcl))

;;; Convert from a CMU CL representation of a Unix environment to a
;;; SBCL representation.
(defun unix-environment-sbcl-from-cmucl (cmucl)
  (mapcar
   (lambda (cons)
     (destructuring-bind (key . val) cons
       (declare (type keyword key) (type simple-string val))
       (concatenate 'simple-string (symbol-name key) "=" val)))
   cmucl))

;;;; Import wait3(2) from Unix.

(sb-alien:def-alien-routine ("wait3" c-wait3) sb-c-call:int
  (status sb-c-call:int :out)
  (options sb-c-call:int)
  (rusage sb-c-call:int))

(defconstant wait-wnohang #-svr4 1 #+svr4 #o100)
(defconstant wait-wuntraced #-svr4 2 #+svr4 4)
(defconstant wait-wstopped #-svr4 #o177 #+svr4 wait-wuntraced)

(defun wait3 (&optional do-not-hang check-for-stopped)
  "Return any available status information on child process. "
  (multiple-value-bind (pid status)
      (c-wait3 (logior (if do-not-hang
			   wait-wnohang
			   0)
		       (if check-for-stopped
			   wait-wuntraced
			   0))
	       0)
    (cond ((or (minusp pid)
	       (zerop pid))
	   nil)
	  ((eql (ldb (byte 8 0) status)
		wait-wstopped)
	   (values pid
		   :stopped
		   (ldb (byte 8 8) status)))
	  ((zerop (ldb (byte 7 0) status))
	   (values pid
		   :exited
		   (ldb (byte 8 8) status)))
	  (t
	   (let ((signal (ldb (byte 7 0) status)))
	     (values pid
		     (if (position signal
				   #.(vector
				      (sb-unix:unix-signal-number :sigstop)
				      (sb-unix:unix-signal-number :sigtstp)
				      (sb-unix:unix-signal-number :sigttin)
				      (sb-unix:unix-signal-number :sigttou)))
			 :stopped
			 :signaled)
		     signal
		     (not (zerop (ldb (byte 1 7) status)))))))))

;;;; process control stuff

(defvar *active-processes* nil
  "List of process structures for all active processes.")

(defstruct (process (:copier nil))
  pid		      ; PID of child process
  %status             ; either :RUNNING, :STOPPED, :EXITED, or :SIGNALED
  exit-code	      ; either exit code or signal
  core-dumped	      ; T if a core image was dumped
  pty		      ; stream to child's pty, or NIL
  input		      ; stream to child's input, or NIL
  output	      ; stream from child's output, or NIL
  error		      ; stream from child's error output, or NIL
  status-hook	      ; closure to call when PROC changes status
  plist		      ; a place for clients to stash things
  cookie)             ; list of the number of pipes from the subproc

(defmethod print-object ((process process) stream)
  (print-unreadable-object (process stream :type t)
    (format stream
	    "~D ~S"
	    (process-pid process)
	    (process-status process)))
  process)

(defun process-status (proc)
  "Return the current status of process.  The result is one of :RUNNING,
   :STOPPED, :EXITED, or :SIGNALED."
  (get-processes-status-changes)
  (process-%status proc))

(defun process-wait (proc &optional check-for-stopped)
  "Wait for PROC to quit running for some reason.  Returns PROC."
  (loop
      (case (process-status proc)
	(:running)
	(:stopped
	 (when check-for-stopped
	   (return)))
	(t
	 (when (zerop (car (process-cookie proc)))
	   (return))))
      (sb-sys:serve-all-events 1))
  proc)

#-hpux
;;; Find the current foreground process group id.
(defun find-current-foreground-process (proc)
  (sb-alien:with-alien ((result sb-c-call:int))
    (multiple-value-bind
	  (wonp error)
	(sb-unix:unix-ioctl (sb-sys:fd-stream-fd (process-pty proc))
			    sb-unix:TIOCGPGRP
			    (sb-alien:alien-sap (sb-alien:addr result)))
      (unless wonp
	(error "TIOCPGRP ioctl failed: ~S" (strerror error)))
      result))
  (process-pid proc))

(defun process-kill (proc signal &optional (whom :pid))
  "Hand SIGNAL to PROC. If WHOM is :PID, use the kill Unix system call. If
   WHOM is :PROCESS-GROUP, use the killpg Unix system call. If WHOM is
   :PTY-PROCESS-GROUP deliver the signal to whichever process group is
   currently in the foreground."
  (let ((pid (ecase whom
	       ((:pid :process-group)
		(process-pid proc))
	       (:pty-process-group
		#-hpux
		(find-current-foreground-process proc)))))
    (multiple-value-bind
	  (okay errno)
	(case whom
	  #+hpux
	  (:pty-process-group
	   (sb-unix:unix-ioctl (sb-sys:fd-stream-fd (process-pty proc))
			       sb-unix:TIOCSIGSEND
			       (sb-sys:int-sap
				(sb-unix:unix-signal-number signal))))
	  ((:process-group #-hpux :pty-process-group)
	   (sb-unix:unix-killpg pid signal))
	  (t
	   (sb-unix:unix-kill pid signal)))
      (cond ((not okay)
	     (values nil errno))
	    ((and (eql pid (process-pid proc))
		  (= (sb-unix:unix-signal-number signal)
		     (sb-unix:unix-signal-number :sigcont)))
	     (setf (process-%status proc) :running)
	     (setf (process-exit-code proc) nil)
	     (when (process-status-hook proc)
	       (funcall (process-status-hook proc) proc))
	     t)
	    (t
	     t)))))

(defun process-alive-p (proc)
  "Return T if the process is still alive, NIL otherwise."
  (let ((status (process-status proc)))
    (if (or (eq status :running)
	    (eq status :stopped))
	t
	nil)))

(defun process-close (proc)
  "Close all streams connected to PROC and stop maintaining the status slot."
  (macrolet ((frob (stream abort)
	       `(when ,stream (close ,stream :abort ,abort))))
    (frob (process-pty    proc)   t) ; Don't FLUSH-OUTPUT to dead process, ..
    (frob (process-input  proc)   t) ; .. 'cause it will generate SIGPIPE.
    (frob (process-output proc) nil)
    (frob (process-error  proc) nil))
  (sb-sys:without-interrupts
   (setf *active-processes* (delete proc *active-processes*)))
  proc)

;;; the handler for SIGCHLD signals that RUN-PROGRAM establishes
(defun sigchld-handler (ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  (get-processes-status-changes))

(defun get-processes-status-changes ()
  (loop
      (multiple-value-bind (pid what code core)
	  (wait3 t t)
	(unless pid
	  (return))
	(let ((proc (find pid *active-processes* :key #'process-pid)))
	  (when proc
	    (setf (process-%status proc) what)
	    (setf (process-exit-code proc) code)
	    (setf (process-core-dumped proc) core)
	    (when (process-status-hook proc)
	      (funcall (process-status-hook proc) proc))
	    (when (position what #(:exited :signaled))
	      (sb-sys:without-interrupts
	       (setf *active-processes*
		     (delete proc *active-processes*)))))))))

;;;; RUN-PROGRAM and close friends

;;; list of file descriptors to close when RUN-PROGRAM exits due to an error
(defvar *close-on-error* nil)

;;; list of file descriptors to close when RUN-PROGRAM returns in the parent
(defvar *close-in-parent* nil)

;;; list of handlers installed by RUN-PROGRAM
(defvar *handlers-installed* nil)

#+FreeBSD
(def-alien-type nil
  (struct sgttyb
	  (sg-ispeed sb-c-call:char)	; input speed
	  (sg-ospeed sb-c-call:char)	; output speed
	  (sg-erase sb-c-call:char)	; erase character
	  (sg-kill sb-c-call:char)	; kill character
	  (sg-flags sb-c-call:short)))	; mode flags
#+OpenBSD
(def-alien-type nil
  (struct sgttyb
	  (sg-four sb-c-call:int)
	  (sg-chars (array sb-c-call:char 4))
	  (sg-flags sb-c-call:int)))

;;; Find an unused pty. Return three values: the file descriptor for
;;; the master side of the pty, the file descriptor for the slave side
;;; of the pty, and the name of the tty device for the slave side.
(defun find-a-pty ()
  (dolist (char '(#\p #\q))
    (dotimes (digit 16)
      (let* ((master-name (format nil "/dev/pty~C~X" char digit))
	     (master-fd (sb-unix:unix-open master-name
					   sb-unix:o_rdwr
					   #o666)))
	(when master-fd
	  (let* ((slave-name (format nil "/dev/tty~C~X" char digit))
		 (slave-fd (sb-unix:unix-open slave-name
					      sb-unix:o_rdwr
					      #o666)))
	    (when slave-fd
	      ;; comment from classic CMU CL:
	      ;;   Maybe put a vhangup here?
	      ;;
	      ;; FIXME: It seems as though this logic should be in
	      ;; OPEN-PTY, not FIND-A-PTY (both from the comments
	      ;; documenting DEFUN FIND-A-PTY, and from the
	      ;; connotations of the function names).
	      ;;
	      ;; FIXME: It would be nice to have a note, and/or a pointer
	      ;; to some reference material somewhere, explaining
	      ;; why we need this on *BSD and not on Linux.
              #+bsd
	      (sb-alien:with-alien ((stuff (sb-alien:struct sgttyb)))
		(let ((sap (sb-alien:alien-sap stuff)))
		  (sb-unix:unix-ioctl slave-fd sb-unix:TIOCGETP sap)
		  (setf (sb-alien:slot stuff 'sg-flags)
			;; This is EVENP|ODDP, the same numeric code
			;; both on FreeBSD and on OpenBSD. -- WHN 20000929
			#o300) ; EVENP|ODDP
		  (sb-unix:unix-ioctl slave-fd sb-unix:TIOCSETP sap)
		  (sb-unix:unix-ioctl master-fd sb-unix:TIOCGETP sap)
		  (setf (sb-alien:slot stuff 'sg-flags)
			(logand (sb-alien:slot stuff 'sg-flags)
				;; This is ~ECHO, the same numeric
				;; code both on FreeBSD and on OpenBSD.
				;; -- WHN 20000929
				(lognot 8))) ; ~ECHO
		  (sb-unix:unix-ioctl master-fd sb-unix:TIOCSETP sap)))
	      (return-from find-a-pty
		(values master-fd
			slave-fd
			slave-name)))
	    (sb-unix:unix-close master-fd))))))
  (error "could not find a pty"))

(defun open-pty (pty cookie)
  (when pty
    (multiple-value-bind
	  (master slave name)
	(find-a-pty)
      (push master *close-on-error*)
      (push slave *close-in-parent*)
      (when (streamp pty)
	(multiple-value-bind (new-fd errno) (sb-unix:unix-dup master)
	  (unless new-fd
	    (error "couldn't SB-UNIX:UNIX-DUP ~D: ~A" master (strerror errno)))
	  (push new-fd *close-on-error*)
	  (copy-descriptor-to-stream new-fd pty cookie)))
      (values name
	      (sb-sys:make-fd-stream master :input t :output t)))))

(defmacro round-bytes-to-words (n)
  `(logand (the fixnum (+ (the fixnum ,n) 3)) (lognot 3)))

(defun string-list-to-c-strvec (string-list)
  ;; Make a pass over STRING-LIST to calculate the amount of memory
  ;; needed to hold the strvec.
  (let ((string-bytes 0)
	;; We need an extra for the null, and an extra 'cause exect
	;; clobbers argv[-1].
	(vec-bytes (* #-alpha 4 #+alpha 8 (+ (length string-list) 2))))
    (declare (fixnum string-bytes vec-bytes))
    (dolist (s string-list)
      (enforce-type s simple-string)
      (incf string-bytes (round-bytes-to-words (1+ (length s)))))
    ;; Now allocate the memory and fill it in.
    (let* ((total-bytes (+ string-bytes vec-bytes))
	   (vec-sap (sb-sys:allocate-system-memory total-bytes))
	   (string-sap (sap+ vec-sap vec-bytes))
	   (i #-alpha 4 #+alpha 8))
      (declare (type (and unsigned-byte fixnum) total-bytes i)
	       (type sb-sys:system-area-pointer vec-sap string-sap))
      (dolist (s string-list)
	(declare (simple-string s))
	(let ((n (length s)))
	  ;; Blast the string into place.
	  (sb-kernel:copy-to-system-area (the simple-string s)
					 (* sb-vm:vector-data-offset
					    sb-vm:word-bits)
					 string-sap 0
					 (* (1+ n) sb-vm:byte-bits))
	  ;; Blast the pointer to the string into place.
	  (setf (sap-ref-sap vec-sap i) string-sap)
	  (setf string-sap (sap+ string-sap (round-bytes-to-words (1+ n))))
	  (incf i #-alpha 4 #+alpha 8)))
      ;; Blast in the last null pointer.
      (setf (sap-ref-sap vec-sap i) (int-sap 0))
      (values vec-sap (sap+ vec-sap #-alpha 4 #+alpha 8) total-bytes))))

(defmacro with-c-strvec ((var str-list) &body body)
  (let ((sap (gensym "SAP-"))
	(size (gensym "SIZE-")))
    `(multiple-value-bind
      (,sap ,var ,size)
      (string-list-to-c-strvec ,str-list)
      (unwind-protect
	   (progn
	     ,@body)
	(sb-sys:deallocate-system-memory ,sap ,size)))))

(sb-alien:def-alien-routine spawn sb-c-call:int
  (program sb-c-call:c-string)
  (argv (* sb-c-call:c-string))
  (envp (* sb-c-call:c-string))
  (pty-name sb-c-call:c-string)
  (stdin sb-c-call:int)
  (stdout sb-c-call:int)
  (stderr sb-c-call:int))

;;; Is UNIX-FILENAME the name of a file that we can execute?
(defun unix-filename-is-executable-p (unix-filename)
  (declare (type simple-string unix-filename))
  (values (and (eq (sb-unix:unix-file-kind unix-filename) :file)
	       (sb-unix:unix-access unix-filename sb-unix:x_ok))))

;;; FIXME: There shouldn't be two semiredundant versions of the
;;; documentation. Since this is a public extension function, the
;;; documentation should be in the doc string. So all information from
;;; this comment should be merged into the doc string, and then this
;;; comment can go away.
;;;
;;; RUN-PROGRAM uses fork() and execve() to run a different program.
;;; Strange stuff happens to keep the Unix state of the world
;;; coherent.
;;;
;;; The child process needs to get its input from somewhere, and send
;;; its output (both standard and error) to somewhere. We have to do
;;; different things depending on where these somewheres really are.
;;;
;;; For input, there are five options:
;;;  -- T: Just leave fd 0 alone. Pretty simple.
;;;  -- "file": Read from the file. We need to open the file and
;;;     pull the descriptor out of the stream. The parent should close
;;;     this stream after the child is up and running to free any 
;;;     storage used in the parent.
;;;  -- NIL: Same as "file", but use "/dev/null" as the file.
;;;  -- :STREAM: Use Unix pipe() to create two descriptors. Use
;;;     SB-SYS:MAKE-FD-STREAM to create the output stream on the
;;;     writeable descriptor, and pass the readable descriptor to
;;;     the child. The parent must close the readable descriptor for
;;;     EOF to be passed up correctly.
;;;  -- a stream: If it's a fd-stream, just pull the descriptor out
;;;     of it. Otherwise make a pipe as in :STREAM, and copy 
;;;     everything across.
;;;
;;; For output, there are five options:
;;;  -- T: Leave descriptor 1 alone.
;;;  -- "file": dump output to the file.
;;;  -- NIL: dump output to /dev/null.
;;;  -- :STREAM: return a stream that can be read from.
;;;  -- a stream: if it's a fd-stream, use the descriptor in it.
;;;     Otherwise, copy stuff from output to stream.
;;;
;;; For error, there are all the same options as output plus:
;;;  -- :OUTPUT: redirect to the same place as output.
;;;
;;; RUN-PROGRAM returns a PROCESS structure for the process if
;;; the fork worked, and NIL if it did not.
(defun run-program (program args
		    &key
		    (env nil env-p)
		    (environment (if env-p
				     (unix-environment-sbcl-from-cmucl env)
				     (posix-environ))
				 environment-p)
		    (wait t)
		    pty
		    input
		    if-input-does-not-exist
		    output
		    (if-output-exists :error)
		    (error :output)
		    (if-error-exists :error)
		    status-hook)
  "RUN-PROGRAM creates a new Unix process running the Unix program found in
   the file specified by the PROGRAM argument.  ARGS are the standard
   arguments that can be passed to a Unix program. For no arguments, use NIL
   (which means that just the name of the program is passed as arg 0).

   RUN-PROGRAM will either return NIL or a PROCESS structure.  See the CMU
   Common Lisp Users Manual for details about the PROCESS structure.

   notes about Unix environments (as in the :ENVIRONMENT and :ENV args):
     1. The SBCL implementation of RUN-PROGRAM, like Perl and many other
        programs, but unlike the original CMU CL implementation, copies
        the Unix environment by default.
     2. Running Unix programs from a setuid process, or in any other
        situation where the Unix environment is under the control of someone
        else, is a mother lode of security problems. If you are contemplating
        doing this, read about it first. (The Perl community has a lot of good
        documentation about this and other security issues in script-like
        programs.)

   The &KEY arguments have the following meanings:
     :ENVIRONMENT
        a list of SIMPLE-STRINGs describing the new Unix environment (as
        in \"man environ\"). The default is to copy the environment of
        the current process.
     :ENV
        an alternative lossy representation of the new Unix environment,
        for compatibility with CMU CL
     :WAIT
        If non-NIL (default), wait until the created process finishes.  If
        NIL, continue running Lisp until the program finishes.
     :PTY
        Either T, NIL, or a stream.  Unless NIL, the subprocess is established
	under a PTY.  If :pty is a stream, all output to this pty is sent to
	this stream, otherwise the PROCESS-PTY slot is filled in with a stream
	connected to pty that can read output and write input.
     :INPUT
        Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
	input for the current process is inherited.  If NIL, /dev/null
	is used.  If a pathname, the file so specified is used.  If a stream,
	all the input is read from that stream and send to the subprocess.  If
	:STREAM, the PROCESS-INPUT slot is filled in with a stream that sends 
	its output to the process. Defaults to NIL.
     :IF-INPUT-DOES-NOT-EXIST (when :INPUT is the name of a file)
        can be one of:
           :ERROR to generate an error
           :CREATE to create an empty file
           NIL (the default) to return NIL from RUN-PROGRAM
     :OUTPUT 
        Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
	output for the current process is inherited.  If NIL, /dev/null
	is used.  If a pathname, the file so specified is used.  If a stream,
	all the output from the process is written to this stream. If
	:STREAM, the PROCESS-OUTPUT slot is filled in with a stream that can
	be read to get the output. Defaults to NIL.
     :IF-OUTPUT-EXISTS (when :OUTPUT is the name of a file)
        can be one of:
           :ERROR (the default) to generate an error
           :SUPERSEDE to supersede the file with output from the program
           :APPEND to append output from the program to the file 
           NIL to return NIL from RUN-PROGRAM, without doing anything
     :ERROR and :IF-ERROR-EXISTS
        Same as :OUTPUT and :IF-OUTPUT-EXISTS, except that :ERROR can also be
	specified as :OUTPUT in which case all error output is routed to the
	same place as normal output.
     :STATUS-HOOK
        This is a function the system calls whenever the status of the
        process changes.  The function takes the process as an argument."

  (when (and env-p environment-p)
    (error "can't specify :ENV and :ENVIRONMENT simultaneously"))
  ;; Make sure that the interrupt handler is installed.
  (sb-sys:enable-interrupt :sigchld #'sigchld-handler)
  ;; Prepend the program to the argument list.
  (push (namestring program) args)
  (let (;; Clear various specials used by GET-DESCRIPTOR-FOR to
	;; communicate cleanup info.
	*close-on-error*
	*close-in-parent*
	*handlers-installed*
	;; Establish PROC at this level so that we can return it.
	proc
	;; It's friendly to allow the caller to pass any string
	;; designator, but internally we'd like SIMPLE-STRINGs.
	(simple-args (mapcar (lambda (x) (coerce x 'simple-string)) args)))
    (unwind-protect
	 (let (;; FIXME: The old code here used to do
	       ;;   (MERGE-PATHNAMES PROGRAM "path:"),
	       ;; which is the right idea (searching through the Unix
	       ;; PATH). Unfortunately, there is no logical pathname
	       ;; "path:" defined in sbcl-0.6.10. It would probably be 
	       ;; reasonable to restore Unix PATH searching in SBCL, e.g.
	       ;; with a function FIND-EXECUTABLE-FILE-IN-POSIX-PATH.
	       ;; (I don't want to do it with search lists the way
	       ;; that CMU CL did, because those are a non-ANSI
	       ;; extension which I'd like to get rid of. -- WHN)
	       (pfile (unix-namestring program t))
	       (cookie (list 0)))
	   (unless pfile
	     (error "no such program: ~S" program))
	   (unless (unix-filename-is-executable-p pfile)
	     (error "not executable: ~S" program))
	   (multiple-value-bind (stdin input-stream)
	       (get-descriptor-for input cookie
				   :direction :input
				   :if-does-not-exist if-input-does-not-exist)
	     (multiple-value-bind (stdout output-stream)
		 (get-descriptor-for output cookie
				     :direction :output
				     :if-exists if-output-exists)
	       (multiple-value-bind (stderr error-stream)
		   (if (eq error :output)
		       (values stdout output-stream)
		       (get-descriptor-for error cookie
					   :direction :output
					   :if-exists if-error-exists))
		 (multiple-value-bind (pty-name pty-stream)
		     (open-pty pty cookie)
		   ;; Make sure we are not notified about the child
		   ;; death before we have installed the PROCESS
		   ;; structure in *ACTIVE-PROCESSES*.
		   (sb-sys:without-interrupts
		    (with-c-strvec (args-vec simple-args)
		      (with-c-strvec (environment-vec environment)
			(let ((child-pid
			       (without-gcing
				(spawn pfile args-vec environment-vec pty-name
				       stdin stdout stderr))))
			  (when (< child-pid 0)
			    (error "couldn't fork child process: ~A"
				   (strerror)))
			  (setf proc (make-process :pid child-pid
						   :%status :running
						   :pty pty-stream
						   :input input-stream
						   :output output-stream
						   :error error-stream
						   :status-hook status-hook
						   :cookie cookie))
			  (push proc *active-processes*))))))))))
      (dolist (fd *close-in-parent*)
	(sb-unix:unix-close fd))
      (unless proc
	(dolist (fd *close-on-error*)
	  (sb-unix:unix-close fd))
	(dolist (handler *handlers-installed*)
	  (sb-sys:remove-fd-handler handler))))
    (when (and wait proc)
      (process-wait proc))
    proc))

;;; Install a handler for any input that shows up on the file
;;; descriptor. The handler reads the data and writes it to the
;;; stream.
(defun copy-descriptor-to-stream (descriptor stream cookie)
  (incf (car cookie))
  (let ((string (make-string 256))
	handler)
    (setf handler
	  (sb-sys:add-fd-handler
	   descriptor
	   :input #'(lambda (fd)
		      (declare (ignore fd))
		      (loop
			  (unless handler
			    (return))
			  (multiple-value-bind
				(result readable/errno)
			      (sb-unix:unix-select (1+ descriptor)
						   (ash 1 descriptor)
						   0 0 0)
			    (cond ((null result)
				   (error "~@<couldn't select on sub-process: ~
                                           ~2I~_~A~:>"
					  (strerror readable/errno)))
				  ((zerop result)
				   (return))))
			(sb-alien:with-alien ((buf (sb-alien:array
						    sb-c-call:char
						    256)))
			  (multiple-value-bind
				(count errno)
			      (sb-unix:unix-read descriptor
						 (alien-sap buf)
						 256)
			    (cond ((or (and (null count)
					    (eql errno sb-unix:eio))
				       (eql count 0))
				   (sb-sys:remove-fd-handler handler)
				   (setf handler nil)
				   (decf (car cookie))
				   (sb-unix:unix-close descriptor)
				   (return))
				  ((null count)
				   (sb-sys:remove-fd-handler handler)
				   (setf handler nil)
				   (decf (car cookie))
				   (error
				    "~@<couldn't read input from sub-process: ~
                                     ~2I~_~A~:>"
				    (strerror errno)))
				  (t
				   (sb-kernel:copy-from-system-area
				    (alien-sap buf) 0
				    string (* sb-vm:vector-data-offset
					      sb-vm:word-bits)
				    (* count sb-vm:byte-bits))
				   (write-string string stream
						 :end count)))))))))))

;;; Find a file descriptor to use for object given the direction.
;;; Returns the descriptor. If object is :STREAM, returns the created
;;; stream as the second value.
(defun get-descriptor-for (object
			   cookie
			   &rest keys
			   &key direction
			   &allow-other-keys)
  (cond ((eq object t)
	 ;; No new descriptor is needed.
	 (values -1 nil))
	((eq object nil)
	 ;; Use /dev/null.
	 (multiple-value-bind
	       (fd errno)
	     (sb-unix:unix-open "/dev/null"
				(case direction
				  (:input sb-unix:o_rdonly)
				  (:output sb-unix:o_wronly)
				  (t sb-unix:o_rdwr))
				#o666)
	   (unless fd
	     (error "~@<couldn't open \"/dev/null\": ~2I~_~A~:>"
		    (strerror errno)))
	   (push fd *close-in-parent*)
	   (values fd nil)))
	((eq object :stream)
	 (multiple-value-bind (read-fd write-fd) (sb-unix:unix-pipe)
	   (unless read-fd
	     (error "couldn't create pipe: ~A" (strerror write-fd)))
	   (case direction
	     (:input
	      (push read-fd *close-in-parent*)
	      (push write-fd *close-on-error*)
	      (let ((stream (sb-sys:make-fd-stream write-fd :output t)))
		(values read-fd stream)))
	     (:output
	      (push read-fd *close-on-error*)
	      (push write-fd *close-in-parent*)
	      (let ((stream (sb-sys:make-fd-stream read-fd :input t)))
		(values write-fd stream)))
	     (t
	      (sb-unix:unix-close read-fd)
	      (sb-unix:unix-close write-fd)
	      (error "Direction must be either :INPUT or :OUTPUT, not ~S."
		     direction)))))
	((or (pathnamep object) (stringp object))
	 (with-open-stream (file (apply #'open object keys))
	   (multiple-value-bind
		 (fd errno)
	       (sb-unix:unix-dup (sb-sys:fd-stream-fd file))
	     (cond (fd
		    (push fd *close-in-parent*)
		    (values fd nil))
		   (t
		    (error "couldn't duplicate file descriptor: ~A"
			   (strerror errno)))))))
	((sb-sys:fd-stream-p object)
	 (values (sb-sys:fd-stream-fd object) nil))
	((streamp object)
	 (ecase direction
	   (:input
	    ;; FIXME: We could use a better way of setting up
	    ;; temporary files, both here and in LOAD-FOREIGN.
	    (dotimes (count
		       256
		      (error "could not open a temporary file in /tmp"))
	      (let* ((name (format nil "/tmp/.run-program-~D" count))
		     (fd (sb-unix:unix-open name
					    (logior sb-unix:o_rdwr
						    sb-unix:o_creat
						    sb-unix:o_excl)
					    #o666)))
		(sb-unix:unix-unlink name)
		(when fd
		  (let ((newline (string #\Newline)))
		    (loop
			(multiple-value-bind
			      (line no-cr)
			    (read-line object nil nil)
			  (unless line
			    (return))
			  (sb-unix:unix-write fd line 0 (length line))
			  (if no-cr
			      (return)
			      (sb-unix:unix-write fd newline 0 1)))))
		  (sb-unix:unix-lseek fd 0 sb-unix:l_set)
		  (push fd *close-in-parent*)
		  (return (values fd nil))))))
	   (:output
	    (multiple-value-bind (read-fd write-fd)
		(sb-unix:unix-pipe)
	      (unless read-fd
		(error "couldn't create pipe: ~S" (strerror write-fd)))
	      (copy-descriptor-to-stream read-fd object cookie)
	      (push read-fd *close-on-error*)
	      (push write-fd *close-in-parent*)
	      (values write-fd nil)))))
	(t
	 (error "invalid option to RUN-PROGRAM: ~S" object))))
