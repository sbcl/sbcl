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

#-win32
(progn
  (define-alien-routine wrapped-environ (* c-string))
  (defun posix-environ ()
    "Return the Unix environment (\"man environ\") as a list of SIMPLE-STRINGs."
    (c-strings->string-list (wrapped-environ))))

;#+win32 (sb-alien:define-alien-routine msvcrt-environ (* c-string))

;;; Convert as best we can from an SBCL representation of a Unix
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
     (declare (string string))
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
       (declare (type keyword key) (string val))
       (concatenate 'simple-string (symbol-name key) "=" val)))
   cmucl))

;;;; Import wait3(2) from Unix.

#-win32
(define-alien-routine ("waitpid" c-waitpid) sb-alien:int
  (pid sb-alien:int)
  (status sb-alien:int :out)
  (options sb-alien:int))

#-win32
(defun waitpid (pid &optional do-not-hang check-for-stopped)
  #+sb-doc
  "Return any available status information on child process with PID."
  (multiple-value-bind (pid status)
      (c-waitpid pid
                 (logior (if do-not-hang
                             sb-unix:wnohang
                             0)
                         (if check-for-stopped
                             sb-unix:wuntraced
                             0)))
    (cond ((or (minusp pid)
               (zerop pid))
           nil)
          ((eql (ldb (byte 8 0) status)
                sb-unix:wstopped)
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
                                      sb-unix:sigstop
                                      sb-unix:sigtstp
                                      sb-unix:sigttin
                                      sb-unix:sigttou))
                         :stopped
                         :signaled)
                     signal
                     (not (zerop (ldb (byte 1 7) status)))))))))

;;;; process control stuff
(defvar *active-processes* nil
  #+sb-doc
  "List of process structures for all active processes.")

#-win32
(defvar *active-processes-lock*
  (sb-thread:make-mutex :name "Lock for active processes."))

;;; *ACTIVE-PROCESSES* can be accessed from multiple threads so a
;;; mutex is needed. More importantly the sigchld signal handler also
;;; accesses it, that's why we need without-interrupts.
(defmacro with-active-processes-lock (() &body body)
  #-win32
  `(sb-thread::with-system-mutex (*active-processes-lock*)
     ,@body)
  #+win32
  `(progn ,@body))

(defstruct (process (:copier nil))
  pid                 ; PID of child process
  %status             ; either :RUNNING, :STOPPED, :EXITED, or :SIGNALED
  exit-code           ; either exit code or signal
  core-dumped         ; T if a core image was dumped
  #-win32 pty                 ; stream to child's pty, or NIL
  input               ; stream to child's input, or NIL
  output              ; stream from child's output, or NIL
  error               ; stream from child's error output, or NIL
  status-hook         ; closure to call when PROC changes status
  plist               ; a place for clients to stash things
  cookie)             ; list of the number of pipes from the subproc

(defmethod print-object ((process process) stream)
  (print-unreadable-object (process stream :type t)
    (let ((status (process-status process)))
     (if (eq :exited status)
         (format stream "~S ~S" status (process-exit-code process))
         (format stream "~S ~S" (process-pid process) status)))
    process))

#+sb-doc
(setf (documentation 'process-p 'function)
      "T if OBJECT is a PROCESS, NIL otherwise.")

#+sb-doc
(setf (documentation 'process-pid 'function) "The pid of the child process.")

#+win32
(define-alien-routine ("GetExitCodeProcess@8" get-exit-code-process)
    int
  (handle unsigned) (exit-code unsigned :out))

(defun process-status (process)
  #+sb-doc
  "Return the current status of PROCESS.  The result is one of :RUNNING,
   :STOPPED, :EXITED, or :SIGNALED."
  (get-processes-status-changes)
  (process-%status process))

#+sb-doc
(setf (documentation 'process-exit-code 'function)
      "The exit code or the signal of a stopped process.")

#+sb-doc
(setf (documentation 'process-core-dumped 'function)
      "T if a core image was dumped by the process.")

#+sb-doc
(setf (documentation 'process-pty 'function)
      "The pty stream of the process or NIL.")

#+sb-doc
(setf (documentation 'process-input 'function)
      "The input stream of the process or NIL.")

#+sb-doc
(setf (documentation 'process-output 'function)
      "The output stream of the process or NIL.")

#+sb-doc
(setf (documentation 'process-error 'function)
      "The error stream of the process or NIL.")

#+sb-doc
(setf (documentation 'process-status-hook  'function)
      "A function that is called when PROCESS changes its status.
The function is called with PROCESS as its only argument.")

#+sb-doc
(setf (documentation 'process-plist  'function)
      "A place for clients to stash things.")

(defun process-wait (process &optional check-for-stopped)
  #+sb-doc
  "Wait for PROCESS to quit running for some reason. When
CHECK-FOR-STOPPED is T, also returns when PROCESS is stopped. Returns
PROCESS."
  (loop
      (case (process-status process)
        (:running)
        (:stopped
         (when check-for-stopped
           (return)))
        (t
         (when (zerop (car (process-cookie process)))
           (return))))
      (sb-sys:serve-all-events 1))
  process)

#-win32
;;; Find the current foreground process group id.
(defun find-current-foreground-process (proc)
  (with-alien ((result sb-alien:int))
    (multiple-value-bind
          (wonp error)
        (sb-unix:unix-ioctl (sb-sys:fd-stream-fd (process-pty proc))
                            sb-unix:TIOCGPGRP
                            (alien-sap (sb-alien:addr result)))
      (unless wonp
        (error "TIOCPGRP ioctl failed: ~S" (strerror error)))
      result))
  (process-pid proc))

#-win32
(defun process-kill (process signal &optional (whom :pid))
  #+sb-doc
  "Hand SIGNAL to PROCESS. If WHOM is :PID, use the kill Unix system call. If
   WHOM is :PROCESS-GROUP, use the killpg Unix system call. If WHOM is
   :PTY-PROCESS-GROUP deliver the signal to whichever process group is
   currently in the foreground."
  (let ((pid (ecase whom
               ((:pid :process-group)
                (process-pid process))
               (:pty-process-group
                (find-current-foreground-process process)))))
    (multiple-value-bind
          (okay errno)
        (case whom
          ((:process-group)
           (sb-unix:unix-killpg pid signal))
          (t
           (sb-unix:unix-kill pid signal)))
      (cond ((not okay)
             (values nil errno))
            ((and (eql pid (process-pid process))
                  (= signal sb-unix:sigcont))
             (setf (process-%status process) :running)
             (setf (process-exit-code process) nil)
             (when (process-status-hook process)
               (funcall (process-status-hook process) process))
             t)
            (t
             t)))))

(defun process-alive-p (process)
  #+sb-doc
  "Return T if PROCESS is still alive, NIL otherwise."
  (let ((status (process-status process)))
    (if (or (eq status :running)
            (eq status :stopped))
        t
        nil)))

(defun process-close (process)
  #+sb-doc
  "Close all streams connected to PROCESS and stop maintaining the
status slot."
  (macrolet ((frob (stream abort)
               `(when ,stream (close ,stream :abort ,abort))))
    #-win32
    (frob (process-pty process) t)   ; Don't FLUSH-OUTPUT to dead process,
    (frob (process-input process) t) ; .. 'cause it will generate SIGPIPE.
    (frob (process-output process) nil)
    (frob (process-error process) nil))
  ;; FIXME: Given that the status-slot is no longer updated,
  ;; maybe it should be set to :CLOSED, or similar?
  (with-active-processes-lock ()
   (setf *active-processes* (delete process *active-processes*)))
  process)

(defun get-processes-status-changes ()
  (let (exited)
    (with-active-processes-lock ()
      (setf *active-processes*
            (delete-if #-win32
                       (lambda (proc)
                         ;; Wait only on pids belonging to processes
                         ;; started by RUN-PROGRAM. There used to be a
                         ;; WAIT3 call here, but that makes direct
                         ;; WAIT, WAITPID usage impossible due to the
                         ;; race with the SIGCHLD signal handler.
                         (multiple-value-bind (pid what code core)
                             (waitpid (process-pid proc) t t)
                           (when pid
                             (setf (process-%status proc) what)
                             (setf (process-exit-code proc) code)
                             (setf (process-core-dumped proc) core)
                             (when (process-status-hook proc)
                               (push proc exited))
                             t)))
                       #+win32
                       (lambda (proc)
                         (multiple-value-bind (ok code)
                             (get-exit-code-process (process-pid proc))
                           (when (and (plusp ok) (/= code 259))
                             (setf (process-%status proc) :exited
                                   (process-exit-code proc) code)
                             (when (process-status-hook proc)
                               (push proc exited))
                             t)))
                       *active-processes*)))
    ;; Can't call the hooks before all the processes have been deal
    ;; with, as calling a hook may cause re-entry to
    ;; GET-PROCESS-STATUS-CHANGES. That may be OK when using waitpid,
    ;; but in the Windows implementation it would be deeply bad.
    (dolist (proc exited)
      (let ((hook (process-status-hook proc)))
        (when hook
          (funcall hook proc))))))

;;;; RUN-PROGRAM and close friends

;;; list of file descriptors to close when RUN-PROGRAM exits due to an error
(defvar *close-on-error* nil)

;;; list of file descriptors to close when RUN-PROGRAM returns in the parent
(defvar *close-in-parent* nil)

;;; list of handlers installed by RUN-PROGRAM.  FIXME: nothing seems
;;; to set this.
#-win32
(defvar *handlers-installed* nil)

;;; Find an unused pty. Return three values: the file descriptor for
;;; the master side of the pty, the file descriptor for the slave side
;;; of the pty, and the name of the tty device for the slave side.
#-(or win32 openbsd)
(progn
  (define-alien-routine ptsname c-string (fd int))
  (define-alien-routine grantpt boolean (fd int))
  (define-alien-routine unlockpt boolean (fd int))

  (defun find-a-pty ()
    ;; First try to use the Unix98 pty api.
    (let* ((master-name (coerce (format nil "/dev/ptmx") 'base-string))
           (master-fd (sb-unix:unix-open master-name
                                         sb-unix:o_rdwr
                                         #o666)))
      (when master-fd
        (grantpt master-fd)
        (unlockpt master-fd)
        (let* ((slave-name (ptsname master-fd))
               (slave-fd (sb-unix:unix-open slave-name
                                            sb-unix:o_rdwr
                                            #o666)))
          (when slave-fd
            (return-from find-a-pty
              (values master-fd
                      slave-fd
                      slave-name)))
          (sb-unix:unix-close master-fd))
        (error "could not find a pty")))
    ;; No dice, try using the old-school method.
    (dolist (char '(#\p #\q))
      (dotimes (digit 16)
        (let* ((master-name (coerce (format nil "/dev/pty~C~X" char digit)
                                    'base-string))
               (master-fd (sb-unix:unix-open master-name
                                             sb-unix:o_rdwr
                                             #o666)))
          (when master-fd
            (let* ((slave-name (coerce (format nil "/dev/tty~C~X" char digit)
                                       'base-string))
                   (slave-fd (sb-unix:unix-open slave-name
                                                sb-unix:o_rdwr
                                                #o666)))
              (when slave-fd
                (return-from find-a-pty
                  (values master-fd
                          slave-fd
                          slave-name)))
              (sb-unix:unix-close master-fd))))))
    (error "could not find a pty")))
#+openbsd
(progn
  (define-alien-routine openpty int (amaster int :out) (aslave int :out)
                        (name (* char)) (termp (* t)) (winp (* t)))
  (defun find-a-pty ()
    (with-alien ((name-buf (array char 16)))
      (multiple-value-bind (return-val master-fd slave-fd)
          (openpty (cast name-buf (* char)) nil nil)
        (if (zerop return-val)
            (values master-fd
                    slave-fd
                    (sb-alien::c-string-to-string (alien-sap name-buf)
                                                  (sb-impl::default-external-format)
                                                  'character))
            (error "could not find a pty"))))))

#-win32
(defun open-pty (pty cookie &key (external-format :default))
  (when pty
    (multiple-value-bind
          (master slave name)
        (find-a-pty)
      (push master *close-on-error*)
      (push slave *close-in-parent*)
      (when (streamp pty)
        (multiple-value-bind (new-fd errno) (sb-unix:unix-dup master)
          (unless new-fd
            (error "couldn't SB-UNIX:UNIX-DUP ~W: ~A" master (strerror errno)))
          (push new-fd *close-on-error*)
          (copy-descriptor-to-stream new-fd pty cookie external-format)))
      (values name
              (sb-sys:make-fd-stream master :input t :output t
                                     :element-type :default
                                     :dual-channel-p t)))))

(defmacro round-bytes-to-words (n)
  (let ((bytes-per-word (/ sb-vm:n-machine-word-bits sb-vm:n-byte-bits)))
    `(logandc2 (the fixnum (+ (the fixnum ,n)
                              (1- ,bytes-per-word))) (1- ,bytes-per-word))))

(defun string-list-to-c-strvec (string-list)
  (let* ((bytes-per-word (/ sb-vm:n-machine-word-bits sb-vm:n-byte-bits))
         ;; We need an extra for the null, and an extra 'cause exect
         ;; clobbers argv[-1].
         (vec-bytes (* bytes-per-word (+ (length string-list) 2)))
         (octet-vector-list (mapcar (lambda (s)
                                      (string-to-octets s :null-terminate t))
                                    string-list))
         (string-bytes (reduce #'+ octet-vector-list
                               :key (lambda (s)
                                      (round-bytes-to-words (length s)))))
         (total-bytes (+ string-bytes vec-bytes))
         ;; Memory to hold the vector of pointers and all the strings.
         (vec-sap (sb-sys:allocate-system-memory total-bytes))
         (string-sap (sap+ vec-sap vec-bytes))
         ;; Index starts from [1]!
         (vec-index-offset bytes-per-word))
    (declare (index string-bytes vec-bytes total-bytes)
             (sb-sys:system-area-pointer vec-sap string-sap))
    (dolist (octets octet-vector-list)
      (declare (type (simple-array (unsigned-byte 8) (*)) octets))
      (let ((size (length octets)))
        ;; Copy string.
        (sb-kernel:copy-ub8-to-system-area octets 0 string-sap 0 size)
        ;; Put the pointer in the vector.
        (setf (sap-ref-sap vec-sap vec-index-offset) string-sap)
        ;; Advance string-sap for the next string.
        (setf string-sap (sap+ string-sap (round-bytes-to-words size)))
        (incf vec-index-offset bytes-per-word)))
    ;; Final null pointer.
    (setf (sap-ref-sap vec-sap vec-index-offset) (int-sap 0))
    (values vec-sap (sap+ vec-sap bytes-per-word) total-bytes)))

(defmacro with-c-strvec ((var str-list) &body body)
  (with-unique-names (sap size)
    `(multiple-value-bind (,sap ,var ,size)
         (string-list-to-c-strvec ,str-list)
       (unwind-protect
            (progn
              ,@body)
         (sb-sys:deallocate-system-memory ,sap ,size)))))

(sb-alien:define-alien-routine spawn
    #-win32 sb-alien:int
    #+win32 sb-win32::handle
  (program sb-alien:c-string)
  (argv (* sb-alien:c-string))
  (stdin sb-alien:int)
  (stdout sb-alien:int)
  (stderr sb-alien:int)
  (search sb-alien:int)
  (envp (* sb-alien:c-string))
  (pty-name sb-alien:c-string)
  (wait sb-alien:int))

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
                    #-win32 (env nil env-p)
                    #-win32 (environment
                             (if env-p
                                 (unix-environment-sbcl-from-cmucl env)
                                 (posix-environ))
                             environment-p)
                    (wait t)
                    search
                    #-win32 pty
                    input
                    if-input-does-not-exist
                    output
                    (if-output-exists :error)
                    (error :output)
                    (if-error-exists :error)
                    status-hook
                    (external-format :default))
  #+sb-doc
  #.(concatenate
     'string
     ;; The Texinfoizer is sensitive to whitespace, so mind the
     ;; placement of the #-win32 pseudosplicings.
     "RUN-PROGRAM creates a new process specified by the PROGRAM
argument. ARGS are the standard arguments that can be passed to a
program. For no arguments, use NIL (which means that just the
name of the program is passed as arg 0).

The program arguments and the environment are encoded using the
default external format for streams.

RUN-PROGRAM will return a PROCESS structure. See the CMU Common Lisp
Users Manual for details about the PROCESS structure."#-win32"

   Notes about Unix environments (as in the :ENVIRONMENT and :ENV args):

   - The SBCL implementation of RUN-PROGRAM, like Perl and many other
     programs, but unlike the original CMU CL implementation, copies
     the Unix environment by default.

   - Running Unix programs from a setuid process, or in any other
     situation where the Unix environment is under the control of someone
     else, is a mother lode of security problems. If you are contemplating
     doing this, read about it first. (The Perl community has a lot of good
     documentation about this and other security issues in script-like
     programs.)""

   The &KEY arguments have the following meanings:
"#-win32"
   :ENVIRONMENT
      a list of STRINGs describing the new Unix environment
      (as in \"man environ\"). The default is to copy the environment of
      the current process.
   :ENV
      an alternative lossy representation of the new Unix environment,
      for compatibility with CMU CL""
   :SEARCH
      Look for PROGRAM in each of the directories in the child's $PATH
      environment variable.  Otherwise an absolute pathname is required.
   :WAIT
      If non-NIL (default), wait until the created process finishes.  If
      NIL, continue running Lisp until the program finishes."#-win32"
   :PTY
      Either T, NIL, or a stream.  Unless NIL, the subprocess is established
      under a PTY.  If :pty is a stream, all output to this pty is sent to
      this stream, otherwise the PROCESS-PTY slot is filled in with a stream
      connected to pty that can read output and write input.""
   :INPUT
      Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
      input for the current process is inherited.  If NIL, "
      #-win32"/dev/null"#+win32"nul""
      is used.  If a pathname, the file so specified is used.  If a stream,
      all the input is read from that stream and sent to the subprocess.  If
      :STREAM, the PROCESS-INPUT slot is filled in with a stream that sends
      its output to the process. Defaults to NIL.
   :IF-INPUT-DOES-NOT-EXIST (when :INPUT is the name of a file)
      can be one of:
         :ERROR to generate an error
         :CREATE to create an empty file
         NIL (the default) to return NIL from RUN-PROGRAM
   :OUTPUT
      Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
      output for the current process is inherited.  If NIL, "
      #-win32"/dev/null"#+win32"nul""
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
      process changes.  The function takes the process as an argument.
   :EXTERNAL-FORMAT
      The external-format to use for :INPUT, :OUTPUT, and :ERROR :STREAMs.")
  #-win32
  (when (and env-p environment-p)
    (error "can't specify :ENV and :ENVIRONMENT simultaneously"))
  ;; Prepend the program to the argument list.
  (push (namestring program) args)
  (labels (;; It's friendly to allow the caller to pass any string
           ;; designator, but internally we'd like SIMPLE-STRINGs.
           ;;
           ;; Huh?  We let users pass in symbols and characters for
           ;; the arguments, but call NAMESTRING on the program
           ;; name... -- RMK
           (simplify-args (args)
             (loop for arg in args
                   as escaped-arg = (escape-arg arg)
                   collect (coerce escaped-arg 'simple-string)))
           (escape-arg (arg)
             #-win32 arg
             ;; Apparently any spaces or double quotes in the arguments
             ;; need to be escaped on win32.
             #+win32 (if (position-if
                          (lambda (c) (find c '(#\" #\Space))) arg)
                         (write-to-string arg)
                         arg)))
    (let (;; Clear various specials used by GET-DESCRIPTOR-FOR to
          ;; communicate cleanup info.
          *close-on-error*
          *close-in-parent*
          ;; Some other binding used only on non-Win32.  FIXME:
          ;; nothing seems to set this.
          #-win32 *handlers-installed*
          ;; Establish PROC at this level so that we can return it.
          proc
          (simple-args (simplify-args args))
          (progname (native-namestring program))
          ;; Gag.
          (cookie (list 0)))
      (unwind-protect
           ;; Note: despite the WITH-* names, these macros don't
           ;; expand into UNWIND-PROTECT forms.  They're just
           ;; syntactic sugar to make the rest of the routine slightly
           ;; easier to read.
           (macrolet ((with-fd-and-stream-for (((fd stream) which &rest args)
                                               &body body)
                        `(multiple-value-bind (,fd ,stream)
                             ,(ecase which
                                ((:input :output)
                                 `(get-descriptor-for ,@args))
                                (:error
                                 `(if (eq ,(first args) :output)
                                      ;; kludge: we expand into
                                      ;; hard-coded symbols here.
                                      (values stdout output-stream)
                                      (get-descriptor-for ,@args))))
                           ,@body))
                      (with-open-pty (((pty-name pty-stream) (pty cookie))
                                      &body body)
                        #+win32 `(declare (ignore ,pty ,cookie))
                        #+win32 `(let (,pty-name ,pty-stream) ,@body)
                        #-win32 `(multiple-value-bind (,pty-name ,pty-stream)
                                     (open-pty ,pty ,cookie)
                                   ,@body))
                      (with-args-vec ((vec args) &body body)
                        `(with-c-strvec (,vec ,args)
                           ,@body))
                      (with-environment-vec ((vec env) &body body)
                        #+win32 `(let (,vec) ,@body)
                        #-win32 `(with-c-strvec (,vec ,env) ,@body)))
             (with-fd-and-stream-for ((stdin input-stream) :input
                                      input cookie
                                      :direction :input
                                      :if-does-not-exist if-input-does-not-exist
                                      :external-format external-format
                                      :wait wait)
               (with-fd-and-stream-for ((stdout output-stream) :output
                                        output cookie
                                        :direction :output
                                        :if-exists if-output-exists
                                        :external-format external-format)
                 (with-fd-and-stream-for ((stderr error-stream)  :error
                                          error cookie
                                          :direction :output
                                          :if-exists if-error-exists
                                          :external-format external-format)
                   (with-open-pty ((pty-name pty-stream) (pty cookie))
                     ;; Make sure we are not notified about the child
                     ;; death before we have installed the PROCESS
                     ;; structure in *ACTIVE-PROCESSES*.
                     (let (child)
                       (with-active-processes-lock ()
                         (with-args-vec (args-vec simple-args)
                           (with-environment-vec (environment-vec environment)
                             (setq child (without-gcing
                                           (spawn progname args-vec
                                                  stdin stdout stderr
                                                  (if search 1 0)
                                                  environment-vec pty-name
                                                  (if wait 1 0))))
                             (unless (= child -1)
                               (setf proc
                                     (apply
                                      #'make-process
                                      :pid child
                                      :input input-stream
                                      :output output-stream
                                      :error error-stream
                                      :status-hook status-hook
                                      :cookie cookie
                                      #-win32 (list :pty pty-stream
                                                    :%status :running)
                                      #+win32 (if wait
                                                  (list :%status :exited
                                                        :exit-code child)
                                                  (list :%status :running))))
                               (push proc *active-processes*)))))
                       ;; Report the error outside the lock.
                       (when (= child -1)
                         (error "couldn't fork child process: ~A"
                                (strerror)))))))))
        (dolist (fd *close-in-parent*)
          (sb-unix:unix-close fd))
        (unless proc
          (dolist (fd *close-on-error*)
            (sb-unix:unix-close fd))
          ;; FIXME: nothing seems to set this.
          #-win32
          (dolist (handler *handlers-installed*)
            (sb-sys:remove-fd-handler handler))))
      #-win32
      (when (and wait proc)
        (process-wait proc))
      proc)))

;;; Install a handler for any input that shows up on the file
;;; descriptor. The handler reads the data and writes it to the
;;; stream.
(defun copy-descriptor-to-stream (descriptor stream cookie external-format)
  (incf (car cookie))
  (let* (handler
         (buf (make-array 256 :element-type '(unsigned-byte 8)))
         (read-end 0))
    (setf handler
          (sb-sys:add-fd-handler
           descriptor
           :input
           (lambda (fd)
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
                         (if (eql sb-unix:eintr readable/errno)
                             (return)
                             (error "~@<Couldn't select on sub-process: ~
                                        ~2I~_~A~:>"
                                    (strerror readable/errno))))
                        ((zerop result)
                         (return))))
                (multiple-value-bind (count errno)
                    (with-pinned-objects (buf)
                      (sb-unix:unix-read descriptor
                                         (sap+ (vector-sap buf) read-end)
                                         (- (length buf) read-end)))
                  (cond
                    ((and #-win32 (or (and (null count)
                                           (eql errno sb-unix:eio))
                                      (eql count 0))
                          #+win32 (<= count 0))
                     (sb-sys:remove-fd-handler handler)
                     (setf handler nil)
                     (decf (car cookie))
                     (sb-unix:unix-close descriptor)
                     (unless (zerop read-end)
                       ;; Should this be an END-OF-FILE?
                       (error "~@<non-empty buffer when EOF reached ~
                               while reading from child: ~S~:>" buf))
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
                     (incf read-end count)
                     (let* ((decode-end read-end)
                            (string (handler-case
                                        (octets-to-string
                                         buf :end read-end
                                         :external-format external-format)
                                      (end-of-input-in-character (e)
                                        (setf decode-end
                                              (octet-decoding-error-start e))
                                        (octets-to-string
                                         buf :end decode-end
                                         :external-format external-format)))))
                       (unless (zerop (length string))
                         (write-string string stream)
                         (when (/= decode-end (length buf))
                           (replace buf buf :start2 decode-end :end2 read-end))
                         (decf read-end decode-end))))))))))))

;;; FIXME: something very like this is done in SB-POSIX to treat
;;; streams as file descriptor designators; maybe we can combine these
;;; two?  Additionally, as we have a couple of user-defined streams
;;; libraries, maybe we should have a generic function for doing this,
;;; so user-defined streams can play nicely with RUN-PROGRAM (and
;;; maybe also with SB-POSIX)?
(defun get-stream-fd-and-external-format (stream direction)
  (typecase stream
    (sb-sys:fd-stream
     (values (sb-sys:fd-stream-fd stream) nil (stream-external-format stream)))
    (synonym-stream
     (get-stream-fd-and-external-format
      (symbol-value (synonym-stream-symbol stream)) direction))
    (two-way-stream
     (ecase direction
       (:input
        (get-stream-fd-and-external-format
         (two-way-stream-input-stream stream) direction))
       (:output
        (get-stream-fd-and-external-format
         (two-way-stream-output-stream stream) direction))))))


;;; Find a file descriptor to use for object given the direction.
;;; Returns the descriptor. If object is :STREAM, returns the created
;;; stream as the second value.
(defun get-descriptor-for (object
                           cookie
                           &rest keys
                           &key direction (external-format :default) wait
                           &allow-other-keys)
  (declare (ignore wait)) ;This is explained below.
  ;; Our use of a temporary file dates back to very old CMUCLs, and
  ;; was probably only ever intended for use with STRING-STREAMs,
  ;; which are ordinarily smallish.  However, as we've got
  ;; user-defined stream classes, we can end up trying to copy
  ;; arbitrarily much data into the temp file, and so are liable to
  ;; run afoul of disk quotas or to choke on small /tmp file systems.
  (flet ((make-temp-fd ()
           (multiple-value-bind (fd name/errno)
               (sb-unix:sb-mkstemp "/tmp/.run-program-XXXXXX" #o0600)
             (unless fd
               (error "could not open a temporary file: ~A"
                      (strerror name/errno)))
             (unless (sb-unix:unix-unlink name/errno)
               (sb-unix:unix-close fd)
               (error "failed to unlink ~A" name/errno))
             fd)))
    (cond ((eq object t)
           ;; No new descriptor is needed.
           (values -1 nil))
          ((eq object nil)
           ;; Use /dev/null.
           (multiple-value-bind
                 (fd errno)
               (sb-unix:unix-open #-win32 #.(coerce "/dev/null" 'base-string)
                                  #+win32 #.(coerce "nul" 'base-string)
                                  (case direction
                                    (:input sb-unix:o_rdonly)
                                    (:output sb-unix:o_wronly)
                                    (t sb-unix:o_rdwr))
                                  #o666)
             (unless fd
               (error #-win32 "~@<couldn't open \"/dev/null\": ~2I~_~A~:>"
                      #+win32 "~@<couldn't open \"nul\" device: ~2I~_~A~:>"
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
                (let ((stream (sb-sys:make-fd-stream write-fd :output t
                                                     :element-type :default
                                                     :external-format
                                                     external-format)))
                  (values read-fd stream)))
               (:output
                (push read-fd *close-on-error*)
                (push write-fd *close-in-parent*)
                (let ((stream (sb-sys:make-fd-stream read-fd :input t
                                                     :element-type :default
                                                     :external-format
                                                     external-format)))
                  (values write-fd stream)))
               (t
                (sb-unix:unix-close read-fd)
                (sb-unix:unix-close write-fd)
                (error "Direction must be either :INPUT or :OUTPUT, not ~S."
                       direction)))))
          ((or (pathnamep object) (stringp object))
           ;; GET-DESCRIPTOR-FOR uses &allow-other-keys, so rather
           ;; than munge the &rest list for OPEN, just disable keyword
           ;; validation there.
           (with-open-stream (file (apply #'open object :allow-other-keys t
                                          keys))
             (multiple-value-bind
                   (fd errno)
                 (sb-unix:unix-dup (sb-sys:fd-stream-fd file))
               (cond (fd
                      (push fd *close-in-parent*)
                      (values fd nil))
                     (t
                      (error "couldn't duplicate file descriptor: ~A"
                             (strerror errno)))))))
          ((streamp object)
           (ecase direction
             (:input
              (block nil
                ;; If we can get an fd for the stream, let the child
                ;; process use the fd for its descriptor.  Otherwise,
                ;; we copy data from the stream into a temp file, and
                ;; give the temp file's descriptor to the
                ;; child.
                (multiple-value-bind (fd stream format)
                    (get-stream-fd-and-external-format object :input)
                  (declare (ignore format))
                  (when fd
                    (return (values fd stream))))
                ;; FIXME: if we can't get the file descriptor, since
                ;; the stream might be interactive or otherwise
                ;; block-y, we can't know whether we can copy the
                ;; stream's data to a temp file, so if RUN-PROGRAM was
                ;; called with :WAIT NIL, we should probably error.
                ;; However, STRING-STREAMs aren't fd-streams, but
                ;; they're not prone to blocking; any user-defined
                ;; streams that "read" from some in-memory data will
                ;; probably be similar to STRING-STREAMs.  So maybe we
                ;; should add a STREAM-INTERACTIVE-P generic function
                ;; for problems like this?  Anyway, the machinery is
                ;; here, if you feel like filling in the details.
                #|
                (when (and (null wait) #<some undetermined criterion>)
                  (error "~@<don't know how to get an fd for ~A, and so ~
                             can't ensure that copying its data to the ~
                             child process won't hang~:>" object))
                |#
                (let ((fd (make-temp-fd))
                      (newline (string #\Newline)))
                  (loop
                     (multiple-value-bind
                           (line no-cr)
                         (read-line object nil nil)
                       (unless line
                         (return))
                       (let ((vector (string-to-octets line)))
                         (sb-unix:unix-write
                          fd vector 0 (length vector)))
                       (if no-cr
                           (return)
                           (sb-unix:unix-write fd newline 0 1))))
                  (sb-unix:unix-lseek fd 0 sb-unix:l_set)
                  (push fd *close-in-parent*)
                  (return (values fd nil)))))
             (:output
              (block nil
                ;; Similar to the :input trick above, except we
                ;; arrange to copy data from the stream.  This is
                ;; slightly saner than the input case, since we don't
                ;; buffer to a file, but I think we may still lose if
                ;; there's unflushed data in the stream buffer and we
                ;; give the file descriptor to the child.
                (multiple-value-bind (fd stream format)
                    (get-stream-fd-and-external-format object :output)
                  (declare (ignore format))
                  (when fd
                    (return (values fd stream))))
                (multiple-value-bind (read-fd write-fd)
                    (sb-unix:unix-pipe)
                  (unless read-fd
                    (error "couldn't create pipe: ~S" (strerror write-fd)))
                  (copy-descriptor-to-stream read-fd object cookie
                                             external-format)
                  (push read-fd *close-on-error*)
                  (push write-fd *close-in-parent*)
                  (return (values write-fd nil)))))))
          (t
           (error "invalid option to RUN-PROGRAM: ~S" object)))))
