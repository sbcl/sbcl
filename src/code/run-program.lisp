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

(in-package "SB-IMPL")

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

#+win32
(progn
  (defun decode-windows-environment (environment)
    (loop until (zerop (sap-ref-8 environment 0))
          collect
          (let ((string (sb-alien::c-string-to-string environment
                                                      (sb-alien::default-c-string-external-format)
                                                      'character)))
            (loop for value = (sap-ref-8 environment 0)
                  do (setf environment (sap+ environment 1))
                  until (zerop value))
            string)))

  (defun encode-windows-environment (list)
    (let* ((external-format (sb-alien::default-c-string-external-format))
           octets
           (length 1)) ;; 1 for \0 at the very end
      (setf octets
            (loop for x in list
                  for octet =
                  (string-to-octets x :external-format external-format
                                      :null-terminate t)
                  collect octet
                  do
                  (incf length (length octet))))
      (let ((mem (allocate-system-memory length))
            (index 0))

        (loop for string in octets
              for length = (length string)
              do
              (copy-ub8-to-system-area string 0 mem index length)
              (incf index length))
        (setf (sap-ref-8 mem index) 0)
        (values mem mem length))))

  (defun posix-environ ()
    (decode-windows-environment
     (alien-funcall (extern-alien "GetEnvironmentStrings"
                                  (function system-area-pointer))))))

;;; Convert from a CMU CL representation of a Unix environment to a
;;; SBCL representation.
(defun unix-environment-sbcl-from-cmucl (cmucl)
  (mapcar
   (lambda (cons)
     (destructuring-bind (key . val) cons
       (declare (type keyword key) (string val))
       (concatenate 'simple-string (symbol-name key) "=" val)))
   cmucl))

#-win32
(define-alien-routine ("waitpid" c-waitpid) int
  (pid int)
  (status int :out)
  (options int))

#-win32
(defun waitpid (pid)
  "Return any available status information on child process with PID."
  (multiple-value-bind (pid status)
      (c-waitpid pid
                 (logior sb-unix:wnohang sb-unix:wuntraced sb-unix:wcontinued))
    (cond ((or (minusp pid)
               (zerop pid))
           (values nil nil nil))
          ((wifcontinued status)
           (values :running
                   nil
                   nil))
          ((wifstopped status)
           (values :stopped
                   (ldb (byte 8 8) status)
                   nil))
          ((zerop (ldb (byte 7 0) status))
           (values :exited
                   (ldb (byte 8 8) status)
                   nil))
          (t
           (let ((signal (ldb (byte 7 0) status)))
             (values (if (position signal
                                   #.(vector
                                      sb-unix:sigstop
                                      sb-unix:sigtstp
                                      sb-unix:sigttin
                                      sb-unix:sigttou))
                         :stopped
                         :signaled)
                     signal
                     (not (zerop (ldb (byte 1 7) status)))))))))

#-win32
(define-alien-routine wifcontinued boolean
  (status int))

#-win32
(define-alien-routine wifstopped boolean
  (status int))

;;;; process control stuff
(define-load-time-global *active-processes* nil
  "List of process structures for all active processes.")

(define-load-time-global *active-processes-lock*
  (sb-thread:make-mutex :name "Lock for active processes."))

(define-load-time-global *spawn-lock*
  (sb-thread:make-mutex :name "Around spawn()."))

;;; *ACTIVE-PROCESSES* can be accessed from multiple threads so a
;;; mutex is needed. More importantly the sigchld signal handler also
;;; accesses it, that's why we need without-interrupts.
(defmacro with-active-processes-lock (() &body body)
  `(with-system-mutex (*active-processes-lock*)
     ,@body))

(deftype process-status ()
  '(member :running :stopped :exited :signaled))

(defstruct (process (:copier nil))
  (pid     nil :type word :read-only t) ; PID of child process
  (%status nil :type process-status)
  %exit-code                            ; either exit code or signal
  core-dumped                           ; T if a core image was dumped
  #-win32 pty                           ; stream to child's pty, or NIL
  input                                 ; stream to child's input, or NIL
  output                                ; stream from child's output, or NIL
  error                                 ; stream from child's error output, or NIL
  status-hook                           ; closure to call when PROC changes status
  plist                                 ; a place for clients to stash things
  (cookie nil :type cons :read-only t)  ; list of the number of pipes from the subproc
  #+win32 copiers ; list of sb-win32::io-copier
  #+win32 (handle nil :type (or null (signed-byte 32)))
  #-win32
  serve-event-pipe)
(declaim (freeze-type process))

(defmethod print-object ((process process) stream)
  (print-unreadable-object (process stream :type t)
    (let ((status (process-status process)))
      (if (eq :exited status)
          (format stream "~S ~S" status (process-%exit-code process))
          (format stream "~S ~S" (process-pid process) status)))
    process))

#+win32
(define-alien-routine ("GetExitCodeProcess" get-exit-code-process)
    int
  (handle unsigned) (exit-code unsigned :out))

(defun process-exit-code (process)
  "Return the exit code of PROCESS."
  (or (process-%exit-code process)
      (progn (get-processes-status-changes)
             (process-%exit-code process))))

(defun process-status (process)
  "Return the current status of PROCESS.  The result is one of :RUNNING,
   :STOPPED, :EXITED, or :SIGNALED."
  (get-processes-status-changes)
  (process-%status process))

(setf (documentation 'process-exit-code 'function)
      "The exit code or the signal of a stopped process."
      (documentation 'process-core-dumped 'function)
      "T if a core image was dumped by the process."
      (documentation 'process-pty 'function)
      "The pty stream of the process or NIL."
      (documentation 'process-input 'function)
      "The input stream of the process or NIL."
      (documentation 'process-output 'function)
      "The output stream of the process or NIL."
      (documentation 'process-error 'function)
      "The error stream of the process or NIL."
      (documentation 'process-status-hook 'function) "A function that is called when PROCESS changes its status.
The function is called with PROCESS as its only argument."
      (documentation 'process-plist 'function)
      "A place for clients to stash things."
      (documentation 'process-p 'function)
      "T if OBJECT is a PROCESS, NIL otherwise."
      (documentation 'process-pid 'function) "The pid of the child process.")

#-win32
(defun setup-serve-event-pipe (process)
  (unless (process-serve-event-pipe process)
    (multiple-value-bind (read-fd write-fd) (sb-unix:unix-pipe)
      (let (handler)
        (setf handler
              (add-fd-handler read-fd :input
                              (lambda (fd)
                                (setf (process-serve-event-pipe process) nil)
                                (remove-fd-handler handler)
                                (sb-unix:unix-close fd))))
        (setf (process-serve-event-pipe process) (list* read-fd write-fd handler))))))

#-win32
(defun close-serve-event-pipe (process)
  (let ((pipe (process-serve-event-pipe process)))
    (when pipe
      (setf (process-serve-event-pipe process) nil)
      (destructuring-bind (read write . handler) pipe
        (remove-fd-handler handler)
        (sb-unix:unix-close read)
        (when write
          (sb-unix:unix-close write))))))

(defun wake-serve-event (process)
  #+win32 (declare (ignorable process))
  #-win32
  (let ((pipe (process-serve-event-pipe process)))
    (when pipe
      (sb-unix:unix-close (shiftf (cadr pipe) nil)))))

(defun process-wait (process &optional check-for-stopped)
  "Wait for PROCESS to quit running for some reason. When
CHECK-FOR-STOPPED is T, also returns when PROCESS is stopped. Returns
PROCESS."
  (declare (ignorable check-for-stopped))
  #+win32
  (sb-win32::win32-process-wait process)
  #-win32
  (progn
    (setup-serve-event-pipe process)
    (loop
     (case (process-status process)
       (:running)
       (:stopped
        (when check-for-stopped
          (return)))
       (t
        (when (zerop (car (process-cookie process)))
          (return))))
     (serve-all-events 10))
    (close-serve-event-pipe process))
  process)

#-win32
;;; Find the current foreground process group id.
(defun find-current-foreground-process (proc)
  (with-alien ((result int))
    (multiple-value-bind
          (wonp error)
        (sb-unix:unix-ioctl (fd-stream-fd (process-pty proc))
                            sb-unix:TIOCGPGRP
                            (alien-sap (addr result)))
      (unless wonp
        (error "TIOCPGRP ioctl failed: ~S" (strerror error)))
      result))
  (process-pid proc))

#-win32
(defun process-kill (process signal &optional (whom :pid))
  "Hand SIGNAL to PROCESS. If WHOM is :PID, use the kill Unix system call. If
   WHOM is :PROCESS-GROUP, use the killpg Unix system call. If WHOM is
   :PTY-PROCESS-GROUP deliver the signal to whichever process group is
   currently in the foreground.
   Returns T if successful, otherwise returns NIL and error number (two values)."
  (let ((pid (ecase whom
               ((:pid :process-group)
                (process-pid process))
               (:pty-process-group
                (find-current-foreground-process process)))))
    (let ((result (if (eq whom :process-group)
                      (sb-unix:unix-killpg pid signal)
                      (sb-unix:unix-kill pid signal))))
      (or (zerop result)
          (values nil (sb-unix::get-errno))))))

#+win32
(defun process-kill (process signal &optional (whom :pid))
  (declare (ignore signal whom))
  (get-processes-status-changes)
  (let ((handle (process-handle process)))
    (when handle
      (prog1 (sb-win32::terminate-process handle 1)
        (get-processes-status-changes)))))

(defun process-alive-p (process)
  "Return T if PROCESS is still alive, NIL otherwise."
  (let ((status (process-status process)))
    (if (or (eq status :running)
            (eq status :stopped))
        t
        nil)))

(defun process-close (process)
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
  #+win32
  (let ((handle (shiftf (process-handle process) nil)))
    (when handle
      (or (plusp (sb-win32:close-handle handle))
          (sb-win32::win32-error 'process-close))))
  process)

(defun get-processes-status-changes ()
  (let (changed)
    (with-active-processes-lock ()
      (setf *active-processes*
            (delete-if #-win32
                       (lambda (proc)
                         ;; Wait only on pids belonging to processes
                         ;; started by RUN-PROGRAM. There used to be a
                         ;; WAIT3 call here, but that makes direct
                         ;; WAIT, WAITPID usage impossible due to the
                         ;; race with the SIGCHLD signal handler.
                         (multiple-value-bind (status code core)
                             (waitpid (process-pid proc))
                           (when status
                             (wake-serve-event proc)
                             (setf (process-%status proc) status)
                             (setf (process-%exit-code proc) code)
                             (when (process-status-hook proc)
                               (push proc changed))
                             (when (member status '(:exited :signaled))
                               (setf (process-core-dumped proc) core)
                               t))))
                       #+win32
                       (lambda (proc)
                         (let ((handle (process-handle proc)))
                           (when handle
                             (multiple-value-bind (ok code)
                                 (sb-win32::get-exit-code-process handle)
                               (when (and (plusp ok)
                                          (/= code sb-win32::still-active))
                                 (setf (process-%status proc) :exited
                                       (process-%exit-code proc) code)
                                 (sb-win32::close-handle handle)
                                 (setf (process-handle proc) nil)
                                 (when (process-status-hook proc)
                                   (push proc changed))
                                 t)))))
                       *active-processes*)))
    ;; Can't call the hooks before all the processes have been deal
    ;; with, as calling a hook may cause re-entry to
    ;; GET-PROCESSES-STATUS-CHANGES. That may be OK when using waitpid,
    ;; but in the Windows implementation it would be deeply bad.
    (dolist (proc changed)
      (let ((hook (process-status-hook proc)))
        (funcall hook proc)))))

;;;; RUN-PROGRAM and close friends

;;; list of file descriptors to close when RUN-PROGRAM exits due to an error
(defvar *close-on-error* nil)

;;; list of file descriptors to close when RUN-PROGRAM returns in the parent
(defvar *close-in-parent* nil)

;;; list of handlers installed by RUN-PROGRAM.
(defvar *handlers-installed* nil)

;;; Find an unused pty. Return three values: the file descriptor for
;;; the master side of the pty, the file descriptor for the slave side
;;; of the pty, and the name of the tty device for the slave side.
#-(or win32 openbsd freebsd dragonfly)
(progn
  (define-alien-routine ptsname c-string (fd int))
  (define-alien-routine grantpt boolean (fd int))
  (define-alien-routine unlockpt boolean (fd int))

  (defun find-a-pty ()
    ;; First try to use the Unix98 pty api.
    (let ((master-fd (sb-unix:unix-open "/dev/ptmx"
                                        (logior sb-unix:o_rdwr
                                                sb-unix:o_noctty)
                                        #o666)))
      (when master-fd
        (grantpt master-fd)
        (unlockpt master-fd)
        (let* ((slave-name (ptsname master-fd))
               (slave-fd (sb-unix:unix-open slave-name
                                            (logior sb-unix:o_rdwr
                                                    sb-unix:o_noctty)
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
        (let* ((master-name (with-output-to-string (str nil :element-type 'base-char)
                              (format str "/dev/pty~C~X" char digit)))
               (master-fd (sb-unix:unix-open master-name
                                             (logior sb-unix:o_rdwr
                                                     sb-unix:o_noctty)
                                             #o666)))
          (when master-fd
            (let* ((slave-name (with-output-to-string (str nil :element-type 'base-char)
                                 (format str "/dev/tty~C~X" char digit)))
                   (slave-fd (sb-unix:unix-open slave-name
                                                (logior sb-unix:o_rdwr
                                                        sb-unix:o_noctty)
                                                #o666)))
              (when slave-fd
                (return-from find-a-pty
                  (values master-fd
                          slave-fd
                          slave-name)))
              (sb-unix:unix-close master-fd))))))
    (error "could not find a pty")))

#+(or openbsd freebsd dragonfly)
(progn
  (define-alien-routine openpty int (amaster int :out) (aslave int :out)
                        (name (* char)) (termp (* t)) (winp (* t)))
  (defun find-a-pty ()
    (with-alien ((name-buf (array char #.path-max)))
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
    ;; ptsname is not thread-safe, ptsname_r is.
    (with-active-processes-lock ()
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
                (make-fd-stream master :input t :output t
                                       :external-format external-format
                                       :element-type :default
                                       :dual-channel-p t))))))

;; Null terminate strings only C-side: otherwise we can run into
;; A-T-S-L even for simple encodings like ASCII.  Multibyte encodings
;; may need more than a single byte of zeros; assume 4 byte is enough
;; for everyone.
#-win32
(defmacro round-null-terminated-bytes-to-words (n)
  `(logandc2 (the sb-vm:signed-word (+ (the fixnum ,n)
                                       4 (1- sb-vm:n-machine-word-bytes)))
             (1- sb-vm:n-machine-word-bytes)))

#-win32
(defun string-list-to-c-strvec (string-list)
  (let* (;; We need an extra for the null, and an extra 'cause exect
         ;; clobbers argv[-1].
         (vec-bytes (* sb-vm:n-machine-word-bytes (+ (length string-list) 2)))
         (octet-vector-list (mapcar (lambda (s)
                                      (string-to-octets s))
                                    string-list))
         (string-bytes (reduce #'+ octet-vector-list
                               :key (lambda (s)
                                      (round-null-terminated-bytes-to-words
                                       (length s)))))
         (total-bytes (+ string-bytes vec-bytes))
         ;; Memory to hold the vector of pointers and all the strings.
         (vec-sap (allocate-system-memory total-bytes))
         (string-sap (sap+ vec-sap vec-bytes))
         ;; Index starts from [1]!
         (vec-index-offset sb-vm:n-machine-word-bytes))
    (declare (sb-vm:signed-word vec-bytes)
             (sb-vm:word string-bytes total-bytes)
             (system-area-pointer vec-sap string-sap))
    (dolist (octets octet-vector-list)
      (declare (type (simple-array (unsigned-byte 8) (*)) octets))
      (let ((size (length octets)))
        ;; Copy string.
        (copy-ub8-to-system-area octets 0 string-sap 0 size)
        ;; NULL-terminate it
        ;; (As it says up top: "assume 4 byte is enough for everyone.")
        (let ((sap (sap+ string-sap size)))
          (setf (sap-ref-8 sap 0) 0 (sap-ref-8 sap 1) 0
                (sap-ref-8 sap 2) 0 (sap-ref-8 sap 3) 0))
        ;; Put the pointer in the vector.
        (setf (sap-ref-sap vec-sap vec-index-offset) string-sap)
        ;; Advance string-sap for the next string.
        (setf string-sap (sap+ string-sap
                               (round-null-terminated-bytes-to-words size)))
        (incf vec-index-offset sb-vm:n-machine-word-bytes)))
    ;; Final null pointer.
    (setf (sap-ref-sap vec-sap vec-index-offset) (int-sap 0))
    (values vec-sap (sap+ vec-sap sb-vm:n-machine-word-bytes) total-bytes)))

#-win32
(defmacro with-args ((var str-list) &body body)
  (with-unique-names (sap size)
    `(multiple-value-bind (,sap ,var ,size)
         (string-list-to-c-strvec ,str-list)
       (unwind-protect
            (progn
              ,@body)
         (deallocate-system-memory ,sap ,size)))))

(defmacro with-environment ((var str-list &key null) &body body)
  (once-only ((null null))
    (with-unique-names (sap size)
      `(multiple-value-bind (,sap ,var ,size)
           (if ,null
               (values nil (int-sap 0))
               #-win32 (string-list-to-c-strvec ,str-list)
               #+win32 (encode-windows-environment ,str-list))
         (unwind-protect
              (progn
                ,@body)
           (unless ,null
             (deallocate-system-memory ,sap ,size)))))))
#-win32
(define-alien-routine spawn
     int
  (program c-string)
  (argv (* c-string))
  (stdin int)
  (stdout int)
  (stderr int)
  (search int)
  (envp (* c-string))
  (pty-name c-string)
  (channel (array int 2))
  (dir c-string)
  (preserve-fds (* int)))

#-win32
(define-alien-routine wait-for-exec
  int
  (pid int)
  (channel (array int 2)))

#+win32
(defun escape-arg (arg stream)
  ;; Normally, #\\ doesn't have to be escaped
  ;; But if #\" follows #\\, then they have to be escaped.
  ;; Do that by counting the number of consequent backslashes, and
  ;; upon encoutering #\" immediately after them, output the same
  ;; number of backslashes, plus one for #\"
  (write-char #\" stream)
  (loop with slashes = 0
        for i below (length arg)
        for previous-char = #\a then char
        for char = (char arg i)
        do
        (case char
          (#\"
           (loop repeat slashes
                 do (write-char #\\ stream))
           (write-string "\\\"" stream))
          (t
           (write-char char stream)))
        (case char
          (#\\
           (incf slashes))
          (t
           (setf slashes 0)))
        finally
        ;; The final #\" counts too, but doesn't need to be escaped itself
        (loop repeat slashes
              do (write-char #\\ stream)))
  (write-char #\" stream))

#+win32
(defun prepare-args (args escape)
  (%with-output-to-string (str)
    (loop for (arg . rest) on args
          do
          (cond ((and escape
                      (find-if (lambda (c) (find c '(#\Space #\Tab #\")))
                               arg))
                 (escape-arg arg str))
                (t
                 (write-string arg str)))
          (when rest
            (write-char #\Space str)))))

#-win32
(defun prepare-args (args)
  (if (every #'simple-string-p args)
      args
      (loop for arg in args
            collect (coerce arg 'simple-string))))

(defmacro coerce-or-copy (object type)
  `(if (typep ,object ,type)
       (copy-seq ,object)
       (coerce ,object ,type)))

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
                      (environment
                       (when env-p
                         (unix-environment-sbcl-from-cmucl env))
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
                      (external-format :default)
                      directory
                      preserve-fds
                      #+win32 (escape-arguments t)
                      #+win32 (window nil))
  "RUN-PROGRAM creates a new process specified by PROGRAM.
ARGS is a list of strings to be passed literally to the new program.
In POSIX environments, this list becomes the array supplied as the second
parameter to the execv() or execvp() system call, each list element becoming
one array element. The strings should not contain shell escaping, as there is
no shell involvement. Further note that while conventionally the process
receives its own pathname in argv[0], that is automatic, and the 0th string
should not be present in ARGS.

The program arguments and the environment are encoded using the
default external format for streams.

RUN-PROGRAM will return a PROCESS structure. See the CMU Common Lisp
Users Manual for details about the PROCESS structure.

   Notes about Unix environments (as in the :ENVIRONMENT and :ENV args):

   - The SBCL implementation of RUN-PROGRAM, like Perl and many other
     programs, but unlike the original CMU CL implementation, copies
     the Unix environment by default.
   - Running Unix programs from a setuid process, or in any other
     situation where the Unix environment is under the control of someone
     else, is a mother lode of security problems. If you are contemplating
     doing this, read about it first. (The Perl community has a lot of good
     documentation about this and other security issues in script-like
     programs.)

   The &KEY arguments have the following meanings:
   :ENVIRONMENT
      a list of STRINGs describing the new Unix environment
      (as in \"man environ\"). The default is to copy the environment of
      the current process.
   :ENV
      an alternative lossy representation of the new Unix environment,
      for compatibility with CMU CL
   :SEARCH
      Look for PROGRAM in each of the directories in the child's $PATH
      environment variable.  Otherwise an absolute pathname is required.
   :WAIT
      If non-NIL (default), wait until the created process finishes.  If
      NIL, continue running Lisp until the program finishes.
   :PTY (not supported on win32)
      Either T, NIL, or a stream.  Unless NIL, the subprocess is established
      under a PTY.  If :pty is a stream, all output to this pty is sent to
      this stream, otherwise the PROCESS-PTY slot is filled in with a stream
      connected to pty that can read output and write input.
   :INPUT
      Either T, NIL, a pathname, a stream, or :STREAM.
      T: the standard input for the current process is inherited.
      NIL: /dev/null (nul on win32) is used.
      pathname: the specified file is used.
      stream: all the input is read from that stream and sent to the
      subprocess.
      :STREAM: the PROCESS-INPUT slot is filled in with a stream that sends
      its output to the process.
      Defaults to NIL.
   :IF-INPUT-DOES-NOT-EXIST (when :INPUT is the name of a file)
      can be one of:
         :ERROR to generate an error
         :CREATE to create an empty file
         NIL (the default) to return NIL from RUN-PROGRAM
   :OUTPUT
      Either T, NIL, a pathname, a stream, or :STREAM.
      T: the standard output for the current process is inherited.
      NIL: /dev/null (nul on win32) is used.
      pathname: the specified file is used.
      stream: all the output from the process is written to this stream.
      :STREAM: the PROCESS-OUTPUT slot is filled in with a stream that can be
      read to get the output.
      Defaults to NIL.
   :ERROR
      Same as :OUTPUT, additionally accepts :OUTPUT, making all error
      output routed to the same place as normal output.
      Defaults to :OUTPUT.
   :IF-OUTPUT-EXISTS (when :OUTPUT is the name of a file)
      can be one of:
         :ERROR (the default) to generate an error
         :SUPERSEDE to supersede the file with output from the program
         :APPEND to append output from the program to the file
         NIL to return NIL from RUN-PROGRAM, without doing anything
   :IF-ERROR-EXISTS
      Same as :IF-OUTPUT-EXISTS, controlling :ERROR output to files.
      Ignored when :ERROR :OUTPUT.
      Defaults to :ERROR.
   :STATUS-HOOK
      This is a function the system calls whenever the status of the
      process changes.  The function takes the process as an argument.
   :EXTERNAL-FORMAT
      The external-format to use for :INPUT, :OUTPUT, and :ERROR :STREAMs.
   :DIRECTORY
      Specifies the directory in which the program should be run.
      NIL (the default) means the directory is unchanged.

   :PRESERVE-FDS
      A sequence of file descriptors which should remain open in the child
      process.

   Windows specific options:
   :ESCAPE-ARGUMENTS (default T)
      Controls escaping of the arguments passed to CreateProcess.
   :WINDOW (default NIL)
      When NIL, the subprocess decides how it will display its window. The
      following options control how the subprocess window should be displayed:
      :HIDE, :SHOW-NORMAL, :SHOW-MAXIMIZED, :SHOW-MINIMIZED, :SHOW-NO-ACTIVATE,
      :SHOW-MIN-NO-ACTIVE, :SHOW-NA.
      Note: console application subprocesses may or may not display a console
      window depending on whether the SBCL runtime is itself a console or GUI
      application. Invoke CMD /C START to consistently display a console window
      or use the :WINDOW :HIDE option to consistently hide the console window."
  (when (and env-p environment-p)
    (error "can't specify :ENV and :ENVIRONMENT simultaneously"))
  (let* (;; Clear various specials used by GET-DESCRIPTOR-FOR to
         ;; communicate cleanup info.
         *close-on-error*
         *close-in-parent*
         *handlers-installed*
         ;; Establish PROC at this level so that we can return it.
         proc
         (progname (native-namestring program))
         (args (prepare-args (cons progname args) #+win32 escape-arguments))
         (directory (and directory (native-namestring directory)))
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
                               `(get-descriptor-for ,which ,@args))
                              (:error
                               `(if (eq ,(first args) :output)
                                    ;; kludge: we expand into
                                    ;; hard-coded symbols here.
                                    (values stdout output-stream)
                                    (get-descriptor-for ,which ,@args))))
                         (unless ,fd
                           (return-from run-program))
                         ,@body))
                    (with-open-pty (((pty-name pty-stream) (pty cookie))
                                    &body body)
                      (declare (ignorable pty-name pty-stream pty cookie))
                      #+win32
                      `(progn ,@body)
                      #-win32
                      `(multiple-value-bind (,pty-name ,pty-stream)
                           (open-pty ,pty ,cookie :external-format external-format)
                         ,@body)))
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
                                      :if-does-not-exist :create
                                      :external-format external-format)
               (with-fd-and-stream-for ((stderr error-stream)  :error
                                        error cookie
                                        :direction :output
                                        :if-exists if-error-exists
                                        :if-does-not-exist :create
                                        :external-format external-format)
                 ;; Make sure we are not notified about the child
                 ;; death before we have installed the PROCESS
                 ;; structure in *ACTIVE-PROCESSES*.
                 (let (child #+win32 handle)
                   (with-environment (environment-vec environment
                                      :null (not (or environment environment-p)))
                     (with-alien (#-win32 (channel (array int 2)))
                       (with-open-pty ((pty-name pty-stream) (pty cookie))
                         (setf (values child #+win32 handle)
                               #+win32
                               (with-system-mutex (*spawn-lock*)
                                 (sb-win32::mswin-spawn
                                  progname
                                  args
                                  stdin stdout stderr
                                  search environment-vec directory
                                  window
                                  preserve-fds))
                               #-win32
                               (let ((preserve-fds
                                       (and preserve-fds
                                            (sort (coerce-or-copy preserve-fds
                                                                  '(simple-array (signed-byte #.(alien-size int)) (*)))
                                                  #'<))))
                                 (with-pinned-objects (preserve-fds)
                                   (with-args (args-vec args)
                                     (with-system-mutex (*spawn-lock*)
                                       (spawn progname args-vec
                                              stdin stdout stderr
                                              (if search 1 0)
                                              environment-vec pty-name
                                              channel
                                              directory
                                              (if preserve-fds
                                                  (vector-sap preserve-fds)
                                                  (int-sap 0))))))))
                         (unless (minusp child)
                           #-win32
                           (setf child (wait-for-exec child channel))
                           (unless (minusp child)
                             (setf proc
                                   (make-process
                                    :input input-stream
                                    :output output-stream
                                    :error error-stream
                                    :status-hook status-hook
                                    :cookie cookie
                                    #-win32 :pty #-win32 pty-stream
                                    :%status :running
                                    :pid child
                                    #+win32 :copiers #+win32 *handlers-installed*
                                    #+win32 :handle #+win32 handle))
                             (with-active-processes-lock ()
                               (push proc *active-processes*))
                             ;; In case a sigchld signal was missed
                             ;; when the process wasn't on
                             ;; *active-processes*
                             (unless wait
                               (get-processes-status-changes)))))))
                   ;; Report the error outside the lock.
                   (case child
                     (-1
                      (error "Couldn't fork child process: ~A"
                             (strerror)))
                     (-2
                      (error "Couldn't execute ~S: ~A"
                             progname (strerror)))
                     (-3
                      (error "Couldn't change directory to ~S: ~A"
                             directory (strerror)))))))))
      (dolist (fd *close-in-parent*)
        (sb-unix:unix-close fd))
      (unless proc
        (dolist (fd *close-on-error*)
          (sb-unix:unix-close fd))
        #-win32
        (dolist (handler *handlers-installed*)
          (remove-fd-handler handler)))
      (when (and wait proc)
        (unwind-protect
             (process-wait proc)
          #-win32
          (dolist (handler *handlers-installed*)
            (remove-fd-handler handler)))))
    proc))

;;; Install a handler for any input that shows up on the file
;;; descriptor. The handler reads the data and writes it to the
;;; stream.
#-win32
(defun copy-descriptor-to-stream (descriptor stream cookie external-format)
  (incf (car cookie))
  (let* ((handler nil)
         (buf (make-array 256 :element-type '(unsigned-byte 8)))
         (read-end 0)
         (et (stream-element-type stream))
         (copy-fun
          (cond
            ((member et '(character base-char))
             (lambda ()
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
                   (decf read-end decode-end)))))
            (t
             (lambda ()
               (handler-bind
                   ((type-error
                      (lambda (c)
                        (error 'simple-type-error
                                         :format-control
                                         "Error using ~s for program output:~@
                                          ~a"
                                         :format-arguments
                                         (list stream c)
                                         :expected-type
                                         (type-error-expected-type c)
                                         :datum
                                         (type-error-datum c)))))
                 (write-sequence buf stream :end read-end))
               (setf read-end 0))))))
    (setf handler
          (add-fd-handler
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
                    ((or (and (null count)
                              (eql errno sb-unix:eio))
                         (eql count 0))
                     (remove-fd-handler handler)
                     (setf handler nil)
                     (decf (car cookie))
                     (sb-unix:unix-close descriptor)
                     (unless (zerop read-end)
                       ;; Should this be an END-OF-FILE?
                       (error "~@<non-empty buffer when EOF reached ~
                               while reading from child: ~S~:>" buf))
                     (return))
                    ((null count)
                     (remove-fd-handler handler)
                     (setf handler nil)
                     (decf (car cookie))
                     (error
                      "~@<couldn't read input from sub-process: ~
                                     ~2I~_~A~:>"
                      (strerror errno)))
                    (t
                     (incf read-end count)
                     (funcall copy-fun))))))))
    (push handler *handlers-installed*)))

#+win32
(defun copy-descriptor-to-stream (descriptor stream cookie external-format)
  (declare (ignore cookie))
  (push (sb-win32::make-io-copier :pipe descriptor
                                  :stream stream
                                  :external-format external-format)
        *handlers-installed*))

;;; FIXME: something very like this is done in SB-POSIX to treat
;;; streams as file descriptor designators; maybe we can combine these
;;; two?  Additionally, as we have a couple of user-defined streams
;;; libraries, maybe we should have a generic function for doing this,
;;; so user-defined streams can play nicely with RUN-PROGRAM (and
;;; maybe also with SB-POSIX)?
(defun get-stream-fd-and-external-format (stream direction)
  (typecase stream
    (fd-stream
     (values (fd-stream-fd stream) nil (stream-external-format stream)))
    (synonym-stream
     (get-stream-fd-and-external-format
      (resolve-synonym-stream stream) direction))
    (two-way-stream
     (ecase direction
       (:input
        (get-stream-fd-and-external-format
         (two-way-stream-input-stream stream) direction))
       (:output
        (get-stream-fd-and-external-format
         (two-way-stream-output-stream stream) direction))))))

(defun get-temporary-directory ()
  #-win32 (or (sb-ext:posix-getenv "TMPDIR")
              "/tmp")
  #+win32 (or (sb-ext:posix-getenv "TEMP")
              "C:/Temp"))


;;; Find a file descriptor to use for object given the direction.
;;; Returns the descriptor. If object is :STREAM, returns the created
;;; stream as the second value.
(defun get-descriptor-for (argument object cookie
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
  (labels ((fail (format &rest arguments)
             (error "~s error processing ~s argument:~% ~?" 'run-program argument format arguments))
           (make-temp-fd ()
             (multiple-value-bind (fd name/errno)
                 (sb-unix:sb-mkstemp (format nil "~a/.run-program-XXXXXX"
                                             (get-temporary-directory))
                                     #o0600)
               (unless fd
                 (fail "could not open a temporary file: ~A"
                       (strerror name/errno)))
               #+win32
               (setf (sb-win32::inheritable-handle-p fd) t)
               ;; Can't unlink an open file on Windows
               #-win32
               (unless (sb-unix:unix-unlink name/errno)
                 (sb-unix:unix-close fd)
                 (fail "failed to unlink ~A" name/errno))
               fd)))
    (let ((dev-null #-win32 "/dev/null" #+win32 "nul"))
      (cond ((eq object t)
             ;; No new descriptor is needed.
             (values -1 nil))
            ((or (eq object nil)
                 (and (typep object 'broadcast-stream)
                      (not (broadcast-stream-streams object))))
             ;; Use /dev/null.
             (multiple-value-bind
                   (fd errno)
                 (sb-unix:unix-open dev-null
                                    (case direction
                                      (:input sb-unix:o_rdonly)
                                      (:output sb-unix:o_wronly)
                                      (t sb-unix:o_rdwr))
                                    #o666
                                    #+win32 :overlapped #+win32 nil)
               (unless fd
                 (fail "~@<couldn't open ~S: ~2I~_~A~:>"
                       dev-null (strerror errno)))
               #+win32
               (setf (sb-win32::inheritable-handle-p fd) t)
               (push fd *close-in-parent*)
               (values fd nil)))
            ((eq object :stream)
             (multiple-value-bind (read-fd write-fd) (sb-unix:unix-pipe)
               (unless read-fd
                 (fail "couldn't create pipe: ~A" (strerror write-fd)))
               #+win32
               (setf (sb-win32::inheritable-handle-p read-fd)
                     (eq direction :input)
                     (sb-win32::inheritable-handle-p write-fd)
                     (eq direction :output))
               (case direction
                 (:input
                    (push read-fd *close-in-parent*)
                    (push write-fd *close-on-error*)
                    (let ((stream (make-fd-stream write-fd :output t
                                                         :element-type :default
                                                         :external-format
                                                         external-format)))
                      (values read-fd stream)))
                 (:output
                    (push read-fd *close-on-error*)
                    (push write-fd *close-in-parent*)
                    (let ((stream (make-fd-stream read-fd :input t
                                                         :element-type :default
                                                         :external-format
                                                         external-format)))
                      (values write-fd stream)))
                 (t
                    (sb-unix:unix-close read-fd)
                    (sb-unix:unix-close write-fd)
                    (fail "Direction must be either :INPUT or :OUTPUT, not ~S."
                           direction)))))
            ((or (pathnamep object) (stringp object))
             ;; GET-DESCRIPTOR-FOR uses &allow-other-keys, so rather
             ;; than munge the &rest list for OPEN, just disable keyword
             ;; validation there.
             (with-open-stream (file (or (apply #'open object
                                                :allow-other-keys t
                                                #+win32 :overlapped #+win32 nil
                                                keys)
                                         ;; :if-input-does-not-exist nil
                                         ;; can result in this
                                         (return-from get-descriptor-for)))
               (multiple-value-bind
                     (fd errno)
                   (sb-unix:unix-dup (fd-stream-fd file))
                 (cond (fd
                        (push fd *close-in-parent*)
                        (values fd nil))
                       (t
                        (fail "couldn't duplicate file descriptor: ~A"
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
                      (et (stream-element-type object)))
                  (cond ((member et '(character base-char))
                         (loop
                           (multiple-value-bind
                                 (line no-cr)
                               (read-line object nil nil)
                             (unless line
                               (return))
                             (let ((vector (string-to-octets
                                            line
                                            :external-format external-format)))
                               (sb-unix:unix-write
                                fd vector 0 (length vector)))
                             (if no-cr
                               (return)
                               (sb-unix:unix-write
                                fd #.(string #\Newline) 0 1)))))
                        (t
                         (handler-bind
                             ((type-error
                                (lambda (c)
                                  (error 'simple-type-error
                                         :format-control
                                         "Error using ~s for program input:~@
                                          ~a"
                                         :format-arguments
                                         (list object c)
                                         :expected-type
                                         (type-error-expected-type c)
                                         :datum
                                         (type-error-datum c)))))
                           (loop with buf = (make-array 256 :element-type '(unsigned-byte 8))
                                 for p = (read-sequence buf object)
                                 until (zerop p)
                                 do (sb-unix:unix-write fd buf 0 p)))))
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
                    #-win32 (sb-unix:unix-pipe)
                    #+win32 (sb-win32::make-named-pipe)
                  (unless read-fd
                    (fail "couldn't create pipe: ~S" (strerror write-fd)))
                  #+win32
                  (setf (sb-win32::inheritable-handle-p write-fd) t)
                  (copy-descriptor-to-stream read-fd object cookie
                                             external-format)
                  (push read-fd *close-on-error*)
                  (push write-fd *close-in-parent*)
                  (return (values write-fd nil)))))))
          (t
           (fail "invalid option: ~S" object))))))

#+(or linux sunos haiku)
(defun software-version ()
  "Return a string describing version of the supporting software, or NIL
  if not available."
  (or sb-sys::*software-version*
      (setf sb-sys::*software-version*
            (possibly-base-stringize
             #+linux
             (with-open-file (f "/proc/sys/kernel/osrelease") (read-line f))
             #-linux
             (string-trim '(#\newline)
                          (%with-output-to-string (stream)
                           (run-program "/bin/uname"
                                        ;; "-r" on haiku just prints "1"
                                        ;; but "-v" prints some detail.
                                        #+haiku '("-v")
                                        #-haiku '("-r")
                                        :output stream)))))))
