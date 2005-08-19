;;;; support for threads in the target machine

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

;;; set the doc here because in early-thread FDOCUMENTATION is not
;;; available, yet
#!+sb-doc
(setf (sb!kernel:fdocumentation '*current-thread* 'variable)
      "Bound in each thread to the thread itself.")

(defstruct (thread (:constructor %make-thread))
  #!+sb-doc
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  name
  %sap)

#!+sb-doc
(setf (sb!kernel:fdocumentation 'thread-name 'function)
      "The name of the thread. Setfable.")

(def!method print-object ((thread thread) stream)
  (if (thread-name thread)
      (print-unreadable-object (thread stream :type t :identity t)
        (prin1 (thread-name thread) stream))
      (print-unreadable-object (thread stream :type t :identity t)
        ;; body is empty => there is only one space between type and
        ;; identity
        ))
  thread)

(defun thread-state (thread)
  (let ((state
         (sb!sys:sap-int
          (sb!sys:sap-ref-sap (thread-%sap thread)
                              (* sb!vm::thread-state-slot
                                 sb!vm::n-word-bytes)))))
    (ecase state
      (#.(sb!vm:fixnumize 0) :starting)
      (#.(sb!vm:fixnumize 1) :running)
      (#.(sb!vm:fixnumize 2) :suspended)
      (#.(sb!vm:fixnumize 3) :dead))))

(defun thread-alive-p (thread)
  #!+sb-doc
  "Check if THREAD is running."
  (not (eq :dead (thread-state thread))))

;; A thread is eligible for gc iff it has finished and there are no
;; more references to it. This list is supposed to keep a reference to
;; all running threads.
(defvar *all-threads* ())
(defvar *all-threads-lock* (make-mutex :name "all threads lock"))

(defun list-all-threads ()
  #!+sb-doc
  "Return a list of the live threads."
  (with-mutex (*all-threads-lock*)
    (copy-list *all-threads*)))

(declaim (inline current-thread-sap))
(defun current-thread-sap ()
  (sb!vm::current-thread-offset-sap sb!vm::thread-this-slot))

(declaim (inline current-thread-sap-id))
(defun current-thread-sap-id ()
  (sb!sys:sap-int
   (sb!vm::current-thread-offset-sap sb!vm::thread-os-thread-slot)))

(defun init-initial-thread ()
  (let ((initial-thread (%make-thread :name "initial thread"
                                      :%sap (current-thread-sap))))
    (setq *current-thread* initial-thread)
    ;; Either *all-threads* is empty or it contains exactly one thread
    ;; in case we are in reinit since saving core with multiple
    ;; threads doesn't work.
    (setq *all-threads* (list initial-thread))))

;;;;

#!+sb-thread
(progn
  (define-alien-routine ("create_thread" %create-thread)
      system-area-pointer
    (lisp-fun-address unsigned-long))

  (define-alien-routine "block_deferrable_signals_and_inhibit_gc"
    void)

  (define-alien-routine reap-dead-thread void
    (thread-sap system-area-pointer))

  (declaim (inline futex-wait futex-wake))

  (sb!alien:define-alien-routine "futex_wait"
      int (word unsigned-long) (old-value unsigned-long))

  (sb!alien:define-alien-routine "futex_wake"
      int (word unsigned-long) (n unsigned-long)))

;;; used by debug-int.lisp to access interrupt contexts
#!-(and sb-fluid sb-thread) (declaim (inline sb!vm::current-thread-offset-sap))
#!-sb-thread
(defun sb!vm::current-thread-offset-sap (n)
  (declare (type (unsigned-byte 27) n))
  (sb!sys:sap-ref-sap (alien-sap (extern-alien "all_threads" (* t)))
               (* n sb!vm:n-word-bytes)))

;;;; spinlocks

(defstruct spinlock
  #!+sb-doc
  "Spinlock type."
  (name nil :type (or null simple-string))
  (value 0))

(declaim (inline get-spinlock release-spinlock))

;;; The bare 2 here and below are offsets of the slots in the struct.
;;; There ought to be some better way to get these numbers
(defun get-spinlock (spinlock new-value)
  (declare (optimize (speed 3) (safety 0))
           #!-sb-thread
           (ignore spinlock new-value))
  ;; %instance-set-conditional can test for 0 (which is a fixnum) and
  ;; store any value
  #!+sb-thread
  (loop until
        (eql (sb!vm::%instance-set-conditional spinlock 2 0 new-value) 0)))

(defun release-spinlock (spinlock)
  (declare (optimize (speed 3) (safety 0))
           #!-sb-thread (ignore spinlock))
  ;; %instance-set-conditional cannot compare arbitrary objects
  ;; meaningfully, so
  ;; (sb!vm::%instance-set-conditional spinlock 2 our-value 0)
  ;; does not work for bignum thread ids.
  #!+sb-thread
  (sb!vm::%instance-set spinlock 2 0))

(defmacro with-spinlock ((spinlock) &body body)
  (sb!int:with-unique-names (lock)
    `(let ((,lock ,spinlock))
      (get-spinlock ,lock *current-thread*)
      (unwind-protect
           (progn ,@body)
        (release-spinlock ,lock)))))

;;;; mutexes

(defstruct mutex
  #!+sb-doc
  "Mutex type."
  (name nil :type (or null simple-string))
  (value nil))

#!+sb-doc
(setf (sb!kernel:fdocumentation 'make-mutex 'function)
      "Create a mutex."
      (sb!kernel:fdocumentation 'mutex-name 'function)
      "The name of the mutex. Setfable."
      (sb!kernel:fdocumentation 'mutex-value 'function)
      "The value of the mutex. NIL if the mutex is free. Setfable.")

#!+sb-thread
(declaim (inline mutex-value-address))
#!+sb-thread
(defun mutex-value-address (mutex)
  (declare (optimize (speed 3)))
  (sb!ext:truly-the
   sb!vm:word
   (+ (sb!kernel:get-lisp-obj-address mutex)
      (- (* 3 sb!vm:n-word-bytes) sb!vm:instance-pointer-lowtag))))

(defun get-mutex (mutex &optional new-value (wait-p t))
  #!+sb-doc
  "Acquire MUTEX, setting it to NEW-VALUE or some suitable default
value if NIL.  If WAIT-P is non-NIL and the mutex is in use, sleep
until it is available"
  (declare (type mutex mutex) (optimize (speed 3)))
  (unless new-value (setf new-value *current-thread*))
  #!-sb-thread
  (let ((old-value (mutex-value mutex)))
    (when (and old-value wait-p)
      (error "In unithread mode, mutex ~S was requested with WAIT-P ~S and ~
              new-value ~S, but has already been acquired (with value ~S)."
             mutex wait-p new-value old-value))
    (setf (mutex-value mutex) new-value)
    t)
  #!+sb-thread
  (let (old)
    (when (eql new-value (mutex-value mutex))
      (warn "recursive lock attempt ~S~%" mutex)
      (format *debug-io* "Thread: ~A~%" *current-thread*)
      (sb!debug:backtrace most-positive-fixnum *debug-io*)
      (force-output *debug-io*))
    (loop
     (unless
         (setf old (sb!vm::%instance-set-conditional mutex 2 nil new-value))
       (return t))
     (unless wait-p (return nil))
     (futex-wait (mutex-value-address mutex)
                 (sb!kernel:get-lisp-obj-address old)))))

(defun release-mutex (mutex)
  #!+sb-doc
  "Release MUTEX by setting it to NIL. Wake up threads waiting for
this mutex."
  (declare (type mutex mutex))
  (setf (mutex-value mutex) nil)
  #!+sb-thread
  (futex-wake (mutex-value-address mutex) 1))

;;;; waitqueues/condition variables

(defstruct (waitqueue (:constructor %make-waitqueue))
  #!+sb-doc
  "Waitqueue type."
  (name nil :type (or null simple-string))
  (data nil))

(defun make-waitqueue (&key name)
  #!+sb-doc
  "Create a waitqueue."
  (%make-waitqueue :name name))

#!+sb-doc
(setf (sb!kernel:fdocumentation 'waitqueue-name 'function)
      "The name of the waitqueue. Setfable.")

#!+sb-thread
(declaim (inline waitqueue-data-address))
#!+sb-thread
(defun waitqueue-data-address (waitqueue)
  (declare (optimize (speed 3)))
  (sb!ext:truly-the
   sb!vm:word
   (+ (sb!kernel:get-lisp-obj-address waitqueue)
      (- (* 3 sb!vm:n-word-bytes) sb!vm:instance-pointer-lowtag))))

(defun condition-wait (queue mutex)
  #!+sb-doc
  "Atomically release MUTEX and enqueue ourselves on QUEUE.  Another
thread may subsequently notify us using CONDITION-NOTIFY, at which
time we reacquire MUTEX and return to the caller."
  #!-sb-thread (declare (ignore queue))
  (assert mutex)
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let ((value (mutex-value mutex)))
    (unwind-protect
         (let ((me *current-thread*))
           ;; XXX we should do something to ensure that the result of this setf
           ;; is visible to all CPUs
           (setf (waitqueue-data queue) me)
           (release-mutex mutex)
           ;; Now we go to sleep using futex-wait.  If anyone else
           ;; manages to grab MUTEX and call CONDITION-NOTIFY during
           ;; this comment, it will change queue->data, and so
           ;; futex-wait returns immediately instead of sleeping.
           ;; Ergo, no lost wakeup
           (futex-wait (waitqueue-data-address queue)
                       (sb!kernel:get-lisp-obj-address me)))
      ;; If we are interrupted while waiting, we should do these things
      ;; before returning.  Ideally, in the case of an unhandled signal,
      ;; we should do them before entering the debugger, but this is
      ;; better than nothing.
      (get-mutex mutex value))))

(defun condition-notify (queue)
  #!+sb-doc
  "Notify one of the threads waiting on QUEUE."
  #!-sb-thread (declare (ignore queue))
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let ((me *current-thread*))
    ;; no problem if >1 thread notifies during the comment in
    ;; condition-wait: as long as the value in queue-data isn't the
    ;; waiting thread's id, it matters not what it is
    ;; XXX we should do something to ensure that the result of this setf
    ;; is visible to all CPUs
    (setf (waitqueue-data queue) me)
    (futex-wake (waitqueue-data-address queue) 1)))

(defun condition-broadcast (queue)
  #!+sb-doc
  "Notify all threads waiting on QUEUE."
  #!-sb-thread (declare (ignore queue))
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let ((me *current-thread*))
    (setf (waitqueue-data queue) me)
    (futex-wake (waitqueue-data-address queue) (ash 1 30))))

;;;; job control, independent listeners

(defstruct session
  (lock (make-mutex :name "session lock"))
  (threads nil)
  (interactive-threads nil)
  (interactive-threads-queue (make-waitqueue)))

(defvar *session* nil)

;;; the debugger itself tries to acquire the session lock, don't let
;;; funny situations (like getting a sigint while holding the session
;;; lock) occur
(defmacro with-session-lock ((session) &body body)
  #!-sb-thread (declare (ignore session))
  #!-sb-thread
  `(locally ,@body)
  #!+sb-thread
  `(sb!sys:without-interrupts
    (with-mutex ((session-lock ,session))
      ,@body)))

(defun new-session ()
  (make-session :threads (list *current-thread*)
                :interactive-threads (list *current-thread*)))

(defun init-job-control ()
  (setf *session* (new-session)))

(defun %delete-thread-from-session (thread session)
  (with-session-lock (session)
    (setf (session-threads session)
          (delete thread (session-threads session))
          (session-interactive-threads session)
          (delete thread (session-interactive-threads session)))))

(defun call-with-new-session (fn)
  (%delete-thread-from-session *current-thread* *session*)
  (let ((*session* (new-session)))
    (funcall fn)))

(defmacro with-new-session (args &body forms)
  (declare (ignore args))               ;for extensibility
  (sb!int:with-unique-names (fb-name)
    `(labels ((,fb-name () ,@forms))
      (call-with-new-session (function ,fb-name)))))

;;; Remove thread from its session, if it has one.
#!+sb-thread
(defun handle-thread-exit (thread)
  (with-mutex (*all-threads-lock*)
    (setq *all-threads* (delete thread *all-threads*)))
  (when *session*
    (%delete-thread-from-session thread *session*)))

(defun terminate-session ()
  #!+sb-doc
  "Kill all threads in session except for this one.  Does nothing if current
thread is not the foreground thread."
  ;; FIXME: threads created in other threads may escape termination
  (let ((to-kill
         (with-session-lock (*session*)
           (and (eq *current-thread*
                    (car (session-interactive-threads *session*)))
                (session-threads *session*)))))
    ;; do the kill after dropping the mutex; unwind forms in dying
    ;; threads may want to do session things
    (dolist (thread to-kill)
      (unless (eq thread *current-thread*)
        ;; terminate the thread but don't be surprised if it has
        ;; exited in the meantime
        (handler-case (terminate-thread thread)
          (interrupt-thread-error ()))))))

;;; called from top of invoke-debugger
(defun debugger-wait-until-foreground-thread (stream)
  "Returns T if thread had been running in background, NIL if it was
interactive."
  (declare (ignore stream))
  #!-sb-thread nil
  #!+sb-thread
  (prog1
      (with-session-lock (*session*)
        (not (member *current-thread*
                     (session-interactive-threads *session*))))
    (get-foreground)))

(defun get-foreground ()
  #!-sb-thread t
  #!+sb-thread
  (let ((was-foreground t))
    (loop
     (with-session-lock (*session*)
       (let ((int-t (session-interactive-threads *session*)))
         (when (eq (car int-t) *current-thread*)
           (unless was-foreground
             (format *query-io* "Resuming thread ~A~%" *current-thread*))
           (return-from get-foreground t))
         (setf was-foreground nil)
         (unless (member *current-thread* int-t)
           (setf (cdr (last int-t))
                 (list *current-thread*)))
         (condition-wait
          (session-interactive-threads-queue *session*)
          (session-lock *session*)))))))

(defun release-foreground (&optional next)
  #!+sb-doc
  "Background this thread.  If NEXT is supplied, arrange for it to
have the foreground next."
  #!-sb-thread (declare (ignore next))
  #!-sb-thread nil
  #!+sb-thread
  (with-session-lock (*session*)
    (when (rest (session-interactive-threads *session*))
      (setf (session-interactive-threads *session*)
            (delete *current-thread* (session-interactive-threads *session*))))
    (when next
      (setf (session-interactive-threads *session*)
            (list* next
                   (delete next (session-interactive-threads *session*)))))
    (condition-broadcast (session-interactive-threads-queue *session*))))

(defun foreground-thread ()
  (car (session-interactive-threads *session*)))

(defun make-listener-thread (tty-name)
  (assert (probe-file tty-name))
  (let* ((in (sb!unix:unix-open (namestring tty-name) sb!unix:o_rdwr #o666))
         (out (sb!unix:unix-dup in))
         (err (sb!unix:unix-dup in)))
    (labels ((thread-repl ()
               (sb!unix::unix-setsid)
               (let* ((sb!impl::*stdin*
                       (sb!sys:make-fd-stream in :input t :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*stdout*
                       (sb!sys:make-fd-stream out :output t :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*stderr*
                       (sb!sys:make-fd-stream err :output t :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*tty*
                       (sb!sys:make-fd-stream err :input t :output t
                                              :buffering :line
                                              :dual-channel-p t))
                      (sb!impl::*descriptor-handlers* nil))
                 (with-new-session ()
                   (unwind-protect
                        (sb!impl::toplevel-repl nil)
                     (sb!int:flush-standard-output-streams))))))
      (make-thread #'thread-repl))))

;;;; the beef

(defun make-thread (function &key name)
  #!+sb-doc
  "Create a new thread of NAME that runs FUNCTION. When the function
returns the thread exits."
  #!-sb-thread (declare (ignore function name))
  #!-sb-thread (error "Not supported in unithread builds.")
  #!+sb-thread
  (let* ((thread (%make-thread :name name))
         (setup-p nil)
         (real-function (coerce function 'function))
         (thread-sap
          (%create-thread
           (sb!kernel:get-lisp-obj-address
            (lambda ()
              ;; FIXME: use semaphores?
              (loop until setup-p)
              ;; in time we'll move some of the binding presently done in C
              ;; here too
              (let ((*current-thread* thread)
                    (sb!kernel::*restart-clusters* nil)
                    (sb!kernel::*handler-clusters* nil)
                    (sb!kernel::*condition-restarts* nil)
                    (sb!impl::*descriptor-handlers* nil)) ; serve-event
                ;; can't use handling-end-of-the-world, because that flushes
                ;; output streams, and we don't necessarily have any (or we
                ;; could be sharing them)
                (unwind-protect
                     (catch 'sb!impl::toplevel-catcher
                       (catch 'sb!impl::%end-of-the-world
                         (with-simple-restart
                             (terminate-thread
                              (format nil "~~@<Terminate this thread (~A)~~@:>"
                                      *current-thread*))
                           ;; now that most things have a chance to work
                           ;; properly without messing up other threads, it's
                           ;; time to enable signals
                           (sb!unix::reset-signal-mask)
                           (unwind-protect
                                (funcall real-function)
                             ;; we're going down, can't handle
                             ;; interrupts sanely anymore
                             (block-deferrable-signals-and-inhibit-gc)))))
                  ;; and remove what can be the last reference to the
                  ;; thread object
                  (handle-thread-exit thread)
                  0))
              (values))))))
    (when (sb!sys:sap= thread-sap (sb!sys:int-sap 0))
      (error "Can't create a new thread"))
    (setf (thread-%sap thread) thread-sap)
    (with-mutex (*all-threads-lock*)
      (push thread *all-threads*))
    (with-session-lock (*session*)
      (push thread (session-threads *session*)))
    (setq setup-p t)
    (sb!impl::finalize thread (lambda () (reap-dead-thread thread-sap)))
    thread))

(defun destroy-thread (thread)
  #!+sb-doc
  "Deprecated. Same as TERMINATE-THREAD."
  (terminate-thread thread))

(define-condition interrupt-thread-error (error)
  ((thread :reader interrupt-thread-error-thread :initarg :thread)
   (errno :reader interrupt-thread-error-errno :initarg :errno))
  #!+sb-doc
  (:documentation "Interrupting thread failed.")
  (:report (lambda (c s)
             (format s "interrupt thread ~A failed (~A: ~A)"
                     (interrupt-thread-error-thread c)
                     (interrupt-thread-error-errno c)
                     (strerror (interrupt-thread-error-errno c))))))

#!+sb-doc
(setf (sb!kernel:fdocumentation 'interrupt-thread-error-thread 'function)
      "The thread that was not interrupted."
      (sb!kernel:fdocumentation 'interrupt-thread-error-errno 'function)
      "The reason why the interruption failed.")

(defun interrupt-thread (thread function)
  #!+sb-doc
  "Interrupt the live THREAD and make it run FUNCTION. A moderate
degree of care is expected for use of interrupt-thread, due to its
nature: if you interrupt a thread that was holding important locks
then do something that turns out to need those locks, you probably
won't like the effect."
  #!-sb-thread (declare (ignore thread))
  ;; not quite perfect, because it does not take WITHOUT-INTERRUPTS
  ;; into account
  #!-sb-thread
  (funcall function)
  #!+sb-thread
  (if (eq thread *current-thread*)
      (funcall function)
      (let ((function (coerce function 'function)))
        (multiple-value-bind (res err)
            ;; protect against gcing just when the ub32 address is
            ;; just ready to be passed to C
            (sb!sys::with-pinned-objects (function)
              (sb!unix::syscall ("interrupt_thread"
                                 system-area-pointer sb!alien:unsigned-long)
                                thread
                                (thread-%sap thread)
                                (sb!kernel:get-lisp-obj-address function)))
          (unless res
            (error 'interrupt-thread-error :thread thread :errno err))))))

(defun terminate-thread (thread)
  #!+sb-doc
  "Terminate the thread identified by THREAD, by causing it to run
SB-EXT:QUIT - the usual cleanup forms will be evaluated"
  (interrupt-thread thread 'sb!ext:quit))

;;; internal use only.  If you think you need to use this, either you
;;; are an SBCL developer, are doing something that you should discuss
;;; with an SBCL developer first, or are doing something that you
;;; should probably discuss with a professional psychiatrist first
#!+sb-thread
(defun symbol-value-in-thread (symbol thread)
  (let ((thread-sap (thread-%sap thread)))
    (let* ((index (sb!vm::symbol-tls-index symbol))
           (tl-val (sb!sys:sap-ref-word thread-sap
                                        (* sb!vm:n-word-bytes index))))
      (if (eql tl-val sb!vm::unbound-marker-widetag)
          (sb!vm::symbol-global-value symbol)
          (sb!kernel:make-lisp-obj tl-val)))))
