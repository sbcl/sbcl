;;;; support for threads in the target machine

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-THREAD")

;;; symbols to protect from tree-shaker, for some tests
(export '(%thread-local-references
          current-thread-sap
          get-spinlock
          release-spinlock
          spinlock
          with-deathlok
          with-session-lock
          with-spinlock))

#+(or linux win32 freebsd darwin openbsd)
(defmacro my-kernel-thread-id ()
  `(sb-ext:truly-the
    (unsigned-byte 32)
    (sap-int (sb-vm::current-thread-offset-sap sb-vm::thread-os-kernel-tid-slot))))

;;; CAS Lock
;;;
;;; Locks don't come any simpler -- or more lightweight than this. While
;;; this is probably a premature optimization for most users, we still
;;; need it internally for implementing condition variables outside Futex
;;; builds.

(defmacro with-cas-lock ((place) &body body)
  "Runs BODY with interrupts disabled and *CURRENT-THREAD* compare-and-swapped
into PLACE instead of NIL. PLACE must be a place acceptable to
COMPARE-AND-SWAP, and must initially hold NIL.

WITH-CAS-LOCK is suitable mostly when the critical section needing protection
is very small, and cost of allocating a separate lock object would be
prohibitive. While it is the most lightweight locking constructed offered by
SBCL, it is also the least scalable if the section is heavily contested or
long.

WITH-CAS-LOCK can be entered recursively."
  `(without-interrupts
     (%with-cas-lock (,place) ,@body)))

(defmacro %with-cas-lock ((place) &body body &environment env)
  (with-unique-names (owner self)
    (multiple-value-bind (vars vals old new cas-form read-form)
        (sb-ext:get-cas-expansion place env)
      `(let* (,@(mapcar #'list vars vals)
              (,owner (progn
                        (barrier (:read))
                        ,read-form))
              (,self *current-thread*)
              (,old nil)
              (,new ,self))
         (unwind-protect
              (progn
                (unless (eq ,owner ,self)
                  (loop until (loop repeat 100
                                    when (and (progn
                                                (barrier (:read))
                                                (not ,read-form))
                                              (not (setf ,owner ,cas-form)))
                                    return t
                                    else
                                    do (sb-ext:spin-loop-hint))
                        do (thread-yield)))
                ,@body)
           (unless (eq ,owner ,self)
             (let ((,old ,self)
                   (,new nil))
               (unless (eq ,old ,cas-form)
                 (bug "Failed to release CAS lock!")))))))))

(defmacro grab-cas-lock (place &environment env)
  (with-unique-names (owner self)
    (multiple-value-bind (vars vals old new cas-form read-form)
        (sb-ext:get-cas-expansion place env)
      `(let* (,@(mapcar #'list vars vals)
              (,owner (progn
                        (barrier (:read))
                        ,read-form))
              (,self *current-thread*)
              (,old nil)
              (,new ,self))
         (unless (eq ,owner ,self)
           (loop until (loop repeat 100
                             when (and (progn
                                         (barrier (:read))
                                         (not ,read-form))
                                       (not (setf ,owner ,cas-form)))
                             return t
                             else
                             do (sb-ext:spin-loop-hint))
                 do (thread-yield)))))))

(defmacro release-cas-lock (place &environment env)
  (with-unique-names (self)
    (multiple-value-bind (vars vals old new cas-form read-form)
        (sb-ext:get-cas-expansion place env)
      (declare (ignore read-form))
      `(let* (,@(mapcar #'list vars vals)
              (,self *current-thread*))
         (let ((,old ,self)
               (,new nil))
           (unless (eq ,old ,cas-form)
             (bug "Failed to release CAS lock!")))))))
;;; Conditions

(define-condition thread-error (error)
  ((thread :reader thread-error-thread :initarg :thread))
  (:documentation
   "Conditions of type THREAD-ERROR are signalled when thread operations fail.
The offending thread is initialized by the :THREAD initialization argument and
read by the function THREAD-ERROR-THREAD."))

(define-condition simple-thread-error (thread-error simple-condition)
  ())

(define-condition thread-deadlock (thread-error)
  ((cycle :initarg :cycle :reader thread-deadlock-cycle))
  (:report
   (lambda (condition stream)
     (let* ((*print-circle* t)
            (cycle (thread-deadlock-cycle condition))
            (start (caar cycle)))
       (format stream "Deadlock cycle detected:~%")
       (loop for part = (pop cycle)
             while part
             do (format stream "    ~S~%  waited for:~%    ~S~%  owned by:~%"
                        (car part)
                        (cdr part)))
       (format stream "    ~S~%" start)))))

(setf (documentation 'thread-error-thread 'function)
 "Return the offending thread that the THREAD-ERROR pertains to.")

(define-condition symbol-value-in-thread-error (cell-error thread-error)
  ((info :reader symbol-value-in-thread-error-info :initarg :info))
  (:report
   (lambda (condition stream)
     (destructuring-bind (op problem)
         (symbol-value-in-thread-error-info condition)
       (format stream "Cannot ~(~A~) value of ~S in ~S: ~S"
               op
               (cell-error-name condition)
               (thread-error-thread condition)
               (ecase problem
                 (:unbound-in-thread "the symbol is unbound in thread.")
                 (:no-tls-value "the symbol has no thread-local value.")
                 (:thread-dead "the thread has exited.")
                 (:invalid-tls-value "the thread-local value is not valid."))))))
  (:documentation
   "Signalled when SYMBOL-VALUE-IN-THREAD or its SETF version fails due to eg.
the symbol not having a thread-local value, or the target thread having
exited. The offending symbol can be accessed using CELL-ERROR-NAME, and the
offending thread using THREAD-ERROR-THREAD."))

(define-condition join-thread-error (thread-error)
  ((problem :initarg :problem :reader join-thread-problem))
  (:report (lambda (c s)
             (ecase (join-thread-problem c)
               (:abort
                (format s "Joining thread failed: thread ~A ~
                           did not return normally."
                        (thread-error-thread c)))
               (:timeout
                (format s "Joining thread timed out: thread ~A ~
                           did not exit in time."
                        (thread-error-thread c)))
               (:foreign
                (format s "Joining thread failed: thread ~A ~
                           is not a lisp thread."
                        (thread-error-thread c)))
               (:self-join
                (format s "In thread ~A, attempt to join the current ~
                           thread."
                        (thread-error-thread c))))))
  (:documentation
   "Signalled when joining a thread fails due to abnormal exit of the thread
to be joined. The offending thread can be accessed using
THREAD-ERROR-THREAD."))

(setf (documentation 'join-thread-problem 'function)
  "Return the reason that a JOIN-THREAD-ERROR was signaled. Possible values are
:TIMEOUT, :ABORT, :FOREIGN, and :SELF-JOIN.")

(define-deprecated-function :late "1.0.29.17" join-thread-error-thread thread-error-thread
    (condition)
  (thread-error-thread condition))

(define-condition interrupt-thread-error (thread-error) ()
  (:report (lambda (c s)
             (format s "Interrupt thread failed: thread ~A has exited."
                     (thread-error-thread c))))
  (:documentation
   "Signalled when interrupting a thread fails because the thread has already
exited. The offending thread can be accessed using THREAD-ERROR-THREAD."))

(define-deprecated-function :late "1.0.29.17" interrupt-thread-error-thread thread-error-thread
    (condition)
  (thread-error-thread condition))

(defmacro try-set-os-thread-name (str)
  #-sb-thread (declare (ignore str))
  ;; If NIL or non-base-string, just leave the OS thread name alone
  #+sb-thread
  `(with-alien ((sb-set-os-thread-name (function void system-area-pointer) :extern))
     (when (simple-base-string-p ,str)
       (with-pinned-objects (,str)
         (alien-funcall sb-set-os-thread-name (vector-sap ,str))))))

(declaim (inline thread-name))
(defun thread-name (thread)
 "Name of the thread. Can be assigned to using SETF. A thread name must be
a simple-string (not necessarily unique) or NIL."
  (thread-%name thread))
(defun (setf thread-name) (name thread)
  (setq name (possibly-base-stringize name))
  (setf (thread-%name thread) name) ; will fail if non-simple
  ;; Not all native thread APIs can set the name of a random thread, so only try to do it
  ;; if changing your own name.
  (when (eq thread *current-thread*) (try-set-os-thread-name name))
  name)

(defmethod print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (let* ((values (cond ((thread-alive-p thread) :running)
                         ;; don't call JOIN-THREAD, just read the result if ALIVE-P is NIL
                         ((listp (thread-result thread)) (thread-result thread))
                         (t :aborted)))
           (state (cond ((eq values :running)
                         (let* ((thing (progn
                                         (barrier (:read))
                                         (thread-waiting-for thread))))
                           (typecase thing
                             (null '(:running))
                             (cons
                              ;; It's a DX cons, can't look at it.
                              (list "waiting on a mutex with a timeout"))
                             (t
                              (list "waiting on:" thing)))))
                        ((eq values :aborted) '(:aborted))
                        (t :finished)))
           (*print-array* nil)
           ;; Don't want to see 10,000 strings or something
           (*print-length* 2)
           (*print-level* 4))
      (format stream
              ;; if not finished, show the STATE as a list.
              ;; if finished, show the VALUES.
              "~@[tid=~D ~]~@[~S ~]~:[~{~I~A~^~2I~_ ~}~_~;~A~:[ no values~; values: ~:*~{~S~^, ~}~]~]"
              (or #+(or linux win32 freebsd darwin openbsd)
                  (thread-os-tid thread))
              (thread-name thread)
              (eq :finished state)
              state
              values))))

(defmethod print-object ((mutex mutex) stream)
  (let ((name (mutex-name mutex)))
    (print-unreadable-object (mutex stream :type t :identity (not name))
      #+sb-futex
      (format stream "~@[~S ~]~[free~;taken~;contested~:;err~] owner=~X"
              name (mutex-state mutex) (vmthread-name (mutex-%owner mutex)))
      #-sb-futex
      (let ((owner (mutex-owner mutex))
            (*print-circle* t))
        (if owner
            (format stream "~@[~S ~]~2I~_owner: ~S" name owner)
            (format stream "~@[~S ~](free)" name))))))

;; NB: ephemeral threads must terminate strictly before the test of NTHREADS>1
;; in DEINIT, i.e. this is not a promise that the thread will terminate
;; just-in-time for the final call out to save, but rather by an earlier time.
(defun thread-ephemeral-p (thread)
  "Return T if THREAD is `ephemeral', which indicates that this thread is
used by SBCL for internal purposes, and specifically that our runtime knows how
to terminate this thread cleanly prior to core file saving without signalling
an error in that case."
  (thread-%ephemeral-p thread))

;;; Ensure that THREAD is in *ALL-THREADS*.
(defmacro update-all-threads (key thread)
  `(let ((addr ,key))
     (barrier (:read))
     (let ((old *all-threads*))
       (loop
         ;; If ADDR exists, then we have a bug in the thread exit handler.
         ;; The workaround here would be to delete the old thread first,
         ;; but I'd rather find out about the bug than bury it.
         (aver (not (avl-find addr old)))
         (let ((new (avl-insert old addr ,thread)))
           (when (eq old (setq old (sb-ext:cas *all-threads* old new))) (return)))))))

(defun vmthread-name (vmthread)
  (binding* ((node (avl-find (vmthread-id->addr vmthread) *all-threads*) :exit-if-null)
             (thread (avlnode-data node) :exit-if-null)
             (name (thread-name thread) :exit-if-null))
    (return-from vmthread-name name))
  vmthread) ; lacking a string, the identifier constitutes its name

;;; Translate a 'struct thread*' to a SB-THREAD:THREAD.
;;; I'd like to do this simply by reading the 'lisp_thread' field,
;;; but that's dangerous.  Ultimately I think we will have to either store a
;;; refcount in the structure (freeing it only when its last referer is gone)
;;; or implement hazard pointers. Either one is going to be tricky because we can't
;;; store the refcount in the structure if it can go away while we're trying
;;; to increment the count. But having the ability to manipulate the structure
;;; of any thread from any thread would simplify other things in this file
;;; as well as making MUTEX-OWNER more efficient.
(defun mutex-owner-lookup (vmthread)
  ;; Convert the "fixnum-encoded" thread ID to a word.
  ;; It's possible that a race could cause find to fail. If the mutex
  ;; really has a dead thread as its owner, you've got bigger problems.
  ;; Moreover, because 'struct thread' can be recycled (very quickly)
  ;; it's possible for the following sequence to occur: thread T1 at address A1
  ;; grabs the mutex, then thread T2 reads MUTEX-%OWNER slot, then T1 exits
  ;; and T3 gets allocated at address A1, and grabs the mutex.
  ;; Thread T2 then performs AVL-FIND and concludes that T3 is the apparent owner.
  ;; Well, as the docstring at MUTEX-OWNER says, it is "racy by design".
  (acond ((avl-find (vmthread-id->addr vmthread) *all-threads*) (avlnode-data it))
         ((= vmthread 0) nil)
         ;; This is the same keyword that SYMBOL-VALUE-IN-THREAD can return on error.
         ;; If people don't like seeing it, we could return instead
         ;;   (LOAD-TIME-VALUE (%make-thread "dead-thread" nil nil))
         ;; indicating that you observed a value of %OWNER which no longer exists.
         (t :thread-dead)))

(defun %list-all-threads ()
  ;; No lock needed, just an atomic read, since tree mutations can't happen.
  ;; Of course by the time we're done collecting nodes, the tree can have
  ;; been replaced by a different tree.
  (barrier (:read))
  (avltree-filter (lambda (node)
                    (let ((thread (avlnode-data node)))
                      (when (= (thread-%visible thread) 1)
                        thread)))
                  *all-threads*))

(defun list-all-threads ()
  "Return a list of the live threads. Note that the return value is
potentially stale even before the function returns, as new threads may be
created and old ones may exit at any time."
  (delete sb-impl::*finalizer-thread* (%list-all-threads)))

;;; used by debug-int.lisp to access interrupt contexts

(sb-ext:define-load-time-global *initial-thread* nil)

;;; *JOINABLE-THREADS* is a list of THREAD instances.
;;; I had attempted to construct the list using the thread's memory to create cons
;;; cells but that turned out to be flawed- the cells must be freshly heap-allocated,
;;; because ATOMIC-POP is vulnerable to the A/B/A problem if cells are reused.
;;; Example: initial state: *JOINABLE-THREADS* -> node1 -> node2 -> node3.
;;; After reading *JOINABLE-THREADS* we want to CAS it to node2.
;;; If, after reading the variable, all of node1, node2, and node3 are popped
;;; by another thread, and then node1 is reused, and made to point to node4,
;;; then the new state is: *JOINABLE-THREADS* -> node1 -> node4
;;; which looks like CAS(*joinable-threads*, node1, node2) should succeed,
;;; but it should not. The LL/SC model would detect that, but CAS can not.
;;;
;;; A thread is pushed into *JOINABLE-THREADS* while still using its lisp stack.
;;; This is fine, because the C code will perform a join, which will effectively
;;; wait until the lisp thread is off its stack. It won't have to wait long,
;;; because pushing into *JOINABLE-THREADS* is the last thing to happen in lisp.
;;; In theory we could support some mode of keeping the memory while joining
;;; the pthread, but we currently do not.
(sb-ext:define-load-time-global *joinable-threads* nil)
(declaim (list *joinable-threads*)) ; list of threads

;;; Copy some slots from the C 'struct thread' into the SB-THREAD:THREAD.
(defmacro copy-primitive-thread-fields (this)
  `(progn
     (setf (thread-primitive-thread ,this) (current-thread-sap-int))
     #-win32
     (setf (thread-os-thread ,this)
           (sap-int (sb-vm::current-thread-offset-sap sb-vm::thread-os-thread-slot)))))

(defmacro set-thread-control-stack-slots (this)
  `(setf (thread-control-stack-start ,this) (get-lisp-obj-address sb-vm:*control-stack-start*)
         (thread-control-stack-end ,this) (get-lisp-obj-address sb-vm:*control-stack-end*)))

(defvar *session*)

;;; Not uncoincidentally, the variables assigned here are also
;;; listed in SB-KERNEL::*SAVE-LISP-CLOBBERED-GLOBALS*
(defun init-main-thread ()
  (/show0 "Entering INIT-MAIN-THREAD")
  (setf sb-impl::*exit-lock* (make-mutex :name "Exit Lock")
        sb-vm::*allocator-mutex* (make-mutex :name "Allocator")
        *make-thread-lock* (make-mutex :name "Make-Thread Lock"))
  (let* ((name "main thread")
         (thread (%make-thread name nil (make-semaphore :name name))))
    (copy-primitive-thread-fields thread)
    (set-thread-control-stack-slots thread)
    ;; Run the macro-generated function which writes some values into the TLS,
    ;; most especially *CURRENT-THREAD*.
    (init-thread-local-storage thread)
    (setf *initial-thread* thread)
    (setf *joinable-threads* nil)
    (setq *session* (new-session thread))
    (setq *all-threads*
          (avl-insert nil
                      (sb-thread::thread-primitive-thread sb-thread:*current-thread*)
                      thread))))

(defun main-thread ()
  "Returns the main thread of the process."
  *initial-thread*)

(defun main-thread-p (&optional (thread *current-thread*))
  "True if THREAD, defaulting to current thread, is the main thread of the process."
  (eq thread *initial-thread*))

(locally (declare (sb-c::tlab :system)) (defun sys-tlab-list (&rest args) args))

(defmacro return-from-thread (values-form &key allow-exit)
  "Unwinds from and terminates the current thread, with values from
VALUES-FORM as the results visible to JOIN-THREAD.

If current thread is the main thread of the process (see
MAIN-THREAD-P), signals an error unless ALLOW-EXIT is true, as
terminating the main thread would terminate the entire process. If
ALLOW-EXIT is true, returning from the main thread is equivalent to
calling SB-EXT:EXIT with :CODE 0 and :ABORT NIL.

See also: ABORT-THREAD and SB-EXT:EXIT."
  `(%return-from-thread (multiple-value-call #'sys-tlab-list ,values-form) ,allow-exit))

(defun %return-from-thread (values allow-exit)
  (let ((self *current-thread*))
    (cond ((main-thread-p self)
           (unless allow-exit
             (error 'simple-thread-error
                    :format-control "~@<Tried to return ~S as values from main thread, ~
                                     but exit was not allowed.~:@>"
                    :format-arguments (list values)
                    :thread self))
           (sb-ext:exit :code 0))
          (t
           (throw '%return-from-thread (values-list values))))))

(defun abort-thread (&key allow-exit)
  "Unwinds from and terminates the current thread abnormally, causing
JOIN-THREAD on current thread to signal an error unless a
default-value is provided.

If current thread is the main thread of the process (see
MAIN-THREAD-P), signals an error unless ALLOW-EXIT is true, as
terminating the main thread would terminate the entire process. If
ALLOW-EXIT is true, aborting the main thread is equivalent to calling
SB-EXT:EXIT code 1 and :ABORT NIL.

Invoking the initial ABORT restart established by MAKE-THREAD is
equivalent to calling ABORT-THREAD in other than main threads.
However, whereas ABORT restart may be rebound, ABORT-THREAD always
unwinds the entire thread. (Behaviour of the initial ABORT restart for
main thread depends on the :TOPLEVEL argument to
SB-EXT:SAVE-LISP-AND-DIE.)

See also: RETURN-FROM-THREAD and SB-EXT:EXIT."
  (let ((self *current-thread*))
    (cond ((main-thread-p self)
           (unless allow-exit
             (error 'simple-thread-error
                    :format-control "~@<Tried to abort initial thread, but ~
                                     exit was not allowed.~:@>"))
           (sb-ext:exit :code 1))
          (t
           ;; We /could/ use TOPLEVEL-CATCHER or %END-OF-THE-WORLD as well, but
           ;; this seems tidier. Those to are a bit too overloaded already.
           (throw '%abort-thread t)))))


;;;; Aliens, low level stuff

;;; *STARTING-THREADS* receives special treatment by the garbage collector.
;;; The contents of it are pinned (in that respect it is like *PINNED-OBJECTS*)
;;; but also the STARTUP-INFO of each thread is pinned.
(sb-ext:define-load-time-global *starting-threads* nil)
(declaim (list *starting-threads*)) ; list of threads

#+sb-thread
(progn
  #+sb-futex
  (progn
    (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (define-structure-slot-addressor mutex-state-address
        ;; """ (Futexes are 32 bits in size on all platforms, including 64-bit systems.) """
        ;; which means we need to add 4 bytes to get to the low 32 bits of the slot contents
        ;; where we store state. This would be prettier if we had 32-bit raw slots.
        :structure mutex
        :slot state
        :byte-offset (+ #+(and 64-bit big-endian) 4))
      (define-structure-slot-addressor waitqueue-token-address
        :structure waitqueue
        :slot token
        :byte-offset (+ #+(and 64-bit big-endian) 4)))

    (export 'futex-wake) ; for naughty users only
    (declaim (inline futex-wait futex-wake))

    (define-alien-routine "futex_wake" int (word-addr unsigned) (n unsigned-long))

    (defun futex-wait (word-addr oldval to-sec to-usec)
      (with-alien ((%wait (function int unsigned
                                    (unsigned 32)
                                    long unsigned-long)
                          :extern "futex_wait"))
        (with-interrupts
          (alien-funcall %wait word-addr oldval to-sec to-usec))))))

(defmacro with-deadlocks ((thread lock &optional (timeout nil timeoutp)) &body forms)
  (with-unique-names (n-thread n-lock new n-timeout)
    `(let* ((,n-thread ,thread)
            (,n-lock ,lock)
            (,n-timeout ,(when timeoutp
                           `(or ,timeout sb-impl::*deadline*)))
            (,new (if ,n-timeout
                      ;; Using CONS tells the rest of the system there's a
                      ;; timeout in place, so it isn't considered a deadlock.
                      (cons ,n-timeout ,n-lock)
                      ,n-lock)))
       (declare (dynamic-extent ,new))
       ;; No WITHOUT-INTERRUPTS, since WITH-DEADLOCKS is used
       ;; in places where interrupts should already be disabled.
       (unwind-protect
            (progn
              (setf (thread-waiting-for ,n-thread) ,new)
              (barrier (:memory))
              ,@forms)
         ;; Interrupt handlers and GC save and restore any
         ;; previous wait marks using WITHOUT-THREAD-WAITING-FOR
         (setf (thread-waiting-for ,n-thread) nil)
         (barrier (:memory))))))

;;;; Mutexes

(setf (documentation 'make-mutex 'function) "Create a mutex."
      (documentation 'mutex-name 'function) "The name of the mutex. Setfable.")

(sb-ext:define-load-time-global **deadlock-lock** nil)

;;; Signals an error if owner of LOCK is waiting on a lock whose release
;;; depends on the current thread. Does not detect deadlocks from sempahores.
;;; Limited to 10 threads because it may not terminate if the threads
;;; keep locking different locks in the meantime.
#+sb-thread
(defun check-deadlock ()
  (let* ((self *current-thread*)
         (origin (thread-waiting-for self))
         unlock-deadlock-lock)
    (labels ((detect-deadlock (lock limit)
               (declare (fixnum limit))
               (barrier (:read))
               (let ((other-vmthread-id (mutex-%owner lock)))
                 (cond ((= limit 0) nil)
                       ((= other-vmthread-id 0) nil)
                       ((= (current-vmthread-id) other-vmthread-id)
                        ;; We're now committed to signaling the
                        ;; error and breaking the deadlock, so
                        ;; mark us as no longer waiting on the
                        ;; lock. This ensures that a single
                        ;; deadlock is reported in only one
                        ;; thread, and that we don't look like
                        ;; we're waiting on the lock when print
                        ;; stuff -- because that may lead to
                        ;; further deadlock checking, in turn
                        ;; possibly leading to a bogus vicious
                        ;; metacycle on PRINT-OBJECT.
                        (grab-cas-lock **deadlock-lock**)
                        (setf unlock-deadlock-lock t)
                        (list (cons self origin)))
                       (t
                        (let* ((other-thread (mutex-owner-lookup other-vmthread-id))
                               (other-lock (when (thread-p other-thread)
                                             (barrier (:read))
                                             (thread-waiting-for other-thread))))
                          ;; If the thread is waiting with a timeout OTHER-LOCK
                          ;; is a cons, and we don't consider it a deadlock -- since
                          ;; it will time out on its own sooner or later.
                          (when (mutex-p other-lock)
                            (let ((chain (detect-deadlock other-lock (1- limit))))
                              (when (and (consp chain)
                                         ;; Recheck that the mutex is still owned by the same thread.
                                         (progn (barrier (:read))
                                                (let ((owner (mutex-%owner lock)))
                                                  (= owner other-vmthread-id)))
                                         ;; See if it hasn't been set to NIL above by another thread.
                                         (eq (progn (barrier (:read))
                                                    (thread-waiting-for other-thread))
                                             other-lock))
                                (cons (cons other-thread other-lock)
                                      chain))))))))))
      ;; Timeout means there is no deadlock
      (when (mutex-p origin)
        (let ((chain (detect-deadlock origin 10)))
          (when (consp chain)
            (setf (thread-waiting-for self) nil)
            (sb-thread:barrier (:memory))
            (release-cas-lock **deadlock-lock**)
            (with-interrupts
              (error 'thread-deadlock
                     :thread *current-thread*
                     :cycle (let ((last (last chain)))
                              (append last (butlast chain))))))
          (when unlock-deadlock-lock
            (release-cas-lock **deadlock-lock**)))
        t))))

;;;; WAIT-FOR -- waiting on arbitrary conditions

(defun %%wait-for (test stop-sec stop-usec)
  (declare (function test))
  (declare (dynamic-extent test))
  (labels ((try ()
             (declare (optimize (safety 0)))
             (awhen (funcall test)
               (return-from %%wait-for it)))
           (tick (sec usec)
             (declare (type fixnum sec usec))
             ;; TICK is microseconds
             (+ usec (* 1000000 sec)))
           (get-tick ()
             (multiple-value-call #'tick
               (decode-internal-time (get-internal-real-time)))))
    (let* ((timeout-tick (when stop-sec (tick stop-sec stop-usec)))
           (start (get-tick))
           ;; Rough estimate of how long a single attempt takes.
           (try-ticks (progn
                        (try) (try) (try)
                        (max 1 (truncate (- (get-tick) start) 3)))))
      ;; Scale sleeping between attempts:
      ;;
      ;; Start by sleeping for as many ticks as an average attempt
      ;; takes, then doubling for each attempt.
      ;;
      ;; Max out at 0.1 seconds, or the 2 x time of a single try,
      ;; whichever is longer -- with a hard cap of 10 seconds.
      ;;
      ;; FIXME: Maybe the API should have a :MAX-SLEEP argument?
      (loop with max-ticks = (max 100000 (min (* 2 try-ticks)
                                              (expt 10 7)))
            for scale of-type fixnum = 1
            then (let ((x (logand most-positive-fixnum (* 2 scale))))
                   (if (> scale x)
                       most-positive-fixnum
                       x))
            do (try)
               (let* ((now (get-tick))
                      (sleep-ticks (min (* try-ticks scale) max-ticks))
                      (sleep
                        (if timeout-tick
                            ;; If sleep would take us past the
                            ;; timeout, shorten it so it's just
                            ;; right.
                            (if (>= (+ now sleep-ticks) timeout-tick)
                                (- timeout-tick now)
                                sleep-ticks)
                            sleep-ticks)))
                 (declare (type fixnum sleep))
                 (cond ((plusp sleep)
                        ;; microseconds to seconds and nanoseconds
                        (multiple-value-bind (sec nsec)
                            (truncate (* 1000 sleep) (expt 10 9))
                          (with-interrupts
                            (sb-unix:nanosleep sec nsec))))
                       (t
                        (return-from %%wait-for nil))))))))

(defun %wait-for (test timeout)
  (declare (function test))
  (declare (dynamic-extent test))
  (tagbody
   :restart
     (multiple-value-bind (to-sec to-usec stop-sec stop-usec deadlinep)
         (decode-timeout timeout)
       (declare (ignore to-sec to-usec))
       (return-from %wait-for
         (or (%%wait-for test stop-sec stop-usec)
             (when deadlinep
               (signal-deadline)
               (go :restart)))))))

(defmacro sb-ext:wait-for (test-form &key timeout)
  "Wait until TEST-FORM evaluates to true, then return its primary value.
If TIMEOUT is provided, waits at most approximately TIMEOUT seconds before
returning NIL.

If WITH-DEADLINE has been used to provide a global deadline, signals a
DEADLINE-TIMEOUT if TEST-FORM doesn't evaluate to true before the
deadline.

Experimental: subject to change without prior notice."
  `(dx-flet ((wait-for-test () (progn ,test-form)))
     (%wait-for #'wait-for-test ,timeout)))

(defmacro with-progressive-timeout ((name &key seconds)
                                    &body body)
  "Binds NAME as a local function for BODY. Each time #'NAME is called, it
returns SECONDS minus the time that has elapsed since BODY was entered, or
zero if more time than SECONDS has elapsed. If SECONDS is NIL, #'NAME
returns NIL each time."
  (with-unique-names (deadline time-left sec)
    `(let* ((,sec ,seconds)
            (,deadline
              (when ,sec
                (+ (get-internal-real-time)
                   (round (* ,seconds internal-time-units-per-second))))))
       (flet ((,name ()
                (when ,deadline
                  (let ((,time-left (- ,deadline (get-internal-real-time))))
                    (if (plusp ,time-left)
                        (* (coerce ,time-left 'single-float)
                           (sb-xc:/ 1.0f0 internal-time-units-per-second))
                        0)))))
         ,@body))))

(defun %try-mutex (mutex)
  (declare (type mutex mutex) (optimize (speed 3)))
  (cond #+sb-futex
        (t
         ;; From the Mutex 2 algorithm from "Futexes are Tricky" by Ulrich Drepper.
         (let ((id (current-vmthread-id)))
           (cond ((= (sb-ext:cas (mutex-state mutex) 0 1) 0)
                  (setf (mutex-%owner mutex) id)
                  t) ; GRAB-MUTEX wants %TRY-MUTEX to return boolean, not generalized boolean
                 ((= (mutex-%owner mutex) id)
                  (error "Recursive lock attempt ~S." mutex)))))
        #-sb-futex
        (t
         (barrier (:read))
         (let ((old (mutex-%owner mutex)))
           (when (= (current-vmthread-id) old)
             (error "Recursive lock attempt ~S." mutex))
           #-sb-thread
           (when (/= old 0)
             (error "Strange deadlock on ~S in an unithreaded build?" mutex))
           (and (zerop old)
                ;; Don't even bother to try to CAS if it looks bad.
                (zerop (sb-ext:compare-and-swap (mutex-%owner mutex) 0
                                                (current-vmthread-id))))))))

;;; memory aid: this is "pthread_mutex_timedlock" without the pthread
;;; and no messing about with *DEADLINE* or deadlocks. It's just locking.
#+sb-thread
(defun %mutex-timedlock (mutex to-sec to-usec stop-sec stop-usec)
  (declare (type mutex mutex) (optimize (speed 3)))
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (declare (ignorable to-sec to-usec))
  (cond
   #+sb-futex
   (t
    ;; This is a fairly direct translation of the Mutex 2 algorithm from
    ;; "Futexes are Tricky" by Ulrich Drepper.
    ;;
    ;; void lock () {
    ;;   int c;
    ;;   if ((c = cmpxchg(val, 0, 1)) != 0)
    ;;     do {
    ;;       if (c == 2 || cmpxchg(val, 1, 2) != 0)
    ;;         futex_wait(&val, 2);
    ;;    } while ((c = cmpxchg(val, 0, 2)) != 0);
    ;; }
    ;;
    (symbol-macrolet ((val (mutex-state mutex)))
      (let ((c (sb-ext:cas val 0 1))) ; available -> taken
        (unless (= c 0) ; Got it right off the bat?
          (nlx-protect
           (if (not stop-sec)
               (loop                    ; untimed
                     ;; Mark it as contested, and sleep, unless it is now in state 0.
                     (when (or (eql c 2) (/= 0 (sb-ext:cas val 1 2)))
                       (with-pinned-objects (mutex)
                         (futex-wait (mutex-state-address mutex) 2 -1 0)))
                     ;; Try to get it, still marking it as contested.
                     (when (= 0 (setq c (sb-ext:cas val 0 2))) (return))) ; win
               (loop             ; same as above but check for timeout
                     (when (or (eql c 2) (/= 0 (sb-ext:cas val 1 2)))
                       (if (eql 1 (with-pinned-objects (mutex)
                                    (futex-wait (mutex-state-address mutex) 2 to-sec to-usec)))
                           ;; -1 = EWOULDBLOCK, possibly spurious wakeup
                           ;;  0 = normal wakeup
                           ;;  1 = ETIMEDOUT ***DONE***
                           ;;  2 = EINTR, a spurious wakeup
                           (return-from %mutex-timedlock nil)))
                     (when (= 0 (setq c (sb-ext:cas val 0 2))) (return)) ; win
                     ;; Update timeout
                     (setf (values to-sec to-usec)
                           (sb-impl::relative-decoded-times stop-sec stop-usec))))
           ;; Unwinding because futex-wait allows interrupts, wake up another futex
           (with-pinned-objects (mutex)
             (futex-wake (mutex-state-address mutex) 1)))))
      (setf (mutex-%owner mutex) (current-vmthread-id))
      t))
   #-sb-futex
   (t
    (flet ((cas ()
           (loop repeat 100
                 when (and (progn (barrier (:read))
                                  (zerop (mutex-%owner mutex)))
                           (zerop (sb-ext:compare-and-swap (mutex-%owner mutex) 0
                                                           (current-vmthread-id))))
                 do (return-from cas t)
                 else
                 do
                    (sb-ext:spin-loop-hint))
           ;; Check for pending interrupts.
           (with-interrupts nil)))
      (declare (dynamic-extent #'cas))
      (%%wait-for #'cas stop-sec stop-usec)))))

#+ultrafutex
(progn
(declaim (inline fast-futex-wait))
(defun fast-futex-wait (word-addr oldval to-sec to-usec)
  (with-alien ((%wait (function int unsigned
                                #+freebsd unsigned #-freebsd (unsigned 32)
                                long unsigned-long)
                      :extern "futex_wait"))
    (alien-funcall %wait word-addr oldval to-sec to-usec)))
(declaim (sb-ext:maybe-inline %wait-for-mutex-algorithm-3))
(defun %wait-for-mutex-algorithm-3 (mutex)
  #+nil ; in case I want to count calls to this function
  (let ((sap (int-sap (thread-primitive-thread *current-thread*)))
        (disp (ash sb-vm::thread-slow-path-allocs-slot sb-vm:word-shift)))
    (incf (sap-ref-word sap disp)))
  ;; #+ultrafutex does not support deadlines for now. It might eventually,
  ;; but would have to fall back to the older code if there is a deadline.
  (aver (null sb-impl::*deadline*))
  (symbol-macrolet ((val (mutex-state mutex)))
    (let* ((mutex (sb-ext:truly-the mutex mutex))
           (c (sb-ext:cas val 0 1))) ; available -> taken
      (unless (= c 0) ; Got it right off the bat?
        (unless (= c 2)
          (setq c (%raw-instance-xchg/word mutex (get-dsd-index mutex state) 2)))
        (loop while (/= c 0)
              do (with-pinned-objects (mutex)
                   (fast-futex-wait (mutex-state-address mutex) 2 -1 0))
                 (setq c (%raw-instance-xchg/word mutex (get-dsd-index mutex state) 2))))))))

#+mutex-benchmarks
(symbol-macrolet ((val (mutex-state mutex)))
  (export '(wait-for-mutex-algorithm-2
            wait-for-mutex-algorithm-3
            wait-for-mutex-2-partial-inline
            wait-for-mutex-3-partial-inline))
  (declaim (sb-ext:maybe-inline %wait-for-mutex-algorithm-2
                                %wait-for-mutex-algorithm-3))
  (sb-ext:define-load-time-global *grab-mutex-calls-performed* 0)

  (defun %wait-for-mutex-algorithm-2 (mutex)
    (incf *grab-mutex-calls-performed*)
    (let* ((mutex (sb-ext:truly-the mutex mutex))
           (c (sb-ext:cas val 0 1))) ; available -> taken
      (unless (= c 0) ; Got it right off the bat?
        (loop
          ;; Mark it as contested, and sleep, unless it is now in state 0.
          (when (or (eql c 2) (/= 0 (sb-ext:cas val 1 2)))
            (with-pinned-objects (mutex)
              (fast-futex-wait (mutex-state-address mutex) 2 -1 0)))
          ;; Try to get it, still marking it as contested.
          (when (= 0 (setq c (sb-ext:cas val 0 2))) (return)))))) ; win

  (defun wait-for-mutex-algorithm-2 (mutex)
    (declare (inline %wait-for-mutex-algorithm-2))
    (let ((mutex (sb-ext:truly-the mutex mutex)))
      (%wait-for-mutex-algorithm-2 mutex)
      (setf (mutex-%owner mutex) (current-vmthread-id))))
  ;; The improvement with algorithm 3 is fairly negligible.
  ;; Code size is a little less. More improvement comes from doing the
  ;; partial-inline algorithms which perform one CAS without a function call.
  (defun wait-for-mutex-algorithm-3 (mutex)
    (declare (inline %wait-for-mutex-algorithm-3))
    (let ((mutex (sb-ext:truly-the mutex mutex)))
      (%wait-for-mutex-algorithm-3 mutex)
      (setf (mutex-%owner mutex) (current-vmthread-id))))
  (defmacro wait-for-mutex-2-partial-inline (mutex)
    `(let ((m ,mutex))
       (or (= (sb-ext:cas (mutex-state m) 0 1) 0) (%wait-for-mutex-algorithm-2 m))
       (setf (mutex-%owner m) (current-vmthread-id))))
  (defmacro wait-for-mutex-3-partial-inline (mutex)
    `(let ((m ,mutex))
       (or (= (sb-ext:cas (mutex-state m) 0 1) 0) (%wait-for-mutex-algorithm-3 m))
       (setf (mutex-%owner m) (current-vmthread-id))))
  ;; This is like RELEASE-MUTEX but without keyword arg parsing
  ;; and all the different error modes.
  (export 'fast-release-mutex)
  (defun fast-release-mutex (mutex)
    (let ((mutex (sb-ext:truly-the mutex mutex)))
      (setf (mutex-%owner mutex) 0)
      (unless (eql (sb-ext:atomic-decf (mutex-state mutex) 1) 1)
        (setf (mutex-state mutex) 0)
        (with-pinned-objects (mutex)
          (futex-wake (mutex-state-address mutex) 1))))))

#+sb-thread
(macrolet ((guts ()
   `(tagbody
     :again
       (return-from %wait-for-mutex
         (or (%mutex-timedlock mutex to-sec to-usec stop-sec stop-usec)
             (when deadlinep
               (signal-deadline)
               ;; FIXME: substract elapsed time from timeout...
               (setf (values to-sec to-usec stop-sec stop-usec deadlinep)
                     (decode-timeout timeout))
               (go :again)))))))
  (defun deadlock-aware-mutex-wait
      (mutex timeout to-sec to-usec stop-sec stop-usec deadlinep &aux (self *current-thread*))
    (block %wait-for-mutex
      (with-deadlocks (self mutex timeout)
        (with-interrupts (check-deadlock))
        (guts))))
  (defun mutex-wait (mutex timeout to-sec to-usec stop-sec stop-usec deadlinep)
    (block %wait-for-mutex (guts))))

(define-deprecated-function :early "1.0.37.33" get-mutex (grab-mutex)
    (mutex &optional new-owner (waitp t) (timeout nil))
  (declare (ignorable waitp timeout))
  (when (and new-owner (neq new-owner *current-thread*))
    (error "GET-MUTEX won't get a mutex on behalf of a different thread"))
  (or (%try-mutex mutex)
      #+sb-thread
      (when waitp
        (multiple-value-call #'deadlock-aware-mutex-wait mutex timeout (decode-timeout timeout)))))

(declaim (ftype (sfunction (mutex &key (:waitp t) (:timeout (or null (real 0)))) boolean) grab-mutex))
(defun grab-mutex (mutex &key (waitp t) (timeout nil))
  "Acquire MUTEX for the current thread. If WAITP is true (the default) and
the mutex is not immediately available, sleep until it is available.

If TIMEOUT is given, it specifies a relative timeout, in seconds, on how long
GRAB-MUTEX should try to acquire the lock in the contested case.

If GRAB-MUTEX returns T, the lock acquisition was successful. In case of WAITP
being NIL, or an expired TIMEOUT, GRAB-MUTEX may also return NIL which denotes
that GRAB-MUTEX did -not- acquire the lock.

Notes:

  - GRAB-MUTEX is not interrupt safe. The correct way to call it is:

      (WITHOUT-INTERRUPTS
        ...
        (ALLOW-WITH-INTERRUPTS (GRAB-MUTEX ...))
        ...)

    WITHOUT-INTERRUPTS is necessary to avoid an interrupt unwinding the call
    while the mutex is in an inconsistent state while ALLOW-WITH-INTERRUPTS
    allows the call to be interrupted from sleep.

  - (GRAB-MUTEX <mutex> :timeout 0.0) differs from
    (GRAB-MUTEX <mutex> :waitp nil) in that the former may signal a
    DEADLINE-TIMEOUT if the global deadline was due already on entering
    GRAB-MUTEX.

    The exact interplay of GRAB-MUTEX and deadlines are reserved to change in
    future versions.

  - It is recommended that you use WITH-MUTEX instead of calling GRAB-MUTEX
    directly.
"
  (declare (ignorable waitp timeout))
  (or (%try-mutex mutex)
      #+sb-thread
      (when waitp
        (multiple-value-call #'deadlock-aware-mutex-wait mutex timeout (decode-timeout timeout)))))

(declaim (ftype (sfunction (mutex) boolean) grab-mutex-no-check-deadlock))
#+sb-thread ; WITH-MUTEX will never expand to call this if #-sb-thread
(defun grab-mutex-no-check-deadlock (mutex)
  ;; Always wait with infinite timeout, never examine the waiting-for graph.
  ;; This _does_ support *DEADLINE* hence the DECODE-TIMEOUT call.
  (or (%try-mutex mutex)
      (multiple-value-call #'mutex-wait mutex nil (decode-timeout nil))))

(declaim (ftype (sfunction (mutex &key (:if-not-owner (member :punt :warn :error :force))) null)
                release-mutex))
(defun release-mutex (mutex &key (if-not-owner :punt))
  "Release MUTEX by setting it to NIL. Wake up threads waiting for
this mutex.

RELEASE-MUTEX is not interrupt safe: interrupts should be disabled
around calls to it.

If the current thread is not the owner of the mutex then it silently
returns without doing anything (if IF-NOT-OWNER is :PUNT), signals a
WARNING (if IF-NOT-OWNER is :WARN), or releases the mutex anyway (if
IF-NOT-OWNER is :FORCE)."
  (declare (type mutex mutex))
  ;; Order matters: set owner to NIL before releasing state.
  (let* ((self (current-vmthread-id))
         (old-owner (sb-ext:compare-and-swap (mutex-%owner mutex) self 0)))
    (unless (= self old-owner)
      (ecase if-not-owner
        ((:punt) (return-from release-mutex nil))
        ((:warn)
         (warn "Releasing ~S, owned by another thread: ~S" mutex (vmthread-name old-owner)))
        ((:error)
         (error "Releasing ~S, owned by another thread: ~S" mutex (vmthread-name old-owner)))
        ((:force)))
      (setf (mutex-%owner mutex) 0)
      ;; FIXME: Is a :memory barrier too strong here?  Can we use a :write
      ;; barrier instead?
      (barrier (:memory)))
    #+sb-futex
    (when (/= old-owner 0)
      (unless (eql (sb-ext:atomic-decf (mutex-state mutex) 1) 1)
        (setf (mutex-state mutex) 0)
        (sb-thread:barrier (:write)) ; paranoid ?
        (with-pinned-objects (mutex)
            (futex-wake (mutex-state-address mutex) 1)))
      nil)))


;;;; Waitqueues/condition variables

#+(and sb-thread (not sb-futex))
(progn
  (defun %waitqueue-enqueue (thread queue)
    (setf (thread-waiting-for thread) queue)
    (let ((head (waitqueue-%head queue))
          (tail (waitqueue-%tail queue))
          (new (list thread)))
      (unless head
        (setf (waitqueue-%head queue) new))
      (when tail
        (setf (cdr tail) new))
      (setf (waitqueue-%tail queue) new)
      nil))
  (defun %waitqueue-drop (thread queue)
    (setf (thread-waiting-for thread) nil)
    (let ((head (waitqueue-%head queue)))
      (do ((list head (cdr list))
           (prev nil list))
          ((or (null list)
               (eq (car list) thread))
           (when list
             (let ((rest (cdr list)))
               (cond (prev
                      (setf (cdr prev) rest))
                     (t
                      (setf (waitqueue-%head queue) rest
                            prev rest)))
               (unless rest
                 (setf (waitqueue-%tail queue) prev)))))))
    nil)
  (defun %waitqueue-wakeup (queue n)
    (declare (fixnum n))
    (loop with next = nil
          while (plusp n)
          do (setq next (let ((head (waitqueue-%head queue))
                              (tail (waitqueue-%tail queue)))
                          (when head
                            (if (eq head tail)
                                (setf (waitqueue-%head queue) nil
                                      (waitqueue-%tail queue) nil)
                                (setf (waitqueue-%head queue) (cdr head)))
                            (car head))))
          while next
          do (when (eq queue (sb-ext:compare-and-swap
                              (thread-waiting-for next) queue nil))
               (decf n)))
    nil))

(defmethod print-object ((waitqueue waitqueue) stream)
  (acond ((waitqueue-name waitqueue)
          (print-unreadable-object (waitqueue stream :type t :identity t)
            (write-string it stream)))
         (t
          (print-unreadable-object (waitqueue stream :type t :identity t)))))

(setf (documentation 'waitqueue-name 'function) "The name of the waitqueue. Setfable."
      (documentation 'make-waitqueue 'function) "Create a waitqueue.")

(defmacro nlx-protect-futex (protected &body cleanup)
  (declare (ignorable cleanup))
  #+sb-futex
  `(nlx-protect ,protected
                ,@cleanup)
  #-sb-futex
  protected)

(defun %condition-wait (queue mutex
                        check-deadlock
                        timeout to-sec to-usec stop-sec stop-usec deadlinep)
  #-sb-thread
  (declare (ignore queue mutex check-deadlock to-sec to-usec stop-sec stop-usec deadlinep))
  #-sb-thread
  (sb-ext:wait-for nil :timeout timeout) ; Yeah...
  #+sb-thread
  (let ((me *current-thread*))
    (declare (ignorable me)) ; not used if #+sb-futex
    (barrier (:read))
    (unless (holding-mutex-p mutex)
      (error "The current thread is not holding ~s." mutex))
    (let ((status :interrupted))
      ;; Need to disable interrupts so that we don't miss grabbing
      ;; the mutex on our way out.
      (without-interrupts
        (nlx-protect-futex
         (unwind-protect
              (cond
                #+sb-futex
                (t
                 (with-pinned-objects (queue)
                   (setf (waitqueue-token queue) (my-kernel-thread-id))
                   (release-mutex mutex)
                   ;; Now we go to sleep using futex-wait. If anyone else
                   ;; manages to grab MUTEX and call CONDITION-NOTIFY during
                   ;; this comment, it will change the token, and so futex-wait
                   ;; returns immediately instead of sleeping. Ergo, no lost
                   ;; wakeup. We may get spurious wakeups, but that's ok.
                   (setf status
                         (case (allow-with-interrupts
                                 (futex-wait (waitqueue-token-address queue)
                                             (my-kernel-thread-id)
                                             ;; our way of saying "no
                                             ;; timeout":
                                             (or to-sec -1)
                                             (or to-usec 0)))
                           ((1)
                            ;;  1 = ETIMEDOUT
                            :timeout)
                           (t
                            ;; -1 = EWOULDBLOCK, possibly spurious wakeup
                            ;;  0 = normal wakeup
                            ;;  2 = EINTR, a spurious wakeup
                            :ok)))))
                #-sb-futex
                (t
                 (%with-cas-lock ((waitqueue-%owner queue))
                   (%waitqueue-enqueue me queue))
                 (release-mutex mutex)
                 (setf status
                       (or (flet ((wakeup ()
                                    (barrier (:read))
                                    (unless (eq queue (thread-waiting-for me))
                                      :ok)))
                             (declare (dynamic-extent #'wakeup))
                             (allow-with-interrupts
                               (%%wait-for #'wakeup stop-sec stop-usec)))
                           :timeout))))
           #-sb-futex
           (%with-cas-lock ((waitqueue-%owner queue))
             (if (eq queue (thread-waiting-for me))
                 (%waitqueue-drop me queue)
                 (unless (eq :ok status)
                   ;; CONDITION-NOTIFY thinks we've been woken up, but really
                   ;; we're unwinding. Wake someone else up.
                   (%waitqueue-wakeup queue 1))))
           ;; Update timeout for mutex re-aquisition unless we are
           ;; already past the requested timeout.
           (when (and (eq :ok status) to-sec)
             (setf (values to-sec to-usec)
                   (sb-impl::relative-decoded-times stop-sec stop-usec))
             (when (and (zerop to-sec) (not (plusp to-usec)))
               (setf status :timeout)))
           ;; If we ran into deadline, try to get the mutex before
           ;; signaling. If we don't unwind it will look like a normal
           ;; return from user perspective.
           (when (and (eq :timeout status) deadlinep)
             (let ((got-it (%try-mutex mutex)))
               (allow-with-interrupts
                 (signal-deadline)
                 (cond (got-it
                        (return-from %condition-wait t))
                       (t
                        ;; The deadline may have changed.
                        (setf (values to-sec to-usec stop-sec stop-usec deadlinep)
                              (decode-timeout timeout))
                        (setf status :ok))))))
           ;; Re-acquire the mutex for normal return.
           (when (eq :ok status)
             (unless (or (%try-mutex mutex)
                         (allow-with-interrupts
                           (funcall (if check-deadlock #'deadlock-aware-mutex-wait #'mutex-wait)
                                            mutex timeout to-sec to-usec
                                            stop-sec stop-usec deadlinep)))
               (setf status :timeout))))
          ;; Unwinding because futex-wait and %wait-for-mutex above
          ;; allow interrupts, wake up another futex
          (with-pinned-objects (queue)
            (futex-wake (waitqueue-token-address queue) 1))))
      ;; Determine actual return value. :ok means (potentially
      ;; spurious) wakeup => T. :timeout => NIL.
      (case status
        (:ok
         (if timeout
             (multiple-value-bind (sec usec)
                 (sb-impl::relative-decoded-times stop-sec stop-usec)
               (values t sec usec))
             t))
        (:timeout
         nil)
        (t
         ;; The only case we return normally without re-acquiring
         ;; the mutex is when there is a :TIMEOUT that runs out.
         (bug "%CONDITION-WAIT: invalid status on normal return: ~S" status))))))

(sb-c::define-source-transform condition-wait (queue mutex &key timeout &environment env)
  ;; As with mutexes, a timeout may make deadlock detection irrelevant
  (if (or timeout (deadlock-detection-policy-p env))
      `(values (deadlock-aware-condvar-wait ,queue ,mutex ,timeout))
      `(values (condvar-wait ,queue ,mutex))))
(defun deadlock-aware-condvar-wait (queue mutex timeout)
  (multiple-value-call #'%condition-wait queue mutex t timeout (decode-timeout timeout)))
(defun condvar-wait (queue mutex)
  (multiple-value-call #'%condition-wait queue mutex nil nil (decode-timeout nil)))

(declaim (ftype (sfunction (waitqueue mutex &key (:timeout (or null (real 0)))) boolean) condition-wait))
(defun condition-wait (queue mutex &key timeout)
  "Atomically release MUTEX and start waiting on QUEUE until another thread
wakes us up using either CONDITION-NOTIFY or CONDITION-BROADCAST on
QUEUE, at which point we re-acquire MUTEX and return T.

Spurious wakeups are possible.

If TIMEOUT is given, it is the maximum number of seconds to wait,
including both waiting for the wakeup and the time to re-acquire
MUTEX. When neither a wakeup nor a re-acquisition occurs within the
given time, returns NIL without re-acquiring MUTEX.

If CONDITION-WAIT unwinds, it may do so with or without MUTEX being
held.

Important: Since CONDITION-WAIT may return without CONDITION-NOTIFY or
CONDITION-BROADCAST having occurred, the correct way to write code
that uses CONDITION-WAIT is to loop around the call, checking the
associated data:

  (defvar *data* nil)
  (defvar *queue* (make-waitqueue))
  (defvar *lock* (make-mutex))

  ;; Consumer
  (defun pop-data (&optional timeout)
    (with-mutex (*lock*)
      (loop until *data*
            do (or (condition-wait *queue* *lock* :timeout timeout)
                   ;; Lock not held, must unwind without touching *data*.
                   (return-from pop-data nil)))
      (pop *data*)))

  ;; Producer
  (defun push-data (data)
    (with-mutex (*lock*)
      (push data *data*)
      (condition-notify *queue*)))
"
  ;; %CONDITION-WAIT can return 3 values. In most situations the values are never used,
  ;; but the semaphore implementation uses them.
  (values (deadlock-aware-condvar-wait queue mutex timeout)))

(declaim (ftype (sfunction (waitqueue &optional (and fixnum (integer 1))) null)
                condition-notify))
(defun condition-notify (queue &optional (n 1))
  "Notify N threads waiting on QUEUE.

IMPORTANT: The same mutex that is used in the corresponding CONDITION-WAIT
must be held by this thread during this call."
  #-sb-thread
  (declare (ignore queue n))
  #-sb-thread
  (error "Not supported in unithread builds.")
  #+sb-thread
  (cond
   #+sb-futex
   (t
      ;; No problem if >1 thread notifies during the comment in condition-wait:
      ;; as long as the value in queue-data isn't the waiting thread's id, it
      ;; matters not what it is. We rely on kernel thread ID being nonzero.
      ;;
      ;; XXX we should do something to ensure that the result of this setf
      ;; is visible to all CPUs.
      ;;
      ;; ^-- surely futex_wake() involves a memory barrier?
      (setf (waitqueue-token queue) 0)
      (with-pinned-objects (queue)
        (futex-wake (waitqueue-token-address queue) n))
      nil)
   #-sb-futex
   (t
    (with-cas-lock ((waitqueue-%owner queue))
      (%waitqueue-wakeup queue n)))))


(declaim (ftype (sfunction (waitqueue) null) condition-broadcast))
(defun condition-broadcast (queue)
  "Notify all threads waiting on QUEUE.

IMPORTANT: The same mutex that is used in the corresponding CONDITION-WAIT
must be held by this thread during this call."
  (condition-notify queue
                    ;; On a 64-bit platform truncating M-P-F to an int
                    ;; results in -1, which wakes up only one thread.
                    (ldb (byte 29 0)
                         most-positive-fixnum)))


;;;; Semaphores

(defun make-semaphore (&key name (count 0))
  "Create a semaphore with the supplied COUNT and NAME."
  (declare (inline make-mutex make-waitqueue))
  (%make-semaphore count
                   (make-mutex :name name)
                   (make-waitqueue :name name)))

(defun semaphore-name (semaphore)
  "The name of the semaphore INSTANCE. Setfable."
  (waitqueue-name (semaphore-queue semaphore)))

(defun (setf semaphore-name) (newval semaphore)
  (setf (waitqueue-name (semaphore-queue semaphore)) newval))

(defstruct (semaphore-notification (:constructor make-semaphore-notification ())
                                   (:copier nil))
  "Semaphore notification object. Can be passed to WAIT-ON-SEMAPHORE and
TRY-SEMAPHORE as the :NOTIFICATION argument. Consequences are undefined if
multiple threads are using the same notification object in parallel."
  (%status nil :type boolean))
(declaim (sb-ext:freeze-type semaphore-notification))

(setf (documentation 'make-semaphore-notification 'function)
      "Constructor for SEMAPHORE-NOTIFICATION objects. SEMAPHORE-NOTIFICATION-STATUS
is initially NIL.")

(declaim (inline semaphore-notification-status))
(defun semaphore-notification-status (semaphore-notification)
  "Returns T if a WAIT-ON-SEMAPHORE or TRY-SEMAPHORE using
SEMAPHORE-NOTIFICATION has succeeded since the notification object was created
or cleared."
  (barrier (:read))
  (semaphore-notification-%status semaphore-notification))

(declaim (inline clear-semaphore-notification))
(defun clear-semaphore-notification (semaphore-notification)
  "Resets the SEMAPHORE-NOTIFICATION object for use with another call to
WAIT-ON-SEMAPHORE or TRY-SEMAPHORE."
  (barrier (:write)
    (setf (semaphore-notification-%status semaphore-notification) nil)))

(declaim (inline semaphore-count))
(defun semaphore-count (instance)
  "Returns the current count of the semaphore INSTANCE."
  (barrier (:read))
  (semaphore-%count instance))

(declaim (ftype (sfunction (semaphore (integer 1) (or boolean real)
                            (or null semaphore-notification) symbol)
                           t)
                %decrement-semaphore))
(defun %decrement-semaphore (semaphore n wait notification context)
  (when (and notification (semaphore-notification-status notification))
    (with-simple-restart (continue "Clear notification status and continue.")
      (error "~@<Semaphore notification object status not cleared on ~
              entry to ~S on ~S.~:@>"
             context semaphore))
    (clear-semaphore-notification notification))

  ;; A more direct implementation based directly on futexes should be
  ;; possible.
  ;;
  ;; We need to disable interrupts so that we don't forget to
  ;; decrement the waitcount (which would happen if an asynch
  ;; interrupt should catch us on our way out from the loop.)
  ;;
  ;; FIXME: No timeout on initial mutex acquisition.
  (with-system-mutex ((semaphore-mutex semaphore) :allow-with-interrupts t)
    (flet ((success (new-count)
             (prog1
                 (setf (semaphore-%count semaphore) new-count)
               (when notification
                 (setf (semaphore-notification-%status notification) t)))))
      ;; Quick check: can we decrement right away? If not, return or
      ;; enter the wait loop.
      (cond
        ((let ((old-count (semaphore-%count semaphore)))
           (when (>= old-count n)
             (success (- old-count n)))))
        ((not wait)
         nil)
        (t
         (unwind-protect
              (binding* ((old-count nil)
                         (timeout (when (realp wait) wait))
                         ((to-sec to-usec stop-sec stop-usec deadlinep)
                          (when wait
                            (decode-timeout timeout))))
                ;; Need to use ATOMIC-INCF despite the lock, because
                ;; on our way out from here we might not be locked
                ;; anymore -- so another thread might be tweaking this
                ;; in parallel using ATOMIC-DECF. No danger over
                ;; overflow, since there it at most one increment per
                ;; thread waiting on the semaphore.
                (sb-ext:atomic-incf (semaphore-waitcount semaphore))
                (loop until (>= (setf old-count (semaphore-%count semaphore)) n)
                   do (multiple-value-bind (wakeup-p remaining-sec remaining-usec)
                          (%condition-wait
                           (semaphore-queue semaphore)
                           (semaphore-mutex semaphore)
                           ;; I would think deadlock detection can be skipped. The mutex
                           ;; is not visible to clients of the semaphore
                           t
                           timeout to-sec to-usec stop-sec stop-usec deadlinep)
                        (when (or (not wakeup-p)
                                  (and (eql remaining-sec 0)
                                       (eql remaining-usec 0)))
                          (return-from %decrement-semaphore nil)) ; timeout
                        (when remaining-sec
                          (setf to-sec remaining-sec
                                to-usec remaining-usec))))
                (success (- old-count n)))
           ;; Need to use ATOMIC-DECF as we may unwind without the
           ;; lock being held!
           (sb-ext:atomic-decf (semaphore-waitcount semaphore))))))))

(declaim (ftype (sfunction (semaphore &key
                                      (:n (integer 1))
                                      (:timeout (real (0)))
                                      (:notification semaphore-notification))
                           (or null (integer 0)))
                wait-on-semaphore))
(defun wait-on-semaphore (semaphore &key (n 1) timeout notification)
  "Decrement the count of SEMAPHORE by N if the count would not be negative.

Else blocks until the semaphore can be decremented. Returns the new count of
SEMAPHORE on success.

If TIMEOUT is given, it is the maximum number of seconds to wait. If the count
cannot be decremented in that time, returns NIL without decrementing the
count.

If NOTIFICATION is given, it must be a SEMAPHORE-NOTIFICATION object whose
SEMAPHORE-NOTIFICATION-STATUS is NIL. If WAIT-ON-SEMAPHORE succeeds and
decrements the count, the status is set to T."
  (%decrement-semaphore
   semaphore n (or timeout t) notification 'wait-on-semaphore))

(declaim (ftype (sfunction (semaphore &optional
                                      (integer 1) semaphore-notification)
                           (or null (integer 0)))
                try-semaphore))
(defun try-semaphore (semaphore &optional (n 1) notification)
  "Try to decrement the count of SEMAPHORE by N. If the count were to
become negative, punt and return NIL, otherwise return the new count of
SEMAPHORE.

If NOTIFICATION is given it must be a semaphore notification object
with SEMAPHORE-NOTIFICATION-STATUS of NIL. If the count is decremented,
the status is set to T."
  (%decrement-semaphore semaphore n nil notification 'try-semaphore))

(declaim (ftype (sfunction (semaphore &optional (integer 1)) null) signal-semaphore))
(defun signal-semaphore (semaphore &optional (n 1))
  "Increment the count of SEMAPHORE by N. If there are threads waiting
on this semaphore, then N of them is woken up."
  (declare (type (integer 1) n))
  ;; Need to disable interrupts so that we don't lose a wakeup after
  ;; we have incremented the count.
  (with-system-mutex ((semaphore-mutex semaphore) :allow-with-interrupts t)
    (let ((waitcount (semaphore-waitcount semaphore))
          (count (incf (semaphore-%count semaphore) n)))
      (when (plusp waitcount)
        (condition-notify (semaphore-queue semaphore) (min waitcount count))))))


;;;; Job control, independent listeners

(defstruct (session (:copier nil))
  ;; New threads are atomically pushed into NEW-ENROLLEES without acquiring the
  ;; session lock. Any operation on the session that transfers ownership of the
  ;; foreground must move the enrollees into THREADS while holding the lock.
  (new-enrollees)
  (lock (make-mutex :name "session lock"))
  ;; If we wanted to get fancy, these next 2 lists might become lockfree linked lists
  ;; so that it would be possible for threads to exit without acquiring the lock.
  ;; It might be tricky (i.e. too much trouble) to figure out how to reimplement
  ;; the various operations on a session though.
  (threads nil)
  (interactive-threads nil)
  (interactive-threads-queue (make-waitqueue :name "session")))
(declaim (sb-ext:freeze-type session))

;;; The debugger itself tries to acquire the session lock, don't let
;;; funny situations (like getting a sigint while holding the session
;;; lock) occur. At the same time we need to allow interrupts while
;;; *waiting* for the session lock for things like GET-FOREGROUND to
;;; be interruptible.
;;;
;;; FIXME: It might be good to have a way to enforce lock ordering invariants
(defmacro with-session-lock ((session) &body body)
  `(with-system-mutex ((session-lock ,session) :allow-with-interrupts t)
     (%enroll-new-threads ,session)
     ,@body))

;;; Move new enrollees into SESSION-THREADS. The session lock must be held.
;;; The reason this can be lazy is that a thread is never an interactive thread
;;; just by joining a session, so it doesn't involve itself with the waitqueue;
;;; it can't cause a thread waiting on the condition to wake.
;;; i.e. even if a newly created thread were required to obtain the lock to insert
;;; itself into the session, it would not and could not have any effect on any other
;;; thread in the session.
(defun %enroll-new-threads (session)
  (declare (sb-c::tlab :system))
  (loop (let ((thread (sb-ext:atomic-pop (session-new-enrollees session))))
          (cond ((not thread) (return))
                ((thread-alive-p thread)
                 ;; Can it become dead immediately upon insertion into THREADS?
                 ;; No, because to become dead, it must acquire the session lock.
                 (push thread (session-threads session)))))))

(defun new-session (thread)
  (make-session :threads (list thread)
                :interactive-threads (list thread)))

(defun %delete-thread-from-session (thread &aux (session *session*))
  (with-session-lock (session)
    ;; One of two things about THREAD must be true, either:
    ;; - it was transferred from SESSION-NEW-ENROLLEES to SESSION-THREADS
    ;; - it was NOT yet transferred from SESSION-NEW-ENROLLEES.
    ;; There can't be an "in flight" state of having done the atomic-pop from
    ;; SESSION-NEW-ENROLLEES but not the push into THREADS, because anyone manipulating
    ;; the THREADS list must be holding the session lock.
    (let ((was-foreground (eq thread (foreground-thread session))))
      (setf (session-threads session) (delq1 thread (session-threads session))
            (session-interactive-threads session)
            (delq1 thread (session-interactive-threads session)))
      (when was-foreground
        (condition-notify (session-interactive-threads-queue session))))))

(defun call-with-new-session (fn)
  (%delete-thread-from-session *current-thread*)
  (let ((*session* (new-session *current-thread*)))
    (funcall fn)))

(defmacro with-new-session (args &body forms)
  (declare (ignore args))               ;for extensibility
  `(call-with-new-session (lambda () ,@forms)))

;;; WITH-DEATHLOK ensures that the 'struct thread' and/or OS thread won't go away
;;; by synchronizing with HANDLE-THREAD-EXIT.
(defmacro with-deathlok ((thread &optional c-thread) &body body)
  `(with-system-mutex ((thread-interruptions-lock ,thread))
     ,@(if c-thread
           `((let ((,c-thread (thread-primitive-thread ,thread))) ,@body))
           body)))

(sb-ext:define-load-time-global *sprof-data* nil)
#+allocator-metrics
(sb-ext:define-load-time-global *allocator-metrics* nil)

(defvar sb-ext:*invoke-debugger-hook* nil
  "This is either NIL or a designator for a function of two arguments,
   to be run when the debugger is about to be entered.  The function is
   run with *INVOKE-DEBUGGER-HOOK* bound to NIL to minimize recursive
   errors, and receives as arguments the condition that triggered
   debugger entry and the previous value of *INVOKE-DEBUGGER-HOOK*

   This mechanism is an SBCL extension similar to the standard *DEBUGGER-HOOK*.
   In contrast to *DEBUGGER-HOOK*, it is observed by INVOKE-DEBUGGER even when
   called by BREAK.")

(defun %exit-other-threads ()
  (with-deadline (:seconds nil :override t)
    ;; Grabbing this lock prevents new threads from
    ;; being spawned, and guarantees that *ALL-THREADS*
    ;; is up to date.
    (grab-mutex *make-thread-lock*)
    #+sb-thread (sb-impl::finalizer-thread-stop)
    (let ((timeout sb-ext:*exit-timeout*)
          (code *exit-in-progress*)
          (current *current-thread*)
          (joinees nil)
          (main nil))
      ;; Don't invoke the debugger on errors in cleanup forms in unwind-protect
      (setf sb-ext:*invoke-debugger-hook*
            (lambda (c h)
              (sb-debug::debugger-disabled-hook c h :quit nil)
              (abort-thread :allow-exit t)))
      (dolist (thread (list-all-threads))
        (cond ((eq thread current))
              ((main-thread-p thread)
               (setf main thread))
              (t
               (handler-case
                   (progn
                     (terminate-thread thread)
                     (push thread joinees))
                 (interrupt-thread-error ())))))
      (with-progressive-timeout (time-left :seconds timeout)
        (dolist (thread joinees)
          (join-thread thread :default t :timeout (time-left)))
        ;; Need to defer till others have joined, because when main
        ;; thread exits, we're gone. Can't use TERMINATE-THREAD -- would
        ;; get the exit code wrong.
        ;; This isn't a problem if *forcibly-terminate-threads-on-exit* is NIL -
        ;; the thread that calls EXIT provides the exit code. It's that simple.
        (when main
          (handler-case
              (interrupt-thread
               main
               (lambda ()
                 (setf *exit-in-progress* (list code))
                 (throw 'sb-impl::%end-of-the-world t)))
            (interrupt-thread-error ()))
          ;; Normally this never finishes, as once the main-thread unwinds we
          ;; exit with the right code, but if times out before that happens,
          ;; we will exit after returning -- or rathe racing the main thread
          ;; to calling OS-EXIT.
          (join-thread main :default t :timeout (time-left)))))))

;;; Pretty much don't use this.  It has the same problems as %EXIT-OTHER-THREADS.
(defun terminate-session ()
  "Kill all threads in session except for this one.  Does nothing if current
thread is not the foreground thread."
  ;; FIXME: threads created in other threads may escape termination
  (let* ((session *session*)
         (to-kill (with-session-lock (session)
                    (and (eq *current-thread* (foreground-thread session))
                         (session-threads session)))))
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
  #-sb-thread nil
  #+sb-thread
  (prog1
      (with-session-lock (*session*)
        (let ((foreground (foreground-thread)))
          (unless (or (null foreground)
                      (eq foreground *current-thread*))
            (format *error-output* "~%The current thread is not at the foreground,~@
SB-THREAD:RELEASE-FOREGROUND has to be called in ~s~%for this thread to enter the debugger.~%"
                    foreground))
          (not (member *current-thread*
                       (session-interactive-threads *session*)))))
    (get-foreground)))

(defun get-foreground ()
  #-sb-thread t
  #+sb-thread
  (let ((session *session*)
        (was-foreground t))
    (loop
     (/show0 "Looping in GET-FOREGROUND")
     (with-session-lock (session)
       (symbol-macrolet
           ((interactive-threads (session-interactive-threads session)))
         (cond
           ((null interactive-threads)
            (setf was-foreground nil
                  interactive-threads (list *current-thread*)))
           ((not (eq (first interactive-threads) *current-thread*))
            (setf was-foreground nil)
            (unless (member *current-thread* interactive-threads)
              (setf interactive-threads
                    (append interactive-threads (list *current-thread*))))
            (condition-wait
             (session-interactive-threads-queue session)
             (session-lock session)))
           (t
            (unless was-foreground
              (format *query-io* "Resuming thread ~A~%" *current-thread*))
            (return-from get-foreground t))))))))

(defun release-foreground (&optional next)
  "Background this thread.  If NEXT is supplied, arrange for it to
have the foreground next."
  #-sb-thread (declare (ignore next))
  #-sb-thread nil
  #+sb-thread
  (let ((session *session*))
    (with-session-lock (session)
      (symbol-macrolet
          ((interactive-threads (session-interactive-threads session)))
        (setf interactive-threads
              (delete *current-thread* interactive-threads))
        (when (and next (thread-alive-p next))
          (setf interactive-threads
                (list* next (delete next interactive-threads))))
        (condition-notify (session-interactive-threads-queue session))))))

(defun interactive-threads (&optional (session *session*))
  "Return the interactive threads of SESSION defaulting to the current
session."
  (session-interactive-threads session))

(defun foreground-thread (&optional (session *session*))
  "Return the foreground thread of SESSION defaulting to the current
session."
  (first (interactive-threads session)))

#-win32
(defun make-listener-thread (tty-name)
  (aver (probe-file tty-name))
  (let* ((in (sb-unix:unix-open (namestring tty-name) sb-unix:o_rdwr #o666))
         (out (sb-unix:unix-dup in))
         (err (sb-unix:unix-dup in)))
    (labels ((thread-repl ()
               (sb-unix::unix-setsid)
               (let* ((sb-impl::*stdin*
                        (make-fd-stream in :input t :buffering :line
                                           :dual-channel-p t))
                      (sb-impl::*stdout*
                        (make-fd-stream out :output t :buffering :line
                                            :dual-channel-p t))
                      (sb-impl::*stderr*
                        (make-fd-stream err :output t :buffering :line
                                            :dual-channel-p t))
                      (sb-impl::*tty*
                        (make-fd-stream err :input t :output t
                                            :buffering :line
                                            :dual-channel-p t))
                      (sb-impl::*descriptor-handlers* nil))
                 (with-new-session ()
                   (unwind-protect
                        (sb-impl::toplevel-repl nil)
                     (flush-standard-output-streams))))))
      (make-thread #'thread-repl))))


;;;; The beef
#+sb-thread
(progn
;;; Return T if the thread was created
(defun os-thread-create (thread thread-sap)
  (aver (memq thread *starting-threads*))
  #+(or arm mips)
  (bug "This and many other things will crash on MIPS/ARM either
  until they do linkage tables like everyone else, or until all those
  things can deal with either way of doing linkage.")
  ;; New thread's arena starts out as this thread's arena.
  (setf (sap-ref-sap thread-sap (ash sb-vm::thread-arena-slot sb-vm:word-shift))
        (sb-vm::current-thread-offset-sap sb-vm::thread-arena-slot))
  #+win32
  (/= 0 (alien-funcall (extern-alien "create_thread"
                                     (function unsigned system-area-pointer))
                       thread-sap))
  #-win32
  (let ((attr (foreign-symbol-sap "new_lisp_thread_attr" t))
        (c-tramp
          (foreign-symbol-sap #+os-thread-stack "new_thread_trampoline_switch_stack"
                              #-os-thread-stack "new_thread_trampoline")))
    (and (= 0 #+os-thread-stack
              (alien-funcall (extern-alien "pthread_attr_setstacksize"
                                           (function int system-area-pointer unsigned))
                             attr sb-unix::pthread-min-stack)
              #-os-thread-stack
              (with-alien ((setstack (function int system-area-pointer system-area-pointer
                                               unsigned) :extern "pthread_attr_setstack"))
                #+c-stack-is-control-stack
                (alien-funcall setstack attr
                               (sap-ref-sap thread-sap (ash sb-vm::thread-control-stack-start-slot
                                                            sb-vm:word-shift))
                               (extern-alien "thread_control_stack_size" unsigned))
                #-c-stack-is-control-stack
                (alien-funcall setstack attr
                               (sap-ref-sap thread-sap (ash sb-vm::thread-alien-stack-start-slot
                                                            sb-vm:word-shift))
                               (extern-alien "thread_alien_stack_size" unsigned))))
         ;; From "Workaround a problem ... on NetBSD" (git rev af8c4a2933)
         ;; If I had to guess, it wrongly assumed existence of memory outside of
         ;; the exactly provided stack bounds.
         #+netbsd
         (= 0 (with-alien ((setguard (function int system-area-pointer unsigned)
                                     :extern "pthread_attr_setguardsize"))
                (alien-funcall setguard attr 0)))
         (with-pinned-objects (thread)
           (= 0 (alien-funcall
                 (extern-alien "pthread_create"
                               (function int system-area-pointer system-area-pointer
                                         system-area-pointer system-area-pointer))
                 (struct-slot-sap thread thread os-thread) attr c-tramp thread-sap))))))

(defmacro free-thread-struct (memory)
  `(alien-funcall (extern-alien "free_thread_struct" (function void system-area-pointer))
                 ,memory))

;;; The "funny fixnum" address format would do no good - AVL-FIND and AVL-DELETE
;;; expect normal happy lisp integers, even if a bignum.
(defun delete-from-all-threads (addr)
  (declare (type sb-vm:word addr))
  (barrier (:read))
  (let ((old *all-threads*))
    (loop
      (aver (avl-find addr old))
      (let ((new (avl-delete addr old)))
        (when (eq old (setq old (sb-ext:cas *all-threads* old new)))
          (return))))))

(defun primitive-join (thread dispose)
  ;; It's safe to read from the other thread's memory, because the current thread
  ;; has ownership of that memory now. And we can't call this on a FOREIGN-THREAD.
  (let ((c-thread (descriptor-sap (thread-startup-info thread))))
    (setf (thread-startup-info thread) 0)
    ;; Clean up *ALL-THREADS*
    (delete-from-all-threads (sap-int c-thread))
    ;; Release the OS and/or pthread resources
    #+win32
    (with-alien ((wait (function unsigned unsigned unsigned) :extern "WaitForSingleObject")
                 (close (function int unsigned) :extern "CloseHandle"))
      (let ((os-thread (sap-ref-word c-thread
                                     (ash sb-vm::thread-os-thread-slot sb-vm::word-shift))))
        (alien-funcall wait os-thread #xffffffff)
        (alien-funcall close os-thread)))
    #-win32
    (with-alien ((join (function int unsigned unsigned) :extern "pthread_join"))
      (alien-funcall join (thread-os-thread thread) 0) ; no result pointer
      (setf (thread-os-thread thread) 0))
    (cond (dispose
           (free-thread-struct c-thread)
           nil)
          (t ; Return the originally mapped address
           (sap-ref-sap c-thread (ash sb-vm::thread-os-address-slot sb-vm:word-shift))))))

;;; Helper for SB-POSIX:FORK so that the child starts with no joinable threads.
;;; It might work to just set *JOINABLE-THREADS* to NIL in the child, but it's better to prune
;;; the *ALL-THREADS* tree as well. Must be called with the *MAKE-THREAD-LOCK* held
;;; or interrupts inhibited or both.
;;; Heuristic decides when to stop trying to free. Passing in #'IDENTITY means that
;;; all joinables should be processed. Passing in #'CDR or #'CDDR returns early if there
;;; are not at least 1 or 2 threads respectively that could be joined.
(export '%dispose-thread-structs)
(defun %dispose-thread-structs (&key (retain 0))
  (loop (unless (nthcdr retain *joinable-threads*) (return))
        (let ((item (sb-ext:atomic-pop *joinable-threads*)))
          (if item (primitive-join item t) (return)))))

;;; Allocate lisp thread memory, attempting first to join any exited
;;; threads, freeing their memory. Possibly reuse the memory from one of
;;; the exited threads,
;;; Why do it this way instead of having JOIN-THREAD just defer to pthread_join() ?
;;; Because the interface would be less lispy. e.g. what happens if you don't join
;;; a thread - do you leak the memory?; That's bad. GC doesn't clean up threads.
;;; It could if we used finalizers, but finalizers have an additional problem:
;;; if the user does call JOIN-THREAD then the finalizer should do nothing.
;;; But there's no "atomic pthread_join + cancel-finalization", short of blocking
;;; signals around the join perhaps.
;;; One more thing- it's illegal to pthread_join() a thread more than once,
;;; but we allow JOIN-THREAD more than one. I think that's a bug.
;;; Ours has the meaning of "get result if done, otherwise wait"
;;; which is not the same as deallocation of the thread's OS resources.
(defun allocate-thread-memory ()
  (let ((reuse (let ((corpse (sb-ext:atomic-pop *joinable-threads*)))
                 (when corpse (primitive-join corpse nil)))))
    ;; If there is more than 1 more joinable, join all but 1.
    ;; Two threads could both find > 1 thread to join, and both do
    ;; a join, leaving 0 to join. That's ok.
    (%dispose-thread-structs :retain 1)
    (let ((thread-sap (alien-funcall (extern-alien "alloc_thread_struct"
                                                   (function system-area-pointer
                                                             system-area-pointer))
                                     (or reuse (int-sap 0)))))
      (when (and (not reuse) (/= (sap-int thread-sap) 0))
        ;; these would have been done already if reusing the memory
        ;; of a completed thread.
        (macrolet ((prot (fun)
                     `(alien-funcall (extern-alien ,fun (function void int
                                                                  system-area-pointer))
                                     1 thread-sap)))
          #-win32
          (prot "protect_control_stack_guard_page")
          (prot "protect_binding_stack_guard_page")
          (prot "protect_alien_stack_guard_page")))
      (unless (= (sap-int thread-sap) 0) thread-sap))))

;;; Remove thread from its session, if it has one, and from *all-threads*.
;;; Also clobber the pointer to the primitive thread
;;; which makes THREAD-ALIVE-P return false hereafter.
(defmacro handle-thread-exit ()
  '(let* ((thread *current-thread*)
           ;; use the "funny fixnum" representation
           (c-thread (%make-lisp-obj (thread-primitive-thread thread)))
           (sem (thread-semaphore thread)))
      ;; This AVER failed when I messed up deletion from *STARTING-THREADS*.
      ;; That in turn caused a failure in GC because a fixnum is not a legal value
      ;; for the startup info when observed by GC.
      (aver (not (memq thread *starting-threads*)))
      ;; System threads exit peacefully when asked, and they don't bother anyone.
      ;; They must not participate in shutting other threads down.
      (when (and *exit-in-progress* (not (thread-ephemeral-p thread)))
        (%exit))
      ;; If collecting allocator metrics, transfer them to the global list
      ;; so that we can summarize over exited threads.
      #+allocator-metrics
      (let ((metrics (cons (thread-name thread) (allocator-histogram))))
        (sb-ext:atomic-push metrics *allocator-metrics*))
      ;; Stash the primitive thread SAP for reuse, but clobber the PRIMITIVE-THREAD
      ;; slot which makes ALIVE-P return NIL.
      ;; A minor TODO: can this lock acquire/release be moved to where we actually
      ;; unmap the memory an do a pthread_join()? I would think so, because until then,
      ;; there is no real harm in reading the memory.  In this state the pthread library
      ;; will usually return ESRCH if you try to use the pthread id - it's a valid
      ;; pointer, but it knows that it has no underlying OS thread.
      (with-deathlok (thread)
        (when sem ; ordinary lisp thread, not FOREIGN-THREAD
          (setf (thread-startup-info thread) c-thread))
        ;; Accept no further interruptions. Other threads can't add new ones to the queue
        ;; as doing so requires grabbing the per-thread mutex which we currently own.
        ;; Deferrable signals are masked at this point, but it is best to tidy up
        ;; any stray data such as captured closure values.
        (setf (thread-interruptions thread) nil
              (thread-primitive-thread thread) 0)
        (setf (sap-ref-8 (current-thread-sap) ; state_word.sprof_enable
                         (1+ (ash sb-vm:thread-state-word-slot sb-vm:word-shift)))
              0)
        ;; Take ownership of our statistical profiling data and transfer the results to
        ;; the global pool. This doesn't need to synchronize with the signal handler,
        ;; which is effectively disabled now, but does synchronize via the interruptions
        ;; mutex with any other thread trying to read this thread's data.
        (let ((sprof-data (sb-vm::current-thread-offset-sap sb-vm:thread-sprof-data-slot)))
          (unless (= (sap-int sprof-data) 0)
            (setf (sap-ref-word (descriptor-sap c-thread)
                                (ash sb-vm:thread-sprof-data-slot sb-vm:word-shift))
                  0)
            ;; Operation on the global list must be atomic.
            (sb-ext:atomic-push (cons sprof-data thread) *sprof-data*)))
        (barrier (:write)))
      ;; After making the thread dead, remove from session. If this were done first,
      ;; we'd just waste time moving the thread into SESSION-THREADS (if it wasn't there)
      ;; only to remove it right away.
      (when *session*
        (%delete-thread-from-session thread))
      (cond
        ;; If possible, logically remove from *ALL-THREADS* by flipping a bit.
        ;; Foreign threads remove themselves. They don't have an exit semaphore,
        ;; so that's how we know which is which.
        (sem
         ;; Tree pruning is the responsibility of thread creators, not dying threads.
         ;; Creators have to manipulate the tree anyway, and they need access to the old
         ;; structure to grab the memory.
         (let ((old (sb-ext:cas (thread-%visible thread) 1 -1)))
           ;; now (LIST-ALL-THREADS) won't see it
           (aver (eql old 1)))
         (locally (declare (sb-c::tlab :system))
           (sb-ext:atomic-push thread *joinable-threads*)))
        (t ; otherwise, physically remove from *ALL-THREADS*
         ;; The memory allocation/deallocation is handled in C.
         ;; I would like to combine the recycle bin for foreign and lisp threads though.
         (delete-from-all-threads (get-lisp-obj-address c-thread))))
      (when sem
        (setf (thread-semaphore thread) nil) ; nobody needs to wait on it now
        ;;
        ;; We go out of our way to support something pthreads don't:
        ;;  "The results of multiple simultaneous calls to pthread_join()
        ;;   specifying the same target thread are undefined."
        ;;   - https://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_join.html
        ;; and for std::thread
        ;;   "No synchronization is performed on *this itself. Concurrently calling join()
        ;;    on the same thread object from multiple threads constitutes a data race
        ;;    that results in undefined behavior."
        ;;   - https://en.cppreference.com/w/cpp/thread/thread/join
        ;; That's because (among other reasons), pthread_join deallocates memory.
        ;; But in so far as our join does not equate to resource freeing, and our exit flag is
        ;; our own kind of semaphore, we simply signal it using an arbitrarily huge count.
        ;; See the comment in 'thread-structs.lisp' about why this isn't CONDITION-BROADCAST
        ;; on a condition var. (Good luck trying to make this many threads)
        (signal-semaphore sem 1000000))))

(defun run (); All threads other than the initial thread start via this function.
  (set-thread-control-stack-slots *current-thread*)
  (let ((name (thread-%name *current-thread*)))
    (try-set-os-thread-name name))
  (flet ((unmask-signals ()
           (let ((mask (svref (thread-startup-info *current-thread*) 4)))
                  (if mask
                      ;; If the original mask (at thread creation time) was provided,
                      ;; then restore exactly that mask.
                      (with-pinned-objects (mask)
                        (sb-unix::pthread-sigmask sb-unix::SIG_SETMASK mask nil))
                      ;; Otherwise just do the usual thing
                      (sb-unix::pthread-sigmask sb-unix::SIG_UNBLOCK
                                                (foreign-symbol-sap "thread_start_sigset" t)
                                                nil)))))
    ;; notinline keeps array off the call stack by getting it out of the curent frame
    (declare (notinline unmask-signals))
    ;; Signals other than stop-for-GC  are masked. The WITH/WITHOUT noise is pure cargo-cultism.
    ;; When I tried removing them, some regression test failed. I have no idea why.
    ;; Honestly I don't think anybody knows, or certainly not anybody who cared enough to write
    ;; down something about how having these here addresses an edge case involving interrupts.
    (without-interrupts
     (with-local-interrupts
    ;; Using handling-end-of-the-world would be a bit tricky
    ;; due to other catches and interrupts, so we essentially
    ;; re-implement it here. Once and only once more.
       (catch 'sb-impl::toplevel-catcher
       (catch 'sb-impl::%end-of-the-world
       (catch '%abort-thread
          (restart-bind ((abort
                           (lambda ()
                             (throw '%abort-thread nil))
                           :report-function
                           (lambda (stream)
                             (format stream "~@<abort thread (~a)~@:>"
                                     *current-thread*))))
            (without-interrupts
              (unwind-protect
                   (with-local-interrupts
                     ;; New threads should start with SIGPROF blocked if profiling is enabled
                     ;; on just a subset of threads. Ideally we'd add to the blocked mask
                     ;; right now, but it suffices to just leave the enabling bit at its default
                     ;; of 0; at worst, one undesired signal would be received.
                     (when (eq *profiled-threads* :all)
                       (setf (sap-ref-8 (current-thread-sap) ; state_word.sprof_enable
                                        (1+ (ash sb-vm:thread-state-word-slot sb-vm:word-shift)))
                             1))
                     (unmask-signals)
                     (let ((list
                             (multiple-value-call #'sys-tlab-list
                                (unwind-protect
                                     (catch '%return-from-thread
                                       (sb-c::inspect-unwinding
                                        ;; STARTUP-INFO =
                                        ;;   #(c-trampoline starting-thread-cell func args sigmask fp-modes)
                                        ;; Clobbering STARTUP-INFO prevents garbage retention,
                                        ;; but is there some significance to using the value 0?
                                        (apply (svref (thread-startup-info *current-thread*) 2)
                                               (prog1 (svref (thread-startup-info *current-thread*) 3)
                                                 (setf (thread-startup-info *current-thread*) 0)))
                                        #'sb-di::catch-runaway-unwind))
                                  (when (and *exit-in-progress*
                                             (not (thread-ephemeral-p *current-thread*)))
                                    (sb-impl::call-exit-hooks))))))
                       #+sb-safepoint (sb-kernel::gc-safepoint)
                       (setf (thread-result *current-thread*) list)))
                ;; we're going down, can't handle interrupts
                ;; sanely anymore. gc remains enabled.
                (with-alien ((%block (function int system-area-pointer)
                                     :extern "block_deferrable_signals"))
                  (alien-funcall %block (int-sap 0)))
                ;; we don't want to run interrupts in a dead
                ;; thread when we leave without-interrupts.
                ;; this potentially causes important
                ;; interupts to be lost: sigint comes to
                ;; mind.
                (setq *interrupt-pending* nil)
                #+sb-safepoint
                (setq *thruption-pending* nil)
                (handle-thread-exit))))))))))
  ;; this returns to C, so return a single value
  0)
) ; end PROGN for #+sb-thread

(defun make-thread (function &key name arguments)
  "Create a new thread of NAME that runs FUNCTION with the argument
list designator provided (defaults to no argument). Thread exits when
the function returns. The return values of FUNCTION are kept around
and can be retrieved by JOIN-THREAD.

Invoking the initial ABORT restart established by MAKE-THREAD
terminates the thread.

See also: RETURN-FROM-THREAD, ABORT-THREAD."
  #-sb-thread (declare (ignore function name arguments))
  #-sb-thread (error "Not supported in unithread builds.")
  #+sb-thread
  (let ((name (when name (possibly-base-stringize-to-heap name))))
    (assert (or (atom arguments) (null (cdr (last arguments))))
            (arguments)
            "Argument passed to ~S, ~S, is an improper list."
            'make-thread arguments)
    (start-thread (sb-vm:without-arena "make-thread"
                      (%make-thread name nil (make-semaphore :name name)))
                  (coerce function 'function)
                  (ensure-list arguments))))

;;; System-internal use only
#+sb-thread
(defun make-system-thread (name function arguments symbol)
  (let ((thread (%make-thread name t (make-semaphore :name name))))
    (when symbol
      (aver (not (symbol-value symbol)))
      (set symbol thread))
    (start-thread thread function arguments)))

#+sb-thread
(defun start-thread (thread function arguments)
  (let* ((trampoline
          (lambda (arg)
            ;; If an error occurs prior to getting the thread into a consistent lisp state,
            ;; there's no chance of debugging anything anyway.
            (declare (optimize (safety 0)))
            (let ((new-thread
                   (%make-lisp-obj (logior (get-lisp-obj-address arg)
                                           sb-vm:instance-pointer-lowtag))))
              ;; Now that this thread is known to GC, NEW-THREAD is either implicitly
              ;; pinned (on conservative gc) or movable. It can hance be deleted from
              ;; *STARTING-THREADS* list which occurs lazily on the next MAKE-THREAD.
              ;; To avoid unnecessary GC work meanwhile, smash the cell in *STARTING-THREADS*
              ;; that points to NEW-THREAD. That cell is pointed to by the startup-info.
              (rplaca (svref (thread-startup-info new-thread) 1) 0)
              (init-thread-local-storage new-thread) ; assign *CURRENT-THREAD*
              ;; Expose this thread in *ALL-THREADS*.
              ;; Why not set this before calling pthread_create() ? If it fails there should
              ;; be no transient effect on the list of all threads. But it's indeterminate
              ;; whether the creating or created thread will make progress first,
              ;; so they both do this assignment.
              (setf (thread-%visible new-thread) 1)
              ;; Foreign threads don't pass the saved FP modes, so the modes have to be
              ;; restored here and not in RUN.
              #+(or win32 darwin freebsd)
              (setf (sb-vm:floating-point-modes)
                    (svref (thread-startup-info *current-thread*) 5)))
            (run)))
         (saved-sigmask (make-array (* sb-unix::sizeof-sigset_t sb-vm:n-byte-bits)
                                    :element-type 'bit :initial-element 0))
         (child-sigmask (make-array (* sb-unix::sizeof-sigset_t sb-vm:n-byte-bits)
                                    :element-type 'bit :initial-element 0))
         (created))
    (declare (dynamic-extent saved-sigmask child-sigmask))
    (with-pinned-objects (saved-sigmask child-sigmask) ; if not stack-allocated
      ;; Block deferrables to ensure that the new thread is unaffected by signals
      ;; before the various interrupt-related special vars are set up.
      ;; Preserve the current mask into SAVED-SIGMASK and CHILD-SIGMASK.
      (sb-unix::pthread-sigmask sb-unix::SIG_BLOCK
                                (foreign-symbol-sap "thread_start_sigset" t)
                                saved-sigmask)
      (replace child-sigmask saved-sigmask)
      ;; Ensure that timers and interrupt-thread are directed only to "user" threads.
      #+unix
      (when (thread-ephemeral-p thread)
        (with-alien ((sigaddset (function int system-area-pointer int) :extern "sigaddset"))
          ;; It is essentially impossible to call INTERRUPT-THREAD on the finalizer
          ;; thread, as it spends most of its time with interrupts disabled.
          ;; So it doesn't much matter if SIGURG is added to the signal mask here.
          ;; Unfortunately, this means that SB-INTROSPECT's MAP-ROOTS will hang
          ;; if you give it the finalizer thread.
          ;; The right answer may be that introspect needs to utilize some kind
          ;; of stop-the-thread API that it shares in common with stop-the-world.
          (alien-funcall sigaddset (vector-sap child-sigmask) sb-unix:sigalrm))))
    (binding* ((thread-sap (allocate-thread-memory) :EXIT-IF-NULL)
               ;; CELL becomes the first element in the list of *STARTING-THREADS*
               ;; I think it has to get allocated outside of the called thread.
               (cell (locally (declare (sb-c::tlab :system))
                       (list thread)))
               (startup-info
                (vector trampoline cell function arguments
                        (if (position 1 child-sigmask) ; if there are any signals masked
                            (copy-seq child-sigmask) ; heap-allocate to pass to the new thread
                            nil) ; otherwise, don't pass the saved mask
                        ;; pass fp modes if neccessary, clearing the accrued exception bits
                        (+ #+(or win32 darwin freebsd)
                           (dpb 0 sb-vm:float-sticky-bits (sb-vm:floating-point-modes))))))
      (setf (thread-primitive-thread thread) (sap-int thread-sap)
            (thread-startup-info thread) startup-info)
      ;; Add new thread to *ALL-THREADS* now so that if the creator asserts
      ;; something about "all" threads, it can find the new thread.
      ;; But there is a slight ploy involved: the thread does not appear in
      ;; (LIST-ALL-THREADS) until a POSIX thread is successfully started.
      (setf (thread-%visible thread) 0)
      (update-all-threads (sap-int thread-sap) thread)
      (when *session*
        (locally (declare (sb-c::tlab :system))
          (sb-ext:atomic-push thread (session-new-enrollees *session*))))
      ;; Absence of the startup semaphore notwithstanding, creation is synchronized
      ;; so that we can prevent new threads from starting, typically in SB-POSIX:FORK
      ;; or SAVE-LISP-AND-DIE.
      ;; The locks also guards access to *STARTING-THREADS* - a lockfree list wouldn't
      ;; improve concurrency, as long as creation is synchronized anyway.
      (dx-flet ((thunk ()
        ;; Consing THREAD into *STARTING-THREADS* pins it as well as some elements
        ;; of startup-info. Consequently those objects can be safely manipulated
        ;; from C before inserting the thread into 'all_threads'.
        ;; Assuming that new threads are scheduled by the OS as fast as we can create
        ;; them, there should usually be only one item to delete from *STARTING-THREADS*.
        (let ((old (delete 0 *starting-threads*)))
          (setf *starting-threads* (rplacd cell old))
          (barrier (:write))
          ;; Assign the thread's *CURRENT-THREAD*. This is GC-safe because
          ;; the thread instance is pinned via *STARTING-THREADS*.
          (setf (sap-ref-lispobj thread-sap (ash sb-vm::thread-lisp-thread-slot
                                                 sb-vm:word-shift))
                thread)
          (setq created (os-thread-create thread thread-sap))
          (cond (created
                 ;; Still holding the MAKE-THREAD-LOCK, expose the thread in *all-threads*.
                 ;; In this manner, anyone who acquires the MAKE-THREAD-LOCK can be sure that
                 ;; (list-all-threads) enumerates every running thread.
                 ;; On CPUs where CAS can spuriously fail, this probably needs to loop and retry.
                 ;; It's ok if the thread changed 0 -> {1 | -1}, then failure here is correct.
                 (sb-ext:cas (thread-%visible thread) 0 1))
                (t ; unlikely. Out of memory perhaps?
                 (setq *starting-threads* old))))))
        ;; System threads are created with the mutex already held
        (if (thread-ephemeral-p thread)
            (thunk)
            (sb-thread::call-with-system-mutex #'thunk *make-thread-lock*)))
      (unless created ; Remove side-effects of trying to create
        (delete-from-all-threads (sap-int thread-sap))
        (when *session*
          (%delete-thread-from-session thread))
        (free-thread-struct thread-sap)))
    (with-pinned-objects (saved-sigmask)
      (sb-unix::pthread-sigmask sb-unix::SIG_SETMASK saved-sigmask nil))
    (if created thread (error "Could not create new OS thread."))))


(defun join-thread (thread &key (default nil defaultp) timeout)
  "Suspend current thread until THREAD exits. Return the result values
of the thread function.

If THREAD does not exit within TIMEOUT seconds and DEFAULT is
supplied, return two values: 1) DEFAULT 2) :TIMEOUT. If DEFAULT is not
supplied, signal a JOIN-THREAD-ERROR with JOIN-THREAD-PROBLEM equal
to :TIMEOUT.

If THREAD does not exit normally (i.e. aborted) and DEFAULT is
supplied, return two values: 1) DEFAULT 2) :ABORT. If DEFAULT is not
supplied, signal a JOIN-THREAD-ERROR with JOIN-THREAD-PROBLEM equal
to :ABORT.

If THREAD is the current thread, signal a JOIN-THREAD-ERROR with
JOIN-THREAD-PROBLEM equal to :SELF-JOIN.

Trying to join the main thread causes JOIN-THREAD to block until
TIMEOUT occurs or the process exits: when the main thread exits, the
entire process exits.

Users should not rely on the ability to join a chosen THREAD from more
than one other thread simultaneously. Future changes to JOIN-THREAD may
directly call the underlying thread library, and not all threading
implementations consider such usage to be well-defined.

NOTE: Return convention in case of a timeout is experimental and
subject to change."
  (when (eq thread *current-thread*)
    (error 'join-thread-error :thread thread :problem :self-join))

  ;; First, free up the pthread resources of any thread(s), not necessarily
  ;; one we're tryinng to join.
  #+sb-thread
  (when (cddr *joinable-threads*)      ; if strictly > 2 are joinable,
    (without-interrupts (%dispose-thread-structs :retain 2)))
  ;; No result semaphore indicates that there's nothing to wait for-
  ;; the thread is either a foreign-thread, or finished running.
  (let* ((semaphore (thread-semaphore thread))
         (problem
          (cond (semaphore
                 (if (wait-on-semaphore semaphore :timeout timeout) nil :timeout))
                ((typep thread 'foreign-thread) :foreign))))
    (unless problem
      (cond ((listp (thread-result thread))
             ;; Implementation note, were this to become a native pthread semaphore-
             ;; we would need a sem_post() here to support concurrent join of THREAD
             ;; from N>1 other threads, because the hypothetical sem_post() in HANDLE-THREAD-EXIT
             ;; will only bump the count up by 1, and so at most 1 waiter would execute.
             ;; [Alternatively - still hypothetically - HANDLE-THREAD-EXIT could sem_post()
             ;; as many times as there are currently running threads, which seems iffy]
             ;; And though it's potentially poor style to wait on a chosen thread from N>1 other
             ;; threads, it happens, and users might rely on it, though POSIX specifically
             ;; precludes that in the spec of pthread_join() which this isn't exactly.
             ;; Also for what it's worth, the win32 WaitOnSingleObject and GetExitCodeThread
             ;; APIs have no such prohibition as long as the object handle remains valid.
             (return-from join-thread (values-list (thread-result thread))))
            (t
             (setq problem :abort))))
    (if defaultp
        (values default problem)
        (error 'join-thread-error :thread thread :problem problem))))

(defun destroy-thread (thread)
  (terminate-thread thread))

(declaim (sb-ext:deprecated
          :late ("SBCL" "1.2.15")
          (function destroy-thread :replacement terminate-thread)))

;;; Called from the signal handler.
#-(or sb-safepoint win32)
(defun run-interruption ()
  (let ((interruption (with-deathlok (*current-thread*)
                        (pop (thread-interruptions *current-thread*)))))
    ;; If there is more to do, then resignal and let the normal
    ;; interrupt deferral mechanism take care of the rest. From the
    ;; OS's point of view the signal we are in the handler for is no
    ;; longer pending, so the signal will not be lost.
    (when (thread-interruptions *current-thread*)
      (sb-unix:raise sb-unix:sigurg))
    ;; FIXME: does this really respect the promised ordering of interruptions?
    ;; It looks backwards to raise first and run the popped function second.
    (when interruption
      (funcall interruption))))

#+sb-safepoint
(defun run-interruption (*current-internal-error-context*)
  (in-interruption () ;the non-thruption code does this in the signal handler
    (let ((interruption (with-deathlok (*current-thread*)
                          (pop (thread-interruptions *current-thread*)))))
      (when interruption
        (funcall interruption)
        ;; I tried implementing this function as an explicit LOOP, because
        ;; if we are currently processing the thruption queue, why not do
        ;; all of them in one go instead of one-by-one?
        ;;
        ;; I still think LOOPing would be basically the right thing
        ;; here.  But suppose some interruption unblocked deferrables.
        ;; Will the next one be happy with that?  The answer is "no", at
        ;; least in the sense that there are tests which check that
        ;; deferrables are blocked at the beginning of a thruption, and
        ;; races that make those tests fail.  Whether the tests are
        ;; misguided or not, it seems easier/cleaner to loop implicitly
        ;; -- and it's also what AK had implemented in the first place.
        ;;
        ;; The implicit loop is achieved by returning to C, but having C
        ;; call back to us immediately.  The runtime will reset the sigmask
        ;; in the mean time.
        ;; -- DFL
        (setf *thruption-pending* t)))))

#+(or linux win32 freebsd darwin openbsd)
(progn
  (declaim (ftype (sfunction (thread) (or null (unsigned-byte 32))) thread-os-tid))
  (defun thread-os-tid (thread)
    (if (eq *current-thread* thread)
        (my-kernel-thread-id)
        (with-deathlok (thread c-thread)
          (unless (= c-thread 0)
            (sap-ref-32 (int-sap c-thread)
                        (+ (ash sb-vm::thread-os-kernel-tid-slot sb-vm:word-shift)
                           #+(and 64-bit big-endian) 4)))))))

(defun interrupt-thread (thread function)
  (declare (ignorable thread))
  "Interrupt THREAD and make it run FUNCTION.

The interrupt is asynchronous, and can occur anywhere with the exception of
sections protected using SB-SYS:WITHOUT-INTERRUPTS.

FUNCTION is called with interrupts disabled, under
SB-SYS:ALLOW-WITH-INTERRUPTS. Since functions such as GRAB-MUTEX may try to
enable interrupts internally, in most cases FUNCTION should either enter
SB-SYS:WITH-INTERRUPTS to allow nested interrupts, or
SB-SYS:WITHOUT-INTERRUPTS to prevent them completely.

When a thread receives multiple interrupts, they are executed in the order
they were sent -- first in, first out.

This means that a great degree of care is required to use INTERRUPT-THREAD
safely and sanely in a production environment. The general recommendation is
to limit uses of INTERRUPT-THREAD for interactive debugging, banning it
entirely from production environments -- it is simply exceedingly hard to use
correctly.

With those caveats in mind, what you need to know when using it:

 * If calling FUNCTION causes a non-local transfer of control (ie. an
   unwind), all normal cleanup forms will be executed.

   However, if the interrupt occurs during cleanup forms of an UNWIND-PROTECT,
   it is just as if that had happened due to a regular GO, THROW, or
   RETURN-FROM: the interrupted cleanup form and those following it in the
   same UNWIND-PROTECT do not get executed.

   SBCL tries to keep its own internals asynch-unwind-safe, but this is
   frankly an unreasonable expectation for third party libraries, especially
   given that asynch-unwind-safety does not compose: a function calling
   only asynch-unwind-safe function isn't automatically asynch-unwind-safe.

   This means that in order for an asynch unwind to be safe, the entire
   callstack at the point of interruption needs to be asynch-unwind-safe.

 * In addition to asynch-unwind-safety you must consider the issue of
   reentrancy. INTERRUPT-THREAD can cause function that are never normally
   called recursively to be re-entered during their dynamic contour,
   which may cause them to misbehave. (Consider binding of special variables,
   values of global variables, etc.)

Taken together, these two restrict the \"safe\" things to do using
INTERRUPT-THREAD to a fairly minimal set. One useful one -- exclusively for
interactive development use is using it to force entry to debugger to inspect
the state of a thread:

  (interrupt-thread thread #'break)

Short version: be careful out there."
  ;; POSIX says:
  ;; "If an application attempts to use a thread ID whose lifetime has ended,
  ;;  the behavior is undefined."
  ;; so we use the death lock to keep the thread alive, unless it already isn't.
  ;;
  (when (with-deathlok (thread c-thread)
          ;; Return T if couldn't interrupt.
          (cond ((eql c-thread 0) t)
                (t (%interrupt-thread thread function) nil)))
    (error 'interrupt-thread-error :thread thread)))

(defun %interrupt-thread (thread function)
  ;; Append to the end of the interruptions queue. It's
  ;; O(N), but it does not hurt to slow interruptors down a
  ;; bit when the queue gets long.
  (setf (thread-interruptions thread)
        (append (thread-interruptions thread)
                ;; It seems to me that this junk should be in RUN-INTERRUPTION,
                ;; but it doesn't really matter where it goes.
                (list (lambda ()
                        (barrier (:memory)) ; why???
                        (without-interrupts (allow-with-interrupts (funcall function)))))))
  ;; We use SIGURG because it satisfies a lot of requirements that
  ;; other people have thought about more than we have.
  ;; See https://golang.org/src/runtime/signal_unix.go where they describe
  ;; which signal works best for their sigPreempt.
  ;; It's basically the same use-case as here.
  #-sb-safepoint (sb-unix:pthread-kill (thread-os-thread thread) sb-unix:sigurg)
  #+sb-safepoint
  (with-alien ((wake (function void system-area-pointer) :extern "wake_thread"))
    (with-pinned-objects (thread)
      (alien-funcall wake (sap+ (int-sap (get-lisp-obj-address thread))
                                (- sb-vm:instance-pointer-lowtag)))))
  nil)

(defun terminate-thread (thread)
  "Terminate the thread identified by THREAD, by interrupting it and
causing it to call SB-EXT:ABORT-THREAD with :ALLOW-EXIT T.

The unwind caused by TERMINATE-THREAD is asynchronous, meaning that
eg. thread executing

  (let (foo)
     (unwind-protect
         (progn
            (setf foo (get-foo))
            (work-on-foo foo))
       (when foo
         ;; An interrupt occurring inside the cleanup clause
         ;; will cause cleanups from the current UNWIND-PROTECT
         ;; to be dropped.
         (release-foo foo))))

might miss calling RELEASE-FOO despite GET-FOO having returned true if
the interrupt occurs inside the cleanup clause, eg. during execution
of RELEASE-FOO.

Thus, in order to write an asynch unwind safe UNWIND-PROTECT you need
to use WITHOUT-INTERRUPTS:

  (let (foo)
    (sb-sys:without-interrupts
      (unwind-protect
          (progn
            (setf foo (sb-sys:allow-with-interrupts
                        (get-foo)))
            (sb-sys:with-local-interrupts
              (work-on-foo foo)))
       (when foo
         (release-foo foo)))))

Since most libraries using UNWIND-PROTECT do not do this, you should never
assume that unknown code can safely be terminated using TERMINATE-THREAD."
  (interrupt-thread thread (lambda () (abort-thread :allow-exit t))))

(define-alien-routine "thread_yield" int)

(setf (documentation 'thread-yield 'function)
      "Yield the processor to other threads.")

;;; internal use only.  If you think you need to use these, either you
;;; are an SBCL developer, are doing something that you should discuss
;;; with an SBCL developer first, or are doing something that you
;;; should probably discuss with a professional psychiatrist first
#+sb-thread
(progn
  (defun %symbol-value-in-thread (symbol thread &aux (tlsindex (symbol-tls-index symbol)))
    (with-deathlok (thread c-thread)
      (if (/= c-thread 0)
          ;; Avoid loading not-really-an-object markers into a descriptor register.
          (macrolet ((read-using (reader) `(,reader (int-sap c-thread) tlsindex)))
            (let ((bits (read-using sap-ref-word)))
              (cond ((eql bits sb-vm:no-tls-value-marker) (values nil :no-tls-value))
                    ((eql (logand bits sb-vm:widetag-mask) sb-vm:unbound-marker-widetag)
                     (values nil :unbound-in-thread))
                    (t (values (read-using sap-ref-lispobj) :ok)))))
          (values nil :thread-dead))))

  (defun %set-symbol-value-in-thread (symbol thread value)
    (with-deathlok (thread c-thread)
      (if (/= c-thread 0)
          (let ((offset (symbol-tls-index symbol)))
            (cond ((zerop offset)
                   (values nil :no-tls-value))
                  (t
                   (setf (sap-ref-lispobj (int-sap c-thread) offset) value)
                   (values value :ok))))
          (values nil :thread-dead))))

  ;; Get values from the TLS area of the current thread.
  ;; Disregard duplicates and immediate objects.
  (defun %thread-local-references ()
    ;; TLS-INDEX-START is a word number relative to thread base.
    ;; *FREE-TLS-INDEX* - which is only manipulated by machine code  - is an
    ;; offset from thread base to the next usable TLS cell as a byte offset
    ;; (raw value) manifesting in Lisp as a fixnum.
    ;; The sign bit of sb-vm::*free-tls-index* is a semaphore,
    ;; except on PPC where it isn't, but masking is fine in any case.
    (do ((index (- (ash (logand sb-vm::*free-tls-index* most-positive-fixnum)
                        sb-vm:n-fixnum-tag-bits)
                   sb-vm:n-word-bytes)
                (- index sb-vm:n-word-bytes))
         ;; (There's almost no reason this couldn't work on any thread.)
         (sap (current-thread-sap))
         (list))
        ((< index (ash sb-vm::primitive-thread-object-length sb-vm:word-shift))
         list)
      ;; NO-TLS-VALUE-MARKER may or may not satisfy IS-LISP-POINTER,
      ;; so be sure to exclude it in case it does.
      (unless (eql (sap-ref-word sap index) sb-vm:no-tls-value-marker)
        (let ((obj (sap-ref-lispobj sap index)))
          (when (and obj ; don't bother returning NIL
                     (sb-vm:is-lisp-pointer (get-lisp-obj-address obj))
                     (not (memq obj list)))
            (push obj list)))))))

(defun symbol-value-in-thread (symbol thread &optional (errorp t))
  "Return the local value of SYMBOL in THREAD, and a secondary value of T
on success.

If the value cannot be retrieved (because the thread has exited or because it
has no local binding for NAME) and ERRORP is true signals an error of type
SYMBOL-VALUE-IN-THREAD-ERROR; if ERRORP is false returns a primary value of
NIL, and a secondary value of NIL.

Can also be used with SETF to change the thread-local value of SYMBOL.

SYMBOL-VALUE-IN-THREAD is primarily intended as a debugging tool, and not as a
mechanism for inter-thread communication."
  (declare (symbol symbol) (thread thread))
  #+sb-thread
  (multiple-value-bind (res status) (%symbol-value-in-thread symbol thread)
    (if (eq :ok status)
        (values res t)
        (if errorp
            (error 'symbol-value-in-thread-error
                   :name symbol
                   :thread thread
                   :info (list :read status))
            (values nil nil))))
  #-sb-thread
  (if (boundp symbol)
      (values (symbol-value symbol) t)
      (if errorp
          (error 'symbol-value-in-thread-error
                 :name symbol
                 :thread thread
                 :info (list :read :unbound-in-thread))
          (values nil nil))))

(defun (setf symbol-value-in-thread) (value symbol thread &optional (errorp t))
  (declare (symbol symbol) (thread thread))
  #+sb-thread
  (multiple-value-bind (res status) (%set-symbol-value-in-thread symbol thread value)
    (if (eq :ok status)
        (values res t)
        (if errorp
            (error 'symbol-value-in-thread-error
                   :name symbol
                   :thread thread
                   :info (list :write status))
            (values nil nil))))
  #-sb-thread
  (if (boundp symbol)
      (values (setf (symbol-value symbol) value) t)
      (if errorp
          (error 'symbol-value-in-thread-error
                 :name symbol
                 :thread thread
                 :info (list :write :unbound-in-thread))
          (values nil nil))))



;;; Initialize thread-local special vars other than the GC control specials.
;;; globaldb should indicate that the variable is both :always-thread-local
;;; (which says that the TLS index is nonzero), and :always-bound (which says that
;;; the value in TLS is not UNBOUND-MARKER).
(defun init-thread-local-storage (thread)
  ;; In addition to wanting the expressly unsafe variant of SYMBOL-VALUE, any error
  ;; signaled such as invalid-arg-count would just go totally wrong at this point.
  (declare (optimize (safety 0)))
  #-sb-thread
  (macrolet ((expand () `(setf ,@(apply #'append (cdr *thread-local-specials*)))))
    (setf *current-thread* thread)
    (expand))
  ;; Bear in mind that relative to the #-sb-thread code these assignments require
  ;; a trick because none of the symbols have been thread-locally bound.
  ;; The C runtime shouldn't have to know to prefill most but not all the TLS with
  ;; NO-TLS-VALUE. Hence these symbols' TLS slots contain NO-TLS-VALUE which under
  ;; ordinary circumstances could cause the store to affect SYMBOL-GLOBAL-VALUE.
  ;; So we have to store directly into offsets relative to the primitive thread.
  ;; See %SET-SYMBOL-VALUE-IN-THREAD for comparison.
  ;; Also note that on x86-64, (SETF SAP-REF-LISPOBJ) won't move immediate-to-memory
  ;; using one instruction, but sap-ref-word will.
  ;; So some of these are compile-time converted into their bit representation.
  ;; (Additionally there is a redundant move from THREAD-TN to a sap register
  ;; which could probably be eliminated but only via peephole optimization)
  #+sb-thread
  (let ((sap (current-thread-sap)))
    (macrolet ((expand ()
                 `(setf (sap-ref-lispobj sap ,(info :variable :wired-tls '*current-thread*))
                        thread
                        ,@(loop for (var form) in (cdr *thread-local-specials*)
                                for index = (info :variable :wired-tls var)
                                append
                                (cond #+x86-64
                                      ((equal form '(sb-kernel:make-unbound-marker))
                                       `((sap-ref-word sap ,index) ,(sb-vm::unbound-marker-bits)))
                                      #+x86-64
                                      ((eq form nil)
                                       `((sap-ref-word sap ,index) ,sb-vm:nil-value))
                                      (t
                                       `((sap-ref-lispobj sap ,index) ,form)))))))
      (expand)))
  thread)

(eval-when (:compile-toplevel)
  ;; Inform genesis of the index <-> symbol mapping made by DEFINE-THREAD-LOCAL
  (with-open-file (output (sb-cold:find-bootstrap-file "output/tls-init.lisp-expr" t)
                          :direction :output :if-exists :supersede)
    (let ((list (mapcar (lambda (x &aux (symbol (car x)))
                          (cons (info :variable :wired-tls symbol) symbol))
                        (cdr *thread-local-specials*)))
          (*package* *keyword-package*))
      (write list :stream output :readably t :pretty nil)
      (terpri output)))
  ;; Prevent further use of DEFINE-THREAD-LOCAL after compiling this file
  ;; because the definition of INIT-THREAD-LOCAL-STORAGE is now frozen.
  (setf *thread-local-specials* (cons :final (cdr *thread-local-specials*))))

;;;; Stepping

(defun thread-stepping ()
  (sap-ref-lispobj (current-thread-sap)
                   (* sb-vm::thread-stepping-slot sb-vm:n-word-bytes)))

(defun (setf thread-stepping) (value)
  (setf (sap-ref-lispobj (current-thread-sap)
                         (* sb-vm::thread-stepping-slot sb-vm:n-word-bytes))
        value))

;;;; Diagnostic tool

#+sb-devel
(defun dump-thread ()
  (let* ((primobj (sb-vm::primitive-object 'sb-vm::thread))
         (slots (sb-vm::primitive-object-slots primobj))
         (sap (current-thread-sap))
         (thread-obj-len (sb-vm::primitive-object-length primobj))
         (names (make-array thread-obj-len :initial-element "")))
    (loop for slot across slots
          do
          (setf (aref names (sb-vm::slot-offset slot)) (sb-vm::slot-name slot)))
    (flet ((safely-read (sap offset &aux (bits (sap-ref-word sap offset)))
             (cond ((eql bits sb-vm:no-tls-value-marker) :no-tls-value)
                   ((eql (logand bits sb-vm:widetag-mask) sb-vm:unbound-marker-widetag) :unbound)
                   (t (sap-ref-lispobj sap offset))))
           (show (sym val)
             (let ((*print-right-margin* 128)
                   (*print-lines* 4))
               (format t " ~3d ~30a : ~s~%"
                       #+sb-thread (ash sym (- sb-vm:word-shift))
                       #-sb-thread 0
                       #+sb-thread (sb-vm:symbol-from-tls-index sym)
                       #-sb-thread sym
                       val))))
      (format t "~&TLS: (base=~x)~%" (sap-int sap))
      (loop for tlsindex from sb-vm:n-word-bytes below
            #+sb-thread (ash sb-vm::*free-tls-index* sb-vm:n-fixnum-tag-bits)
            #-sb-thread (ash thread-obj-len sb-vm:word-shift)
            by sb-vm:n-word-bytes
            do
         (unless (<= sb-vm::thread-allocator-histogram-slot
                     (ash tlsindex (- sb-vm:word-shift))
                     (1- sb-vm::thread-lisp-thread-slot))
           (let ((thread-slot-name
                  (if (< tlsindex (ash thread-obj-len sb-vm:word-shift))
                           (aref names (ash tlsindex (- sb-vm:word-shift))))))
                 (if (and thread-slot-name (neq thread-slot-name 'sb-vm::lisp-thread))
                     (format t " ~3d ~30a : #x~x~%" (ash tlsindex (- sb-vm:word-shift))
                             thread-slot-name (sap-ref-word sap tlsindex))
                     (let ((val (safely-read sap tlsindex)))
                       (unless (eq val :no-tls-value)
                         (show tlsindex val)))))))
      (let ((from (descriptor-sap sb-vm:*binding-stack-start*))
            (to (binding-stack-pointer-sap)))
        (format t "~%Binding stack: (depth ~d)~%"
                (/ (sap- to from) (* sb-vm:binding-size sb-vm:n-word-bytes)))
        (loop
          (when (sap>= from to) (return))
          (let ((val (safely-read from 0))
                (sym #+sb-thread (sap-ref-word from sb-vm:n-word-bytes) ; a TLS index
                     #-sb-thread (sap-ref-lispobj from sb-vm:n-word-bytes)))
            (show sym val))
          (setq from (sap+ from (* sb-vm:binding-size sb-vm:n-word-bytes))))))))

(macrolet ((histogram-value (c-thread index)
             `(sap-ref-word (int-sap ,c-thread)
                            (ash (+ sb-vm::thread-allocator-histogram-slot ,index)
                                 sb-vm:word-shift)))
           (metric (c-thread slot)
             `(sap-ref-word (int-sap ,c-thread)
                            (ash ,slot sb-vm:word-shift)))
           (histogram-array-length ()
             (+ sb-vm::n-histogram-bins-small
                (* 2 sb-vm::n-histogram-bins-large))))

(export '(allocator-histogram print-allocator-histogram reset-allocator-histogram))
(defun allocator-histogram (&optional (thread *current-thread*))
  (if (eq thread :all)
      (labels ((vector-sum (a b)
                 (let ((result (make-array (length a) :element-type 'fixnum)))
                   (dotimes (i (length result) result)
                     (setf (aref result i) (+ (aref a i) (aref b i))))))
               (sum (a b)
                 (list (vector-sum (first a) (first b)) ; bin counts
                       (vector-sum (second a) (second b)) ; nbytes in large bins
                       (+ (third a) (third b)) ; unboxed total
                       (+ (fourth a) (fourth b))))) ; boxed total
        ;; can get a NIL if a thread exited by the time we got to asking for its data
        (reduce #'sum (delete nil
                              (mapcar 'allocator-histogram (%list-all-threads)))))
      (with-deathlok (thread c-thread)
        (unless (= c-thread 0)
          (let ((a (make-array (histogram-array-length) :element-type 'fixnum))
                (boxed (metric c-thread sb-vm::thread-tot-bytes-alloc-boxed-slot))
                (unboxed (metric c-thread sb-vm::thread-tot-bytes-alloc-unboxed-slot)))
            (declare (dynamic-extent a))
            (dotimes (i (length a))
              (setf (aref a i) (histogram-value c-thread i)))
            (list (subseq a 0 (+ sb-vm::n-histogram-bins-small
                                 sb-vm::n-histogram-bins-large))
                  (subseq a (+ sb-vm::n-histogram-bins-small
                               sb-vm::n-histogram-bins-large))
                  unboxed
                  boxed))))))

(defun reset-allocator-histogram (&optional (thread *current-thread*))
  (if (eq thread :all)
      (mapc #'reset-allocator-histogram (%list-all-threads))
      (with-deathlok (thread c-thread)
        (unless (= c-thread 0)
          (setf (metric c-thread sb-vm::thread-tot-bytes-alloc-boxed-slot) 0
                (metric c-thread sb-vm::thread-tot-bytes-alloc-unboxed-slot) 0
                (metric c-thread sb-vm::thread-slow-path-allocs-slot) 0)
          (dotimes (i (histogram-array-length))
            (setf (histogram-value c-thread i) 0)))))))

(defun print-allocator-histogram (&optional (thread-or-values *current-thread*))
  (destructuring-bind (counts large-allocated tot-bytes-unboxed tot-bytes-boxed)
      (if (listp thread-or-values)
          thread-or-values ; histogram was already gathered, just print it
          (allocator-histogram thread-or-values))
    (let* ((tot-bins (length counts))
           (tot-objects (reduce #'+ counts))
           (bin-label (make-array tot-bins))
           (bin-nbytes (make-array tot-bins))
           (cumulative 0))
      (dotimes (i sb-vm::n-histogram-bins-small)
        (setf (aref bin-label i) (* (1+ i) sb-vm:cons-size sb-vm:n-word-bytes)
              (aref bin-nbytes i) (* (aref counts i) (aref bin-label i))))
      (dotimes (i sb-vm::n-histogram-bins-small)
        (let ((bin-index (+ sb-vm::n-histogram-bins-small i))
              (size-max (ash 1 (+ i sb-vm::first-large-histogram-bin-log2size)))
              (allocated (aref large-allocated i)))
          (setf (aref bin-label bin-index)
                (if (< size-max 1048576)
                    (format nil "< ~d" size-max)
                    (format nil "< 2^~d" (1- (integer-length size-max))))
                (aref bin-nbytes bin-index) allocated)))
      (format t "~& Bin      Size     Allocated     Count    Cum%~%")
      (dotimes (i tot-bins)
        (let ((count (aref counts i)))
          (incf cumulative count)
          (format t "~& ~2d ~10@a ~13d ~9d ~7,2,2f~%"
                  i
                  (aref bin-label i)
                  (aref bin-nbytes i)
                  count
                  (when (plusp tot-objects) (/ cumulative tot-objects)))))
      (let ((tot-bytes (+ tot-bytes-unboxed tot-bytes-boxed)))
        (format t "~& Tot ~23d ~9d~%" tot-bytes tot-objects)
        (when (plusp tot-bytes)
          (format t "; ~D unboxed + ~D boxed bytes (~,1,2F% + ~,1,2F%)~%"
                  tot-bytes-unboxed tot-bytes-boxed
                  (/ tot-bytes-unboxed tot-bytes)
                  (/ tot-bytes-boxed tot-bytes)))))))
