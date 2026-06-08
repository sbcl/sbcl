(in-package :sb-manual)

(defsection @threading (:title "Threading")
  "SBCL supports a fairly low-level threading interface that maps onto
  the host operating system's concept of threads or lightweight
  processes. This means that threads may take advantage of hardware
  multiprocessing on machines that have more than one CPU, but it does
  not allow Lisp control of the scheduler. This is found in the
  `SB-THREAD` package.

  Threads are part of the default build on x86[-64]/ARM64 Linux and
  Windows.

  They are also supported on: x86[-64] Darwin (Mac OS X), x86[-64]
  FreeBSD, x86 SunOS (Solaris), PPC Linux, ARM64 Linux, RISC-V Linux.
  On these platforms threads must be explicitly enabled at build-time,
  see `INSTALL` for directions."
  (@threading-basics section)
  (@special-variables section)
  (@atomic-operations section)
  (@mutex-support section)
  (@semaphores section)
  (@waitqueue/condition-variables section)
  (@barriers section)
  (@sessions/debugging section)
  (@foreign-threads section)
  (@implementation-on-linux-x86oids section))

(defsection @threading-basics (:title "Threading Basics")
  "```
  (make-thread (lambda () (write-line \"Hello, world\")))
  ```"
  (@thread-objects section)
  (@running-threads section)
  (@asynchronous-operations section)
  (@miscellaneous-operations section)
  (@error-conditions section))

(defsection @thread-objects (:title "Thread Objects")
  (sb-thread:thread structure)
  (sb-thread:*current-thread* variable)
  (sb-thread:list-all-threads function)
  (sb-thread:thread-alive-p function)
  (sb-thread:thread-name function)
  (sb-thread:main-thread-p function)
  (sb-thread:main-thread function))

(defsection @running-threads (:title "Running Threads")
  (sb-thread:make-thread function)
  (sb-thread:return-from-thread macro)
  (sb-thread:abort-thread function)
  (sb-thread:join-thread function)
  (sb-thread:thread-yield function))

(defsection @asynchronous-operations (:title "Asynchronous Operations")
  (sb-thread:interrupt-thread function)
  (sb-thread:terminate-thread function))

(defsection @miscellaneous-operations (:title "Miscellaneous Operations")
  (sb-thread:symbol-value-in-thread function))

(defsection @error-conditions (:title "Error Conditions")
  (sb-thread:thread-error condition)
  (sb-thread:thread-error-thread function)
  (sb-thread:symbol-value-in-thread-error condition)
  (sb-thread:interrupt-thread-error condition)
  (sb-thread:join-thread-error condition))

(defsection @special-variables (:title "Special Variables")
  "The interaction of special variables with multiple threads is mostly
  as one would expect, with behaviour very similar to other
  implementations.

  - Global special values are visible across all threads.

  - Bindings (e.g. using LET) are local to the thread.

  - Threads do not inherit dynamic bindings from the parent thread.

  The last point means that

      (defparameter *x* 0)
      (let ((*x* 1))
        (sb-thread:make-thread (lambda () (print *x*))))

  prints `0` and not `1`.

  Note, however, that there is a hard limit on the number of distinct
  symbols that can be bound dynamically in threaded builds (see
  `--tls-limit` in @RUNTIME-OPTIONS). Exceeding this limit triggers
  the low-level error `Thread local storage exhausted.`")

(defsection @atomic-operations (:title "Atomic Operations")
  "Following atomic operations are particularly useful for implementing
  lockless algorithms."
  (sb-ext:atomic-decf macro)
  (sb-ext:atomic-incf macro)
  (sb-ext:atomic-pop macro)
  (sb-ext:atomic-push macro)
  (sb-ext:atomic-update macro)
  (sb-ext:compare-and-swap macro)
  "Our SB-EXT:COMPARE-AND-SWAP is user-extensible by defining functions
  named `(CAS <PLACE>)`, allowing users to add CAS support to new
  places."
  (sb-ext:cas macro)
  (sb-ext:get-cas-expansion function))

(defsection @mutex-support (:title "Mutex Support")
  "Mutexes are used for controlling access to a shared resource. One
  thread is allowed to hold the mutex, others which attempt to take it
  will be made to wait until it's free. Threads are woken in the order
  that they go to sleep.

      (defpackage :demo (:use \"CL\" \"SB-THREAD\" \"SB-EXT\"))

      (in-package :demo)

      (defvar *a-mutex* (make-mutex :name \"my lock\"))

      (defun thread-fn ()
        (format t \"Thread ~A running ~%\" *current-thread*)
        (with-mutex (*a-mutex*)
          (format t \"Thread ~A got the lock~%\" *current-thread*)
          (sleep (random 5)))
        (format t \"Thread ~A dropped lock, dying now~%\" *current-thread*))

      (make-thread #'thread-fn)
      (make-thread #'thread-fn)"
  (sb-thread:mutex structure)
  (sb-thread:with-mutex macro)
  (sb-thread:with-recursive-lock macro)
  (sb-thread:make-mutex function)
  (sb-thread:mutex-name function)
  (sb-thread:mutex-owner function)
  (sb-thread:mutex-value function)
  (sb-thread:grab-mutex function)
  (sb-thread:release-mutex function))

(defsection @semaphores (:title "Semaphores")
  "Semaphores are among other things useful for keeping track of a
  countable resource, e.g. messages in a queue, and sleep when the
  resource is exhausted."
  (sb-thread:semaphore structure)
  (sb-thread:make-semaphore function)
  (sb-thread:signal-semaphore function)
  (sb-thread:wait-on-semaphore function)
  (sb-thread:try-semaphore function)
  (sb-thread:semaphore-count function)
  (sb-thread:semaphore-name function)
  (sb-thread:semaphore-notification structure)
  (sb-thread:make-semaphore-notification function)
  (sb-thread:semaphore-notification-status function)
  (sb-thread:clear-semaphore-notification function))

(defsection @waitqueue/condition-variables
    (:title "Waitqueue/condition variables")
  "These are based on the POSIX condition variable design, hence the
  annoyingly CL-conflicting name. For use when you want to check a
  condition and sleep until it's true. For example: you have a shared
  queue, a writer process checking _queue is empty_ and one or more
  readers that need to know when _queue is not empty_. It sounds
  simple but is astonishingly easy to deadlock if another process runs
  when you weren't expecting it to.

  There are three components:

  - the condition itself (not represented in code)

  - the condition variable (a.k.a. waitqueue) which proxies for it

  - a lock to hold while testing the condition

  Important stuff to be aware of:

  - when calling condition-wait, you must hold the mutex.
    condition-wait will drop the mutex while it waits, and obtain it
    again before returning for whatever reason;

  - likewise, you must be holding the mutex around calls to
    SB-THREAD:CONDITION-NOTIFY;

  - a process may return from SB-THREAD:CONDITION-WAIT in several
    circumstances: it is not guaranteed that the underlying condition
    has become true. You must check that the resource is ready for
    whatever you want to do to it.

          (defvar *buffer-queue* (make-waitqueue))
          (defvar *buffer-lock* (make-mutex :name \"buffer lock\"))

          (defvar *buffer* (list nil))

          (defun reader ()
            (with-mutex (*buffer-lock*)
              (loop
               (condition-wait *buffer-queue* *buffer-lock*)
               (loop
                (unless *buffer* (return))
                (let ((head (car *buffer*)))
                  (setf *buffer* (cdr *buffer*))
                  (format t \"reader ~A woke, read ~A~%\"
                          *current-thread* head))))))

          (defun writer ()
            (loop
             (sleep (random 5))
             (with-mutex (*buffer-lock*)
               (let ((el (intern
                          (string (code-char
                                   (+ (char-code #\A) (random 26)))))))
                 (setf *buffer* (cons el *buffer*)))
               (condition-notify *buffer-queue*))))

          (make-thread #'writer)
          (make-thread #'reader)
          (make-thread #'reader)"
  (sb-thread:waitqueue structure)
  (sb-thread:make-waitqueue function)
  (sb-thread:waitqueue-name function)
  (sb-thread:condition-wait function)
  (sb-thread:condition-notify function)
  (sb-thread:condition-broadcast function))

(defsection @barriers (:title "Barriers")
  "These are based on the Linux kernel barrier design, which is in turn
  based on the Alpha CPU memory model. They are presently implemented for
  x86, x86-64, PPC, ARM64, and RISC-V systems, and behave as compiler
  barriers on all other CPUs.

  In addition to explicit use of the SB-THREAD:BARRIER macro, the
  following functions and macros also serve as :MEMORY barriers:

  - SB-EXT:ATOMIC-DECF, SB-EXT:ATOMIC-INCF, SB-EXT:ATOMIC-PUSH,
    and SB-EXT:ATOMIC-POP

  - SB-EXT:COMPARE-AND-SWAP

  - SB-THREAD:GRAB-MUTEX, SB-THREAD:RELEASE-MUTEX,
    SB-THREAD:WITH-MUTEX and SB-THREAD:WITH-RECURSIVE-LOCK

  - SB-THREAD:SIGNAL-SEMAPHORE, SB-THREAD:TRY-SEMAPHORE and
    SB-THREAD:WAIT-ON-SEMAPHORE

  - SB-THREAD:CONDITION-WAIT, SB-THREAD:CONDITION-NOTIFY and
    SB-THREAD:CONDITION-BROADCAST."
  (sb-thread:barrier macro))

(defsection @sessions/debugging (:title "Sessions/Debugging")
  "If the user has multiple views onto the same Lisp image (for example,
  using multiple terminals, or a windowing system, or network access)
  they are typically set up as multiple _sessions_ such that each view
  has its own collection of foreground, background, and stopped
  threads. A thread which wishes to create a new session can use
  SB-THREAD:WITH-NEW-SESSION to remove itself from the current
  session (which it shares with its parent and siblings) and create a
  fresh one."
  (sb-thread:with-new-session macro)
  #-win32
  (sb-thread:make-listener-thread function)
  "Within a single session, threads arbitrate between themselves for
  the user's attention. A thread may be in one of three notional
  states: foreground, background, or stopped. When a background
  process attempts to print a repl prompt or to enter the debugger, it
  will stop and print a message saying that it has stopped. The user
  at his leisure may switch to that thread to find out what it needs.
  If a background thread enters the debugger, selecting any restart
  will put it back into the background before it resumes. Arbitration
  for the input stream is managed by calls to
  SB-THREAD:GET-FOREGROUND (which may block) and
  SB-THREAD:RELEASE-FOREGROUND."
  (sb-thread:get-foreground function)
  (sb-thread:release-foreground function))

(defsection @foreign-threads (:title "Foreign threads")
  "Direct calls to `pthread_create(3)` (instead of SB-THREAD:MAKE-THREAD)
  create threads that SBCL is not aware of, these are called foreign
  threads. Currently, it is not possible to run Lisp code in such
  threads. This means that the Lisp side signal handlers cannot work.
  The best solution is to start foreign threads with signals blocked,
  but since third party libraries may create threads, it is not always
  feasible to do so. As a workaround, upon receiving a signal in a
  foreign thread, SBCL changes the thread's sigmask to block all
  signals that it wants to handle and resends the signal to the
  current process which should land in a thread that does not block
  it, that is, a Lisp thread.

  The resignalling trick cannot work for synchronously triggered signals
  (`SIGSEGV` and co), take care not to trigger any. Resignalling for
  synchronously triggered signals in foreign threads is subject to
  `--lose-on-corruption`, see @RUNTIME-OPTIONS.")

(defsection @implementation-on-linux-x86oids
    (:title "Implementation on Linux x86oids")
  "Threading is implemented using pthreads and some Linux specific bits
  like futexes.

  On x86, the per-thread local bindings for special variables is
  achieved using the `%fs` segment register to point to a per-thread
  storage area. This may cause interesting results if you link to
  foreign code that expects threading or creates new threads, and the
  thread library in question uses %fs in an incompatible way. On
  x86-64 the r12 register has a similar role.

  Queues require the `futex(2)` system call to be available: this is
  the reason for the NPTL requirement. We test at runtime that this
  system call exists.

  Garbage collection is done with the existing Conservative
  Generational GC. Allocation is done in small (typically 8k) regions:
  each thread has its own region so this involves no stopping.
  However, when a region fills, a lock must be obtained while another
  is allocated, and when a collection is required, all processes are
  stopped. This is achieved by sending them signals, which may make
  for interesting behaviour if they are interrupted in system calls.
  The streams interface is believed to handle the required system call
  restarting correctly, but this may be a consideration when making
  other blocking calls e.g. from foreign library code.

  Large amounts of the SBCL library have not been inspected for
  thread-safety.  Some of the obviously unsafe areas have large locks
  around them, so compilation and fasl loading, for example, cannot be
  parallelized.  Work is ongoing in this area.

  A new thread by default is created in the same POSIX process group and
  session as the thread it was created by.  This has an impact on
  keyboard interrupt handling: pressing your terminal's intr key
  (typically `Control-C`) will interrupt all processes in the
  foreground process group, including Lisp threads that SBCL considers
  to be notionally _background_. This is undesirable, so background
  threads are set to ignore the `SIGINT` signal.

  `SB-THREAD:MAKE-LISTENER-THREAD` in addition to creating a new Lisp
  session makes a new POSIX session, so that pressing `Control-C` in
  one window will not interrupt another listener - this has been found
  to be embarrassing.")
