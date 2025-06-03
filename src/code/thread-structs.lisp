;;;; support for threads needed at cross-compile time

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-THREAD")

;;; An AVL tree of threads keyed by 'struct thread'. NIL is the empty tree.
(sb-ext:define-load-time-global *all-threads* ())
;;; Next TLS index to use. This is shifted by n-fixnum-tag-bits because it holds
;;; a word-aligned raw integer, not a fixnum (but it looks like a fixnum)
(sb-ext:define-load-time-global sb-vm::*free-tls-index* 0)

;;; It's possible to make futex/non-futex switchable at runtime by ensuring that
;;; these synchronization primitive structs contain all the slots for the union
;;; of any kind of backing object.  Some of the #+sb-futex/#-sb-futex cases in
;;; target-thread also have to be changed to a COND rather than a compile-time test.
;;; Among the uses of this would be to test real posix mutexes (with a finalizer on
;;; each lisp-side proxy) or support older C APIs. Posix mutexes work nicely now
;;; because finalizers are more efficient than they were, and when many threads
;;; compete for a mutex, the pthread code seems to do a better job at reducing
;;; cycles spent in the OS.

(sb-xc:defstruct (mutex (:constructor make-mutex (&key name))
                        (:copier nil))
  "Mutex type."
  #+sb-futex (state 0 :type sb-vm:word)
  ;; If adding slots between STATE and NAME, please see futex_name() in linux_os.c
  ;; which attempts to divine a string from a futex word address.
  (name   nil :type (or null simple-string))
  ;; The owner is a non-pointer so that GC pages containing mutexes do not get dirtied
  ;; with mutex ownership change. The natural representation of this is SB-VM:WORD
  ;; but the "funny fixnum" representation - i.e. N_WORD_BITS bits of significance, but
  ;; cast as fixnum when read - avoids consing on 32-bit builds, and also not all of them
  ;; implement RAW-INSTANCE-CAS which would be otherwise needed.
  (%owner 0 :type fixnum))

(sb-xc:defstruct (waitqueue (:copier nil) (:constructor make-waitqueue (&key name)))
  "Waitqueue type."
  ;; futex words are actually 32-bits, but it needs to be a raw slot and we don't have
  ;; 32-bit raw slots on 64-bit machines.
  #+sb-futex (token 0 :type sb-ext:word)
  ;; If adding slots between TOKEN and NAME, please see futex_name() in linux_os.c
  ;; which attempts to divine a string from a futex word address.
  (name nil :type (or null string))
  ;; For WITH-CAS-LOCK: because CONDITION-WAIT must be able to call
  ;; %WAITQUEUE-WAKEUP without re-aquiring the mutex, we need a separate
  ;; lock. In most cases this should be uncontested thanks to the mutex --
  ;; the only case where that might not be true is when CONDITION-WAIT
  ;; unwinds and %WAITQUEUE-DROP is called.
  . #+sb-futex nil
    #-sb-futex (%owner %head %tail))

(sb-xc:defstruct (semaphore (:copier nil)
                            (:constructor %make-semaphore (%count mutex queue)))
  "Semaphore type. The fact that a SEMAPHORE is a STRUCTURE-OBJECT
should be considered an implementation detail, and may change in the
future."
  (%count    0 :type (and (integer 0) fixnum))
  (waitcount 0 :type sb-vm:word)
  (mutex nil :read-only t :type mutex)
  (queue nil :read-only t :type waitqueue))

(declaim (sb-ext:freeze-type waitqueue semaphore))

(sb-ext:define-load-time-global *profiled-threads* :all)
(declaim (type (or (eql :all) list) *profiled-threads*))

(sb-xc:defstruct (thread (:constructor %make-thread (%name %ephemeral-p semaphore))
                         (:copier nil))
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  (%name         nil :type (or null simple-string)) ; C code could read this
  (%ephemeral-p  nil :type boolean :read-only t)
  ;; This is one of a few different views of a lisp thread:
  ;;  1. the memory space (thread->os_addr in C)
  ;;  2. 'struct thread' at some offset into the memory space, coinciding
  ;;     with the SB-VM::THREAD primitive object
  ;;  3. a pthread, which may reside anywhere, possibly the high end of the lisp stack
  ;;     (above the SP given to your entrypoint), whatever the pthread library chooses.
  ;;  4. the THREAD instance (this structure)
  ;; This value is 0 if the thread is not considered alive, though the pthread
  ;; may be running its termination code (unlinking from all_threads etc)
  (primitive-thread 0 :type sb-vm:word)
  ;; This is a redundant copy of the pthread identifier from the primitive thread.
  ;; It's needed in the SB-THREAD:THREAD as well because there are valid reasons to
  ;; manipulate the new thread before it has assigned 'th->os_thread = pthread_self()'.
  ;; While we always have access to the C struct thread from Lisp, apparently the C
  ;; code can't pass "&th->os_thread" as the first argument to pthread_create() for the
  ;; incredible reason that the word might be written *after* the memory pointed at
  ;; by 'th' has already been freed. Such action might seem to violate:
  ;;  "Before returning, a successful call to pthread_create() stores the ID of the
  ;;   new thread in the buffer pointed to by thread"
  ;;  (https://man7.org/linux/man-pages/man3/pthread_create.3.html)
  ;; but that's not exactly what POSIX says, which is only:
  ;;  "Upon successful completion, pthread_create() stores the ID of the created thread
  ;;   in the location referenced by thread."
  ;; (https://pubs.opengroup.org/onlinepubs/007908799/xsh/pthread_create.html)
  ;; so there seems to be some leeway, and linux + glibc provides more of a guarantee.
  ;; Technically we should have only one authoritative source of the pthread identifier,
  ;; but it's not too critical, it's just annoying that there are two sources of
  ;; the same value.
  ;; The slot is somewhat poorly named (for consistency with C) because though it may
  ;; correspond to an OS thread, it could be the case that the threading model has
  ;; user-visible threads that do not map directly to OSs threads (or LWPs).
  ;; Any use of THREAD-OS-THREAD from lisp should take care to ensure validity of
  ;; the thread id by holding the INTERRUPTIONS-LOCK.
  ;; Not needed for win32 threads.
  #-win32 (os-thread 0 :type sb-vm:word)
  ;; Keep a copy of the stack range for use in SB-EXT:STACK-ALLOCATED-P so that
  ;; we don't have to read it from the primitive thread which is unsafe for any
  ;; thread other than the current thread.
  ;; Usually this is a fixed amount below PRIMITIVE-THREAD, but the exact offset
  ;; varies by build configuration, and if #+win32 it is not related in any way.
  (control-stack-start 0 :type sb-vm:word)
  (control-stack-end 0 :type sb-vm:word)
  ;; At the beginning of the thread's life, this is a vector of data required
  ;; to start the user code. At the end, is it pointer to the 'struct thread'
  ;; so that it can be either freed or reused.
  (startup-info 0 :type (or fixnum (simple-vector 6)))
  ;; Whether this thread should be returned in LIST-ALL-THREADS.
  ;; This is almost-but-not-quite the same as what formerly
  ;; might have been known as the %ALIVE-P flag.
  (%visible 1 :type fixnum)
  (interruptions nil :type list)
  (interruptions-lock
   (make-mutex :name "thread interruptions lock")
   :type mutex :read-only t)

  ;; Per-thread memoization of GET-INTERNAL-REAL-TIME, for race-free update.
  ;; This might be a bignum, which is why we bother.
  #-64-bit (observed-internal-real-time-delta-sec 0 :type sb-vm:word)
  #-64-bit (observed-internal-real-time-delta-millisec
            ;; This needs a sentinel that can not possibly match an actual delta.
            ;; I have seen threads start up where 0 and 0 do match sec,msec
            ;; respectively, and then we'd return the cached NIL as if it were
            ;; the time. Forcing mismatch avoids putting in an extra test for NIL.
            (ash sb-ext:most-positive-word -1)
            :type sb-vm:signed-word)
  #-64-bit (internal-real-time)

  (max-stw-pause 0 :type sb-vm:word) ; microseconds
  (sum-stw-pause 0 :type sb-vm:word) ; "
  (ct-stw-pauses 0 :type sb-vm:word) ; to compute the avg
  ;; Measure elapsed time in GC in the resolution that clock_gettime returns
  ;; (nanoseconds) for 64-bit, or microseconds for 32-bit.
  ;; This can indicate >584 years if 64-bit or slightly over an hour if 32-bit.
  ;; Consider restarting your SBCL before wraparound occurs, if you care.
  (gc-virtual-time 0 :type sb-vm:word)

  ;; On succesful execution of the thread's lambda, a list of values.
  (result 0)
  ;; The completion condition _could_ be manifested as a condition var, but a difficulty
  ;; in doing so is that condition vars can always experience a spurious wakeup.
  ;; Dealing with timeouts becomes troublesome then. But we can utilize the fact that
  ;; WAIT-ON-SEMPAHORE implements a timeout, though as its comment says, the timeout
  ;; doesn't account for re-acquiring the internal mutex if that takes nontrivial time,
  ;; which it shouldn't since it guards very little work.
  ;; At any rate, the semaphore abstraction is never subject to spurious wakeup.
  (semaphore nil :type (or null semaphore))
  waiting-for)

(sb-xc:defstruct (foreign-thread
                  (:copier nil)
                  (:include thread (%name "callback"))
                  (:constructor make-foreign-thread ())
                  (:conc-name "THREAD-"))
  "Type of native threads which are attached to the runtime as Lisp threads
temporarily.")

(declaim (sb-ext:freeze-type mutex thread))
#-sb-xc-host
(progn
  (defvar *current-thread*)
  (declaim (type thread *current-thread*)))
