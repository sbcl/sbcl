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

;;; N.B.: If you alter this definition, then you need to verify that FOP-FUNCALL
;;; in genesis can properly emulate MAKE-MUTEX for the altered structure,
;;; or even better, make sure that genesis can emulate any constructor,
;;; provided that it is sufficiently trivial.
(sb-xc:defstruct (mutex (:constructor make-mutex (&key name))
                        (:copier nil))
  "Mutex type."
  ;; C code could (but doesn't currently) access the name
  #+(and sb-thread sb-futex)
  (state    0 :type fixnum)
  ;; If adding slots between STATE and NAME, please see futex_name() in linux_os.c
  ;; which attempts to divine a string from a futex word address.
  (name   nil :type (or null simple-string))
  (%owner nil :type (or null thread)))

(sb-xc:defstruct (thread (:constructor %make-thread (name %ephemeral-p))
                         (:copier nil))
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  (name          nil :type (or null simple-string)) ; C code could read this
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
  ;; Keep a copy of CONTROL-STACK-END from the "primitive" thread.
  ;; Reading that memory for any thread except *CURRENT-THREAD* is not safe
  ;; due to possible unmapping on thread death.
  ;; Usually this is a fixed amount below PRIMITIVE-THREAD, but the exact offset
  ;; varies by build configuration, and if #+win32 it is not related in any way.
  (stack-end 0 :type sb-vm:word)
  (interruptions nil :type list)
  ;; On succesful execution of the thread's lambda a list of values.
  (result 0)
  (interruptions-lock
   (make-mutex :name "thread interruptions lock")
   :type mutex :read-only t)
  (result-lock
   (make-mutex :name "thread result lock")
   :type (or null mutex) :read-only t)
  waiting-for)

(sb-xc:defstruct (foreign-thread
                  (:copier nil)
                  (:include thread (result-lock nil) (name "callback"))
                  (:constructor make-foreign-thread ())
                  (:conc-name "THREAD-"))
  "Type of native threads which are attached to the runtime as Lisp threads
temporarily.")

#+(and sb-safepoint-strictly (not win32))
(sb-xc:defstruct (signal-handling-thread
                  (:copier nil)
                  (:include foreign-thread)
                  (:conc-name "THREAD-"))
  "Asynchronous signal handling thread."
  (signal-number nil :type integer))

#-sb-xc-host (declaim (sb-ext:freeze-type mutex thread))
