;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-THREAD")

(defstruct (thread (:constructor %make-thread)
                   (:copier nil))
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  (name          nil :type (or null simple-string)) ; C code could read this
  (%alive-p      nil :type boolean)
  (%ephemeral-p  nil :type boolean)
  ;; 0 is used on thread-less builds
  (os-thread  (ldb (byte sb-vm:n-word-bits 0) -1) :type sb-vm:word)
  ;; Keep a copy of CONTROL-STACK-END from the "primitive" thread (C memory).
  ;; Reading that memory for any thread except *CURRENT-THREAD* is not safe
  ;; due to possible unmapping on thread death. Technically this is a fixed amount
  ;; below PRIMITIVE-THREAD, but the exact offset varies by build configuration.
  (stack-end 0 :type sb-vm:word)
  ;; Points to the SB-VM::THREAD primitive object.
  ;; Yes, there are three different thread structures.
  (primitive-thread 0 :type sb-vm:word)
  (interruptions nil :type list)
  ;; On succesful execution of the thread's lambda a list of values.
  (result 0)
  (interruptions-lock
   (make-mutex :name "thread interruptions lock")
   :type mutex)
  (result-lock
   (make-mutex :name "thread result lock")
   :type mutex)
  waiting-for)

(declaim (inline thread-alive-p))
(defun thread-alive-p (thread)
  "Return T if THREAD is still alive. Note that the return value is
potentially stale even before the function returns, as the thread may exit at
any time."
  (thread-%alive-p thread))

(def!struct (mutex (:constructor make-mutex (&key name))
                   (:copier nil))
  "Mutex type."
  ;; C code could (but doesn't currently) access the name
  (name   nil :type (or null simple-string))
  (%owner nil :type (or null thread))
  #+(and sb-thread sb-futex)
  (state    0 :type fixnum))
(!set-load-form-method mutex (:xc))

(def!struct (avlnode (:constructor avlnode (key data left right)))
  (left  nil :read-only t)
  (right nil :read-only t)
  (key   0   :read-only t :type sb-vm:word)
  data)

;; The host has a stub for this macro. The cross-compiler doesn't use
;; it until it's seen. So no SB-XC:DEFMACRO needed
#-sb-xc-host
(defmacro with-system-mutex ((mutex &key without-gcing allow-with-interrupts)
                                    &body body)
  `(dx-flet ((with-system-mutex-thunk () ,@body))
     (,(cond (without-gcing
               'call-with-system-mutex/without-gcing)
             (allow-with-interrupts
              'call-with-system-mutex/allow-with-interrupts)
             (t
              'call-with-system-mutex))
       #'with-system-mutex-thunk
       ,mutex)))

;; Similar to above. The host doesn't need this one at all.
#-sb-xc-host
(defmacro with-recursive-system-lock ((lock) &body body)
  `(dx-flet ((recursive-system-lock-thunk () ,@body))
     (call-with-recursive-system-lock #'recursive-system-lock-thunk ,lock)))
