;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

(def!type thread-name () 'simple-string)

(defstruct (thread (:constructor %make-thread)
                   (:copier nil))
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  (name          nil :type (or thread-name null))
  (%alive-p      nil :type boolean)
  (%ephemeral-p  nil :type boolean)
  #-sb-xc-host
  ;; 0 is used on thread-less builds
  (os-thread  (ldb (byte sb!vm:n-word-bits 0) -1) :type sb!vm:word)
  #-sb-xc-host
  ;; Points to the SB-VM::THREAD primitive object.
  ;; Yes, there are three different thread structures.
  (primitive-thread 0 :type sb!vm:word)
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

(def!struct (mutex (:constructor make-mutex (&key name))
                   (:copier nil))
  "Mutex type."
  (name   nil :type (or null thread-name))
  (%owner nil :type (or null thread))
  #!+(and sb-thread sb-futex)
  (state    0 :type fixnum))

;; The host has a stub for this macro. The cross-compiler doesn't use
;; it until it's seen. So no SB!XC:DEFMACRO needed
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
(defmacro with-recursive-system-lock ((lock &key without-gcing) &body body)
  `(dx-flet ((with-recursive-system-lock-thunk () ,@body))
     (,(cond (without-gcing
              'call-with-recursive-system-lock/without-gcing)
             (t
              'call-with-recursive-system-lock))
      #'with-recursive-system-lock-thunk
       ,lock)))
