;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *current-thread* nil))

(def!type thread-name () 'simple-string)

(defstruct (thread (:constructor %make-thread))
  #!+sb-doc
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  (name          nil :type (or thread-name null))
  (%alive-p      nil :type boolean)
  (%ephemeral-p  nil :type boolean)
  #-sb-xc-host
  (os-thread     0 :type sb!vm:word)
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

(def!struct (mutex (:constructor make-mutex (&key name)))
  #!+sb-doc
  "Mutex type."
  (name   nil :type (or null thread-name))
  (%owner nil :type (or null thread))
  #!+(and sb-thread sb-futex)
  (state    0 :type fixnum))
