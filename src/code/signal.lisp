;;;; handling UNIX signals

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!UNIX")

;;;; macros for dynamically enabling and disabling signal handling

;;; Notes on how the without-interrupts/with-interrupts stuff works:
;;;
;;; Before invoking the supplied handler for any of the signals that
;;; can be blocked, the C interrupt support code checks to see whether
;;; *interrupts-enabled* has been bound to NIL. If so, it saves the
;;; signal number and the value of the signal mask (from the signal
;;; context), sets the signal mask to block all blockable signals,
;;; sets *interrupt-pending* and returns without handling the signal.
;;;
;;; When we drop out the without interrupts, we check to see whether
;;; *interrupt-pending* has been set. If so, we call
;;; do-pending-interrupt, which generates a SIGTRAP. The C code
;;; invokes the handler for the saved signal instead of the SIGTRAP
;;; after replacing the signal mask in the signal context with the
;;; saved value. When that hander returns, the original signal mask is
;;; installed, allowing any other pending signals to be handled.
;;;
;;; This means that the cost of without-interrupts is just a special
;;; binding in the case when no signals are delivered (the normal
;;; case). It's only when a signal is actually delivered that we use
;;; any system calls, and by then the cost of the extra system calls
;;; are lost in the noise when compared with the cost of delivering
;;; the signal in the first place.

;;; Magically converted by the compiler into a break instruction.
(defun do-pending-interrupt ()
  (do-pending-interrupt))

#!-gengc (progn

(defvar *interrupts-enabled* t)
(defvar *interrupt-pending* nil)

(sb!xc:defmacro without-interrupts (&body body)
  #!+sb-doc
  "Execute BODY in a context impervious to interrupts."
  (let ((name (gensym)))
    `(flet ((,name () ,@body))
       (if *interrupts-enabled*
	   (unwind-protect
	       (let ((*interrupts-enabled* nil))
		 (,name))
	     ;; FIXME: Does it matter that an interrupt coming in here
	     ;; could be executed before any of the pending interrupts?
	     ;; Or do incoming interrupts have the good grace to check
	     ;; whether interrupts are pending before executing themselves
	     ;; immediately?
	     (when *interrupt-pending*
	       (do-pending-interrupt)))
	   (,name)))))

(sb!xc:defmacro with-interrupts (&body body)
  #!+sb-doc
  "Allow interrupts while executing BODY. As interrupts are normally allowed,
  this is only useful inside a WITHOUT-INTERRUPTS."
  (let ((name (gensym)))
    `(flet ((,name () ,@body))
       (if *interrupts-enabled*
	   (,name)
	   (let ((*interrupts-enabled* t))
	     (when *interrupt-pending*
	       (do-pending-interrupt))
	     (,name))))))

) ; PROGN

;;; On the GENGC system, we have to do it slightly differently because of the
;;; existence of threads. Each thread has a suspends_disabled_count in its
;;; mutator structure. When this value is other then zero, the low level stuff
;;; will not suspend the thread, but will instead set the suspend_pending flag
;;; (also in the mutator). So when we finish the without-interrupts, we just
;;; check the suspend_pending flag and trigger a do-pending-interrupt if
;;; necessary.

#!+gengc
(defmacro without-interrupts (&body body)
  `(unwind-protect
       (progn
	 (locally
	   (declare (optimize (speed 3) (safety 0)))
	   (incf (sb!kernel:mutator-interrupts-disabled-count)))
	 ,@body)
     (locally
       (declare (optimize (speed 3) (safety 0)))
       (when (and (zerop (decf (sb!kernel:mutator-interrupts-disabled-count)))
		  (not (zerop (sb!kernel:mutator-interrupt-pending))))
	 (do-pending-interrupt)))))

;;;; utilities for dealing with signal names and numbers

(defstruct (unix-signal
	    (:constructor make-unix-signal (%name %number %description)))
  %name				    ; signal keyword
  (%number nil :type integer)       ; UNIX signal number
  (%description nil :type string))  ; documentation

(defvar *unix-signals* nil
  #!+sb-doc
  "A list of Unix signal structures.")

(defmacro def-unix-signal (name number description)
  (let ((symbol (intern (symbol-name name))))
    `(progn
       ;; KLUDGE: This PUSH should be probably be something like PUSHNEW if we
       ;; want to be able to cleanly reload this file. (Or perhaps
       ;; *UNIX-SIGNALS* should be a hash table keyed by signal name, or a
       ;; vector keyed by signal number?)
       (push (make-unix-signal ,name ,number ,description) *unix-signals*)
       ;; This is to make the new signal lookup stuff compatible with
       ;; old code which expects the symbol with the same print name as
       ;; our keywords to be a constant with a value equal to the signal
       ;; number.
       (defconstant ,symbol ,number ,description))))

(defun unix-signal-or-lose (arg)
  (let ((signal (find arg *unix-signals*
		      :key (etypecase arg
			     (symbol #'unix-signal-%name)
			     (number #'unix-signal-%number)))))
    (unless signal
      (error "~S is not a valid signal name or number." arg))
    signal))

(defun unix-signal-name (signal)
  #!+sb-doc
  "Return the name of the signal as a string. Signal should be a valid
  signal number or a keyword of the standard UNIX signal name."
  (symbol-name (unix-signal-%name (unix-signal-or-lose signal))))

(defun unix-signal-description (signal)
  #!+sb-doc
  "Return a string describing signal. Signal should be a valid signal
  number or a keyword of the standard UNIX signal name."
  (unix-signal-%description (unix-signal-or-lose signal)))

(defun unix-signal-number (signal)
  #!+sb-doc
  "Return the number of the given signal. Signal should be a valid
  signal number or a keyword of the standard UNIX signal name."
  (unix-signal-%number (unix-signal-or-lose signal)))

;;; Known signals
(def-unix-signal :CHECK 0 "Check")

(def-unix-signal :SIGHUP 1 "Hangup")
(def-unix-signal :SIGINT 2 "Interrupt")
(def-unix-signal :SIGQUIT 3 "Quit")
(def-unix-signal :SIGILL 4 "Illegal instruction")
(def-unix-signal :SIGTRAP 5 "Trace trap")
(def-unix-signal :SIGIOT 6 "Iot instruction")
#!-linux
(def-unix-signal :SIGEMT 7 "Emt instruction")
(def-unix-signal :SIGFPE 8 "Floating point exception")
(def-unix-signal :SIGKILL 9 "Kill")
(def-unix-signal :SIGBUS #!-linux 10 #!+linux 7 "Bus error")
(def-unix-signal :SIGSEGV 11 "Segmentation violation")
#!-linux
(def-unix-signal :SIGSYS 12 "Bad argument to system call")
(def-unix-signal :SIGPIPE 13 "Write on a pipe with no one to read it")
(def-unix-signal :SIGALRM 14 "Alarm clock")
(def-unix-signal :SIGTERM 15 "Software termination signal")
#!+linux
(def-unix-signal :SIGSTKFLT 16 "Stack fault on coprocessor")
(def-unix-signal :SIGURG #!+svr4 21 #!-(or hpux svr4 linux) 16 #!+hpux 29
  #!+linux 23 "Urgent condition present on socket")
(def-unix-signal :SIGSTOP #!-(or hpux svr4 linux) 17 #!+hpux 24 #!+svr4 23
  #!+linux 19 "Stop")
(def-unix-signal :SIGTSTP #!-(or hpux svr4 linux) 18 #!+hpux 25 #!+svr4 24
  #!+linux 20 "Stop signal generated from keyboard")
(def-unix-signal :SIGCONT #!-(or hpux svr4 linux) 19 #!+hpux 26 #!+svr4 25
  #!+linux 18 "Continue after stop")
(def-unix-signal :SIGCHLD #!-(or linux hpux) 20
  #!+hpux 18 #!+linux 17 "Child status has changed")
(def-unix-signal :SIGTTIN #!-(or hpux svr4) 21 #!+hpux 27 #!+svr4 26
  "Background read attempted from control terminal")
(def-unix-signal :SIGTTOU #!-(or hpux svr4) 22 #!+hpux 28 #!+svr4 27
  "Background write attempted to control terminal")
(def-unix-signal :SIGIO #!-(or hpux irix linux) 23 #!+(or hpux irix) 22
  #!+linux 29
  "I/O is possible on a descriptor")
#!-hpux
(def-unix-signal :SIGXCPU #!-svr4 24 #!+svr4 30  "Cpu time limit exceeded")
#!-hpux
(def-unix-signal :SIGXFSZ #!-svr4 25 #!+svr4 31 "File size limit exceeded")
(def-unix-signal :SIGVTALRM #!-(or hpux svr4) 26 #!+hpux 20 #!+svr4 28
    "Virtual time alarm")
(def-unix-signal :SIGPROF #!-(or hpux svr4 linux) 27 #!+hpux 21 #!+svr4 29
  #!+linux 30 "Profiling timer alarm")
(def-unix-signal :SIGWINCH #!-(or hpux svr4) 28 #!+hpux 23 #!+svr4 20
    "Window size change")
(def-unix-signal :SIGUSR1 #!-(or hpux svr4 linux) 30 #!+(or hpux svr4) 16
  #!+linux 10 "User defined signal 1")
(def-unix-signal :SIGUSR2 #!-(or hpux svr4 linux) 31 #!+(or hpux svr4) 17
  #!+linux 12 "User defined signal 2")

#!+mach
(def-unix-signal :SIGEMSG 30 "Mach Emergency message")
#!+mach
(def-unix-signal :SIGMSG 31 "Mach message")

;;; SVR4 (or Solaris?) specific signals
#!+svr4
(def-unix-signal :SIGWAITING 32 "Process's lwps are blocked")

(sb!xc:defmacro sigmask (&rest signals)
  #!+sb-doc
  "Returns a mask given a set of signals."
  (apply #'logior
	 (mapcar #'(lambda (signal)
		     (ash 1 (1- (unix-signal-number signal))))
		 signals)))
