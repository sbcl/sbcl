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
	    (:constructor make-unix-signal (%name %number))
	    (:copier nil))
  ;; signal keyword (e.g. :SIGINT for the Unix SIGINT signal)
  (%name   (required-argument) :type keyword :read-only t)
  ;; signal number
  (%number (required-argument) :type integer :read-only t))

;;; list of all defined UNIX-SIGNALs
(defvar *unix-signals* nil)

(defmacro !def-unix-signal (name number)
  (declare (type keyword name))
  (declare (type (and fixnum unsigned-byte) number))
  `(push (make-unix-signal ,name ,number) *unix-signals*))

(/show0 "signal.lisp 131")

(defun unix-signal-or-lose (designator)
  (or (find designator (the list *unix-signals*)
	    :key (etypecase designator
		   (symbol #'unix-signal-%name)
		   (number #'unix-signal-%number)))
      (error "not a valid signal name or number: ~S" designator)))

(/show0 "signal.lisp 142")

;;; Return the name of the designated signal.
(defun unix-signal-name (designator)
  (symbol-name (unix-signal-%name (unix-signal-or-lose designator))))

(/show0 "signal.lisp 150")

;;; Return the number of the designated signal.
(defun unix-signal-number (designator)
  (unix-signal-%number (unix-signal-or-lose designator)))

(/show0 "signal.lisp 168")

;;; known signals
(/show0 "defining Unix signals")
(!def-unix-signal :CHECK 0) ; check
(/show0 "done defining CHECK")
(!def-unix-signal :SIGHUP 1) ; hangup
(/show0 "done defining SIGHUP")
(!def-unix-signal :SIGINT 2) ; interrupt
(/show0 "done defining SIGINT")
(!def-unix-signal :SIGQUIT 3) ; quit
(!def-unix-signal :SIGILL 4) ; illegal instruction
(!def-unix-signal :SIGTRAP 5) ; trace trap
(!def-unix-signal :SIGIOT 6) ; IOT instruction
#!-linux
(!def-unix-signal :SIGEMT 7) ; EMT instruction
(!def-unix-signal :SIGFPE 8) ; floating point exception
(!def-unix-signal :SIGKILL 9) ; kill
(!def-unix-signal :SIGBUS #!-linux 10 #!+linux 7) ; bus error
(!def-unix-signal :SIGSEGV 11) ; segmentation violation
#!-linux
(!def-unix-signal :SIGSYS 12) ; bad argument to system call
(!def-unix-signal :SIGPIPE 13) ; write on a pipe with no one to read it
(!def-unix-signal :SIGALRM 14) ; alarm clock
(!def-unix-signal :SIGTERM 15) ; software termination signal
#!+linux
(!def-unix-signal :SIGSTKFLT 16) ; stack fault on coprocessor
(!def-unix-signal :SIGURG ; urgent condition present on socket
  #!+svr4 21
  #!-(or hpux svr4 linux) 16
  #!+hpux 29
  #!+linux 23)
(!def-unix-signal :SIGSTOP ; stop
  #!-(or hpux svr4 linux) 17
  #!+hpux 24
  #!+svr4 23
  #!+linux 19)
(!def-unix-signal :SIGTSTP ;  stop signal generated from keyboard
  #!-(or hpux svr4 linux) 18
  #!+hpux 25
  #!+svr4 24
  #!+linux 20)
(!def-unix-signal :SIGCONT ; continue after stop
  #!-(or hpux svr4 linux) 19
  #!+hpux 26
  #!+svr4 25
  #!+linux 18)
(!def-unix-signal :SIGCHLD ; Child status has changed.
  #!-(or linux hpux) 20
  #!+hpux 18
  #!+linux 17)
(!def-unix-signal :SIGTTIN ; background read attempted from control terminal
  #!-(or hpux svr4) 21
  #!+hpux 27
  #!+svr4 26)
(!def-unix-signal :SIGTTOU ; background write attempted to control terminal
  #!-(or hpux svr4) 22
  #!+hpux 28
  #!+svr4 27)
(!def-unix-signal :SIGIO ; I/O is possible on a descriptor.
  #!-(or hpux irix linux) 23
  #!+(or hpux irix) 22
  #!+linux 29)
#!-hpux
(!def-unix-signal :SIGXCPU ; CPU time limit exceeded
  #!-svr4 24
  #!+svr4 30)
#!-hpux
(!def-unix-signal :SIGXFSZ ;  file size limit exceeded
  #!-svr4 25
  #!+svr4 31)
(!def-unix-signal :SIGVTALRM ; virtual time alarm
  #!-(or hpux svr4) 26
  #!+hpux 20
  #!+svr4 28)
(!def-unix-signal :SIGPROF ;  profiling timer alarm
  #!-(or hpux svr4 linux) 27
  #!+hpux 21
  #!+svr4 29
  #!+linux 30)
(!def-unix-signal :SIGWINCH ; window size change
  #!-(or hpux svr4) 28
  #!+hpux 23
  #!+svr4 20)
(!def-unix-signal :SIGUSR1 ;  user-defined signal 1
  #!-(or hpux svr4 linux) 30
  #!+(or hpux svr4) 16
  #!+linux 10)
(!def-unix-signal :SIGUSR2 ; user-defined signal 2
  #!-(or hpux svr4 linux) 31
  #!+(or hpux svr4) 17
  #!+linux 12)

;;; SVR4 (or Solaris?) specific signals
#!+svr4
(!def-unix-signal :SIGWAITING 32) ; Process's LWPs are blocked.

(sb!xc:defmacro sigmask (&rest signals)
  #!+sb-doc
  "Returns a mask given a set of signals."
  (apply #'logior
	 (mapcar (lambda (signal)
		   (ash 1 (1- (unix-signal-number signal))))
		 signals)))

(/show0 "done with signal.lisp")
