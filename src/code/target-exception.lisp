;;;; code for handling Win32 exceptions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!UNIX")

;;;
;;; An awful lot of this stuff is stubbed out for now. We basically
;;; only handle inbound exceptions (the local equivalent to unblockable
;;; signals), and we're only picking off the sigsegv and sigfpe traps.
;;;
;;; This file is based on target-signal.lisp, but most of that went
;;; away. Some of it might want to be put back or emulated.
;;;

;;; SIGINT is handled like BREAK, except that ANSI BREAK ignores
;;; *DEBUGGER-HOOK*, but we want SIGINT's BREAK to respect it, so that
;;; SIGINT in --disable-debugger mode will cleanly terminate the system
;;; (by respecting the *DEBUGGER-HOOK* established in that mode).
;;;
;;; We'd like to have this work, but that would require some method of
;;; delivering a "blockable signal". Windows doesn't really have the
;;; concept, so we need to play with the threading functions to emulate
;;; it (especially since the local equivalent of SIGINT comes in on a
;;; separate thread). This is on the list for fixing later on, and will
;;; be required before we implement threads (because of stop-for-gc).
;;;
;;; This specific bit of functionality may well be implemented entirely
;;; in the runtime.
#|
(defun sigint-%break (format-string &rest format-arguments)
  (flet ((break-it ()
           (apply #'%break 'sigint format-string format-arguments)))
    (sb!thread:interrupt-thread (sb!thread::foreground-thread) #'break-it)))
|#

;;; Actual exception handler. We hit something the runtime doesn't
;;; want to or know how to deal with (that is, not a sigtrap or gc
;;; wp violation), so it calls us here.

(defun sb!kernel:handle-win32-exception (context exception-record)
  (error "An exception occured! Context ~A, exception-record ~A."
         context exception-record))

;;;; etc.

;;; CMU CL comment:
;;;   Magically converted by the compiler into a break instruction.
;;; SBCL/Win32 comment:
;;;   I don't know if we still need this or not. Better safe for now.
(defun receive-pending-interrupt ()
  (receive-pending-interrupt))
