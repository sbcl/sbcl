;;;; code for handling Win32 exceptions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!WIN32")

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
#||
(defun sigint-%break (format-string &rest format-arguments)
  (flet ((break-it ()
           (apply #'%break 'sigint format-string format-arguments)))
    (sb!thread:interrupt-thread (sb!thread::foreground-thread) #'break-it)))
||#

;;; Map Windows Exception code to condition names: symbols or strings
(defvar *exception-code-map*
  (macrolet ((cons-name (symbol)
               `(cons ,symbol ,(remove #\+ (substitute #\_ #\- (string symbol))))))
    (list
     ;; Floating point exceptions
     (cons +exception-flt-divide-by-zero+    'division-by-zero)
     (cons +exception-flt-invalid-operation+ 'floating-point-invalid-operation)
     (cons +exception-flt-underflow+         'floating-point-underflow)
     (cons +exception-flt-overflow+          'floating-point-overflow)
     (cons +exception-flt-inexact-result+    'floating-point-inexact)
     (cons +exception-flt-denormal-operand+  'floating-point-exception)
     (cons +exception-flt-stack-check+       'floating-point-exception)
     ;; Stack overflow
     (cons +exception-stack-overflow+        'sb!kernel::control-stack-exhausted)
     ;; Various
     (cons-name +exception-single-step+)
     (cons-name +exception-access-violation+) ; FIXME: should turn into MEMORY-FAULT-ERROR
                                              ; plus the faulting address
     (cons-name +exception-array-bounds-exceeded+)
     (cons-name +exception-breakpoint+)
     (cons-name +exception-datatype-misalignment+)
     (cons-name +exception-illegal-instruction+)
     (cons-name +exception-in-page-error+)
     (cons-name +exception-int-divide-by-zero+)
     (cons-name +exception-int-overflow+)
     (cons-name +exception-invalid-disposition+)
     (cons-name +exception-noncontinuable-exception+)
     (cons-name +exception-priv-instruction+))))

(define-alien-type ()
    (struct exception-record
            (exception-code dword)
            (exception-flags dword)
            (exception-record system-area-pointer)
            (exception-address system-area-pointer)
            (number-parameters dword)
            (exception-information system-area-pointer)))

;;; Actual exception handler. We hit something the runtime doesn't
;;; want to or know how to deal with (that is, not a sigtrap or gc wp
;;; violation), so it calls us here.
(defun sb!kernel:handle-win32-exception (context-sap exception-record-sap)
  (let* ((record (deref (sap-alien exception-record-sap (* (struct exception-record)))))
         (code (slot record 'exception-code))
         (condition-name (cdr (assoc code *exception-code-map*)))
         (sb!debug:*stack-top-hint* (nth-value 1 (sb!kernel:find-interrupted-name-and-frame))))
    (if condition-name
        (error condition-name)
        (error "An exception occurred in context ~S: ~S. (Exception code: ~S)"
               context-sap exception-record-sap code))))

;;;; etc.

;;; CMU CL comment:
;;;   Magically converted by the compiler into a break instruction.
;;; SBCL/Win32 comment:
;;;   I don't know if we still need this or not. Better safe for now.
(defun receive-pending-interrupt ()
  (receive-pending-interrupt))
