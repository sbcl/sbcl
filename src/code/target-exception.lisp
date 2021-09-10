;;;; code for handling Win32 exceptions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-WIN32")

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
    (sb-thread:interrupt-thread (sb-thread::foreground-thread) #'break-it)))
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
     (cons +exception-stack-overflow+        'sb-kernel::control-stack-exhausted)
     ;; Various
     (cons-name +exception-single-step+)
     (cons +exception-access-violation+ 'memory-fault-error)
     #+x86-64
     (cons +exception-heap-corruption+ 'foreign-heap-corruption)
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
            (exception-information (array system-area-pointer
                                          #.+exception-maximum-parameters+))))

;;; DBG_PRINTEXCEPTION_C shouldn't be fatal, and even if it is related to
;;; something bad, better to print the message than just fail with no info
(defun dbg-printexception-c (record)
  (when (= (slot record 'number-parameters) 2)
    ;; (sap-int (deref (slot record 'exception-information) 0)) =
    ;; length of string including 0-terminator
    (warn "DBG_PRINTEXCEPTION_C: ~a"
          (cast
           (sap-alien (deref (slot record 'exception-information) 1)
                      (* char))
           c-string))))

(defun dbg-printexception-wide-c (record)
  (when (= (slot record 'number-parameters) 4)
    ;; (sap-alien (deref (slot record 'exception-information) 3)) =
    ;; WideCharToMultiByte string
    (warn "DBG_PRINTEXCEPTION_WIDE_C: ~a"
          (cast
           (sap-alien (deref (slot record 'exception-information) 1)
                      (* char))
           system-string))))

(define-condition exception (error)
  ((code :initarg :code :reader exception-code)
   (context :initarg :context :reader exception-context)
   (record :initarg :record :reader exception-record))
  (:report (lambda (c s)
             (format s "An exception occurred in context ~S: ~S. (Exception code: ~S)"
                     (exception-context c)
                     (exception-record c)
                     (exception-code c)))))

;;; Undocumented exception (STATUS_HEAP_CORRUPTION). Occurs when calling free()
;;; with a bad pointer and possibly other places. On 64-bit processes,
;;; frame-based handlers don't get a chance to handle this exception because the
;;; HeapSetInformation() option HeapEnableTerminationOnCorruption is enabled by
;;; default and cannot be disabled. For the sake of interactive development and
;;; error reporting, we special-case this exception in our vectored exception
;;; handler, otherwise the SBCL process would be abruptly terminated.
#+x86-64
(define-condition foreign-heap-corruption (error) ()
  (:report
   #.(format nil "A foreign heap corruption exception occurred. (Exception code: ~S)"
             +exception-heap-corruption+)))

;;; Actual exception handler. We hit something the runtime doesn't
;;; want to or know how to deal with (that is, not a sigtrap or gc wp
;;; violation), so it calls us here.
(defun sb-kernel:handle-win32-exception (context-sap exception-record-sap)
  (let* ((record (deref (sap-alien exception-record-sap (* (struct exception-record)))))
         (code (slot record 'exception-code))
         (condition-name (cdr (assoc code *exception-code-map*)))
         (sb-debug:*stack-top-hint* (sb-kernel:find-interrupted-frame)))
    (cond ((stringp condition-name)
           (error condition-name))
          ((and condition-name
                (subtypep condition-name 'arithmetic-error))
           (multiple-value-bind (op operands)
               (sb-di::decode-arithmetic-error-operands context-sap)
             ;; Reset the accumulated exceptions
             (setf (ldb sb-vm:float-sticky-bits (sb-vm:floating-point-modes)) 0)
             (error condition-name :operation op
                                   :operands operands)))
          ((eq condition-name 'memory-fault-error)
           (error 'memory-fault-error :address
                  (sap-int (deref (slot record 'exception-information) 1))))
          (condition-name
           (error condition-name))
          ((= code +dbg-printexception-c+)
           (dbg-printexception-c record))
          ((= code +dbg-printexception-wide-c+)
           (dbg-printexception-wide-c record))
          (t
           (cerror "Return from the exception handler"
                   'exception :context context-sap :record exception-record-sap
                              :code code)))))


(in-package "SB-UNIX")

(defun sb-kernel:signal-cold-init-or-reinit ()
  "Enable all the default signals that Lisp knows how to deal with."
  (unblock-deferrable-signals)
  (values))
