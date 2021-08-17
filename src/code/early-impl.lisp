;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; entries in STATIC-SYMBOLS table, references to which can be compiled
;;; as though they're special variables
;;;
;;; FIXME: These should be listed once and only once, instead of
;;; listed here and then listed separately (and by now, 2001-06-06,
;;; slightly differently) elsewhere. (Maybe this is resolved?)
(declaim (special *posix-argv*
                  *stderr*
                  sb-vm:*current-catch-block*
                  sb-vm::*current-unwind-protect-block*
                  sb-vm::*alien-stack-pointer*
                  sb-vm:*control-stack-start*
                  sb-vm:*control-stack-end*
                  sb-vm:*binding-stack-start*
                  *allow-with-interrupts*
                  sb-unix::*unblock-deferrables-on-enabling-interrupts-p*
                  *interrupts-enabled*
                  *interrupt-pending*
                  #+sb-safepoint *thruption-pending*
                  #+sb-safepoint *in-safepoint*
                  *free-interrupt-context-index*
                  #-gencgc
                  sb-vm::*allocation-pointer*
                  sb-vm::*binding-stack-pointer*
                  sb-pcl::*cache-miss-values-stack*
                  sb-pcl::*dfun-miss-gfs-on-stack*))

;;; This is a slot of 'struct thread' if multithreaded,
;;; and the symbol-global-value should never be used.
;;; (And in any case it is not really a special var)
#+(and (or x86 x86-64) (not sb-thread))
(defvar *pseudo-atomic-bits* 0)

#+c-stack-is-control-stack
(setf (info :variable :always-bound 'sb-c:*alien-stack-pointer*) :always-bound)

;;; A unique GC id. This is supplied for code that needs to detect
;;; whether a GC has happened since some earlier point in time. For
;;; example:
;;;
;;;   (let ((epoch *gc-epoch*))
;;;      ...
;;;      (unless (eql epoch *gc-epoch)
;;;        ....))
;;;
;;; This isn't just a fixnum counter since then we'd have theoretical
;;; problems when exactly 2^29 GCs happen between epoch
;;; comparisons. Unlikely, but the cost of using a cons instead is too
;;; small to measure. -- JES, 2007-09-30
(declaim (type cons sb-kernel::*gc-epoch*))
(define-load-time-global sb-kernel::*gc-epoch* '(nil . nil))

;;; Default evaluator mode (interpeter / compiler)

(declaim (type (member :compile #+(or sb-eval sb-fasteval) :interpret)
               *evaluator-mode*))
(defparameter *evaluator-mode* :compile
  "Toggle between different evaluator implementations. If set to :COMPILE,
an implementation of EVAL that calls the compiler will be used. If set
to :INTERPRET, an interpreter will be used.")
(declaim (always-bound *evaluator-mode*))

(declaim (inline sb-vm:is-lisp-pointer))
(defun sb-vm:is-lisp-pointer (addr) ; Same as is_lisp_pointer() in C
  #-64-bit (oddp addr)
  #+ppc64 (= (logand addr #b101) #b100)
  #+(and 64-bit (not ppc64)) (not (logtest (logxor addr 3) 3)))
