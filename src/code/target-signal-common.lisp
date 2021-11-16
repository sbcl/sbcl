;;;; The common stuff for signals and exceptions (win32).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS

(in-package "SB-UNIX")

;;; CMU CL comment:
;;;   Magically converted by the compiler into a break instruction.
(defun receive-pending-interrupt ()
  (receive-pending-interrupt))

(defmacro with-interrupt-bindings (&body body)
  `(let*
       ;; KLUDGE: Whatever is on the PCL stacks before the interrupt
       ;; handler runs doesn't really matter, since we're not on the
       ;; same call stack, really -- and if we don't bind these (esp.
       ;; the cache one) we can get a bogus metacircle if an interrupt
       ;; handler calls a GF that was being computed when the interrupt
       ;; hit.
       ((sb-pcl::*cache-miss-values-stack* nil)
        (sb-pcl::*dfun-miss-gfs-on-stack* nil))
     ,@body))

(defun unblock-deferrable-signals ()
  (with-alien ((%unblock-deferrable-signals
                (function void unsigned-long) :extern
                "unblock_deferrable_signals"))
    (alien-funcall %unblock-deferrable-signals 0)
    nil))

(defun with-deferrable-signals-unblocked (enable-interrupts function)
  (cond ((and enable-interrupts
              *unblock-deferrables-on-enabling-interrupts-p*)
         (unwind-protect
              (let (*unblock-deferrables-on-enabling-interrupts-p*)
                (unblock-deferrable-signals)
                (when (or *interrupt-pending*
                          #+sb-safepoint *thruption-pending*)
                  (receive-pending-interrupt))
                (funcall function))
           (alien-funcall (extern-alien "block_deferrable_signals"
                                        (function (values) int))
                          0)))
        (t
         (when (and enable-interrupts
                    (or *interrupt-pending*
                        #+sb-safepoint *thruption-pending*))
           (receive-pending-interrupt))
         (funcall function))))
