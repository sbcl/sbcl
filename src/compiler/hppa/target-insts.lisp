;;;; This file is for stuff which was in CMU CL's insts.lisp
;;;; file, but which in the SBCL build process can't be compiled
;;;; into code for the cross-compilation host.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!HPPA-ASM")

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (break-im5 chunk dstate)))
     (case trap
       (#.cerror-trap
        (nt "Cerror trap")
        (handle-break-args #'snarf-error-junk trap stream dstate))
       (#.breakpoint-trap
        (nt "Breakpoint trap"))
       (#.pending-interrupt-trap
        (nt "Pending interrupt trap"))
       (#.halt-trap
        (nt "Halt trap"))
       (#.fun-end-breakpoint-trap
        (nt "Function end breakpoint trap"))
       (#.single-step-around-trap
        (nt "Single step around trap"))
       (#.error-trap
        (nt "Error trap")
        (handle-break-args (lambda (sap offset trap &optional length-only)
                             (snarf-error-junk sap offset trap length-only nil))
                           trap
                           stream dstate))))))
