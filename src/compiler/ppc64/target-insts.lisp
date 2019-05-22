;;;; target-only parts of the instruction set definition for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PPC64-ASM")

(defun maybe-add-notes (regno dstate)
  (let* ((inst (sap-ref-int (dstate-segment-sap dstate)
                            (dstate-cur-offs dstate)
                4
                (dstate-byte-order dstate)))
         (op (ldb (byte 6 26) inst)))
    (case op
      ;; lwz
      (32
       (when (= regno (ldb (byte 5 16) inst)) ; only for the second
         (case (ldb (byte 5 16) inst)
           ;; reg_CODE
           (19
            (note-code-constant (ldb (byte 16 0) inst) dstate)))))
      ;; addi
      (14
       (when (= regno null-offset)
         (maybe-note-nil-indexed-object (ldb (byte 16 0) inst) dstate))))))

(defun unimp-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (xinstr-data chunk dstate)))
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
       (t
        (handle-break-args #'snarf-error-junk trap stream dstate))))))
