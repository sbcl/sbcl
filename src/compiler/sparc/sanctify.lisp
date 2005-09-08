;;;; Do whatever is necessary to make the given code component
;;;; executable.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "SB!VM")

;;; The comments in some of the cmucl source read that "the i and d
;;; caches are unified, and so there is no need to flush them."
;;;
;;; Other bits of the cmucl source say that this is not true for
;;; "newer machines, such as the SuperSPARC and MicroSPARC based
;;; ones". Welcome to the 21st century... -- CSR, 2002-05-06
(defun sanctify-for-execution (component)
  (without-gcing
   (alien-funcall (extern-alien "os_flush_icache"
                                (function void
                                          system-area-pointer
                                          unsigned-long))
                  (code-instructions component)
                  (* (code-header-ref component code-code-size-slot)
                     n-word-bytes)))
  nil)
