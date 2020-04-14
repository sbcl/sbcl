;;;; some help for the definition of generic non-local exit

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Return a list of TNs that can be used to snapshot the dynamic
;;; state for use with the SAVE- and RESTORE-DYNAMIC-ENVIRONMENT VOPs.
#-unbind-in-unwind
(defun make-dynamic-state-tns ()
  (make-n-tns #.(let ((nsave
                       (sb-c::vop-info-num-results
                        (template-or-lose 'save-dynamic-state)))
                      (nrestore
                       (sb-c::vop-info-num-args
                        (template-or-lose 'restore-dynamic-state))))
                  (aver (= nsave nrestore))
                  nsave)
              *backend-t-primitive-type*))

