;;;; the machine-specific support routines needed by the file assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun generate-call-sequence (name style vop options)
  (declare (ignore options vop))
  (ecase style
    ((:none :raw :full-call-no-return)
     (let ((lr (gensym)))
       (values
        `((progn
            ,lr
            ,@(if (eq style :none)
                  `((load-inline-constant tmp-tn '(:fixup ,name :assembly-routine))
                    (inst br tmp-tn))
                  `((load-inline-constant ,lr '(:fixup ,name :assembly-routine))
                    (inst blr ,lr)))))
        `((:temporary (:sc non-descriptor-reg :from (:eval 0) :to (:eval 1) :offset lr-offset)
                      ,lr)))))))

(defun generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst ret)))
    ((:none :full-call-no-return))))
