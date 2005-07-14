;;;; cross-compile-time-only replacements for make-load-form
;;;; machinery.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; this probably deserves a word of explanation, as somewhat
;;; unusually we require the behaviour of this function to change
;;; depending on not at what stage we build it but at what stage we
;;; run it. This function will be called both when the host compiler
;;; dumps structures of type structure!object and when the
;;; cross-compiler does likewise; we don't control the dumper for the
;;; host compiler, so we use its own machinery for dumping host fasls,
;;; but we need to use target machinery for target fasls. We therefore
;;; dispatch on the presence of :SB-XC-HOST in the *FEATURES* list for
;;; which mechanism to use. This probably counts as a KLUDGE; a proper
;;; solution might be one or more of:
;;;
;;; * change def!struct to have two make-load-form-funs associated
;;;   with it; one to run when CL:MAKE-LOAD-FORM is called, and one
;;;   for SB!XC:MAKE-LOAD-FORM
;;;
;;; * implement MAKE-LOAD-FORM-SAVING-SLOTS properly rather than have
;;;   this magic value, and use it consistently.
;;;
;;; Also, something along these lines can remove the special case in
;;; EMIT-MAKE-LOAD-FORM in src/compiler/main.lisp.

(in-package "SB!INT")

(defun sb!xc:make-load-form-saving-slots (object &rest args
                                          &key slot-names environment)
  (declare (ignore environment))
  (if (member :sb-xc-host *features*)
      ;; we're still building the cross-compiler, so use the host's
      ;; mechanism:
      (apply #'make-load-form-saving-slots object args)
      ;; we're building cold fasls, so use the target's mechanism:
      ;;
      ;; KLUDGE: This is essentially the same definition as for the
      ;; target's MAKE-LOAD-FORM-SAVING-SLOTS; it would be nice to
      ;; share code with that if possible. -- CSR, 2002-05-30
      (if slot-names
          (bug "MAKE-LOAD-FORM-SAVING-SLOTS ~
                called with :SLOT-NAMES argument during cross-compilation")
          :sb-just-dump-it-normally)))

