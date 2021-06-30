;;;; crude selective re-cross-compilation of the target system, like
;;;; Unix make(1), but much flakier because we don't keep track of the
;;;; (many!) dependencies between files

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :sb-cold)

;;; (This file is intended to be loaded into an after-xc.lisp core, so
;;; we don't need to LOAD any machinery (e.g. "src/cold/shared.lisp")
;;; which was already loaded in the course of setting up the system
;;; state which was frozen into that core.)

;;; basic test for up-to-date-ness of output with respect to input in
;;; the sense of Unix make(1)
(defun output-up-to-date-wrt-input-p (output input &optional flags)
  (and (probe-file output)
       ;; (Strict #'> and lax #'>= each have problems here, which
       ;; could become more noticeable as computation speed
       ;; accelerates while Common Lisp's 1-second granularity remains
       ;; the same. We use #'> because it's safer sometimes to
       ;; recompile unnecessarily than sometimes bogusly to assume
       ;; up-to-date-ness.)
       (> (file-write-date output)
          (file-write-date input))
       (if (member :assem flags)
           (every (lambda (stem)
                    (output-up-to-date-wrt-input-p
                     output (stem-source-path stem)))
                  (with-open-file (f "src/assembly/master.lisp")
                    ;; form is (MAPC (LAMBDA ()) '("file" ...))
                    (second (third (read f)))))
           t)))

;;; One possible use-case for slam.sh is to generate a trace-file for
;;; a file that is suddenly of interest, but was not of interest
;;; before.  In order for this to work, we need to reload the stems
;;; and flags from build-order.lisp-expr, the user needs to have added
;;; :trace-file as a flag.
(setf *stems-and-flags*
      (let ((*readtable* *xc-readtable*))
        (read-from-file "^build-order.lisp-expr" nil)))

;;; Don't care about deftransforms that get redefined.
;;; The target condition is defined in 'condition' which is a :not-host file.
;;; Without this, the host would complain about no such condition class
;;; if we try to signal it when executing the cross-compiler.
(define-condition sb-kernel:redefinition-with-deftransform (style-warning)
  ((transform :initarg :transform)))

(defvar *original-thread-local-specials* (cdr sb-thread::*thread-local-specials*))
(setq sb-thread::*thread-local-specials*
      ;; Do a shallow copy-tree. DEFINE-THREAD-LOCAL can perform destructive modification
      (cons :not-final (mapcar (lambda (x) (list (car x) (cadr x)))
                               *original-thread-local-specials*)))

(do-stems-and-flags (stem flags 2)
  (unless (position :not-target flags)
    (let ((srcname (stem-source-path stem))
          (objname (stem-object-path stem flags :target-compile)))
      (when (or (not (output-up-to-date-wrt-input-p objname srcname flags))
                ;; Back to our "new-trace-file" case, also build if
                ;; a trace file is desired but is out-of-date.
                (and (position :trace-file flags)
                     (not (output-up-to-date-wrt-input-p
                           (concatenate 'string (stem-remap-target stem)
                                        ".trace")
                           srcname))))
        (target-compile-stem stem flags)))))

(when (and (eq (car sb-thread::*thread-local-specials*) :not-final)
           (not (equal (cdr sb-thread::*thread-local-specials*)
                       *original-thread-local-specials*)))
  (sb-int:style-warn "Detected modified thread-local-specials.
Slam may not have recompiled everything as required."))
