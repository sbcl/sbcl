;;;; This file contains machinery for collecting forms that, in the
;;;; target Lisp, must happen before top level forms are run. The
;;;; forms are stuffed into named functions which will be explicitly
;;;; called in the appropriate order by !COLD-INIT.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")
;;; FIXME: Perhaps this belongs in the %SYS package like some other
;;; cold load stuff.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *!cold-init-forms*))

(defmacro !begin-collecting-cold-init-forms ()
  #+sb-xc '(eval-when (:compile-toplevel :execute)
             (when (boundp '*!cold-init-forms*)
               (warn "discarding old *!COLD-INIT-FORMS* value"))
             (setf *!cold-init-forms* nil))
  #-sb-xc nil)

;;; Note: Unlike the analogous COLD-INIT macro in CMU CL, this macro
;;; makes no attempt to simulate a top level situation by treating
;;; EVAL-WHEN forms specially.
(defmacro !cold-init-forms (&rest forms)
  ;; In the target Lisp, stuff the forms into a named function which
  ;; will presumably be executed at the appropriate stage of cold load
  ;; (i.e. basically as soon as possible).
  #+sb-xc (progn
            (setf *!cold-init-forms*
                  (nconc *!cold-init-forms* (copy-list forms)))
            nil)
  ;; In the cross-compilation host Lisp, cold load is not a
  ;; meaningful concept. Just execute the forms at load time.
  #-sb-xc `(progn ,@forms))

(defmacro !defun-from-collected-cold-init-forms (name)
  #+sb-xc `(progn
             (defun ,name ()
               ,@*!cold-init-forms*
               (values))
             (eval-when (:compile-toplevel :execute)
               (makunbound '*!cold-init-forms*)))
  #-sb-xc (declare (ignore name)))

;;; !DEFGLOBAL, !DEFPARAMETER and !DEFVAR are named by analogy
;;; with !COLD-INIT-FORMS and (not DEF!FOO) because they are
;;; basically additional cold-init-helpers to avoid the tedious sequence:
;;;    (!begin-collecting-cold-init-forms)
;;;    (defvar *foo*)
;;;    (!cold-init-forms (setq *foo* nil))
;;;    (!defun-from-cold-init-forms !some-cold-init-fun)
;;; or the less respectable (defvar *foo*) and a random SETQ in !COLD-INIT.
;;; Each is like its namesake, but also arranges so that genesis knows
;;; the initialization form, on which it calls EVAL and dumps as a constant
;;; when writing out the cold core image.
(macrolet ((def (wrapper real-name)
             `(defmacro ,wrapper (sym value &optional (doc nil doc-p))
                `(progn (eval-when (:compile-toplevel)
                          (!delayed-cold-set-symbol-value ',sym ',value))
                        (,',real-name ,sym ,value ,@(if doc-p (list doc)))))))
  (def !defglobal defglobal)
  (def !defparameter defparameter)
  (def !defvar defvar))

(defun !delayed-cold-set-symbol-value (symbol value-form)
  ;; Obfuscate the reference into SB-COLD to avoid "bad package for target"
  (let ((list (find-symbol "*SYMBOL-VALUES-FOR-GENESIS*" "SB-COLD")))
    (set list (acons symbol
                     (cons value-form (package-name *package*))
                     (delete symbol (symbol-value list) :key #'car)))))

;;; FIXME: Consider renaming this file asap.lisp,
;;; and the renaming the various things
;;;   *ASAP-FORMS* or *REVERSED-ASAP-FORMS*
;;;   WITH-ASAP-FORMS
;;;   ASAP or EVAL-WHEN-COLD-LOAD
;;;   DEFUN-FROM-ASAP-FORMS
;;; If so, add a comment explaining that ASAP is colloquial English for "as
;;; soon as possible", and has nothing to do with "system area pointer".
