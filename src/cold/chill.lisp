;;;; This file is not used at cold load time. Instead, it can be
;;;; loaded into an initialized SBCL to get it into a nostalgic frame
;;;; of mind, remembering the way things were in cold init, so that it
;;;; can READ code which is ordinarily read only when bootstrapping.
;;;; (This can be useful when debugging the system, since the debugger
;;;; likes to be able to read the source for the code. It can also be
;;;; useful when experimenting with patches on a running system.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defpackage "SB-COLD"
  (:use "CL"))
(in-package "SB-COLD")

;;; We need the #! readtable modifications.
(load (merge-pathnames "shebang.lisp" *load-truename*))

;;; #!+ and #!- now refer to *FEATURES* values (as opposed to the way
;;; that they referred to special target-only *SHEBANG-FEATURES* values
;;; during cold init).
(setf sb-cold:*shebang-features* *features*)
;;; Just in case we want to play with the initial value of
;;; backend-subfeatures
(setf sb-cold:*shebang-backend-subfeatures* sb-c:*backend-subfeatures*)

(handler-bind (#+sb-package-locks (sb-ext:package-locked-error #'continue))
  ;; The nickname SB!XC now refers to the CL package.
  (rename-package "COMMON-LISP" "COMMON-LISP"
                  (cons "SB!XC" (package-nicknames "CL")))
  #+sb-package-locks (sb-ext:unlock-package "CL")

  ;; Any other name SB!FOO refers to the package now called SB-FOO.
  (dolist (package (list-all-packages))
    (let ((name (package-name package))
          (nicknames (package-nicknames package))
          (warm-name-prefix "SB-")
          (cold-name-prefix "SB!"))
      (when (and (> (length name) (length warm-name-prefix))
                 (string= name warm-name-prefix
                          :end1 (length warm-name-prefix)))
        (let* ((stem (subseq name (length cold-name-prefix)))
               (cold-name (concatenate 'simple-string cold-name-prefix stem)))
          (rename-package package name (cons cold-name nicknames)))
        #+sb-package-locks (sb-ext:unlock-package package)))))

;; Reinstate the pre-cold-init variable-defining macros.
(let ((*package* (find-package "SB-INT")))
  (flet ((def (real-name)
           (let ((alias (sb-int:symbolicate "!" real-name)))
             (export alias)
             (setf (macro-function alias) (macro-function real-name)))))
    (def 'sb-ext:defglobal)
    (def 'defparameter)
    (def 'defvar)))

(export '(sb-disassem::!begin-instruction-definitions)
        'sb-disassem)

(export '(sb-int::def!method sb-int::defmacro-mundanely
          sb-int::!cold-init-forms
          sb-int::!coerce-to-specialized
          sb-int::/show sb-int::/noshow sb-int::/show0 sb-int::/noshow0
          sb-int::!uncross-format-control)
        'sb-int)

(setf (macro-function 'sb-int:def!method) (macro-function 'defmethod))
(defmacro sb-int:defmacro-mundanely (name lambda-list &body body)
  `(let () (defmacro ,name ,lambda-list ,@body)))

(defmacro sb-int:!cold-init-forms (&rest forms) `(progn ,@forms))

;; This macro is never defined for the target Lisp,
;; only the cross-compilation host (see "src/code/specializable-array")
;; but it is needed to read x86-64/insts.lisp and other things.
(defmacro sb-int:!coerce-to-specialized (a type)
  (declare (ignore type))
  a)
(defmacro sb-int:!uncross-format-control (s) s)

;; If :sb-show is present, then these symbols are fboundp.
;; Otherwise define them as no-ops.
(unless (fboundp 'sb-int:/show)
  (defmacro sb-int:/show (&rest junk) (declare (ignore junk)))
  (setf (macro-function 'sb-int:/noshow) (macro-function 'sb-int:/show)
        (macro-function 'sb-int:/show0) (macro-function 'sb-int:/show)
        (macro-function 'sb-int:/noshow0) (macro-function 'sb-int:/show)))
