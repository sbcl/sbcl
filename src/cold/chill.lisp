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

(handler-bind ((sb-ext:package-locked-error #'continue))
  ;; The nickname SB!XC now refers to the CL package.
  (rename-package "COMMON-LISP" "COMMON-LISP"
                  (cons "SB!XC" (package-nicknames "CL")))
  (sb-ext:unlock-package "CL")

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
        (sb-ext:unlock-package package)))))
