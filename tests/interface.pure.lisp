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

(in-package :cl-user)

;;; Check for fbound external symbols in public packages that have no
;;; argument list information. (This can happen if we get carried away
;;; with byte compilation, since at least in sbcl-0.6.12 the byte
;;; compiler can't record argument list information.)
(defvar *public-package-names*
  '("SB-ALIEN" "SB-C-CALL" "SB-DEBUG" "SB-EXT" "SB-EXT""SB-GRAY" "SB-MP"
    "SB-PROFILE" "SB-PCL" "COMMON-LISP"))
(defun has-arglist-info-p (function)
  (and (not (typep function 'sb-c::byte-function))
       (sb-kernel:%function-arglist function)))
(defun check-ext-symbols-arglist (package)
  (format t "~% looking at package: ~A" package)
  (do-external-symbols (ext-sym package)
    (when (fboundp ext-sym)
      (let ((fun (symbol-function ext-sym)))
        (unless (has-arglist-info-p fun)
          (error "~%Function ~A (~A) has no argument-list information available, ~%~
                 and is probably byte-compiled.~%" ext-sym fun))))))
(dolist (public-package *public-package-names*)
  (when (find-package public-package)
    (check-ext-symbols-arglist public-package)))
