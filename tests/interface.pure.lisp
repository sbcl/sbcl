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
;;; argument list information. (This used to be possible when we got
;;; carried away with byte compilation, since the byte compiler can't
;;; record argument list information. Now that there's no byte
;;; compiler, that can't happen, but it still shouldn't hurt to check
;;; in case the argument information goes astray some other way.)
(defvar *public-package-names*
  '("SB-ALIEN" "SB-C-CALL" "SB-DEBUG" "SB-EXT" "SB-GRAY" "SB-MP"
    "SB-PROFILE" "SB-PCL" "COMMON-LISP"))
(defun has-arglist-info-p (function)
  (declare (type function function))
  ;; The Lisp-level type FUNCTION can conceal a multitude of sins..
  (case (sb-kernel:get-type function)
    ((#.sb-vm:function-header-type #.sb-vm:closure-function-header-type)
      (sb-kernel:%function-arglist function))
    (#.sb-vm:closure-header-type (has-arglist-info-p
                                  (sb-kernel:%closure-function
                                   function)))
    ;; In code/describe.lisp, ll. 227 (%describe-function), we use a scheme
    ;; like above, and it seems to work. -- MNA 2001-06-12
    ;;
    ;; (There might be other cases with arglist info also.
    ;; FUNCTION-HEADER-TYPE and CLOSURE-HEADER-TYPE just
    ;; happen to be the two case that I had my nose rubbed in when
    ;; debugging a GC problem caused by applying %FUNCTION-ARGLIST to
    ;; a closure. -- WHN 2001-06-05)
    (t nil)))
(defun check-ext-symbols-arglist (package)
  (format t "~% looking at package: ~A" package)
  (do-external-symbols (ext-sym package)
    (when (fboundp ext-sym)
      (let ((fun (symbol-function ext-sym)))
	(cond ((macro-function ext-sym)
	       ;; FIXME: Macro functions should have their argument list
	       ;; information checked separately. Just feeding them into
	       ;; the ordinary-function logic below doesn't work right,
	       ;; though, and I haven't figured out what does work
	       ;; right. For now we just punt.
	       (values))
	      #+nil 
	      ((sb-int:info :function :accessor-for ext-sym)
	       (values))
	      ((typep fun 'generic-function)
                (sb-pcl::generic-function-pretty-arglist fun))
	      (t
	       (let ((fun (symbol-function ext-sym)))
		 (unless (has-arglist-info-p fun)
		   (error "Function ~A has no arg-list information available."
			  ext-sym)))))))))
(dolist (public-package *public-package-names*)
  (when (find-package public-package)
    (check-ext-symbols-arglist public-package)))
(terpri)

;;; FIXME: It would probably be good to require here that every
;;; external symbol either has a doc string or has some good excuse
;;; (like being an accessor for a structure which has a doc string).
