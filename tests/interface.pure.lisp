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
  '("SB-ALIEN" "SB-C-CALL" "SB-DEBUG" "SB-EXT" "SB-GRAY" "SB-MP"
    "SB-PROFILE" "SB-PCL" "COMMON-LISP"))
(defun has-arglist-info-p (function)
  (declare (type function function))
  ;; The Lisp-level type FUNCTION can conceal a multitude of sins..
  (case (sb-kernel:get-type function)
    (#.sb-vm:function-header-type (sb-kernel:%function-arglist function))
    (#.sb-vm:closure-function-header-type (has-arglist-info-p
					   (sb-kernel:%closure-function
					    function)))
    ;; (There might be other cases with arglist info also.
    ;; FUNCTION-HEADER-TYPE and CLOSURE-FUNCTION-HEADER-TYPE just
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
	       ;; FIXME: Check the argument lists of generic functions,
	       ;; instead of just punting like this. (DESCRIBE seems
	       ;; to know how to do it, at least for #'DOCUMENTATION.)
	       (values))
	      (;; FIXME: CONDITION slot accessors (e.g.
	       ;; PRINT-NOT-READABLE-OBJECT) fall into this category.
	       ;; They seem to have argument lists -- since at least
	       ;; DESCRIBE knows how to find them -- but I have
	       ;; neither reverse engineered how it's finding them,
	       ;; nor factored that into a function which can be
	       ;; shared with the logic here..
	       (= (sb-kernel:get-type fun) sb-vm:closure-header-type)
	       (values)) ; ..so for now we just punt.
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

(print "done with interface.pure.lisp")

