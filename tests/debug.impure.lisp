;;;; This file is for testing debugging functionality, using
;;;; test machinery which might have side effects (e.g. 
;;;; executing DEFUN).

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

(cl:in-package :cl-user)

;;;; Check that we get debug arglists right.

;;; Return the debug arglist of the function object FUN as a list, or
;;; punt with :UNKNOWN.
(defun get-arglist (fun)
  (declare (type function fun))
  ;; The Lisp-level type FUNCTION can conceal a multitude of sins..
  (case (sb-kernel:widetag-of fun)
    ((#.sb-vm:simple-fun-header-widetag #.sb-vm:closure-fun-header-widetag)
      (sb-kernel:%simple-fun-arglist fun))
    (#.sb-vm:closure-header-widetag (get-arglist
				     (sb-kernel:%closure-fun fun)))
    ;; In code/describe.lisp, ll. 227 (%describe-fun), we use a scheme
    ;; like above, and it seems to work. -- MNA 2001-06-12
    ;;
    ;; (There might be other cases with arglist info also.
    ;; SIMPLE-FUN-HEADER-WIDETAG and CLOSURE-HEADER-WIDETAG just
    ;; happen to be the two case that I had my nose rubbed in when
    ;; debugging a GC problem caused by applying %SIMPLE-FUN-ARGLIST to
    ;; a closure. -- WHN 2001-06-05)
    (t :unknown)))

(defun zoop (zeep &key beep)
  blurp)
(assert (equal (get-arglist #'zoop) '(zeep &key beep)))

;;; Check some predefined functions too.
;;;
;;; (We don't know exactly what the arguments are, e.g. the first
;;; argument of PRINT might be SB-IMPL::OBJECT or SB-KERNEL::OBJ or
;;; whatever. But we do know the general structure that a correct
;;; answer should have, so we can safely do a lot of checks.)
(destructuring-bind (object-sym &optional-sym stream-sym) (get-arglist #'print)
  (assert (symbolp object-sym))
  (assert (eql &optional-sym '&optional))
  (assert (symbolp stream-sym)))
(destructuring-bind (dest-sym control-sym &rest-sym format-args-sym)
    (get-arglist #'format)
  (assert (symbolp dest-sym))
  (assert (symbolp control-sym))
  (assert (eql &rest-sym '&rest))
  (assert (symbolp format-args-sym)))

;;; success
(quit :unix-status 104)
