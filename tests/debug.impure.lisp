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
    (#.sb-vm:simple-fun-header-widetag
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

;;; Check for backtraces generally being correct.  Ensure that the
;;; actual backtrace finishes (doesn't signal any errors on its own),
;;; and that it contains the frames we expect, doesn't contain any
;;; "bogus stack frame"s, and contains the appropriate toplevel call
;;; and hasn't been cut off anywhere.
(defun verify-backtrace (test-function frame-name
                         &key (key #'first) (test #'eql)
                         (allow-bogus-frames nil))
  (let ((result nil)
        (return-value nil))
    (block outer-handler
      (handler-bind
          ((error #'(lambda (condition)
                      (let ((backtrace (ignore-errors
                                         (sb-debug:backtrace-as-list))))
                        ;; Make sure we find what we're looking for.
                        (if (member frame-name backtrace :key key :test test)
                            (setf result (list :error condition))
                            (print (list :failed :frame frame-name :backtrace backtrace)))
                        ;; Make sure there's no bogus stack frames
                        ;; unless they're explicitly allowed.
                        (when (and (not allow-bogus-frames)
                                   (member "bogus stack frame" backtrace
                                           :key #'first :test #'equal))
                          (print 'verify-backtrace-bogus)
                          (setf result nil))
                        ;; Make sure the backtrace isn't stunted in
                        ;; any way.  (Depends on running in the main
                        ;; thread.)
                        (unless (member 'sb-impl::toplevel-init backtrace
                                        :key #'first :test #'equal)
                          (print 'verify-backtrace-stunted)
                          (setf result nil)))
                      (return-from outer-handler))))
        (funcall test-function)))
    (values result return-value)))

;;; Test for "undefined function" (undefined_tramp) working properly.
;;; Try it with and without tail call elimination, since they can have
;;; different effects.  (Specifically, if undefined_tramp is incorrect
;;; a stunted stack can result from the tail call variant.)
#-(or alpha) ; bug 346
(flet ((optimized ()
         (declare (optimize (speed 2) (debug 1))) ; tail call elimination
         (#:undefined-function 42))
       (not-optimized ()
         (declare (optimize (speed 1) (debug 2))) ; no tail call elimination
         (#:undefined-function 42))
       (test (fun)
         (declare (optimize (speed 1) (debug 2))) ; no tail call elimination
         (funcall fun)))
  #-x86 ; <- known bug (?): fails for me on 0.8.17.31/Linux/x86 -- WHN 2004-12-27
  (dolist (frame '(#-x86 "undefined function" ; bug 353
                   "FLET COMMON-LISP-USER::TEST"))
    (assert (verify-backtrace (lambda () (test #'optimized)) frame
                              :test #'equal
                              :allow-bogus-frames (or #+x86 t))))
  (dolist (frame '(#-x86 "undefined function" ; bug 353
                   "FLET COMMON-LISP-USER::NOT-OPTIMIZED"
                   "FLET COMMON-LISP-USER::TEST"))
    (assert (verify-backtrace (lambda () (test #'not-optimized)) frame
                              :test #'equal
                              :allow-bogus-frames (or #+x86 t)))))

;;; Division by zero was a common error on PPC.  It depended on the
;;; return function either being before INTEGER-/-INTEGER in memory,
;;; or more than MOST-POSITIVE-FIXNUM bytes ahead.  It also depends on
;;; INTEGER-/-INTEGER calling SIGNED-TRUNCATE.  I believe Raymond Toy
;;; says that the Sparc backend (at least for CMUCL) inlines this, so
;;; if SBCL does the same this test is probably not good for the
;;; Sparc.
;;;
;;; Disabling tail call elimination on this will probably ensure that
;;; the return value (to the flet or the enclosing top level form) is
;;; more than MOST-POSITIVE-FIXNUM with the current spaces on OS X.
;;; Enabling it might catch other problems, so do it anyway.
#-alpha ; bug 346
(progn
  (flet ((test-function ()
	   (declare (optimize (speed 2) (debug 1))) ; tail call elimination
	   (/ 42 0)))
    (assert (verify-backtrace #'test-function '/)))

  (flet ((test-function ()
	   (declare (optimize (speed 1) (debug 2))) ; no tail call elimination
	   (/ 42 0)))
    (assert (verify-backtrace #'test-function '/))))

#-(or alpha) ; bug 61
(progn
  (defun throw-test ()
    (throw 'no-such-tag t))
  (assert (verify-backtrace #'throw-test 
                            #-(or x86 sparc) 'throw-test
                            #+(or x86 sparc) "XEP for COMMON-LISP-USER::THROW-TEST" ; bug 354
                            :test #'equal)))

;;; success
(quit :unix-status 104)
