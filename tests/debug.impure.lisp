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

;;; FIXME: This should use some get-argslist like functionality that
;;; we actually export.
;;;
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
(defun verify-backtrace (test-function frame-specs &key (allow-stunted nil))
  (labels ((args-equal (want real)
             (cond ((endp want)
                    (endp real))
                   ((eq '&rest (car want))
                    t)
                   ((or (eq '? (car want)) (equal (car want) (car real)))
                    (args-equal (cdr want) (cdr real)))
                   (t
                    nil))))
    (let ((result nil))
      (block outer-handler
        (handler-bind
            ((error (lambda (condition)
                      ;; find the part of the backtrace we're interested in
                      (let ((backtrace (progn
                                         ;; (backtrace 13)
                                         (member (caar frame-specs)
                                                 (sb-debug:backtrace-as-list)
                                                 :key #'car
                                                 :test #'equal))))
                        
                        (setf result condition)
                        
                        (unless backtrace
                          (print :missing-backtrace)
                          (setf result nil))
                        
                        ;; check that we have all the frames we wanted
                        (mapcar 
                         (lambda (spec frame)
                           (unless (or (not spec)
                                       (and (equal (car spec) (car frame))
                                            (args-equal (cdr spec) 
                                                        (cdr frame))))
                             (print (list :mismatch spec frame))
                             (setf result nil)))
                         frame-specs
                         backtrace)
                        
                        ;; Make sure the backtrace isn't stunted in
                        ;; any way.  (Depends on running in the main
                        ;; thread.)
                        (let ((end (last backtrace 2)))
                          (unless (equal (caar end) 
                                         (if *show-entry-point-details*
                                             '(sb-c::tl-xep sb-impl::toplevel-init)
                                             'sb-impl::toplevel-init))
                            (print (list :backtrace-stunted (caar end)))
                            (setf result nil)))
                        (return-from outer-handler)))))
          (funcall test-function)))
      result)))

(defvar *undefined-function-frame*
  ;; bug 353
  '(#+(or x86 x86-64) "bogus stack frame"
    #-(or x86 x86-64) "undefined function"))

#-(or alpha) ; bug 346
;;; Test for "undefined function" (undefined_tramp) working properly.
;;; Try it with and without tail call elimination, since they can have
;;; different effects.  (Specifically, if undefined_tramp is incorrect
;;; a stunted stack can result from the tail call variant.)
(flet ((optimized ()
         (declare (optimize (speed 2) (debug 1))) ; tail call elimination
         (#:undefined-function 42))
       (not-optimized ()
         (declare (optimize (speed 1) (debug 2))) ; no tail call elimination
         (#:undefined-function 42))
       (test (fun)
         (declare (optimize (speed 1) (debug 2))) ; no tail call elimination
         (funcall fun)))

  (assert (verify-backtrace
           (lambda () (test #'optimized))
           (list *undefined-function-frame*
                 (list '(flet test) #'optimized))))
  
  ;; bug 353: This test fails at least most of the time for x86/linux
  ;; ca. 0.8.20.16. -- WHN
  #-(x86 linux)
  (assert (verify-backtrace 
           (lambda () (test #'not-optimized))
           (list *undefined-function-frame*
                 (list '(flet not-optimized))
                 (list '(flet test) #'not-optimized)))))

#-alpha ; bug 346
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
(flet ((optimized ()
         (declare (optimize (speed 2) (debug 1))) ; tail call elimination
         (/ 42 0))
       (not-optimized ()
         (declare (optimize (speed 1) (debug 2))) ; no tail call elimination
         (/ 42 0))
       (test (fun)
         (declare (optimize (speed 1) (debug 2))) ; no tail call elimination
         (funcall fun)))
  (assert (verify-backtrace (lambda () (test #'optimized))
                            (list '(/ 42 &rest) 
                                  (list '(flet test) #'optimized))))
  (assert (verify-backtrace (lambda () (test #'not-optimized))
                            (list '(/ 42 &rest)
                                  '((flet not-optimized))
                                  (list '(flet test) #'not-optimized)))))

#-(or alpha (and x86 linux)) ; bug 61
(progn
  (defun throw-test ()
    (throw 'no-such-tag t))
  (assert (verify-backtrace #'throw-test '((throw-test)))))

;;; test entry point handling in backtraces

(defun oops ()
  (error "oops"))

(defun bt.1 (&key key)
  (list key))

(defun bt.2 (x)
  (list x))

(defun bt.3 (&key (key (oops)))
  (list key))

;;; ERROR instead of OOPS so that tail call elimination doesn't happen
(defun bt.4 (&optional opt)
  (list (error "error")))

(defun bt.5 (&optional (opt (oops)))
  (list opt))

#-(and x86 linux)
(macrolet ((with-details (bool &body body)
             `(let ((sb-debug:*show-entry-point-details* ,bool))
                ,@body)))

  ;; &MORE-PROCESSOR
  (with-details t
    (assert (verify-backtrace (lambda () (bt.1 :key))
                              '(((sb-c::&more-processor bt.1) &rest)))))
  (with-details nil
    (assert (verify-backtrace (lambda () (bt.1 :key))
                              '((bt.1 :key)))))

  ;; XEP
  (with-details t
    (assert (verify-backtrace #'bt.2
                              '(((sb-c::xep bt.2) 0 ?)))))
  (with-details nil
    (assert (verify-backtrace #'bt.2
                              '((bt.2)))))

  ;; TL-XEP
  (with-details t
    (assert (verify-backtrace #'namestring
                              '(((sb-c::tl-xep namestring) 0 ?)))))
  (with-details nil
    (assert (verify-backtrace #'namestring
                              '((namestring)))))

  ;; VARARGS-ENTRY
  (with-details t
    (assert (verify-backtrace #'bt.3
                             '(((sb-c::varargs-entry bt.3) :key nil)))))
  (with-details nil
    (assert (verify-backtrace #'bt.3
                              '((bt.3 :key nil)))))

  ;; HAIRY-ARG-PROCESSOR
  (with-details t
    (assert (verify-backtrace #'bt.4
                              '(((sb-c::hairy-arg-processor bt.4) ?)))))
  (with-details nil
    (assert (verify-backtrace #'bt.4
                              '((bt.4 ?)))))

  ;; &OPTIONAL-PROCESSOR
  (with-details t
    (assert (verify-backtrace #'bt.5
                              '(((sb-c::&optional-processor bt.5))))))
  (with-details nil
    (assert (verify-backtrace #'bt.5
                              '((bt.5))))))

;;; success
(quit :unix-status 104)
