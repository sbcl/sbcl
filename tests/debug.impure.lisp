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

;;; The debugger doesn't have any native knowledge of the interpreter
(when (eq sb-ext:*evaluator-mode* :interpret)
  (invoke-restart 'run-tests::skip-file))

#+(or x86 x86-64)
(with-test (:name :legal-bpt-lra-object)
  (sb-int:binding* ((code (sb-kernel:fun-code-header #'read))
                    (sap (sb-sys:sap+ (sb-kernel:code-instructions code) 13)) ; random
                    ((bpt-sap bpt-code-obj) (sb-di::make-bpt-lra sap)))
    (declare (ignore bpt-sap))
    ;; This was causing heap corruption
    (assert (zerop (sb-kernel:code-jump-table-words bpt-code-obj)))))


;;;; Check that we get debug arglists right.

(defun zoop (zeep &key beep)
  (declare (ignore zeep beep) (special blurp))
  blurp)
(assert (equal (sb-kernel:%fun-lambda-list #'zoop) '(zeep &key beep)))

;;; Check some predefined functions too.
;;;
;;; (We don't know exactly what the arguments are, e.g. the first
;;; argument of PRINT might be SB-IMPL::OBJECT or SB-KERNEL::OBJ or
;;; whatever. But we do know the general structure that a correct
;;; answer should have, so we can safely do a lot of checks.)
(with-test (:name :predefined-functions-1)
  (destructuring-bind (object-sym andoptional-sym stream-sym)
      (sb-kernel:%fun-lambda-list #'print)
    (assert (symbolp object-sym))
    (assert (eql andoptional-sym '&optional))
    (assert (symbolp stream-sym))))

(with-test (:name :predefined-functions-2)
  (destructuring-bind (dest-sym control-sym andrest-sym format-args-sym)
      (sb-kernel:%fun-lambda-list #'format)
    (assert (symbolp dest-sym))
    (assert (symbolp control-sym))
    (assert (eql andrest-sym '&rest))
    (assert (symbolp format-args-sym))))

;;;; test TRACE

(defmacro with-traced-function ((name &rest options) &body body)
  `(with-output-to-string (*trace-output*)
     (unwind-protect
          (progn
            (trace ,name ,@options)
            ,@body)
       (ignore-errors (untrace ,name)))))

(defun call-collecting-traces (fn trace-arguments)
  (let ((traces nil))
    (flet ((collect (depth what when frame values)
             (declare (ignore frame))
             (flet ((ensure-readable (x)
                      (typecase x
                        (function (sb-impl::%fun-name x))
                        (sb-debug::unprintable-object
                         (sb-debug::unprintable-object-string x))
                        (t x))))
               (push (list* depth
                            (ensure-readable what)
                            when
                            (mapcar #'ensure-readable values))
                     traces))))
      (unwind-protect
           (let ((sb-debug:*trace-report-default* #'collect))
             (eval `(trace ,@trace-arguments))
             (funcall fn))
        (ignore-errors (untrace))
        (assert (null (trace)))))
    (nreverse traces)))

(defmacro collecting-traces ((&rest trace-arguments) &body body)
  `(call-collecting-traces (lambda () ,@body) ',trace-arguments))

(defun trace-this (&optional arg)
  (declare (ignore arg))
  'ok)

(defun mv-trace-this (&optional arg)
  (case arg
    (2 (values 'ok "hi"))
    (3 (values 'ok "hi" :foo))
    (4 (values 'ok "hi" :foo :bar))))

(defun trace-fact (n)
  (if (zerop n)
      1
      (* n (trace-fact (1- n)))))

(with-test (:name (trace :simple))
  (let ((output (with-traced-function (trace-this)
                  (assert (eq 'ok (trace-this))))))
    (assert (search "TRACE-THIS" output))
    (assert (search "returned OK" output))))

(with-test (:name (trace :print-readably))
  (let ((output (with-traced-function (trace-this)
                  (let ((*print-readably* t))
                    (assert (eq 'ok (trace-this (sb-int:make-unprintable-object "foo"))))))))
    (assert (search "TRACE-THIS" output))
    (assert (search "foo" output))
    (assert (search "returned OK" output))))

;;; The following should not work:
;;;  (DEFUN G () (M))
;;;  (DEFMACRO M (&REST r) (format t "M invoked: ~S~%" r))
;;;  (ENCAPSULATE 'M 'foo (lambda (f &rest r) (format t "encapsulation: ~S ~S~%" f r)))
;;;  (G)
;;; If (ENCAPSULATE) were permitted, M's guard trampoline would be replaced by the
;;; encapsulation which is not meaningful. Most uses of the macro M will NOT invoke
;;; the encapsulation since we don't store macro functions in the symbol-function location.
;;; Only a consumer of M being accidentally compiled first and resolving M to an
;;; fdefinition would see the encapulation. That should just be an error.
(with-test (:name :no-macro-encapsulation)
  (assert-error (sb-int:encapsulate 'cond 'tryme
                                    (lambda (f &rest stuff)
                                      (declare (ignore f stuff))))))

(defparameter *breakpoint-tracing-expectations*
  '(:fails-on (or :arm :arm64)
    :broken-on (or :freebsd :ppc :ppc64)))

;;; bug 379
(with-test (:name (trace :encapsulate nil)
                  . #.*breakpoint-tracing-expectations*)
  (let ((output (with-traced-function (trace-this :encapsulate nil)
                  (assert (eq 'ok (trace-this))))))
    (assert (search "TRACE-THIS" output))
    (assert (search "returned OK" output))))

(with-test (:name :breakpoint-trace-multival . #.*breakpoint-tracing-expectations*)
  (let ((output (with-traced-function (mv-trace-this :encapsulate nil)
                  (assert (equal (multiple-value-list (mv-trace-this 2))
                                 '(ok "hi"))))))
    (assert (search "MV-TRACE-THIS" output))
    (assert (search "returned OK" output)))
  (let ((output (with-traced-function (mv-trace-this :encapsulate nil)
                  (assert (equal (multiple-value-list (mv-trace-this 3))
                                 '(ok "hi" :foo))))))
    (assert (search "MV-TRACE-THIS" output))
    (assert (search "returned OK" output)))
  (let ((output (with-traced-function (mv-trace-this :encapsulate nil)
                  (assert (equal (multiple-value-list (mv-trace-this 4))
                                 '(ok "hi" :foo :bar))))))
    (assert (search "MV-TRACE-THIS" output))
    (assert (search "returned OK" output))))

(with-test (:name (trace :encapsulate nil :recursive)
                  . #.*breakpoint-tracing-expectations*)
  (let ((output (with-traced-function (trace-fact :encapsulate nil)
                  (assert (= 120 (trace-fact 5))))))
    (assert (search "TRACE-FACT" output))
    (assert (search "returned 1" output))
    (assert (search "returned 120" output))))

(defun trace-and-fmakunbound-this (x)
  x)

(with-test (:name (trace fmakunbound :bug-667657))
  (trace trace-and-fmakunbound-this)
  (fmakunbound 'trace-and-fmakunbound-this)
  ;; FIXME: this generates a pointless warning that we can't untrace a formerly
  ;; traced function. Function redefinition knows to untrace/re-trace because of
  ;; the setf-fdefn hook. fmakunbound can do something similar - drop the trace.
  (untrace)
  (assert (not (trace))))

(with-test (:name (trace :report nil :smoke))
  (let ((output (with-traced-function (trace-this :report nil)
                  (assert (eq 'ok (trace-this))))))
    (assert (sequence:emptyp output))))

(with-test (:name (trace :report nil :print))
  (let ((output (with-traced-function
                    (trace-fact :report nil :print (sb-debug:arg 0))
                  (assert (eq '2 (trace-fact 2))))))
    (assert (string= output (format nil "2~@
                                         1~@
                                         0~%")))))

(defvar *collected-traces*)
(defun custom-trace-report (depth what when frame values)
  (push (list* depth what when (sb-di:frame-p frame) values)
        *collected-traces*))

(with-test (:name (trace :custom-report))
  (let ((*collected-traces* nil))
    (let ((output (with-traced-function (trace-fact :report custom-trace-report)
                    (trace-fact 2))))
      (assert (zerop (length output)))
      (assert (equalp (reverse *collected-traces*)
                      '((0 trace-fact :enter t 2)
                        (1 trace-fact :enter t 1)
                        (2 trace-fact :enter t 0)
                        (2 trace-fact :exit  t 1)
                        (1 trace-fact :exit  t 1)
                        (0 trace-fact :exit  t 2)))))))

(with-test (:name (trace :anonymous) . #.*breakpoint-tracing-expectations*)
  (assert (equalp (call-collecting-traces
                   (lambda ()
                     (trace-fact 1))
                   `(:function ,#'trace-fact :condition (plusp (sb-debug:arg 0))))
                  '((0 trace-fact :enter 1)
                    (0 trace-fact :exit 1)))))

(defgeneric trace-gf (x)
  (:method         ((x float))      (+ x (call-next-method)))
  (:method :before ((x float))      'bf)
  (:method :around ((x float))      (call-next-method))
  (:method :after  ((x float))      'af)
  (:method         ((x number))     (+ x (call-next-method)))
  (:method :before ((x number))     'bn)
  (:method :around ((x number))     (call-next-method))
  (:method :after  ((x number))     'an)
  (:method         ((x (eql 21.0))) (call-next-method))
  (:method         (x)              0))

(with-test (:name (trace :all-methods))
  (assert (equal (collecting-traces (trace-gf :methods t)
                   (trace-gf 21.0))
                 '((0 trace-gf :enter 21.0)
                   (1 (method trace-gf :around (float))  :enter 21.0)
                   (2 (method trace-gf :around (number)) :enter 21.0)
                   (3 (sb-pcl::combined-method trace-gf) :enter 21.0)
                   (4 (method trace-gf :before (float))  :enter 21.0)
                   (4 (method trace-gf :before (float))  :exit  bf)
                   (4 (method trace-gf :before (number)) :enter 21.0)
                   (4 (method trace-gf :before (number)) :exit  bn)
                   (4 (method trace-gf ((eql 21.0))) :enter 21.0)
                   (5 (method trace-gf (float))      :enter 21.0)
                   (6 (method trace-gf (number))     :enter 21.0)
                   (7 (method trace-gf (t))          :enter 21.0)
                   (7 (method trace-gf (t))          :exit 0)
                   (6 (method trace-gf (number))     :exit 21.0)
                   (5 (method trace-gf (float))      :exit 42.0)
                   (4 (method trace-gf ((eql 21.0))) :exit 42.0)
                   (4 (method trace-gf :after (number))  :enter 21.0)
                   (4 (method trace-gf :after (number))  :exit  an)
                   (4 (method trace-gf :after (float))   :enter 21.0)
                   (4 (method trace-gf :after (float))   :exit  af)
                   (3 (sb-pcl::combined-method trace-gf) :exit  42.0)
                   (2 (method trace-gf :around (number)) :exit  42.0)
                   (1 (method trace-gf :around (float))  :exit  42.0)
                   (0 trace-gf :exit 42.0)))))

(with-test (:name (trace :methods))
  (assert (equal (collecting-traces ((method trace-gf :after (float))
                                     (method trace-gf :around (number))
                                     (method trace-gf (t)))
                   (trace-gf 42.0))
                 '((0 (method trace-gf :around (number)) :enter 42.0)
                   (1 (method trace-gf (t)) :enter 42.0)
                   (1 (method trace-gf (t)) :exit 0)
                   (1 (method trace-gf :after (float)) :enter 42.0)
                   (1 (method trace-gf :after (float)) :exit af)
                   (0 (method trace-gf :around (number)) :exit 84.0)))))

(with-test (:name (trace :methods :encapsulate nil)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces (:encapsulate nil
                                     (method trace-gf :after (float))
                                     (method trace-gf :around (number))
                                     (method trace-gf (t)))
                   (trace-gf 42.0))
                 '((0 (sb-pcl::fast-method trace-gf :around (number)) :enter 42.0)
                   (1 (sb-pcl::fast-method trace-gf (t)) :enter "unused argument")
                   (1 (sb-pcl::fast-method trace-gf (t)) :exit 0)
                   (1 (sb-pcl::fast-method trace-gf :after (float)) :enter "unused argument")
                   (1 (sb-pcl::fast-method trace-gf :after (float)) :exit af)
                   (0 (sb-pcl::fast-method trace-gf :around (number)) :exit 84.0)))))

(defparameter *expected-trace-gf-number+t-trace*
  '((0 (method trace-gf :around (number)) :enter 42.0)
    (1 (method trace-gf (t)) :enter 42.0)
    (1 (method trace-gf (t)) :exit 0)
    (0 (method trace-gf :around (number)) :exit 84.0)))

(with-test (:name (trace :methods :untrace-some))
  (assert (equal (collecting-traces ((method trace-gf :after (float))
                                     (method trace-gf :around (number))
                                     (method trace-gf (t)))
                   (untrace (method trace-gf :after (float)))
                   (trace-gf 42.0))
                 *expected-trace-gf-number+t-trace*)))

(with-test (:name (trace :methods :untrace-many))
  (assert (equal (collecting-traces ((method trace-gf :around (number))
                                     (method trace-gf (t))
                                     trace-gf :methods t)
                   (untrace trace-gf)
                   (trace-gf 42.0))
                 *expected-trace-gf-number+t-trace*)))

(with-test (:name (trace :methods :trace-more))
  (assert (equal (collecting-traces ((method trace-gf (t)))
                   (eval '(trace (method trace-gf :around (number))))
                   (trace-gf 42.0))
                 *expected-trace-gf-number+t-trace*)))

(defgeneric (setf trace-gf) (value)
  (:method ((x float)) (call-next-method))
  (:method :before ((x number)) 'before)
  (:method ((x number)) x))

(with-test (:name (trace :setf-methods))
  (assert (equal (collecting-traces ((method (setf trace-gf) (float))
                                     (method (setf trace-gf) (number))
                                     (method (setf trace-gf) :before (number)))
                                    (setf (trace-gf) 42.0))
                 '((0 (method (setf trace-gf) :before (number)) :enter 42.0)
                   (0 (method (setf trace-gf) :before (number)) :exit before)
                   (0 (method (setf trace-gf) (float)) :enter 42.0)
                   (1 (method (setf trace-gf) (number)) :enter 42.0)
                   (1 (method (setf trace-gf) (number)) :exit 42.0)
                   (0 (method (setf trace-gf) (float)) :exit 42.0)))))

(defun global-fact (x)
  (declare (optimize (debug 3))) ; suppress inlining
  (labels ((fact (x)
             (flet ((multiply (x y)
                      (* x y)))
               (if (zerop x)
                   1
                   (multiply x (fact (1- x)))))))
    (fact x)))

(with-test (:name (trace :labels) . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((labels fact :in global-fact)
                                     (flet multiply :in global-fact))
                   (global-fact 1))
                 '((0 (labels fact :in global-fact) :enter 1)
                   (1 (labels fact :in global-fact) :enter 0)
                   (1 (labels fact :in global-fact) :exit 1)
                   (1 (flet multiply :in global-fact) :enter 1 1)
                   (1 (flet multiply :in global-fact) :exit 1)
                   (0 (labels fact :in global-fact) :exit 1)))))

(defgeneric gfact (x)
  (:method ((x number))
    (declare (optimize (debug 3))) ; suppress inlining
    (flet ((fact (x) (global-fact x)))
      (fact x))))

(with-test (:name (trace :labels :within-method)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((flet fact :in (method gfact (number))))
                   (gfact 3))
                 '((0 (flet fact :in (method gfact (number))) :enter 3)
                   (0 (flet fact :in (method gfact (number))) :exit 6)))))

(with-test (:name (trace :labels :within-untraced-method)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((method gfact (number))
                                     (flet fact :in (method gfact (number))))
                   (untrace (method gfact (number)))
                   (gfact 3))
                 '((0 (flet fact :in (method gfact (number))) :enter 3)
                   (0 (flet fact :in (method gfact (number))) :exit 6)))))

(defun trace-foo ()
  (declare (optimize (debug 3)))
  (flet ((body () 'original-foo))
    (body)))

(defun call-with-trace-foo-redefined (fn)
  (let ((original (fdefinition 'trace-foo)))
    ;; the local function will be named (FLET BODY) instead of (FLET
    ;; BODY :IN TRACE-FOO) unless we use EVAL here.
    (eval '(defun trace-foo ()
            (declare (optimize (debug 3)))
            (flet ((body () 'redefined-foo))
              (body))))
    (unwind-protect
         (funcall fn)
      (setf (fdefinition 'trace-foo) original))))

(with-test (:name (trace :labels :redefined)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((flet body :in trace-foo))
                   (trace-foo)
                   (call-with-trace-foo-redefined 'trace-foo))
                 '((0 (flet body :in trace-foo) :enter)
                   (0 (flet body :in trace-foo) :exit original-foo)
                   (0 (flet body :in trace-foo) :enter)
                   (0 (flet body :in trace-foo) :exit redefined-foo)))))

(defmethod trace-foo-gf ()
  (declare (optimize (debug 3)))
  (flet ((body () 'original-foo))
    (body)))

(defun call-with-trace-foo-gf-redefined (fn)
  ;; using ADD-METHOD and REMOVE-METHOD yields outdated DEBUG-FUN
  ;; info, so work around that using EVAL.
  (eval '(defmethod trace-foo-gf ()
          (declare (optimize (debug 3)))
          (flet ((body () 'redefined-foo))
            (body))))
  (unwind-protect
       (funcall fn)
    (eval '(defmethod trace-foo-gf ()
            (declare (optimize (debug 3)))
            (flet ((body () 'original-foo))
              (body))))))

(with-test (:name (trace :labels :redefined-method)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((flet body :in (method trace-foo-gf ())))
                   (trace-foo-gf)
                   (call-with-trace-foo-gf-redefined 'trace-foo-gf))
                 '((0 (flet body :in (method trace-foo-gf ())) :enter)
                   (0 (flet body :in (method trace-foo-gf ())) :exit original-foo)
                   (0 (flet body :in (method trace-foo-gf ())) :enter)
                   (0 (flet body :in (method trace-foo-gf ())) :exit redefined-foo)))))

(defun fn-with-cmac (x) x)

(define-compiler-macro fn-with-cmac (x)
  (declare (ignore x) (optimize (debug 3))) ; suppress flet inlining
  (flet ((body () 42))
    (body)))

(with-test (:name (trace :compiler-macro)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((compiler-macro fn-with-cmac))
                   (compile nil '(lambda () (fn-with-cmac 0))))
                 '((0 (compiler-macro fn-with-cmac) :enter (fn-with-cmac 0) "unused argument")
                   (0 (compiler-macro fn-with-cmac) :exit 42)))))

(with-test (:name (trace :flet :within-compiler-macro)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((flet body :in (compiler-macro fn-with-cmac)))
                   (compile nil '(lambda () (fn-with-cmac 0))))
                 '((0 (flet body :in (compiler-macro fn-with-cmac)) :enter)
                   (0 (flet body :in (compiler-macro fn-with-cmac)) :exit 42)))))

(defun call-with-compiler-macro-redefined (fn)
  (eval `(define-compiler-macro fn-with-cmac (x)
           (declare (ignore x) (optimize (debug 3)))
           (flet ((body () ''redefined))
             (body))))
  (unwind-protect
       (funcall fn)
    (eval `(define-compiler-macro fn-with-cmac (x)
             (declare (ignore x) (optimize (debug 3)))
             (flet ((body () 42))
               (body))))))

#-(or ppc ppc64)
(with-test (:name (trace :compiler-macro :redefined)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((compiler-macro fn-with-cmac)
                                     (flet body :in (compiler-macro fn-with-cmac)))
                   (compile nil '(lambda () (fn-with-cmac 0)))
                   (call-with-compiler-macro-redefined
                    (lambda () (compile nil '(lambda () (fn-with-cmac 0))))))
                 '((0 (compiler-macro fn-with-cmac) :enter (fn-with-cmac 0) "unused argument")
                   (1 (flet body :in (compiler-macro fn-with-cmac)) :enter)
                   (1 (flet body :in (compiler-macro fn-with-cmac)) :exit 42)
                   (0 (compiler-macro fn-with-cmac) :exit 42)
                   (0 (compiler-macro fn-with-cmac) :enter (fn-with-cmac 0) "unused argument")
                   (1 (flet body :in (compiler-macro fn-with-cmac)) :enter)
                   (1 (flet body :in (compiler-macro fn-with-cmac)) :exit 'redefined)
                   (0 (compiler-macro fn-with-cmac) :exit 'redefined)))))

(defun throw-foo ()
  (throw 'foo 42))

(defun catch-foo ()
  (catch 'foo (throw-foo)))

(with-test (:name (trace :non-local-exit))
  (assert (equal (collecting-traces (throw-foo)
                   (catch-foo))
                 '((0 throw-foo :enter)
                   (0 throw-foo :non-local-exit)))))

(with-test (:name (trace :non-local-exit :standard-report))
  (let ((output (with-traced-function (throw-foo)
                  (catch-foo))))
    (assert (search "exited non-locally" output))))

(defun trace-inner-function (x)
  x)

(defun trace-outer-function (x)
  (declare (optimize (debug 3))) ; avoid tail call optimization
  (trace-inner-function x))

(defun test-trace-inner-function (&key encapsulate)
  (assert (equal (let ((sb-debug:*trace-encapsulate-default* encapsulate))
                   (collecting-traces (trace-inner-function
                                       :wherein trace-outer-function)
                     (trace-outer-function 'outer-value)
                     (trace-inner-function 'inner-value)))
                 '((0 trace-inner-function :enter outer-value)
                   (0 trace-inner-function :exit outer-value)))))

(with-test (:name (trace :wherein :encapsulate t))
  (test-trace-inner-function :encapsulate t))

(with-test (:name (trace :wherein :encapsulate nil)
                  . #.*breakpoint-tracing-expectations*)
  (test-trace-inner-function :encapsulate nil))

(defun test-trace-fact-wherein (&key encapsulate)
  (assert (equal (let ((sb-debug:*trace-encapsulate-default* encapsulate))
                   (collecting-traces (trace-fact :wherein trace-fact)
                     (trace-fact 1)))
                 '((0 trace-fact :enter 0)
                   (0 trace-fact :exit 1)))))

(with-test (:name (trace :wherein :recursive :encapsulate t))
  (test-trace-fact-wherein :encapsulate t))

(with-test (:name (trace :wherein :recursive :encapsulate nil)
                  . #.*breakpoint-tracing-expectations*)
  (test-trace-fact-wherein :encapsulate nil))

(defmacro macro-fact (x)
  (labels ((fact (x) (if (zerop x) 1 (* x (fact (1- x))))))
    (fact x)))

(with-test (:name (trace :macro)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces (macro-fact)
                   (macroexpand-1 '(macro-fact 3)))
                 '((0 macro-fact :enter (macro-fact 3) "unused argument")
                   (0 macro-fact :exit 6)))))

(with-test (:name (trace :labels :within-macro)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((labels fact :in macro-fact))
                   (macroexpand-1 '(macro-fact 0)))
                 '((0 (labels fact :in macro-fact) :enter 0)
                   (0 (labels fact :in macro-fact) :exit 1)))))

(defun call-with-macro-fact-redefined (fn)
  (handler-bind ((sb-kernel:redefinition-with-defmacro #'muffle-warning))
    (eval `(defmacro macro-fact (x)
             (declare (ignore x) (optimize (debug 3)))
             (labels ((fact () 'redefined)) (fact))))
    (unwind-protect
         (funcall fn)
      (eval `(defmacro macro-fact (x)
               (labels ((fact (x) (if (zerop x) 1 (* x (fact (1- x))))))
                 (fact x)))))))

#-(or ppc ppc64)
(with-test (:name (trace :macro :redefined)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces (macro-fact
                                     (labels fact :in macro-fact))
                   (macroexpand-1 '(macro-fact 0))
                   (call-with-macro-fact-redefined
                    (lambda () (macroexpand-1 '(macro-fact 0)))))
                 '((0 macro-fact :enter (macro-fact 0) "unused argument")
                   (1 (labels fact :in macro-fact) :enter 0)
                   (1 (labels fact :in macro-fact) :exit 1) (0 macro-fact :exit 1)
                   (0 macro-fact :enter (macro-fact 0) "unused argument")
                   (1 (labels fact :in macro-fact) :enter)
                   (1 (labels fact :in macro-fact) :exit redefined)
                   (0 macro-fact :exit redefined)))))

(defun (cas trace-cas) (old new x)
  (declare (optimize (debug 3)))
  (flet (((cas body) (o n)
           (+ o n x)))
    (cas (body) old new)))

(with-test (:name (trace :cas)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((cas trace-cas)
                                     (flet (cas body) :in (cas trace-cas)))
                   (cas (trace-cas 1) 21 20))
                 '((0 (cas trace-cas) :enter 21 20 1)
                   (1 (flet (cas body) :in (cas trace-cas)) :enter 21 20)
                   (1 (flet (cas body) :in (cas trace-cas)) :exit 42)
                   (0 (cas trace-cas) :exit 42)))))

(defmethod (cas trace-cas-gf) (old new x)
  (declare (optimize (debug 3)))
  (flet (((cas body) (o n)
           (+ o n x)))
    (cas (body) old new)))

(with-test (:name (trace :cas :generic)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((method (cas trace-cas-gf) (t t t))
                                     (flet (cas body) :in (method (cas trace-cas-gf) (t t t))))
                   (cas (trace-cas-gf 1) 21 20))
                 '((0 (method (cas trace-cas-gf) (t t t)) :enter 21 20 1)
                   (1 (flet (cas body) :in (method (cas trace-cas-gf) (t t t))) :enter 21 20)
                   (1 (flet (cas body) :in (method (cas trace-cas-gf) (t t t))) :exit 42)
                   (0 (method (cas trace-cas-gf) (t t t)) :exit 42)))))

(defun (setf trace-setf) (value x)
  (declare (optimize (debug 3)))
  (flet (((setf body) (value)
           (+ value x)))
    (setf (body) value)))

(with-test (:name (trace :setf)
                  . #.*breakpoint-tracing-expectations*)
  (assert (equal (collecting-traces ((setf trace-setf)
                                     (flet (setf body) :in (setf trace-setf)))
                   (setf (trace-setf 11) 31))
                 '((0 (setf trace-setf) :enter 31 11)
                   (1 (flet (setf body) :in (setf trace-setf)) :enter 31)
                   (1 (flet (setf body) :in (setf trace-setf)) :exit 42)
                   (0 (setf trace-setf) :exit 42)))))

(with-test (:name :bug-414)
  (handler-bind ((warning #'error))
    (with-scratch-file (output "fasl")
      (load (compile-file "bug-414.lisp" :output-file output
                                         :verbose nil :print nil)))
    (with-output-to-string (s)
      (disassemble 'bug-414 :stream s))))

;; A known function can be stored as a code constant in lieu of the
;; usual mode of storing an #<fdefn> and looking up the function from it.
;; One such usage occurs with TAIL-CALL-VARIABLE (e.g. via APPLY).
;; Show that declaring the function locally notinline uses the #<fdefn>
;; by first compiling a call that would have elided the #<fdefn>
;; and then TRACE.
;; XXX: what purpose has the JUNK argument?
(defun test-compile-then-load (filename junk)
  (declare (notinline compile-file load))
  (with-scratch-file (output "fasl")
    (apply 'load (apply 'compile-file filename :output-file output junk)
           junk)))
(compile 'test-compile-then-load)
(with-test (:name :traceable-known-fun)
  (let ((s (make-string-output-stream)))
    (trace compile-file load)
    (let ((*trace-output* s))
      (test-compile-then-load "bug-414.lisp" nil))
    (untrace)
    (assert (>= (count #\Newline (get-output-stream-string s)) 4))))

(with-test (:name :bug-310175)
  ;; KLUDGE: Not all DX-enabled platforms DX CONS, and the compiler
  ;; transforms two-arg-LIST* (and one-arg-LIST) to CONS.  Therefore,
  ;; use two-arg-LIST, which should get through to VOP LIST, and thus
  ;; stack-allocate on a predictable set of machines.
  (let ((dx-arg (list t t)))
    (declare (dynamic-extent dx-arg))
    (flet ((dx-arg-backtrace (x)
             (declare (optimize (debug 2)))
             (prog1 (sb-debug:list-backtrace :count 10)
               (assert (sb-debug::stack-allocated-p x)))))
      (declare (notinline dx-arg-backtrace))
      (assert (member-if (lambda (frame)
                           (and (consp frame)
                                (consp (car frame))
                                (equal '(flet dx-arg-backtrace :in) (butlast (car frame)))
                                (notany #'sb-debug::stack-allocated-p (cdr frame))))
                         (dx-arg-backtrace dx-arg))))))

(with-test (:name :bug-795245)
  (assert
   (eq :ok
       (catch 'done
         (handler-bind
             ((error (lambda (e)
                       (declare (ignore e))
                       (handler-case
                           (sb-debug:print-backtrace :count 100
                                                     :stream (make-broadcast-stream))
                         (error ()
                           (throw 'done :error))
                         (:no-error ()
                           (throw 'done :ok))))))
           (apply '/= nil 1 2 nil))))))

;;;; test infinite error protection

(defmacro nest-errors (n-levels error-form)
  (if (< 0 n-levels)
      `(handler-bind ((error (lambda (condition)
                               (declare (ignore condition))
                               ,error-form)))
        (nest-errors ,(1- n-levels) ,error-form))
      error-form))

(defun erroring-debugger-hook (condition old-debugger-hook)
  (let ((*debugger-hook* old-debugger-hook))
    (format t "recursive condition: ~A~%" condition) (force-output)
    (error "recursive condition: ~A" condition)))

(defun test-infinite-error-protection ()
  ;; after 50 successful throws to SB-IMPL::TOPLEVEL-CATCHER sbcl used
  ;; to halt, it produces so much garbage that's hard to suppress that
  ;; it is tested only once
  (write-line "--HARMLESS BUT ALARMING BACKTRACE COMING UP--")
  (let ((*debugger-hook* #'erroring-debugger-hook))
    (loop repeat 1 do
          (let ((error-counter 0)
                (*terminal-io* (make-broadcast-stream)))
            (assert
             (not (eq
                   :normal-exit
                   (catch 'sb-impl::toplevel-catcher
                     (nest-errors 20 (error "infinite error ~s"
                                            (incf error-counter)))
                     :normal-exit)))))))
  (write-line "--END OF H-B-A-B--"))

;;; *debugger-hook* is now cleared after trying to enter the debugger
;;; *once in ERROR-ERROR, breaking these tests.
(with-test (:name :infinite-error-protection
            :skipped-on :sbcl)
  (enable-debugger)
  (test-infinite-error-protection))

(with-test (:name (:infinite-error-protection :thread)
            :skipped-on (or :sbcl (not :sb-thread)))
  (enable-debugger)
  (let ((thread (sb-thread:make-thread #'test-infinite-error-protection)))
    (loop while (sb-thread:thread-alive-p thread))))

;; unconditional, in case either previous left it enabled
(disable-debugger)

;;;; test some limitations of MAKE-LISP-OBJ

;;; Older GENCGC systems had a bug in the pointer validation used by
;;; MAKE-LISP-OBJ that made SIMPLE-FUN objects always fail to
;;; validate.
(with-test (:name (:make-lisp-obj :simple-funs))
  (sb-sys:without-gcing
    (assert (eq #'identity
                (sb-kernel:make-lisp-obj
                 (sb-kernel:get-lisp-obj-address
                  #'identity))))))

;;; Older CHENEYGC systems didn't perform any real pointer validity
;;; checks beyond "is this pointer to somewhere in heap space".
(with-test (:name (:make-lisp-obj :pointer-validation))
  ;; Fun and games: We need to test MAKE-LISP-OBJ with a known-bogus
  ;; address, but we also need the GC to not pitch a fit if it sees an
  ;; object with said bogus address.  Thus, construct our known-bogus
  ;; object within an area of unboxed storage (a vector) in static
  ;; space.  We'll make it a simple object, (CONS 0 0), which has an
  ;; in-memory representation of two consecutive zero words.  We
  ;; allocate a three-word vector so that we can guarantee a
  ;; double-word aligned double-word of zeros no matter what happens
  ;; with the vector-data-offset (currently double-word aligned).
  (let* ((memory (sb-int:make-static-vector 3 :element-type `(unsigned-byte ,sb-vm:n-word-bits)
                                            :initial-element 0))
         (vector-data-address (sb-sys:sap-int (sb-kernel::vector-sap memory)))
         (object-base-address (logandc2 (+ vector-data-address sb-vm:lowtag-mask) sb-vm:lowtag-mask))
         (object-tagged-address (+ object-base-address sb-vm:list-pointer-lowtag)))
    (multiple-value-bind (object valid-p)
        (sb-kernel:make-lisp-obj object-tagged-address nil)
      (declare (ignore object))
      (assert (not valid-p)))))

(defun test-debugger (control form &rest targets)
  (let ((out (make-string-output-stream))
        (oops t))
    (unwind-protect
         (progn
           (with-simple-restart (debugger-test-done! "Debugger Test Done!")
             (let* ((*debug-io* (make-two-way-stream
                                 (make-string-input-stream control)
                                 (make-broadcast-stream out #+nil *standard-output*)))
                    ;; Initial announcement goes to *ERROR-OUTPUT*
                    (*error-output* *debug-io*)
                    (*invoke-debugger-hook* nil))
               (handler-bind ((error #'invoke-debugger))
                 (eval form))))
           (setf oops nil))
      (when oops
        (error "Uncontrolled unwind from debugger test.")))
    ;; For sanity's sake this is outside the *debug-io* rebinding -- otherwise
    ;; it could swallow our asserts!
    (with-input-from-string (s (get-output-stream-string out))
      (loop for line = (read-line s nil)
            while line
            do (assert targets nil "Line = ~a" line)
               #+nil
               (format *error-output* "Got: ~A~%" line)
               (let ((match (pop targets)))
                 (if (eq '* match)
                     ;; Whatever, till the next line matches.
                     (let ((text (pop targets)))
                       #+nil
                       (format *error-output* "Looking for: ~A~%" text)
                       (unless (search text line)
                         (push text targets)
                         (push match targets)))
                     (unless (search match line)
                       (format *error-output* "~&Wanted: ~S~%   Got: ~S~%" match line)
                       (setf oops t))))))
    ;; Check that we saw everything we wanted
    (when targets
      (error "Missed: ~S" targets))
    (assert (not oops))))

(with-test (:name (:debugger :source 1))
  (test-debugger
   "d
    source 0
    debugger-test-done!"
   `(progn
      (defun this-will-break (x)
               (declare (optimize debug))
               (let* ((y (- x x))
                      (z (/ x y)))
                 (+ x z)))
      (this-will-break 1))
   '*
   "debugger invoked"
   '*
   "DIVISION-BY-ZERO"
   "Operation was (/ 1 0)"
   '*
   "INTEGER-/-INTEGER"
   "(THIS-WILL-BREAK 1)"
   "1]"
   "(/ X Y)"
   "1]"))

(with-test (:name (:debugger :source 2))
  (test-debugger
   "d
    source 0
    debugger-test-done!"
   `(locally (declare (optimize (speed 0) (safety 3) (debug 3)))
      (let ((f #'(lambda (x cont)
                   (print x (make-broadcast-stream))
                   (if (zerop x)
                       (error "~%foo")
                       (funcall cont (1- x) cont)))))
        (funcall f 10 f)))
   '*
   "debugger"
   '*
   "foo"
   '*
   "source: (ERROR \"~%foo\")"
   '*
   "(LAMBDA (X CONT)"
   '*
   "(FUNCALL CONT (1- X) CONT)"
   "1]"))

(with-test (:name (:debugger :bogus-debug-fun :source) :skipped-on :ppc)
  (test-debugger
   "d
    debugger-test-done!"
   `(let ()
      (#.(gensym)))
   '*
   "undefined function"
   '*
   "1]"))

(with-test (:name (disassemble :high-debug-eval))
  (eval `(defun this-will-be-disassembled (x)
           (declare (optimize debug))
           (+ x x)))
  (let* ((oopses (make-string-output-stream))
         (disassembly
           (let ((*error-output* oopses))
             (with-output-to-string (*standard-output*)
               (disassemble 'this-will-be-disassembled)))))
    (with-input-from-string (s disassembly)
      (assert (search "; disassembly for THIS-WILL-BE-DISASSEMBLED"
                      (read-line s))))
    (let ((problems (get-output-stream-string oopses)))
      (unless (zerop (length problems))
        (error problems)))))

(defun this-too-will-be-disasssembled (x)
  (declare (optimize debug))
  (+ x x))

(with-test (:name (disassemble :high-debug-load))
  (let* ((oopses (make-string-output-stream))
         (disassembly
           (let ((*error-output* oopses))
             (with-output-to-string (*standard-output*)
               (disassemble 'this-too-will-be-disasssembled)))))
    (with-input-from-string (s disassembly)
      (assert (equal "; disassembly for THIS-TOO-WILL-BE-DISASSSEMBLED"
                     (read-line s))))
    (let ((problems (get-output-stream-string oopses)))
      (unless (zerop (length problems))
        (error problems)))))

(with-test (:name (:xep-arglist-clean-up :bug-1192929))
  (assert
   (block nil
     (handler-bind ((error (lambda (e)
                             (declare (ignore e))
                             (return (< (length (car (sb-debug:list-backtrace :count 1)))
                                        10)))))
       (funcall (compile nil `(lambda (i) (declare ((mod 65536) i)) i)) nil)))))

;;; bug-1261646

(defun print-backtrace-to-string/debug-print-variable-alist (x)
  (values
   (with-output-to-string (stream)
     (let ((*debug-print-variable-alist* '((*print-length* . 5)
                                           (*print-level* . 3))))
       (sb-debug:print-backtrace :stream stream :count 5)))
   x)) ; Force use of X to prevent flushing

(with-test (:name (:print-frame-call :respect *debug-print-variable-alist*
                   *print-length* :bug-1261646))
  (let* ((printed (print-backtrace-to-string/debug-print-variable-alist
                   (make-array 200 :initial-element 0)))
         (call "(PRINT-BACKTRACE-TO-STRING/DEBUG-PRINT-VARIABLE-ALIST ")
         (position (+ (search call printed) (length call))))
    (assert (eql position (search "#(0 0 0 0 0 ...)" printed :start2 position)))))

(with-test (:name (:print-frame-call :respect *debug-print-variable-alist*
                   *print-level* :bug-1261646))
  (let* ((printed (print-backtrace-to-string/debug-print-variable-alist
                   '(((((1)))))))
         (call "(PRINT-BACKTRACE-TO-STRING/DEBUG-PRINT-VARIABLE-ALIST ")
         (position (+ (search call printed) (length call))))
    (assert (eql position (search "((#))" printed :start2 position)))))


(defvar *x* nil)
(defun foo (a) a)

(with-test (:name :trace-debug-arg)
  (trace foo :print-after (setf *x* (sb-debug:arg 0)))
  (foo 1)
  (assert (eql *x* 1))

  (trace foo :print (setf *x* (sb-debug:arg 0)))
  (foo 2)
  (assert (eql *x* 2))

  (trace foo :condition (eql (setf *x* (sb-debug:arg 0)) 0))
  (foo 3)
  (assert (eql *x* 3))

  (trace foo :condition-after (setf *x* (sb-debug:arg 0)))
  (foo 4)
  (assert (eql *x* 4))

  (trace foo :break (and (setf *x* (sb-debug:arg 0)) nil))
  (foo 5)
  (assert (eql *x* 5))

  (trace foo :break-all (and (setf *x* (sb-debug:arg 0)) nil))
  (foo 6)
  (assert (eql *x* 6))
  (trace foo :break-after (and (setf *x* (sb-debug:arg 0)) nil))
  (foo 7))

(defun frobbleize (arg) (declare (ignore arg)) (sb-debug:list-backtrace) 'win)
(defmethod low-debug-method ((self t))
  (declare (optimize (debug 0)))
  (frobbleize 'me) ; make this not a tail call, so it remains on stack
  5)
(with-test (:name :clean-fast-method-frame-lossage)
  (low-debug-method 42)) ; no need to assert. it either crashes or doesn't

(defun return-65535 ()
  65535)

(with-test (:name :indirect-closure-values)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug:map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame)
                                       (name (sb-debug::frame-call frame)))
                                   (when (or (eq name 'test)
                                             (and (consp name)
                                                  (or (eql (search '(labels f1) name) 0)
                                                      (eql (search '(labels f2) name) 0))))
                                     (incf count)
                                     (assert (eql (var 'a) 2))))))
                              (return))))
        (funcall
         (compile nil
                  `(sb-int:named-lambda test ()
                     (declare (optimize debug))
                     (let ((a 1))
                       (labels
                           ((f1 ()
                              (incf a)
                              (signal 'error))
                            (f2 ()
                              (f1)))
                         (f2))))))))
    (assert (= count 3))))

(with-test (:name :indirect-closure-values.2)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug:map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame)
                                       (name (sb-debug::frame-call frame)))
                                   (when (or (eq name 'test)
                                             (and (consp name)
                                                  (or (eql (search '(labels f1) name) 0)
                                                      (eql (search '(labels f2) name) 0))))
                                     (incf count)
                                     (assert (eql (var 'a) 65535))))))
                              (return))))
        (funcall
         (compile nil
                  `(sb-int:named-lambda test ()
                     (declare (optimize debug))
                     (let ((a (return-65535)))
                       (declare ((unsigned-byte 16) a))
                       (labels
                           ((f1 ()
                              (incf a)
                              (signal 'error))
                            (f2 ()
                              (f1)))
                         (f2))))))))
    (assert (= count 3))))

(with-test (:name :indirect-closure-values.crash)
  (block nil
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            (sb-debug:map-backtrace
                             (lambda (frame)
                               (let ((name (sb-debug::frame-call frame))
                                     (location (sb-di:frame-code-location frame))
                                     (d-fun (sb-di:frame-debug-fun frame)))
                                 (when (eq name 'test)
                                   (assert (sb-di:debug-var-info-available d-fun))
                                   (dolist (v (sb-di:ambiguous-debug-vars d-fun ""))
                                     (assert (not (sb-debug::var-valid-in-frame-p v location frame))))
                                   (return))))))))
      (funcall
       (compile nil
                `(sb-int:named-lambda test ()
                   (declare (optimize debug safety))
                   (signal 'error)
                   (let ((protos '()))
                     (mapcar (lambda (x)
                               (print x))
                             protos))))))))

(with-test (:name :non-tail-self-call-bad-variables)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug:map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame))
                                   (multiple-value-bind (name args)
                                       (sb-debug::frame-call frame)
                                     (when (eq name 'test)
                                       (assert (or (null args)
                                                   (equal args '(nil))))
                                       (incf count))))))
                              (return))))
        (funcall
         (compile nil `(sb-int:named-lambda test (&optional x)
                         (declare (optimize sb-c::recognize-self-calls))
                         (signal 'error :format-control "~a" :format-arguments (list x))
                         (test 1)
                         1)))))
    (assert (= count 1))))

(with-test (:name :local-tail-call-variables)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug:map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame))
                                   (multiple-value-bind (name args)
                                       (sb-debug::frame-call frame)
                                     (when (eq name 'test)
                                       (assert (equal args '(error)))
                                       (incf count))))))
                              (return))))
        (funcall
         (compile nil `(sb-int:named-lambda test (x)
                         (signal x)
                         ;; If :local-tail-call fails, this will fail
                         ;; too, because there's no jump between
                         ;; SIGNAL and the call to TAIL and it will
                         ;; show (flet tail) in the backtrace.
                         (flet ((tail ()))
                           (declare (notinline tail))
                           (tail))))
         'error)))
    (assert (= count 1))))

(with-test (:name :variables-surrounding-inlined-code)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug:map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame))
                                   (multiple-value-bind (name)
                                       (sb-debug::frame-call frame)
                                     (when (eq name 'test)
                                       (assert (equal (sb-debug:var 'l) '(1 2 3)))
                                       (incf count))))))
                              (return))))
        (funcall
         (compile nil `(sb-int:named-lambda test (a i)
                         (declare (optimize (debug 3)))
                         (let ((l (list 1 2 3)))
                           (aref a i)
                           l)))
         #(1) 2)))
    (assert (= count 1))))

(with-test (:name :variables-surrounding-inlined-code.2)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug:map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame))
                                   (multiple-value-bind (name)
                                       (sb-debug::frame-call frame)
                                     (when (eq name 'test)
                                       (assert (equal (sb-debug:var 'l) '(1 2 3)))
                                       (incf count))))))
                              (return))))
        (funcall
         (compile nil `(sb-int:named-lambda test (c)
                         (declare (optimize (debug 3)))
                         (let ((l (list 1 2 3)))
                           (map 'list #'signal c)
                           l)))
         '(error))))
    (assert (= count 1))))

(with-test (:name :properly-tagged-p-internal)
  ;; Pick a code component that has a ton of restarts.
  (let* ((code (sb-kernel:fun-code-header #'sb-impl::update-package-with-variance))
         (n (sb-kernel:code-n-entries code)))
    (sb-sys:with-pinned-objects (code)
      (let* ((base (logandc2 (sb-kernel:get-lisp-obj-address code)
                             sb-vm:lowtag-mask))
             (limit (+ base (sb-ext:primitive-object-size code))))
        (flet ((properly-tagged-p (ptr)
                 (eql (alien-funcall (extern-alien "properly_tagged_p_internal"
                                                   (function int unsigned unsigned))
                                     ptr base)
                      1)))
          ;; For architectures that don't use LRAs, there are exactly 'n-entries'
          ;; properly tagged interior pointers. For those which do use LRAs,
          ;; there are at least that many, because we allow pointing to LRAs,
          ;; but they aren't enumerable so we don't know the actual count.
          (assert (#+(or x86 x86-64 arm64) =
                   #-(or x86 x86-64 arm64) >
                     (loop for ptr from (+ base (* 2 sb-vm:n-word-bytes))
                           below limit count (properly-tagged-p ptr))
                     n))
          ;; Verify that the binary search algorithm for simple-fun-index works.
          (let ((count 0))
            (loop for ptr from base below limit
                  do
               (let ((index (alien-funcall
                             (extern-alien "simple_fun_index"
                                           (function int unsigned unsigned))
                             base ptr)))
                 (unless (eql index -1)
                   (let ((tagged-fun (logior ptr sb-vm:fun-pointer-lowtag)))
                     (assert (properly-tagged-p tagged-fun))
                     (incf count)
                     #+nil
                     (format t "~x -> ~d (~a)~%"
                             ptr index (sb-kernel:make-lisp-obj tagged-fun))))))
            (assert (= count n))))))))

(with-test (:name :repeatable-fasl)
  (with-scratch-file (output1 "fasl")
    (compile-file "bug-414.lisp" ; compile this file, why not
                  ::output-file output1 :verbose nil :print nil)
    (with-scratch-file (output2 "fasl")
      (compile-file "bug-414.lisp" ; compile this file, why not
                    ::output-file output2 :verbose nil :print nil)
      (with-open-file (fasl1 output1 :element-type '(unsigned-byte 8))
        (with-open-file (fasl2 output2 :element-type '(unsigned-byte 8))
          (assert (= (file-length fasl1) (file-length fasl2)))
          (loop repeat (file-length fasl1)
                do (assert (= (read-byte fasl1) (read-byte fasl2)))))))))

;; lp#1901781
(defun ll-unknown (x y) (declare (optimize (debug 0))) (+ x y))
(compile 'll-unknown)
(with-test (:name :unknown-lambda-list)
  (assert (eq (sb-kernel:%fun-lambda-list #'ll-unknown) :unknown)))

;;;; SB-DEBUG:*STACK-TOP-HINT* management

(defun buggy-handler (c)
  (declare (ignore c))
  ;; signal a nondescript condition to avoid triggering WITH-TEST's error
  ;; handling.
  (error 'simple-condition :format-control "buggy handler"))

(defun signal-and-handle-with-buggy-handler ()
  (handler-bind ((program-error #'buggy-handler))
    (signal 'program-error)))

(defun call-getting-stack-top-on-invoke-debugger (fn)
  (block nil
    (let ((*invoke-debugger-hook*
            (lambda (condition hook)
              (declare (ignore condition hook))
              (let ((top (sb-debug::resolve-stack-top-hint)))
                (return (caar (sb-debug:list-backtrace :from top)))))))
      (funcall fn))))

(defun ds-bind-when (x)
  (when x
    (sb-c::ds-bind-error '(foo) 2 3 '((:macro baz . deftype))))
  (print "something to prevent tco"))

(with-test (:name (:stack-top-hint :arg-count-error))
  (assert (eq 'ds-bind-when
              (block nil
                (handler-bind ((error
                                 (lambda (c)
                                   (declare (ignore c))
                                   (let ((top (sb-debug::resolve-stack-top-hint)))
                                     (return (caar (sb-debug:list-backtrace :from top)))))))
                  (ds-bind-when t))))))

;; If an error occurs within a signal handler, we want to see the handling
;; frames in the backtrace.
(with-test (:name (:stack-top-hint :signal))
  (assert (eq 'buggy-handler
              (call-getting-stack-top-on-invoke-debugger
               #'signal-and-handle-with-buggy-handler))))

;; When breaking on signals, we don't need to see the SIGNAL frame or other
;; frames above that.
(with-test (:name (:stack-top-hint :signal :break-on-signals))
  (assert (eq 'signal-and-handle-with-buggy-handler
              (let ((*break-on-signals* t))
                (call-getting-stack-top-on-invoke-debugger
                 #'signal-and-handle-with-buggy-handler)))))
