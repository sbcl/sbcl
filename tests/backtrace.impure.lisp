;;;; This file is for testing backtraces, using test machinery which
;;;; might have side effects (e.g. executing DEFUN).

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

;;; The following objects should be EQUALP to the respective markers
;;; produced by the backtrace machinery.

(defvar *unused-argument*
  (sb-debug::make-unprintable-object "unused argument"))

(defvar *unavailable-argument*
  (sb-debug::make-unprintable-object "unavailable argument"))

(defvar *unavailable-more*
  (sb-debug::make-unprintable-object "more unavailable arguments"))

(defvar *unavailable-lambda-list*
  (sb-debug::make-unprintable-object "unavailable lambda list"))

;;; TEST-FUNCTION is called and has to signal an error at which point
;;; the backtrace will be captured.
;;;
;;; If DETAILS is true, the returned backtrace description is of the
;;; form:
;;;
;;;   (((NAME1 . ARGS1) INFO1)
;;;    ((NAME2 . ARGS2) INFO2)
;;;    ...)
;;;
;;; Otherwise it is of the form
;;;
;;;   ((NAME1 . ARGS1)
;;;    (NAME2 . ARGS2)
;;;    ...)
;;;
(defun call-with-backtrace (cont test-function &key details)
  (flet ((capture-it (condition)
           (let (backtrace)
             (sb-debug:map-backtrace
              (lambda (frame)
                (multiple-value-bind (name args info)
                    (sb-debug::frame-call frame)
                  (push (if details
                            (list (cons name args) info)
                            (cons name args))
                        backtrace))))
             (funcall cont (nreverse backtrace) condition))))
    (handler-bind ((error #'capture-it))
      (funcall test-function))))

;;; Check the backtrace FRAMES against the list of frame
;;; specifications EXPECTED signaling an error if they do not match.
;;;
;;; If DETAILS is true, EXPECTED is a list with elements of the form
;;;
;;;   ((FUNCTION ARGS) INFO)
;;;
;;; Otherwise elements are of the form
;;;
;;;   (FUNCTION ARGS)
;;;
;;; ARGS is a list of expected argument values, but can also contain
;;; the following symbols
;;;
;;;   &REST The corresponding frame in FRAMES can contain an arbitrary
;;;         number of arguments starting at the corresponding
;;;         position.
;;;
;;;   ?     The corresponding frame in FRAMES can have an arbitrary
;;;         argument at the corresponding position.
(defun check-backtrace (frames expected &key details)
  (labels ((args-equal (want actual)
             (cond ((eq want *unavailable-lambda-list*)
                    (equalp want actual))
                   ((eq '&rest (car want))
                    t)
                   ((endp want)
                    (endp actual))
                   ((or (eq '? (car want)) (equal (car want) (car actual)))
                    (args-equal (cdr want) (cdr actual)))
                   ((typep (car want) 'sb-debug::unprintable-object)
                    (equalp (car want) (car actual)))
                   (t
                    nil)))
           (fail (datum &rest arguments)
             (return-from check-backtrace
               (values nil (apply #'sb-kernel:coerce-to-condition
                                  datum 'simple-error 'error arguments)))))
    (mapc (lambda (frame spec)
            (unless (cond
                      ((not spec)
                       t)
                      (details
                       (and (args-equal (car spec)
                                        (car frame))
                            (equal (cdr spec) (cdr frame))))
                      (t
                       (and (equal (car spec) (car frame))
                            (args-equal (cdr spec) (cdr frame)))))
              (fail "~@<Unexpected frame during ~
                    ~:[non-detailed~:;detailed~] check: wanted ~S, got ~
                    ~S~@:>"
                    details spec frame)))
          frames expected))
  t)

;;; Check for backtraces generally being correct.  Ensure that the
;;; actual backtrace finishes (doesn't signal any errors on its own),
;;; and that it contains the frames we expect, doesn't contain any
;;; "bogus stack frame"s, and contains the appropriate toplevel call
;;; and hasn't been cut off anywhere.
;;;
;;; See CHECK-BACKTRACE for an explanation of the structure
;;; EXPECTED-FRAMES.
(defun verify-backtrace (test-function expected-frames &key details
                                                            error)
  (labels ((find-frame (function-name frames)
             (member function-name frames
                     :key (if details #'caar #'car)
                     :test #'equal))
           (fail (datum &rest arguments)
             (return-from verify-backtrace
               (let ((c (apply #'sb-kernel:coerce-to-condition datum 'simple-error 'error arguments)))
                 (if error
                     (error c)
                     (values nil c))))))
    (call-with-backtrace
     (lambda (backtrace condition)
       (declare (ignore condition))
       (let* ((test-function-name (if details
                                      (caaar expected-frames)
                                      (caar expected-frames)))
              (frames (or (find-frame test-function-name backtrace)
                          (fail "~@<~S (expected name ~S) not found in ~
                                backtrace:~@:_~a~@:>"
                                test-function test-function-name
                                (princ-to-string backtrace)))))
         ;; Check that we have all the frames we wanted.
         (multiple-value-bind (successp condition)
             (check-backtrace frames expected-frames :details details)
           (unless successp (fail condition)))
         ;; Make sure the backtrace isn't stunted in any way.
         ;; (Depends on running in the main thread.) FIXME: On Windows
         ;; we get two extra foreign frames below regular frames.
         (unless (find-frame 'sb-impl::toplevel-init frames)
           (fail "~@<Backtrace stunted:~@:_~a~@:>" (princ-to-string backtrace))))
       (return-from verify-backtrace t))
     test-function :details details)))

(defun assert-backtrace (test-function expected-frames &key details)
  (verify-backtrace test-function expected-frames :details details
                                                  :error t))

(defvar *p* (namestring (if sb-c::*merge-pathnames* *load-truename* *load-pathname*)))

(defvar *undefined-function-frame*
  '("undefined function"))

(defun oops ()
  (error "oops"))

;;; Test for "undefined function" (undefined_tramp) working properly.
;;; Try it with and without tail call elimination, since they can have
;;; different effects.  (Specifically, if undefined_tramp is incorrect
;;; a stunted stack can result from the tail call variant.)
(flet ((optimized ()
         (declare (optimize (speed 2) (debug 1))) ; tail call elimination
         (declare (muffle-conditions style-warning))
         (#:undefined-function 42))
       (not-optimized ()
         (declare (optimize (speed 1) (debug 3))) ; no tail call elimination
         (declare (muffle-conditions style-warning))
         (#:undefined-function 42))
       (test (fun)
         (declare (optimize (speed 1) (debug 3))) ; no tail call elimination
         (funcall fun)))

  (with-test (:name (:backtrace :undefined-function :bug-346)
              :skipped-on :interpreter
              ;; Failures on SPARC are due to
              ;; not having a full and valid stack frame for the
              ;; undefined function frame.  See PPC
              ;; undefined_tramp for details.
              :fails-on :sparc)
    (assert-backtrace
     (lambda () (test #'optimized))
     (list (append *undefined-function-frame* '(42))
           (list `(flet test :in ,*p*) #'optimized))))

  ;; bug 353: This test fails at least most of the time for x86/linux
  ;; ca. 0.8.20.16. -- WHN
  (with-test (:name (:backtrace :undefined-function :bug-353)
              :skipped-on :interpreter)
    (assert-backtrace
     (lambda () (test #'not-optimized))
     (list (append *undefined-function-frame* '(42))
           (list `(flet not-optimized :in ,*p*))
           (list `(flet test :in ,*p*) #'not-optimized)))))

(with-test (:name (:backtrace :interrupted-condition-wait)
            :skipped-on (not :sb-thread)
            :broken-on :sb-safepoint) ;; unreliable
  (let ((m (sb-thread:make-mutex))
        (q (sb-thread:make-waitqueue)))
    (assert-backtrace
     (lambda ()
       (sb-thread:with-mutex (m)
         (handler-bind ((timeout (lambda (condition)
                                   (declare (ignore condition))
                                   (error "foo"))))
           (with-timeout 0.1
             (sb-thread:condition-wait q m)))))
     `((sb-thread::%condition-wait ,q ,m t nil nil nil nil nil nil)))))

;;; Division by zero was a common error on PPC. It depended on the
;;; return function either being before INTEGER-/-INTEGER in memory,
;;; or more than MOST-POSITIVE-FIXNUM bytes ahead. It also depends on
;;; INTEGER-/-INTEGER calling SIGNED-TRUNCATE. I believe Raymond Toy
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
         (declare (muffle-conditions style-warning))
         (/ 42 0))
       (not-optimized ()
         (declare (optimize (speed 1) (debug 3))) ; no tail call elimination
         (declare (muffle-conditions style-warning))
         (/ 42 0))
       (test (fun)
         (declare (optimize (speed 1) (debug 3))) ; no tail call elimination
         (funcall fun)))

  (with-test (:name (:backtrace :divide-by-zero :bug-346)
                    :skipped-on :interpreter)
    (assert-backtrace (lambda () (test #'optimized))
                      `((/ 42 &rest)
                        ((flet test :in ,*p*) ,#'optimized))))

  (with-test (:name (:backtrace :divide-by-zero :bug-356)
                    :skipped-on :interpreter)
    (assert-backtrace (lambda () (test #'not-optimized))
                      `((/ 42 &rest)
                        ((flet not-optimized :in ,*p*))
                        ((flet test :in ,*p*) ,#'not-optimized)))))

(defun throw-test ()
  (throw 'no-such-tag t))
(with-test (:name (:backtrace :throw :no-such-tag)
                  :fails-on (or :mips (and :sparc :linux)))
  (assert-backtrace #'throw-test '((throw-test))))

(funcall (checked-compile
          '(lambda ()
            (defun bug-308926 (x)
              (let ((v "foo"))
                (flet ((bar (z)
                         (oops v z)
                         (oops z v)))
                  (bar x)
                  (bar v)))))
          :allow-style-warnings t))
(with-test (:name (:backtrace :bug-308926) :skipped-on :interpreter)
  (assert-backtrace (lambda () (bug-308926 13))
                    '(((flet bar :in bug-308926) 13)
                      (bug-308926 &rest t))))

;;; Test backtrace through assembly routines
;;; :bug-800343
(macrolet ((test (predicate fun
                            &optional (two-arg
                                       (find-symbol (format nil "TWO-ARG-~A" fun)
                                                    "SB-KERNEL")))
             (let ((test-name (make-symbol (format nil "TEST-~A" fun))))
               `(flet ((,test-name (x y)
                         ;; make sure it's not in tail position
                         (list (,fun x y))))
                  (with-test (:name (:backtrace :bug-800343 ,fun)
                              :skipped-on :interpreter)
                    (assert-backtrace
                     (lambda ()
                       (eval `(funcall ,#',test-name 42 t)))
                     '((,two-arg 42 t)
                       #+(or x86 x86-64)
                       ,@(and predicate
                              `((,(find-symbol (format nil "GENERIC-~A" fun) "SB-VM"))))
                       ((flet ,(string test-name) :in ,*p*) 42 t)))))))
           (test-predicates (&rest functions)
             `(progn ,@(mapcar (lambda (function)
                                 `(test t ,@(sb-int:ensure-list function)))
                               functions)))
           (test-functions (&rest functions)
             `(progn ,@(mapcar (lambda (function)
                                 `(test nil ,@(sb-int:ensure-list function)))
                               functions))))
  (test-predicates = < >)
  (test-functions + - * /
                  gcd lcm
                  (logand sb-kernel:two-arg-and)
                  (logior sb-kernel:two-arg-ior)
                  (logxor sb-kernel:two-arg-xor)))

;;; test entry point handling in backtraces

(with-test (:name (:backtrace :xep-too-many-arguments)
            :skipped-on :interpreter)
  ;; CHECKED-COMPILE avoids STYLE-WARNING noise.
  (assert-backtrace (checked-compile '(lambda () (oops 1 2 3 4 5 6))
                                     :allow-style-warnings t)
                    '((oops ? ? ? ? ? ?))))

(defmacro defbt (n ll &body body)
  ;; WTF is this? This is a way to make these tests not depend so much on the
  ;; details of LOAD/EVAL. Around 1.0.57 we changed %SIMPLE-EVAL to be
  ;; slightly smarter, which meant that things which used to have xeps
  ;; suddenly had tl-xeps, etc. This takes care of that.
  `(funcall
    (checked-compile
     '(lambda ()
        (progn
          ;; normal debug info
          (defun ,(intern (format nil "BT.~A.1" n)) ,ll
            ,@body)
          ;; no arguments saved
          (defun ,(intern (format nil "BT.~A.2" n)) ,ll
            (declare (optimize (debug 1) (speed 3)))
            ,@body)
          ;; no lambda-list saved
          (defun ,(intern (format nil "BT.~A.3" n)) ,ll
            (declare (optimize (debug 0)))
            (let (*) ;; disable tail calls enabled by debug-0
              ,@body))))
     :allow-style-warnings t)))

(defbt 1 (&key key)
  (list key))

(defbt 2 (x)
  (list x))

(defbt 3 (&key (key (oops)))
  (list key))

;;; ERROR instead of OOPS so that tail call elimination doesn't happen
(defbt 4 (&optional opt)
  (list (error "error")))

(defbt 5 (&optional (opt (oops)))
  (list opt))

(defbt 6 (&optional (opt nil opt-p))
  (declare (ignore opt))
  (list (error "error ~A" opt-p))) ; use OPT-P

(defbt 7 (&key (key nil key-p))
  (declare (ignore key))
  (list (error "error ~A" key-p))) ; use KEY-P

(defun bug-354 (x)
  (error "XEPs in backtraces: ~S" x))

(with-test (:name (:backtrace :bug-354))
  (assert (not (verify-backtrace (lambda () (bug-354 354))
                                 '((bug-354 354)
                                   (((bug-354 &rest) (:tl :external)) 354)))))
  (assert-backtrace (lambda () (bug-354 354)) '((bug-354 354))))

;;; FIXME: This test really should be broken into smaller pieces
(with-test (:name (:backtrace :tl-xep))
  (assert-backtrace #'namestring '(((namestring) (:external))) :details t)
  (assert-backtrace #'namestring '((namestring))))

(with-test (:name (:backtrace :more-processor))
  ;; CHECKED-COMPILE avoids STYLE-WARNING noise.
  (assert-backtrace (checked-compile '(lambda () (bt.1.1 :key))
                                     :allow-style-warnings t)
                    '(((bt.1.1 :key) (:more)))
                    :details t)
  (assert-backtrace (checked-compile '(lambda () (bt.1.2 :key))
                                     :allow-style-warnings t)
                    '(((bt.1.2 ?) (:more)))
                    :details t)
  (assert-backtrace (lambda () (bt.1.3 :key))
                    `(((bt.1.3  ,*unavailable-more*) (:more)))
                    :details t)
  (assert-backtrace (checked-compile '(lambda () (bt.1.1 :key))
                                     :allow-style-warnings t)
                    '((bt.1.1 :key)))
  (assert-backtrace (checked-compile '(lambda () (bt.1.2 :key))
                                     :allow-style-warnings t)
                    '((bt.1.2 &rest)))
  (assert-backtrace (lambda () (bt.1.3 :key))
                    '((bt.1.3 &rest))))

(with-test (:name (:backtrace :xep))
  (assert-backtrace #'bt.2.1 '(((bt.2.1) (:external))) :details t)
  (assert-backtrace #'bt.2.2 '(((bt.2.2) (:external))) :details t)
  (assert-backtrace #'bt.2.3 `(((bt.2.3) (:external))) :details t)
  (assert-backtrace #'bt.2.1 '((bt.2.1)))
  (assert-backtrace #'bt.2.2 '((bt.2.2)))
  (assert-backtrace #'bt.2.3 `((bt.2.3))))

;;; This test is somewhat deceptively named. Due to confusion in debug
;;; naming these functions used to have sb-c::varargs-entry debug
;;; names for their main lambda.
(with-test (:name (:backtrace :varargs-entry))
  (assert-backtrace #'bt.3.1 '((bt.3.1 :key nil)))
  (assert-backtrace #'bt.3.2 '((bt.3.2 :key ?)))
  (assert-backtrace #'bt.3.3 `((bt.3.3 :key ,*unavailable-argument*)))
  (assert-backtrace #'bt.3.1 '((bt.3.1 :key nil)))
  (assert-backtrace #'bt.3.2 '((bt.3.2 :key ?)))
  (assert-backtrace #'bt.3.3 `((bt.3.3 :key ,*unavailable-argument*))))

;;; This test is somewhat deceptively named. Due to confusion in debug naming
;;; these functions used to have sb-c::hairy-args-processor debug names for
;;; their main lambda.
(with-test (:name (:backtrace :hairy-args-processor))
  (assert-backtrace #'bt.4.1 '((bt.4.1 ?)))
  (assert-backtrace #'bt.4.2 '((bt.4.2 ?)))
  (assert-backtrace #'bt.4.3 `((bt.4.3 ,*unused-argument*)))
  (assert-backtrace #'bt.4.1 '((bt.4.1 ?)))
  (assert-backtrace #'bt.4.2 '((bt.4.2 ?)))
  (assert-backtrace #'bt.4.3 `((bt.4.3 ,*unused-argument*))))

(with-test (:name (:backtrace :optional-processor))
  (assert-backtrace #'bt.5.1 '(((bt.5.1) (:optional))) :details t)
  (assert-backtrace #'bt.5.2 '(((bt.5.2) (:optional))) :details t)
  (assert-backtrace #'bt.5.3 `(((bt.5.3) (:optional)))
                    :details t)
  (assert-backtrace #'bt.5.1 '((bt.5.1)))
  (assert-backtrace #'bt.5.2 '((bt.5.2)))
  (assert-backtrace #'bt.5.3 `((bt.5.3))))

(with-test (:name (:backtrace :unused-optional-with-supplied-p :bug-1498644))
  (assert-backtrace (lambda () (bt.6.1 :opt))
                    `(((bt.6.1 ,*unused-argument*) ()))
                    :details t)
  (assert-backtrace (lambda () (bt.6.2 :opt))
                    `(((bt.6.2 ,*unused-argument*) ()))
                    :details t)
  (assert-backtrace (lambda () (bt.6.3 :opt))
                    `(((bt.6.3 ,*unused-argument*) ()))
                    :details t)
  (assert-backtrace (lambda () (bt.6.1 :opt))
                    `((bt.6.1 ,*unused-argument*)))
  (assert-backtrace (lambda () (bt.6.2 :opt))
                    `((bt.6.2 ,*unused-argument*)))
  (assert-backtrace (lambda () (bt.6.3 :opt))
                    `((bt.6.3 ,*unused-argument*))))

(with-test (:name (:backtrace :unused-key-with-supplied-p))
  (assert-backtrace (lambda () (bt.7.1 :key :value))
                    `(((bt.7.1 :key ,*unused-argument*) ()))
                    :details t)
  (assert-backtrace (lambda () (bt.7.2 :key :value))
                    `(((bt.7.2 :key ,*unused-argument*) ()))
                    :details t)
  (assert-backtrace (lambda () (bt.7.3 :key :value))
                    `(((bt.7.3 :key ,*unused-argument*) ()))
                    :details t)
  (assert-backtrace (lambda () (bt.7.1 :key :value))
                    `((bt.7.1 :key ,*unused-argument*)))
  (assert-backtrace (lambda () (bt.7.2 :key :value))
                    `((bt.7.2 :key ,*unused-argument*)))
  (assert-backtrace (lambda () (bt.7.3 :key :value))
                    `((bt.7.3 :key ,*unused-argument*))))

(defvar *compile-nil-error*
  (checked-compile '(lambda (x)
                      (cons (when x (error "oops")) nil))))
(defvar *compile-nil-non-tc*
  (checked-compile '(lambda (y)
                      (cons (funcall *compile-nil-error* y) nil))))
(with-test (:name (:backtrace compile nil))
  (assert-backtrace (lambda () (funcall *compile-nil-non-tc* 13))
                    `(((lambda (x) :in ,*p*) 13)
                      ((lambda (y) :in ,*p*) 13))))

(with-test (:name (:backtrace :clos-slot-typecheckfun-named))
  (assert-backtrace
   (checked-compile
    `(lambda ()
       (locally (declare (optimize safety))
         (defclass clos-typecheck-test ()
           ((slot :type fixnum)))
         (setf (slot-value (make-instance 'clos-typecheck-test) 'slot) t))))
   '(((sb-pcl::slot-typecheck fixnum) t))))

(with-test (:name (:backtrace :clos-emf-named))
  (assert-backtrace
   (checked-compile
    `(lambda ()
       (progn
         (defgeneric clos-emf-named-test (x)
           (:method ((x symbol)) x)
           (:method :before (x) (assert x)))
         (clos-emf-named-test nil)))
    :allow-style-warnings t)
   '(((sb-pcl::emf clos-emf-named-test) ? ? nil))))

(with-test (:name (:backtrace :bug-310173))
  (flet ((make-fun (n)
           (let* ((names '(a b))
                  (req (loop repeat n collect (pop names))))
             (checked-compile
              `(lambda (,@req &rest rest)
                 (let ((* *)) ; no tail-call
                   (apply '/ ,@req rest)))))))
    (assert-backtrace (lambda ()
                        (funcall (make-fun 0) 10 11 0))
                      `((sb-kernel:two-arg-/ 10/11 0)
                        (/ 10 11 0)
                        ((lambda (&rest rest) :in ,*p*) 10 11 0)))
    (assert-backtrace (lambda ()
                        (funcall (make-fun 1) 10 11 0))
                      `((sb-kernel:two-arg-/ 10/11 0)
                        (/ 10 11 0)
                        ((lambda (a &rest rest) :in ,*p*) 10 11 0)))
    (assert-backtrace (lambda ()
                        (funcall (make-fun 2) 10 11 0))
                      `((sb-kernel:two-arg-/ 10/11 0)
                        (/ 10 11 0)
                        ((lambda (a b &rest rest) :in ,*p*) 10 11 0)))))

(defgeneric gf-dispatch-test/gf (x y)
  (:method ((x integer) (y integer))
    (+ x y)))
(defun gf-dispatch-test/f (z)
  (declare (muffle-conditions style-warning))
  (gf-dispatch-test/gf z))
(with-test (:name (:backtrace :gf-dispatch))
  ;; Fill the cache
  (gf-dispatch-test/gf 1 1)
  ;; Wrong argument count
  (assert-backtrace (lambda () (gf-dispatch-test/f 42))
                    '((gf-dispatch-test/gf 42))))

(defgeneric gf-default-only-test/gf (x y)
  (:method (x y) (+ x y)))
(defun gf-default-only-test/f (z)
  (declare (muffle-conditions style-warning))
  (gf-default-only-test/gf z))
(with-test (:name (:backtrace :default-only))
  (gf-default-only-test/gf 1 1)
  (assert-backtrace (lambda () (gf-default-only-test/f 42))
                    '(((sb-pcl::default-only gf-default-only-test/gf) 42))))

(with-test (:name (:backtrace :local-tail-call))
  (assert-backtrace
   (lambda () (funcall (compile nil `(sb-int:named-lambda test ()
                                       (signal 'error)
                                       (flet ((tail ()))
                                         (declare (notinline tail))
                                         (tail))))))
   '((test))))

(defun fact (n) (if (zerop n) (error "nope") (* n (fact (1- n)))))

#+sb-fasteval
(with-test (:name (:backtrace :interpreted-factorial)
            :skipped-on (not :interpreter))
  (assert-backtrace
   (lambda () (fact 5))
   '((fact 0)
     (sb-interpreter::2-arg-* &rest)
     (fact 1)
     (sb-interpreter::2-arg-* &rest)
     (fact 2)
     (sb-interpreter::2-arg-* &rest)
     (fact 3)
     (sb-interpreter::2-arg-* &rest)
     (fact 4)
     (sb-interpreter::2-arg-* &rest)
     (fact 5))))

(with-test (:name :deleted-args)
  (let ((fun (checked-compile `(lambda (&rest ignore)
                                 (declare (ignore ignore))
                                 (error "x")))))
    (assert (typep (block nil
                     (handler-bind ((error
                                      (lambda (c)
                                        (return (values c
                                                        (sb-debug:list-backtrace))))))
                       (funcall fun)))
                   'simple-error))))

(defun mega-string-replace-fail (x)
  (let ((string (make-string 10000 :initial-element #\z))
        (stream (make-string-output-stream)))
    (block nil
      (handler-bind
          ((condition (lambda (c)
                        (declare (ignore c))
                        (sb-debug:print-backtrace :stream stream)
                        (return-from nil))))
        (replace string x)))
    (get-output-stream-string stream)))

(with-test (:name :long-string-abbreviation)
  (let ((backtrace (mega-string-replace-fail '(#\- 1))))
    (assert (search (concatenate 'string
                                 "-"
                                 (make-string 199 :initial-element #\z)
                                 "...")
                    backtrace))))

(defclass cannot-print-this () ())
(defmethod print-object ((object cannot-print-this) stream)
  (error "No go!"))

(with-test (:name (sb-debug:print-backtrace :no-error print-object))
  ;; Errors during printing objects used to be suppressed in a way
  ;; that required outer condition handlers to behave in a specific
  ;; way.
  (handler-bind ((error (lambda (condition)
                          (error "~@<~S signaled ~A.~@:>"
                                 'sb-debug:print-backtrace condition))))
    (with-output-to-string (stream)
      (labels ((foo (n x)
                 (when (plusp n)
                   (foo (1- n) x))
                 (when (zerop n)
                   (sb-debug:print-backtrace :count 100 :stream stream
                                             :emergency-best-effort t))))
        (foo 100 (make-instance 'cannot-print-this))))))

(with-test (:name (sb-debug:print-backtrace :no-error :circles))
  ;; Errors during printing objects used to be suppressed in a way
  ;; that required outer condition handlers to behave in a specific
  ;; way.
  (handler-bind ((error (lambda (condition)
                          (error "~@<~S signaled ~A.~@:>"
                                 'sb-debug:print-backtrace condition))))
    (with-output-to-string (stream)
      (labels ((foo (n x)
                 (when (plusp n)
                   (foo (1- n) x))
                 (when (zerop n)
                   (sb-debug:print-backtrace :count 100 :stream stream))))
        (foo 100 (let ((list (list t)))
                   (nconc list list)))))))

(with-test (:name :uninitialized-optionals)
  (let ((fun (checked-compile
              `(lambda (l &optional m n)
                 (declare (fixnum l))
                 (values l m n)))))
    (checked-compile-and-assert
        ()
        `(lambda (fun &rest args)
           (block nil
             (handler-bind ((error
                              (lambda (c)
                                c
                                (return (cdar (sb-debug:list-backtrace :count 1))))))
               (apply fun args))))
      ((fun t) (list t) :test #'equalp)
      ((fun t 1) (list t 1) :test #'equalp)
      ((fun t 1 2) (list t 1 2) :test #'equalp)
      ((fun 1 2 3) (values 1 2 3)))))
