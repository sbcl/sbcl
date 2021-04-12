;;;; side-effect-free tests of the condition system

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

;;; Until 0.7.7.21, (MAKE-CONDITION 'FILE-ERROR :PATHNAME "FOO")
;;; wasn't printable, because the REPORT function for FILE-ERROR
;;; referred to unbound slots. This was reported and fixed by Antonio
;;; Martinez (sbcl-devel 2002-09-10).
(format t
        "~&printable now: ~A~%"
        (make-condition 'file-error :pathname "foo"))

(assert (eq
         (block nil
           (macrolet ((opaque-error (arg) `(error ,arg)))
             (handler-bind
                 ((error (lambda (c)
                           (let ((restarts (remove 'res (compute-restarts c)
                                                   :key #'restart-name
                                                   :test-not #'eql)))
                             (assert (= (length restarts) 2))
                             (invoke-restart (second restarts))))))
               (let ((foo1 (make-condition 'error))
                     (foo2 (make-condition 'error)))
                 (restart-case
                     (with-condition-restarts foo1 (list (find-restart 'res))
                       (restart-case
                           (opaque-error foo2)
                         (res () 'int1)
                         (res () 'int2)))
                   (res () 'ext))))))
         'int2))

(assert (eq
         (block nil
           (macrolet ((opaque-error (arg) `(error ,arg)))
             (let ((foo1 (make-condition 'error))
                   (foo2 (make-condition 'error)))
               (handler-bind
                   ((error (lambda (c)
                             (declare (ignore c))
                             (let ((restarts (remove 'res (compute-restarts foo1)
                                                     :key #'restart-name
                                                     :test-not #'eql)))
                               (assert (= (length restarts) 1))
                               (invoke-restart (first restarts))))))
                 (restart-case
                     (with-condition-restarts foo1 (list (find-restart 'res))
                       (restart-case
                           (opaque-error foo2)
                         (res () 'int1)
                         (res () 'int2)))
                   (res () 'ext))))))
         'ext))

(assert (eq
         'ext
         (block nil
           (let ((visible nil)
                 (c1 (make-condition 'error))
                 (c2 (make-condition 'error)))
             (handler-bind
                 ((error
                   (lambda (c)
                     (declare (ignore c))
                     (flet ((check-restarts (length)
                              (assert (= length
                                         (length (remove 'foo (compute-restarts c1)
                                                         :key #'restart-name
                                                         :test-not #'eql))))))
                       (check-restarts 1)
                       (setq visible t)
                       (check-restarts 1)
                       (invoke-restart (find-restart 'foo c1))))))
               (restart-case
                   (restart-case
                       (error c2)
                     (foo () 'in1)
                     (foo () :test (lambda (c) (declare (ignore c)) visible)
                          'in2))
                 (foo () 'ext)))))))

;;; First argument of CERROR is a format control
(assert
 (eq (block nil
       (handler-bind
           ((type-error (lambda (c)
                          (declare (ignore c))
                          (return :failed)))
            (simple-error (lambda (c)
                            (declare (ignore c))
                            (return (if (find-restart 'continue)
                                        :passed
                                        :failed)))))
         (cerror (formatter "Continue from ~A") "bug ~A" :bug)))
     :passed))

(with-test (:name :disallow-bogus-coerce-to-condition)
  ;; COERCE-TO-CONDITION has an ftype which precludes passing junk
  ;; if caught at compile-time.
  ;; A non-constant non-condition-designator was able to sneak through.
  (multiple-value-bind (c err)
      (ignore-errors (sb-kernel::coerce-to-condition
                      (opaque-identity #p"foo")
                      'condition 'feep))
    (declare (ignore c))
    (assert (search "does not designate a condition"
                    (write-to-string err :escape nil)))))

(with-test (:name (handler-bind :smoke))
  (let ((called?))
    (flet ((handler (condition)
             (declare (ignore condition))
             (setf called? t)))
      (macrolet ((test (handler)
                   `(progn
                      (setf called? nil)
                      (handler-bind ((condition ,handler))
                        (signal 'condition))
                      (assert called?))))
        ;; Test optimized special cases.
        (test (lambda (condition) (handler condition)))
        (test #'(lambda (condition) (handler condition)))
        ;; Test default behavior.
        ;; (test 'handler) would require function definition => not pure
        (test #'handler)))))

(with-test (:name (handler-bind :malformed-bindings))
  (flet ((test (binding)
           (assert (eq :ok
                       (handler-case
                           (macroexpand `(handler-bind (,binding)))
                         (simple-error (e)
                           (assert (equal (list binding)
                                          (simple-condition-format-arguments e)))
                           :ok))))))

    (test 1)                     ; not even a list
    (test '())                   ; missing condition type and handler
    (test '(error))              ; missing handler
    (test '(error #'print :foo)) ; too many elements
    ))

;;; clauses in HANDLER-CASE are allowed to have declarations (and
;;; indeed, only declarations)
(with-test (:name (handler-case declare))
  (assert (null (handler-case (error "foo")
                  (error ()
                    (declare (optimize speed)))))))

(with-test (:name (signal warning muffle-warning control-error))
  (handler-case
      (handler-bind ((warning #'muffle-warning))
        (signal 'warning))
    ;; if it's a control error, it had better be printable
    (control-error (c) (format nil "~A" c))
    ;; there had better be an error
    (:no-error (&rest args) (error "No error: ~S" args))))

(with-test (:name (check-type type-error))
  (handler-case
      (funcall (lambda (x) (check-type x fixnum) x) t)
    (type-error (c)
      (assert (and (subtypep (type-error-expected-type c) 'fixnum)
                   (subtypep 'fixnum (type-error-expected-type c))))
      (assert (eq (type-error-datum c) t)))
    (:no-error (&rest rest) (error "no error: ~S" rest))))

;;; ANSI specifies TYPE-ERROR if datum and arguments of ERROR are not
;;; designators for a condition. Reported by Bruno Haible on cmucl-imp
;;; 2004-10-12.
(with-test (:name (error :invalid-arguments type-error))
  (flet ((test (&rest args)
           (multiple-value-bind (res err)
               (ignore-errors (apply #'error args))
             (assert (not res))
             (assert (typep err 'type-error))
             (assert-no-signal (type-error-datum err))
             (assert-no-signal (type-error-expected-type err)))))
    (test '#:no-such-condition)
    (test nil)
    (test t)
    (test 42)
    (test (make-instance 'standard-object))))

;;; If CERROR is given a condition, any remaining arguments are only
;;; used for the continue format control.
(with-test (:name (cerror :condition-object-and-format-arguments))
  (let ((x 0))
    (handler-bind
        ((simple-error (lambda (c) (incf x) (continue c))))
      (cerror "Continue from ~A at ~A"
              (make-condition 'simple-error :format-control "foo"
                                            :format-arguments nil)
              'cerror (get-universal-time))
      (assert (= x 1)))))

;; Test some of the variations permitted by the RESTART-CASE syntax.
(with-test (:name (restart-case :smoke))
  (macrolet
      ((test (clause &optional (expected ''(:ok)) (args '(:ok)))
         `(assert (equal ,expected
                         (multiple-value-list
                          (restart-case
                              (handler-bind
                                  ((error (lambda (c)
                                            (declare (ignore c))
                                            (invoke-restart ',(first clause) ,@args))))
                                (error "foo"))
                            ,clause))))))

    (test (foo (quux) quux))
    (test (foo (&optional quux) quux))
    ;; Multiple values should work.
    (test (foo (a b) (values a b)) '(1 2) (1 2))
    ;; Although somewhat unlikely, these should be legal and return
    ;; the respective keyword when the restart is invoked.
    (test (foo () :report) '(:report) ())
    (test (foo () :interactive) '(:interactive) ())
    (test (foo () :test) '(:test) ())
    ;; Declarations should work normally as part of the restart body.
    (test (foo () :declare ()) '(nil) ())
    (test (foo () :declare () :report "quux") '("quux") ())))

(with-test (:name (restart-case :malformed-clauses))
  (macrolet
      ((test (clause &optional (expected clause))
         `(assert (eq :ok
                      (handler-case
                          (macroexpand
                           `(restart-case (error "foo") ,',clause))
                        (simple-error (e)
                          (assert (equal '(restart-case ,expected)
                                         (simple-condition-format-arguments e)))
                          :ok))))))

    (test :report)                     ; not even a list
    (test ())                          ; empty
    (test (foo))                       ; no lambda-list
    (test (foo :report))               ; no lambda-list
    (test (foo :report "quux"))        ; no lambda-list
    (test (foo :report "quux" (quux))) ; confused report and lambda list
    ))

(with-test (:name :simple-condition-without-args)
  (let ((sc (make-condition 'simple-condition)))
    (assert (not (simple-condition-format-control sc)))
    (assert (not (simple-condition-format-arguments sc)))
    (assert (stringp (prin1-to-string sc)))
    (assert
     (eq :ok
         (handler-case
             (princ-to-string sc)
           (simple-error (c)
             (when (and (equal "No format-control for ~S"
                               (simple-condition-format-control c))
                        (eq sc (car
                                (simple-condition-format-arguments c))))
               :ok)))))))

(with-test (:name :malformed-simple-condition-printing-type-error)
  (assert (eq :type-error
              (handler-case
                  (princ-to-string
                   (make-condition 'simple-error :format-control "" :format-arguments 8))
                (type-error (e)
                  (when (and (eq 'list (type-error-expected-type e))
                             (eql 8 (type-error-datum e)))
                    :type-error))))))

(with-test (:name (:printing-unintitialized-condition :bug-1184586))
  (prin1-to-string (make-condition 'simple-type-error)))

(with-test (:name (:print-undefined-function-condition))
  (handler-case (funcall '#:foo)
    (undefined-function (c) (princ-to-string c))))

;; Printing a READER-ERROR while the underlying stream is still open
;; should print the stream position information.
(with-test (:name (reader-error :stream-error-position-info :open-stream
                                :bug-1264902))
  (locally
   ;; High debug avoids stack-allocating the stream.
   ;; It would be fine to stack-allocate it, because the handler-case does not
   ;; use the stream outside of its extent, however, because ALLOCATE-CONDITION
   ;; doesn't know when you will use the stream, it always replaces a DX stream
   ;; with a dummy. The dummy stream would not have position information.
   (declare (optimize debug))
   (assert
    (search
    "Line: 1, Column: 22, File-Position: 22"
    (with-input-from-string (stream "no-such-package::symbol")
      (handler-case
          (read stream)
        (reader-error (condition) (princ-to-string condition))))))))

;; Printing a READER-ERROR when the underlying stream has been closed
;; should still work, but the stream information will not be printed.
(with-test (:name (reader-error :stream-error-position-info :closed-stream
                                :bug-1264902))
  ;; This test operates on a closed stream that has dynamic extent (theoretically).
  ;; SAFETY 3 prevents a memory fault by not actually stack-allocating it.
  (declare (optimize (safety 3)))
  (assert
   (search
    "Package NO-SUCH-PACKAGE does not exist"
    (handler-case
        (with-input-from-string (stream "no-such-package::symbol")
          (read stream))
      (reader-error (condition) (princ-to-string condition))))))

(with-test (:name (make-condition :non-condition-class))
  (assert (search "does not designate a condition class"
                  (handler-case
                      (make-condition 'standard-class)
                    (type-error (condition)
                      (princ-to-string condition))))))

;; When called with a symbol not designating a condition class,
;; MAKE-CONDITION used to signal an error which printed as "NIL does
;; not designate a condition class.".
(with-test (:name (make-condition :correct-error-for-undefined-condition
                                  :bug-1199223))
  (assert (search (string 'no-such-condition)
                  (handler-case
                      (make-condition 'no-such-condition)
                    (type-error (condition)
                      (princ-to-string condition))))))

;; Using an undefined condition type in a HANDLER-BIND clause should
;; signal an ERROR at runtime. Bug 1378939 was about landing in LDB
;; because of infinite recursion in SIGNAL instead.
(with-test (:name (handler-bind :undefined-condition-type
                   :bug-1378939))
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile '(lambda ()
                         (handler-bind ((no-such-condition-class #'print))
                           (error "does not matter")))
                       :allow-style-warnings t)
    (declare (ignore failure-p warnings))
    (assert (= (length style-warnings) 1))
    (assert-error (funcall fun) simple-error)))

;; Using an undefined condition type in a HANDLER-BIND clause should
;; signal a [STYLE-]WARNING at compile time.
(with-test (:name (handler-bind :undefined-condition-type
                   :compile-time-warning))
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile '(lambda ()
                         (handler-bind ((no-such-condition-class #'print))))
                       :allow-style-warnings t)
    (declare (ignore fun failure-p warnings))
    (assert (= (length style-warnings) 1))))

;; Empty bindings in HANDLER-BIND pushed an empty cluster onto
;; *HANDLER-CLUSTERS* which was not expected by SIGNAL (and wasteful).
(with-test (:name (handler-bind :empty-bindings :bug-1388707))
  (assert-error (handler-bind () (error "Foo")) simple-error))

;; Parsing of #'FUNCTION in %HANDLER-BIND was too liberal.
;; This code should not compile.
(with-test (:name (handler-bind :no-sloppy-semantics))
  (multiple-value-bind (fun failure-p)
      (checked-compile '(lambda (x)
                         (sb-kernel::%handler-bind
                          ((condition (function (lambda (c) (print c)) garb)))
                          (print x)))
                       :allow-failure t)
    (declare (ignore fun))
    (assert failure-p))

  (multiple-value-bind (fun failure-p)
      (checked-compile '(lambda (x)
                         (handler-bind ((warning "woot")) (print x)))
                       :allow-failure t :allow-warnings t)
    (declare (ignore fun))
    (assert failure-p)))

(with-test (:name (handler-bind satisfies :predicate style-warning))
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile
       '(lambda ()
         ;; Just in case we ever change the meaning of #'F in high
         ;; safety so that it evals #'F, this test will break,
         ;; indicating that HANDLER-BIND will have to be changed.
         (declare (optimize (safety 3)))
         (declare (notinline +))
         (handler-bind (((satisfies snorky) #'abort)) (+ 2 2)))
       :allow-style-warnings t)
    (declare (ignore failure-p warnings))
    (assert (= (length style-warnings) 1))
    (assert (= (funcall fun) 4)))) ; there is no runtime error either

(with-test (:name :with-condition-restarts-evaluation-order)
  (let (result)
    (with-condition-restarts (progn
                               (push 1 result)
                               (make-condition 'error))
        (progn (push 2 result) nil)
      (push 3 result))
    (assert (equal result '(3 2 1)))))

(with-test (:name (type-error print *print-pretty*))
  (let ((error (make-condition 'type-error :datum 1 :expected-type 'string)))
    (assert (string= (let ((*print-pretty* nil))
                       (princ-to-string error))
                     "The value 1 is not of type STRING"))
    (assert (string= (let ((*print-pretty* t))
                       (princ-to-string error))
                     "The value
  1
is not of type
  STRING"))))

;;; Instances of LAYOUT for condition classoids created by genesis
;;; should resemble ones created normally. Due to a bug, they did not.
;;; (The LENGTH slot had the wrong value)
(with-test (:name :condition-layout-lengths)
  (loop for wrapper being each hash-value of (sb-kernel:classoid-subclasses
                                              (sb-kernel:find-classoid 'condition))
        for len = (sb-kernel:wrapper-length wrapper)
        minimize len into min
        maximize len into max
        finally (assert (= min max))))

(with-test (:name :allocate-condition-odd-length-keys)
  (multiple-value-bind (newcond error)
      (ignore-errors (make-condition 'warning :a 1 :b))
    (declare (ignore newcond))
    (assert (string= (write-to-string error :escape nil)
                     "odd-length initializer list: (:A 1 :B)."))))

(with-test (:name :type-error-on-dx-object
            :skipped-on :interpreter)
  (handler-case
    (sb-int:dx-let ((a (make-array 3)))
      (setf (aref a 0) a)
      (print (1+ (aref a 0))))
    (error (e)
      (assert (equal (sb-kernel:type-error-datum-stored-type e)
                     '(simple-vector 3))))))

(with-test (:name (:handler-bind-evaluation-count :lp1916302))
  (let (list)
    (handler-bind ((condition (let ((x 0))
                                (lambda (c)
                                  (declare (ignore c))
                                  (push (incf x) list)))))
      (signal 'condition)
      (signal 'condition))
    (assert (equalp '(2 1) list))))

(with-test (:name (:handler-bind-evaluation-count :separate-establishment))
  (let (list)
    (dotimes (i 2)
      (handler-bind ((condition (let ((x 0))
                                  (lambda (c)
                                    (declare (ignore c))
                                    (push (incf x) list)))))
        (signal 'condition)))
    (assert (equalp '(1 1) list))))
