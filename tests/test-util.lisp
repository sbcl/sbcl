(defpackage :test-util
  (:use :cl :sb-ext)
  (:export #:with-test #:report-test-status #:*failures*
           #:really-invoke-debugger
           #:*break-on-failure* #:*break-on-expected-failure*
           #:make-kill-thread #:make-join-thread
           #:checked-compile
           #:runtime))

(in-package :test-util)

(defvar *test-count* 0)
(defvar *test-file* nil)
(defvar *failures* nil)
(defvar *break-on-failure* nil)
(defvar *break-on-expected-failure* nil)

(defvar *threads-to-kill*)
(defvar *threads-to-join*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(sb-posix:putenv (format nil "SBCL_MACHINE_TYPE=~A" (machine-type)))
(sb-posix:putenv (format nil "SBCL_SOFTWARE_TYPE=~A" (software-type)))

#+sb-thread
(defun make-kill-thread (&rest args)
  (let ((thread (apply #'sb-thread:make-thread args)))
    (when (boundp '*threads-to-kill*)
      (push thread *threads-to-kill*))
    thread))

#+sb-thread
(defun make-join-thread (&rest args)
  (let ((thread (apply #'sb-thread:make-thread args)))
    (when (boundp '*threads-to-join*)
      (push thread *threads-to-join*))
    thread))

(defun log-msg (&rest args)
  (apply #'format *trace-output* "~&::: ~@?~%" args)
  (force-output *trace-output*))

(defun log-msg/non-pretty (&rest args)
  (let ((*print-pretty* nil))
    (apply #'log-msg args)))

(defmacro with-test ((&key fails-on broken-on skipped-on name)
                     &body body)
  (let ((block-name (gensym))
        #+sb-thread (threads (gensym "THREADS")))
    (flet ((name-ok (x y)
             (declare (ignore y))
             (typecase x
               (symbol (let ((package (symbol-package x)))
                         (or (null package)
                             (eql package (find-package "CL"))
                             (eql package (find-package "KEYWORD"))
                             (eql (mismatch "SB-" (package-name package)) 3))))
               (integer t))))
      (unless (tree-equal name name :test #'name-ok)
        (error "test name must be all-keywords: ~S" name)))
    `(progn
       (start-test)
       (cond
         ((broken-p ,broken-on)
          (fail-test :skipped-broken ',name "Test broken on this platform"))
         ((skipped-p ,skipped-on)
          (fail-test :skipped-disabled ',name "Test disabled for this combination of platform and features"))
         (t
          (let (#+sb-thread (,threads (sb-thread:list-all-threads))
                (*threads-to-join* nil)
                (*threads-to-kill* nil))
            (block ,block-name
              (handler-bind ((error (lambda (error)
                                      (if (expected-failure-p ,fails-on)
                                          (fail-test :expected-failure ',name error)
                                          (fail-test :unexpected-failure ',name error))
                                      (return-from ,block-name))))
                (progn
                  ;; Non-pretty is for cases like (with-test (:name (let ...)) ...
                  (log-msg/non-pretty "Running ~S" ',name)
                  ,@body
                  #+sb-thread
                  (let ((any-leftover nil))
                    (dolist (thread *threads-to-join*)
                      (ignore-errors (sb-thread:join-thread thread)))
                    (dolist (thread *threads-to-kill*)
                      (ignore-errors (sb-thread:terminate-thread thread)))
                    (setf ,threads (union (union *threads-to-kill*
                                                 *threads-to-join*)
                                          ,threads))
                    #+(and sb-safepoint-strictly (not win32))
                    (dolist (thread (sb-thread:list-all-threads))
                      (when (typep thread 'sb-thread:signal-handling-thread)
                        (ignore-errors (sb-thread:join-thread thread))))
                    (dolist (thread (sb-thread:list-all-threads))
                      (unless (or (not (sb-thread:thread-alive-p thread))
                                  (eql (the sb-thread:thread thread)
                                       sb-thread:*current-thread*)
                                  (member thread ,threads)
                                  (sb-thread:thread-ephemeral-p thread))
                        (setf any-leftover thread)
                        (ignore-errors (sb-thread:terminate-thread thread))))
                    (when any-leftover
                      (fail-test :leftover-thread ',name any-leftover)
                      (return-from ,block-name)))
                  (if (expected-failure-p ,fails-on)
                      (fail-test :unexpected-success ',name nil)
                      ;; Non-pretty is for cases like (with-test (:name (let ...)) ...
                      (log-msg/non-pretty "Success ~S" ',name)))))))))))

(defun report-test-status ()
  (with-standard-io-syntax
      (with-open-file (stream "test-status.lisp-expr"
                              :direction :output
                              :if-exists :supersede)
        (format stream "~s~%" *failures*))))

(defun start-test ()
  (unless (eq *test-file* *load-pathname*)
    (setf *test-file* *load-pathname*)
    (setf *test-count* 0))
  (incf *test-count*))

(defun really-invoke-debugger (condition)
  (with-simple-restart (continue "Continue")
    (let ((*invoke-debugger-hook* *invoke-debugger-hook*))
      (enable-debugger)
      (invoke-debugger condition))))

(defun fail-test (type test-name condition)
  (if (stringp condition)
      (log-msg "~@<~A ~S ~:_~A~:>"
               type test-name condition)
      (log-msg "~@<~A ~S ~:_due to ~S: ~4I~:_\"~A\"~:>"
               type test-name condition condition))
  (push (list type *test-file* (or test-name *test-count*))
        *failures*)
  (unless (stringp condition)
    (when (or (and *break-on-failure*
                   (not (eq type :expected-failure)))
              *break-on-expected-failure*)
      (really-invoke-debugger condition))))

(defun expected-failure-p (fails-on)
  (sb-impl::featurep fails-on))

(defun broken-p (broken-on)
  (sb-impl::featurep broken-on))

(defun skipped-p (skipped-on)
  (sb-impl::featurep skipped-on))

;;; Compile FORM capturing and muffling all [style-]warnings and notes
;;; and return five values: 1) the compiled function 2) a Boolean
;;; indicating whether compilation failed 3) a list of warnings 4) a
;;; list of style-warnigns 5) a list of notes.
;;;
;;; An error can be signaled when COMPILE indicates failure as well as
;;; in case [style-]warning or note conditions are signaled. The
;;; keyword parameters ALLOW-{FAILURE,[STYLE-]WARNINGS,NOTES} control
;;; this behavior. All but ALLOW-NOTES default to NIL.
;;;
;;; Arguments to the ALLOW-{FAILURE,[STYLE-]WARNINGS,NOTES} keyword
;;; parameters are interpreted as type specifiers restricting the
;;; allowed conditions of the respective kind.
(defun checked-compile (form
                        &key
                          allow-failure
                          allow-warnings
                          allow-style-warnings
                          (allow-notes t))
  (let ((warnings '())
        (style-warnings '())
        (notes '())
        (error-output (make-string-output-stream)))
    (handler-bind ((sb-ext:compiler-note
                    (lambda (condition)
                      (push condition notes)
                      (muffle-warning condition)))
                   (style-warning
                    (lambda (condition)
                      (push condition style-warnings)
                      (muffle-warning condition)))
                   (warning
                    (lambda (condition)
                      (push condition warnings)
                      (muffle-warning condition))))
      (multiple-value-bind (function warnings-p failure-p)
          (let ((*error-output* error-output))
            (compile nil form))
        (declare (ignore warnings-p))
        (labels ((fail (kind conditions &optional allowed-type)
                   (error "~@<Compilation of ~S signaled ~A~P:~
                           ~{~@:_~@:_~{~/sb-impl:print-symbol-with-prefix/: ~A~}~}~
                           ~@[~@:_~@:_Allowed type is ~S.~]~@:>"
                          form kind (length conditions)
                          (mapcar (lambda (condition)
                                    (list (type-of condition) condition))
                                  conditions)
                          allowed-type))
                 (check-conditions (kind conditions allow)
                   (cond
                     (allow
                      (let ((offenders (remove-if (lambda (condition)
                                                    (typep condition allow))
                                                  conditions)))
                        (when offenders
                          (fail kind offenders allow))))
                     (conditions
                      (fail kind conditions)))))

          (when (and (not allow-failure) failure-p)
            (let ((output (get-output-stream-string error-output)))
              (error "~@<Compilation of ~S failed~@[ with ~
                      output~@:_~@:_~A~@:_~@:_~].~@:>"
                     form (when (plusp (length output)) output))))

          (check-conditions "warning"       warnings       allow-warnings)
          (check-conditions "style-warning" style-warnings allow-style-warnings)
          (check-conditions "note"          notes          allow-notes)

          ;; Since we may have prevented warnings from being taken
          ;; into account for FAILURE-P by muffling them, adjust the
          ;; second return value accordingly.
          (values function (when (or failure-p warnings) t)
                  warnings style-warnings notes))))))

;;; Repeat calling THUNK until its cumulated runtime, measured using
;;; GET-INTERNAL-RUN-TIME, is larger than PRECISION. Repeat this
;;; REPETITIONS many times and return the time one call to THUNK took
;;; in seconds as a float, according to the minimum of the cumulated
;;; runtimes over the repetitions.
;;; This allows to easily measure the runtime of expressions that take
;;; much less time than one internal time unit. Also, the results are
;;; unaffected, modulo quantization effects, by changes to
;;; INTERNAL-TIME-UNITS-PER-SECOND.
;;; Taking the minimum is intended to reduce the error introduced by
;;; garbage collections occurring at unpredictable times. The inner
;;; loop doubles the number of calls to THUNK each time before again
;;; measuring the time spent, so that the time measurement overhead
;;; doesn't distort the result if calling THUNK takes very little time.
(defun runtime* (thunk repetitions precision)
  (loop repeat repetitions
        minimize
        (loop with start = (get-internal-run-time)
              with duration = 0
              for n = 1 then (* n 2)
              for total-runs = n then (+ total-runs n)
              do (dotimes (i n)
                   (funcall thunk))
                 (setf duration (- (get-internal-run-time) start))
              when (> duration precision)
              return (/ (float duration) (float total-runs)))
        into min-internal-time-units-per-call
        finally (return (/ min-internal-time-units-per-call
                           (float internal-time-units-per-second)))))

(defmacro runtime (form &key (repetitions 3) (precision 10))
  `(runtime* (lambda () ,form) ,repetitions ,precision))
