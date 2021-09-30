(defpackage :test-util
  (:use :cl :sb-ext)
  (:export #:with-test #:report-test-status #:*failures*
           #:really-invoke-debugger
           #:*break-on-failure* #:*break-on-expected-failure*
           #:*elapsed-times*

           ;; type tools
           #:random-type
           #:type-evidently-=
           #:ctype=
           #:assert-tri-eq
           #:random-type

           ;; thread tools
           #:make-kill-thread #:make-join-thread
           #:wait-for-threads
           #:process-all-interrupts
           #:test-interrupt
           ;; cause tests to run in multiple threads
           #:enable-test-parallelism

           ;; MAP-OPTIMIZATION-*
           #:map-optimization-quality-combinations
           #:map-optimize-declarations

           ;; CHECKED-COMPILE and friends
           #:checked-compile #:checked-compile-and-assert
           #:checked-compile-capturing-source-paths
           #:checked-compile-condition-source-paths
           #:assemble
           #:get-simple-fun-instruction-model

           #:scratch-file-name
           #:*scratch-file-prefix*
           #:with-scratch-file
           #:opaque-identity
           #:runtime #:split-string #:integer-sequence #:shuffle))

(in-package :test-util)

(defvar *test-count* 0)
(defvar *test-file* nil)
(defvar *failures* nil)
(defvar *break-on-failure* nil)
(defvar *break-on-expected-failure* nil)

(defvar *threads-to-kill*)
(defvar *threads-to-join*)

(defun setenv (name value)
  #-win32
  (let ((r (sb-alien:alien-funcall
            (sb-alien:extern-alien
             "setenv" (function sb-alien:int (sb-alien:c-string :not-null t)
                                (sb-alien:c-string :not-null t) sb-alien:int))
                          name value 1)))
    (if (minusp r)
        (error "setenv: ~a" (sb-int:strerror))
        r))
  #+win32
  (let ((r (sb-alien:alien-funcall
            (sb-alien:extern-alien "_putenv" (function sb-alien:int (sb-alien:c-string :not-null t)))
                          (format nil "~A=~A" name value))))
    (if (minusp r)
        (error "putenv: ~a" (sb-int:strerror))
        r)))

(setenv "SBCL_MACHINE_TYPE" (machine-type))
(setenv "SBCL_SOFTWARE_TYPE" (software-type))


;;; Type tools

(defun random-type (n)
  `(integer ,(random n) ,(+ n (random n))))

(defun type-evidently-= (x y)
  (and (subtypep x y) (subtypep y x)))

(defun ctype= (left right)
  (let ((a (sb-kernel:specifier-type left)))
    ;; SPECIFIER-TYPE is a memoized function, and TYPE= is a trivial
    ;; operation if A and B are EQ.
    ;; To actually exercise the type operation, remove the memoized parse.
    (sb-int:drop-all-hash-caches)
    (let ((b (sb-kernel:specifier-type right)))
      (assert (not (eq a b)))
      (sb-kernel:type= a b))))

(defmacro assert-tri-eq (expected-result expected-certainp form)
  (sb-int:with-unique-names (result certainp)
    `(multiple-value-bind (,result ,certainp) ,form
       (assert (eq ,expected-result ,result))
       (assert (eq ,expected-certainp ,certainp)))))


;;; Thread tools

(defun make-kill-thread (&rest args)
  #-sb-thread (error "can't make-kill-thread ~s" args)
  #+sb-thread
  (let ((thread (apply #'sb-thread:make-thread args)))
    #-win32 ;; poor thread interruption on safepoints
    (when (boundp '*threads-to-kill*)
      (push thread *threads-to-kill*))
    thread))

#+sb-thread
(defun make-join-thread (&rest args)
  (let ((thread (apply #'sb-thread:make-thread args)))
    (when (boundp '*threads-to-join*)
      (push thread *threads-to-join*))
    thread))

(defun wait-for-threads (threads)
  (mapc (lambda (thread) (sb-thread:join-thread thread :default nil)) threads)
  (assert (not (some #'sb-thread:thread-alive-p threads))))

(defun process-all-interrupts (&optional (thread sb-thread:*current-thread*))
  (sb-ext:wait-for (null (sb-thread::thread-interruptions thread))))

(defun test-interrupt (function-to-interrupt &optional quit-p)
  ;; Tests of interrupting a newly created thread can fail if the creator runs
  ;; so quickly that it bypasses execution of the created thread. So the creator
  ;; needs to wait, to have a facsimile of the situation prior to implementation
  ;; of the so-called pauseless thread start feature.
  ;; Wouldn't you know, it's just reintroducing a startup semaphore.
  ;; And interruption tests are even more likely to fail with :sb-safepoint.
  ;; Noneless, this tries to be robust enough to pass.
  (let* ((sem (sb-thread:make-semaphore))
         (child  (make-kill-thread
                  (lambda ()
                    (sb-thread:signal-semaphore sem)
                    (funcall function-to-interrupt)))))
    (sb-thread:wait-on-semaphore sem)
    (format t "interrupting child ~A~%" child)
    (sb-thread:interrupt-thread child
     (lambda ()
       (format t "child pid ~A~%" sb-thread:*current-thread*)
       (when quit-p (sb-thread:abort-thread))))
    (process-all-interrupts child)
    child))

(defun log-msg (stream &rest args)
  (prog1 (apply #'format stream "~&::: ~@?~%" args)
    (force-output stream)))

(defun log-msg/non-pretty (stream &rest args)
  (let ((*print-pretty* nil))
    (apply #'log-msg stream args)))

(defvar *elapsed-times*)
(defun record-test-elapsed-time (test-name start-time)
  (let ((et (- (get-internal-real-time) start-time)))
    ;; ATOMIC in case we have within-file test concurrency
    ;; (not sure if it actually works, but it looks right anyway)
    (sb-ext:atomic-push (cons et test-name) *elapsed-times*)))

(defun run-test (test-function name fails-on
                 &aux (start-time (get-internal-real-time)))
  (start-test)
  (let (#+sb-thread (threads (sb-thread:list-all-threads))
        (*threads-to-join* nil)
        (*threads-to-kill* nil))
    (handler-bind ((error (lambda (error)
                            (if (expected-failure-p fails-on)
                                (fail-test :expected-failure name error)
                                (fail-test :unexpected-failure name error))
                            (return-from run-test)))
                   (timeout (lambda (error)
                              (if (expected-failure-p fails-on)
                                  (fail-test :expected-failure name error t)
                                  (fail-test :unexpected-failure name error t))
                              (return-from run-test))))
      ;; Non-pretty is for cases like (with-test (:name (let ...)) ...
      (log-msg/non-pretty *trace-output* "Running ~S" name)
      (funcall test-function)
      #+sb-thread
      (let ((any-leftover nil))
        (dolist (thread *threads-to-join*)
          (ignore-errors (sb-thread:join-thread thread)))
        (dolist (thread *threads-to-kill*)
          (ignore-errors (sb-thread:terminate-thread thread)))
        (setf threads (union (union *threads-to-kill*
                                    *threads-to-join*)
                             threads))
        (dolist (thread (sb-thread:list-all-threads))
          (unless (or (not (sb-thread:thread-alive-p thread))
                      (eql (the sb-thread:thread thread)
                           sb-thread:*current-thread*)
                      (member thread threads)
                      (sb-thread:thread-ephemeral-p thread))
            (setf any-leftover thread)
            #-win32
            (ignore-errors (sb-thread:terminate-thread thread))))
        (when any-leftover
          (fail-test :leftover-thread name any-leftover)
          (return-from run-test)))
      (if (expected-failure-p fails-on)
          (fail-test :unexpected-success name nil)
          ;; Non-pretty is for cases like (with-test (:name (let ...)) ...
          (log-msg/non-pretty *trace-output* "Success ~S" name))))
  (record-test-elapsed-time name start-time))

;;; Like RUN-TEST but do not perform any of the automated thread management.
;;; Since multiple threads are executing tests, there is no reason to kill
;;; unrecognized threads.
(sb-ext:define-load-time-global *output-mutex* (sb-thread:make-mutex :name "run-tests output"))
(defun run-test-concurrently (test-spec)
  (destructuring-bind (test-body . name) test-spec
    (sb-thread:with-mutex (*output-mutex*)
      (log-msg/non-pretty *trace-output* "Running ~S" name))
    (let ((stream (make-string-output-stream)))
      (let ((*standard-output* stream)
            (*error-output* stream))
        (let ((f (compile nil `(lambda () ,@test-body))))
          (funcall f))
        (let ((string (get-output-stream-string stream)))
          (sb-thread:with-mutex (*output-mutex*)
            (when (plusp (length string))
              (log-msg/non-pretty *trace-output* "Output from ~S" name)
              (write-string string *trace-output*))
            (log-msg/non-pretty *trace-output* "Success ~S" name)))))))

(defvar *deferred-test-forms*)
(defun enable-test-parallelism ()
  (let ((n (sb-ext:posix-getenv "SBCL_TEST_PARALLEL")))
    (when n
      (setq *deferred-test-forms* (vector (parse-integer n) nil nil)))))

;;; Tests which are not broken in any way and do not mandate sequential
;;; execution are pushed on a worklist to execute in multiple threads.
;;; The purpose of running tests in parallel is to exercise the compiler
;;; to show that it works without acquiring the world lock,
;;; but the nice side effect is that the tests finish quicker.
(defmacro with-test ((&key fails-on broken-on skipped-on name serial slow)
                     &body body)
  (flet ((name-ok (x y)
           (declare (ignore y))
           (typecase x
             (symbol (let ((package (symbol-package x)))
                       (or (null package)
                           (sb-int:system-package-p package)
                           (eql package (find-package "CL"))
                           (eql package (find-package "KEYWORD")))))
             (integer t))))
    (unless (tree-equal name name :test #'name-ok)
      (error "test name must be all-keywords: ~S" name)))
  (cond
    ((broken-p broken-on)
     `(progn
        (start-test)
        (fail-test :skipped-broken ',name "Test broken on this platform")))
    ((skipped-p skipped-on)
     `(progn
        (start-test)
        (fail-test :skipped-disabled ',name "Test disabled for this combination of platform and features")))
    ((and (boundp '*deferred-test-forms*)
          (not serial)
          (or (not fails-on)
              (not (expected-failure-p fails-on))))
     ;; To effectively parallelize calls to COMPILE, we must defer compilation
     ;; until a worker thread has picked off the test from shared worklist.
     ;; Thus we push only the form to be compiled, not a lambda.
     `(push (cons ',body ',name)
            (elt *deferred-test-forms* ,(if slow 1 2))))
    (t
     `(run-test (lambda () ,@body)
                ',name
                ',fails-on))))

(defun report-test-status ()
  (with-standard-io-syntax
    (with-open-file (stream #.(merge-pathnames "test-status.lisp-expr"
                                               *load-pathname*)
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

(defun fail-test (type test-name condition &optional backtrace)
  (if (stringp condition)
      (log-msg *trace-output* "~@<~A ~S ~:_~A~:>"
               type test-name condition)
      (log-msg *trace-output* "~@<~A ~S ~:_due to ~S: ~4I~:_\"~A\"~:>"
               type test-name (type-of condition) condition))
  (push (list type *test-file* (or test-name *test-count*))
        *failures*)
  (unless (stringp condition)
    (when backtrace
      (sb-debug:print-backtrace :from :interrupted-frame))
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

;;;; MAP-{OPTIMIZATION-QUALITY-COMBINATIONS,OPTIMIZE-DECLARATIONS}

(sb-int:defconstant-eqx +optimization-quality-names+
    '(speed safety debug compilation-speed space) #'equal)

(sb-int:defconstant-eqx +optimization-quality-keywords+
    '(:speed :safety :debug :compilation-speed :space) #'equal)

(deftype optimization-quality-range-designator ()
  '(or (eql nil)                                ; skip quality
       (integer 0 3)                            ; one value
       (cons (or (eql nil) (integer 0 3)) list) ; list of values, nil means skip
       (eql t)))                                ; all values

;;; Call FUNCTION with the specified combinations of optimization
;;; quality values.
;;;
;;; MAP-OPTIMIZATION-QUALITY-COMBINATIONS calls FUNCTION with keyword
;;; argument thus expecting a lambda list of the form
;;;
;;;   (&key speed safety debug compilation-speed space)
;;;
;;; or any subset compatible with the generated combinations.
;;;
;;; MAP-OPTIMIZE-DECLARATIONS calls FUNCTION with a list intended to
;;; be spliced into a DECLARE form like this:
;;;
;;;   (lambda (quality-values)
;;;     `(declare (optimize ,@quality-values)))
;;;
;;; The set of combinations is controlled via keyword arguments
;;;
;;;   :FILTER FILTER-FUNCTION
;;;     A function that should be called with optimization quality
;;;     keyword arguments and whose return value controls whether
;;;     FUNCTION should be called for the given combination.
;;;
;;;   (:SPEED | :SAFETY | :DEBUG | :COMPILATION-SPEED | :SPACE) SPEC
;;;     Specify value range for the given optimization quality. SPEC
;;;     can be
;;;
;;;       NIL
;;;         Omit the quality.
;;;
;;;       (INTEGER 0 3)
;;;
;;;         Use the specified value for the quality.
;;;
;;;       (NIL | (INTEGER 0 3))*
;;;         Generate the specified values. A "value" of NIL omits the
;;;         quality from the combination.
;;;
;;;       T
;;;         Generate all values (0, 1, 2, 3) for the quality.
(declaim (ftype (function #.`(function
                              &key
                              ,@(mapcar #'list +optimization-quality-keywords+
                                        '#1=(optimization-quality-range-designator . #1#))
                              (:filter function)))
                map-optimization-quality-combinations
                map-optimize-declarations))
(defun map-optimization-quality-combinations
    (function &key (speed t) (safety t) (debug t) (compilation-speed t) (space t)
                   filter)
  (labels ((map-quantity-values (values thunk)
             (typecase values
               ((eql t)
                (dotimes (i 4) (funcall thunk i)))
               (cons
                (map nil thunk values))
               ((integer 0 3)
                (funcall thunk values))))
           (one-quality (qualities specs values)
             (let ((quality (first qualities))
                   (spec    (first specs)))
               (cond
                 ((not quality)
                  (when (or (not filter) (apply filter values))
                    (apply function values)))
                 ((not spec)
                  (one-quality (rest qualities) (rest specs) values))
                 (t
                  (map-quantity-values
                   spec
                   (lambda (value)
                     (one-quality (rest qualities) (rest specs)
                                  (if value
                                      (list* quality value values)
                                      values)))))))))
    (one-quality +optimization-quality-keywords+
                 (list speed safety debug compilation-speed space)
                 '())))

(defun map-optimize-declarations
    (function &rest args
              &key speed safety debug compilation-speed space filter)
  (declare (ignore speed safety debug compilation-speed space filter))
  (apply #'map-optimization-quality-combinations
         (lambda (&rest args &key &allow-other-keys)
           (funcall function (loop for name in +optimization-quality-names+
                                for keyword in +optimization-quality-keywords+
                                for value = (getf args keyword)
                                when value collect (list name value))))
         args))

(defun expand-optimize-specifier (specifier)
  (etypecase specifier
    (cons
     specifier)
    ((eql nil)
     '(:speed nil :safety nil :debug nil :compilation-speed nil :space nil))
    ((eql :default)
     '(:speed 1 :safety 1 :debug 1 :compilation-speed 1 :space 1))
    ((eql :maximally-safe)
     (list :filter (lambda (&key safety &allow-other-keys)
                     (= safety 3))))
    ((eql :safe)
     (list :filter (lambda (&key speed safety &allow-other-keys)
                     (and (> safety 0) (>= safety speed)))))
    ((eql :quick)
     '(:compilation-speed 1 :space 1))
    ((eql :quick/incomplete)
     '(:compilation-speed nil :space nil))
    ((eql :all)
     '())))

(defun map-optimization-quality-combinations* (function specifier)
  (apply #'map-optimization-quality-combinations
         function (expand-optimize-specifier specifier)))

(defun map-optimize-declarations* (function specifier)
  (apply #'map-optimize-declarations
         function (expand-optimize-specifier specifier)))

;;;; CHECKED-COMPILE

(defun prepare-form (thing &key optimize)
  (cond
    ((functionp thing)
     (error "~@<~S is a function, not a form.~@:>" thing))
    ((not optimize)
     thing)
    ((typep thing '(cons (eql sb-int:named-lambda)))
     `(,@(subseq thing 0 3)
         (declare (optimize ,@optimize))
         ,@(nthcdr 3 thing)))
    ((typep thing '(cons (eql lambda)))
     `(,(first thing) ,(second thing)
        (declare (optimize ,@optimize))
        ,@(nthcdr 2 thing)))
    (t
     (error "~@<Cannot splice ~A declaration into forms other than ~
             ~{~S~#[~; and ~:;, ~]~}: ~S.~@:>"
            'optimize '(lambda sb-int:named-lambda) thing))))

(defun compile-capturing-output-and-conditions
    (form &key name condition-transform)
  (let ((warnings '())
        (style-warnings '())
        (notes '())
        (compiler-errors '())
        (error-output (make-string-output-stream)))
    (flet ((maybe-transform (condition)
             (if condition-transform
                 (funcall condition-transform condition)
                 condition)))
      (handler-bind ((sb-ext:compiler-note
                       (lambda (condition)
                         (push (maybe-transform condition) notes)
                         (muffle-warning condition)))
                     (style-warning
                       (lambda (condition)
                         (push (maybe-transform condition) style-warnings)
                         (muffle-warning condition)))
                     (warning
                       (lambda (condition)
                         (push (maybe-transform condition) warnings)
                         (muffle-warning condition)))
                     (sb-c:compiler-error
                       (lambda (condition)
                         (push (maybe-transform condition) compiler-errors))))
        (multiple-value-bind (function warnings-p failure-p)
            (let ((*error-output* error-output))
              (compile name form))
          (values function warnings-p failure-p
                  warnings style-warnings notes compiler-errors
                  error-output))))))

(defun print-form-and-optimize (stream form-and-optimize &optional colonp atp)
  (declare (ignore colonp atp))
  (destructuring-bind (form . optimize) form-and-optimize
    (format stream "~@:_~@:_~2@T~S~@:_~@:_~
                    with ~:[~
                      default optimization policy~
                    ~;~
                      ~:*~@:_~@:_~2@T~S~@:_~@:_~
                      optimization policy~
                    ~]"
            form optimize)))

(defun print-signaled-conditions (stream conditions &optional colonp atp)
  (declare (ignore colonp atp))
  (format stream "~{~@:_~@:_~{~/sb-ext:print-symbol-with-prefix/: ~A~}~}"
          (mapcar (lambda (condition)
                    (list (type-of condition) condition))
                  conditions)))

;;; Compile FORM capturing and muffling all [style-]warnings and notes
;;; and return six values: 1) the compiled function 2) a Boolean
;;; indicating whether compilation failed 3) a list of warnings 4) a
;;; list of style-warnings 5) a list of notes 6) a list of
;;; SB-C:COMPILER-ERROR conditions.
;;;
;;; An error can be signaled when COMPILE indicates failure as well as
;;; in case [style-]warning or note conditions are signaled. The
;;; keyword parameters
;;; ALLOW-{FAILURE,[STYLE-]WARNINGS,NOTES,COMPILER-ERRORS} control
;;; this behavior. All but ALLOW-NOTES default to NIL.
;;;
;;; Arguments to the
;;; ALLOW-{FAILURE,[STYLE-]WARNINGS,NOTES,COMPILER-ERRORS} keyword
;;; parameters are interpreted as type specifiers restricting the
;;; allowed conditions of the respective kind.
;;;
;;; When supplied, the value of CONDITION-TRANSFORM has to be a
;;; function of one argument, the condition currently being
;;; captured. The returned value is captured and later returned in
;;; place of the condition.
(defun checked-compile (form
                        &key
                        name
                        allow-failure
                        allow-warnings
                        allow-style-warnings
                        (allow-notes t)
                        (allow-compiler-errors allow-failure)
                        condition-transform
                        optimize)
  (sb-int:binding* ((prepared-form (prepare-form form :optimize optimize))
                    ((function nil failure-p
                      warnings style-warnings notes compiler-errors
                      error-output)
                     (compile-capturing-output-and-conditions
                      prepared-form :name name :condition-transform condition-transform)))
    (labels ((fail (kind conditions &optional allowed-type)
               (error "~@<Compilation of~/test-util::print-form-and-optimize/ ~
                       signaled ~A~P:~/test-util::print-signaled-conditions/~
                       ~@[~@:_~@:_Allowed type is ~
                      ~/sb-impl:print-type-specifier/.~]~@:>"
                      (cons form optimize) kind (length conditions) conditions
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
          (error "~@<Compilation of~/test-util::print-form-and-optimize/ ~
                  failed~@[ with output~
                  ~@:_~@:_~2@T~@<~@;~A~:>~@:_~@:_~].~@:>"
                 (cons form optimize) (when (plusp (length output)) output))))

      (check-conditions "warning"        warnings        allow-warnings)
      (check-conditions "style-warning"  style-warnings  allow-style-warnings)
      (check-conditions "note"           notes           allow-notes)
      (check-conditions "compiler-error" compiler-errors allow-compiler-errors)

      ;; Since we may have prevented warnings from being taken
      ;; into account for FAILURE-P by muffling them, adjust the
      ;; second return value accordingly.
      (values function (when (or failure-p warnings) t)
              warnings style-warnings notes compiler-errors))))

(defun print-arguments (stream arguments &optional colonp atp)
  (declare (ignore colonp atp))
  (format stream "~:[~
                    without arguments ~
                  ~;~:*~
                    with arguments~@:_~@:_~
                    ~2@T~@<~{~S~^~@:_~}~:>~@:_~@:_~
                  ~]"
          arguments))

(defun call-capturing-values-and-conditions (function &rest args)
  (let ((values     nil)
        (conditions '()))
    (block nil
      (handler-bind ((condition (lambda (condition)
                                  (push condition conditions)
                                  (typecase condition
                                    (warning
                                     (muffle-warning condition))
                                    (serious-condition
                                     (return))))))
        (setf values (multiple-value-list (apply function args)))))
    (values values (nreverse conditions))))

(defun %checked-compile-and-assert-one-case
    (form optimize function args-thunk expected test allow-conditions)
  (let ((args (multiple-value-list (funcall args-thunk))))
    (flet ((failed-to-signal (expected-type)
             (error "~@<Calling the result of compiling~
                      ~/test-util::print-form-and-optimize/ ~
                      ~/test-util::print-arguments/~
                      returned normally instead of signaling a ~
                      condition of type ~
                      ~/sb-impl:print-type-specifier/.~@:>"
                    (cons form optimize) args expected-type))
           (signaled-unexpected (conditions)
             (error "~@<Calling the result of compiling~
                      ~/test-util::print-form-and-optimize/ ~
                      ~/test-util::print-arguments/~
                      signaled unexpected condition~P~
                      ~/test-util::print-signaled-conditions/~
                      .~@:>"
                    (cons form optimize) args (length conditions) conditions))
           (returned-unexpected (values expected test)
             (error "~@<Calling the result of compiling~
                     ~/test-util::print-form-and-optimize/ ~
                     ~/test-util::print-arguments/~
                     returned values~@:_~@:_~
                     ~2@T~<~{~S~^~@:_~}~:>~@:_~@:_~
                     which is not ~S to~@:_~@:_~
                     ~2@T~<~{~S~^~@:_~}~:>~@:_~@:_~
                     .~@:>"
                    (cons form optimize) args
                    (list values) test (list expected))))
      (multiple-value-bind (values conditions)
          (apply #'call-capturing-values-and-conditions function args)
        (typecase expected
          ((cons (eql condition) (cons t null))
           (let* ((expected-condition-type (second expected))
                  (unexpected (remove-if (lambda (condition)
                                           (typep condition
                                                  expected-condition-type))
                                         conditions))
                  (expected (set-difference conditions unexpected)))
             (cond
               (unexpected
                (signaled-unexpected unexpected))
               ((null expected)
                (failed-to-signal expected-condition-type)))))
          (t
           (let ((expected (funcall expected)))
             (cond
               ((and conditions
                     (not (and allow-conditions
                               (every (lambda (condition)
                                        (typep condition allow-conditions))
                                      conditions))))
                (signaled-unexpected conditions))
               ((not (funcall test values expected))
                (returned-unexpected values expected test))))))))))

(defun %checked-compile-and-assert-one-compilation
    (form optimize other-checked-compile-args cases)
  (let ((function (apply #'checked-compile form
                         (if optimize
                             (list* :optimize optimize
                                    other-checked-compile-args)
                             other-checked-compile-args))))
    (loop for (args-thunk values test allow-conditions) in cases
       do (%checked-compile-and-assert-one-case
           form optimize function args-thunk values test allow-conditions))))

(defun %checked-compile-and-assert (form checked-compile-args cases)
  (let ((optimize (getf checked-compile-args :optimize))
        (other-args (loop for (key value) on checked-compile-args by #'cddr
                          unless (eq key :optimize)
                          collect key and collect value)))
    (map-optimize-declarations*
     (lambda (&optional optimize)
       (%checked-compile-and-assert-one-compilation
        form optimize other-args cases))
     optimize)))

;;; Compile FORM using CHECKED-COMPILE, then call the resulting
;;; function with arguments and assert expected return values
;;; according to CASES.
;;;
;;; Elements of CASES are of the form
;;;
;;;   ((&rest ARGUMENT-FORMS) VALUES-FORM &key TEST ALLOW-CONDITIONS)
;;;
;;; where ARGUMENT-FORMS are evaluated to produce the arguments for
;;; one call of the function and VALUES-FORM is evaluated to produce
;;; the expected return values for that function call.
;;;
;;; TEST is used to compare a list of the values returned by the
;;; function call to the list of values obtained by calling
;;; VALUES-FORM.
;;;
;;; If supplied, the value of ALLOW-CONDITIONS is a type-specifier
;;; indicating which conditions should be allowed (and ignored) during
;;; the function call.
;;;
;;; If VALUES-FORM is of the form
;;;
;;;   (CONDITION CONDITION-TYPE)
;;;
;;; the function call is expected to signal the designated condition
;;; instead of returning values. CONDITION-TYPE is evaluated.
;;;
;;; The OPTIMIZE keyword parameter controls the optimization policies
;;; (or policy) used when compiling FORM. The argument is interpreted
;;; as described for MAP-OPTIMIZE-DECLARATIONS*.
;;;
;;; The other keyword parameters, NAME and
;;; ALLOW-{WARNINGS,STYLE-WARNINGS,NOTES}, behave as with
;;; CHECKED-COMPILE.
(defmacro checked-compile-and-assert ((&key name
                                            allow-warnings
                                            allow-style-warnings
                                            (allow-notes t)
                                            (optimize :quick))
                                         form &body cases)
  (flet ((make-case-form (case)
           (destructuring-bind (args values &key (test ''equal testp)
                                     allow-conditions)
               case
             (let ((conditionp (typep values '(cons (eql condition) (cons t null)))))
               (when (and testp conditionp)
                 (sb-ext:with-current-source-form (case)
                   (error "~@<Cannot use ~S with ~S ~S.~@:>"
                          values :test test)))
               `(list (lambda () (values ,@args))
                      ,(if conditionp
                           `(list 'condition ,(second values))
                           `(lambda () (multiple-value-list ,values)))
                      ,test
                      ,allow-conditions)))))
    `(%checked-compile-and-assert
      ,form (list :name ,name
                  :allow-warnings ,allow-warnings
                  :allow-style-warnings ,allow-style-warnings
                  :allow-notes ,allow-notes
                  :optimize ,optimize)
      (list ,@(mapcar #'make-case-form cases)))))

;;; Like CHECKED-COMPILE, but for each captured condition, capture and
;;; later return a cons
;;;
;;;   (CONDITION . SOURCE-PATH)
;;;
;;; instead. SOURCE-PATH is the path of the source form associated to
;;; CONDITION.
(defun checked-compile-capturing-source-paths (form &rest args)
  (labels ((context-source-path ()
             (let ((context (sb-c::find-error-context nil)))
               (sb-c::compiler-error-context-original-source-path
                context)))
           (add-source-path (condition)
             (cons condition (context-source-path))))
    (apply #'checked-compile form :condition-transform #'add-source-path
           args)))

;;; Similar to CHECKED-COMPILE, but allow compilation failure and
;;; warnings and only return source paths associated to those
;;; conditions.
(defun checked-compile-condition-source-paths (form)
  (let ((source-paths '()))
    (labels ((context-source-path ()
               (let ((context (sb-c::find-error-context nil)))
                 (sb-c::compiler-error-context-original-source-path
                  context)))
             (push-source-path (condition)
               (declare (ignore condition))
               (push (context-source-path) source-paths)))
      (checked-compile form
                       :allow-failure t
                       :allow-warnings t
                       :allow-style-warnings t
                       :condition-transform #'push-source-path))
    (nreverse source-paths)))

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
              for gc-start = *gc-run-time*
              do (dotimes (i n)
                   (funcall thunk))
                 (setf duration (- (get-internal-run-time) start
                                   (- *gc-run-time* gc-start)))
              when (> duration precision)
              return (/ (float duration)
                        (float total-runs)))
        into min-internal-time-units-per-call
        finally (return (/ min-internal-time-units-per-call
                           (float internal-time-units-per-second)))))

(defmacro runtime (form &key (repetitions 5) (precision (* 30
                                                           (/ internal-time-units-per-second 1000))))
  `(runtime* (lambda () ,form) ,repetitions ,precision))

(declaim (notinline opaque-identity))
(defun opaque-identity (x) x)
(compile 'opaque-identity) ; in case this file was loaded as interpreted code

(defun split-string (string delimiter)
  (loop for begin = 0 then (1+ end)
        for end = (position delimiter string) then (position delimiter string :start begin)
        collect (subseq string begin end)
        while end))

(defun integer-sequence (n)
  (loop for i below n collect i))

(defun shuffle (sequence)
  (typecase sequence
    (list
     (coerce (shuffle (coerce sequence 'vector)) 'list))
    (vector ; destructive
     (let ((vector sequence))
       (loop for lim from (1- (length vector)) downto 0
             for chosen = (random (1+ lim))
             unless (= chosen lim)
             do (rotatef (aref vector chosen) (aref vector lim)))
       vector))))

;;; Return a random file name to avoid writing into the source tree.
;;; We can't use any of the interfaces provided in libc because those are inadequate
;;; for purposes of COMPILE-FILE. This is not trying to be robust against attacks.
(defvar *scratch-file-prefix* "sbcl-scratch")
(defun scratch-file-name (&optional extension)
  (let ((a (make-array 10 :element-type 'character)))
    (dotimes (i 10)
      (setf (aref a i) (code-char (+ (char-code #\a) (random 26)))))
    (let ((dir (posix-getenv #+win32 "TMP" #+unix "TMPDIR"))
          (file (format nil "~a~d~a~@[.~a~]" *scratch-file-prefix*
                        (sb-unix:unix-getpid) a extension)))
      (if dir
          (namestring
           (merge-pathnames
            file (truename (parse-native-namestring dir nil *default-pathname-defaults*
                                                    :as-directory t))))
          (concatenate 'string "/tmp/" file)))))

(defmacro with-scratch-file ((var &optional extension) &body forms)
  (sb-int:with-unique-names (tempname)
    `(let ((,tempname (scratch-file-name ,extension)))
       (unwind-protect
            (let ((,var ,tempname)) ,@forms) ; rebind, as test might asssign into VAR
         (ignore-errors (delete-file ,tempname))))))

;;; Take a list of lists and assemble them as though they are
;;; instructions inside the body of a vop. There is no need
;;; to use the INST macro in front of each list.
;;; As a special case, if an atom is the symbol LABEL, it will be
;;; changed to a generated label. At most one such atom may appear.
(defun assemble (instructions)
  (let ((segment (sb-assem:make-segment))
        (label))
    (sb-assem:assemble (segment 'nil)
       (dolist (inst instructions)
         (setq inst (copy-list inst))
         (mapl (lambda (cell &aux (x (car cell)))
                 (when (and (symbolp x) (string= x "LABEL"))
                   (setq label (sb-assem:gen-label))
                   (rplaca cell label)))
               inst)
         (apply #'sb-assem:inst* (car inst) (cdr inst)))
       (when label
         (sb-assem::%emit-label segment nil label)))
    (sb-assem:segment-buffer
     (sb-assem:finalize-segment segment))))

(defun get-simple-fun-instruction-model (fun)
  (declare (type sb-kernel:simple-fun fun))
  (sb-disassem:get-inst-space) ; for effect
  (let* ((code (sb-kernel:fun-code-header fun))
         (segment (sb-disassem:make-code-segment code
                                                 (sb-sys:sap- (sb-vm:simple-fun-entry-sap fun)
                                                              (sb-kernel:code-instructions code))
                                                 (sb-kernel:%simple-fun-text-len fun)))
         (dstate (sb-disassem:make-dstate nil)))
    (setf (sb-disassem::dstate-absolutize-jumps dstate) nil
          (sb-disassem:dstate-segment dstate) segment
          (sb-disassem:dstate-segment-sap dstate) (funcall (sb-disassem:seg-sap-maker segment)))
    (sb-int:collect ((result))
      (loop (let ((pc (sb-disassem:dstate-cur-offs dstate)))
              (result (cons pc (sb-disassem:disassemble-instruction dstate))))
            (when (>= (sb-disassem:dstate-cur-offs dstate) (sb-disassem:seg-length segment))
              (return)))
      (result))))

;;; If a test file is not very tolerant of the statistical profiler, then
;;; it should call this. There seem to be at least 2 common categories of failure:
;;;  * the stack exhaustion test can get a profiling signal when you're already
;;;    near exhaustion, and then the profiler exhausts the stack, which isn't
;;;    handled very well. Example:
;;;    Control stack exhausted, fault: 0xd76b9ff8, PC: 0x806cad2
;;;       0: fp=0xd76ba008 pc=0x806cad2 Foreign function search_dynamic_space
;;;       1: fp=0xd76ba028 pc=0x80633d1 Foreign function search_all_gc_spaces
;;;       2: fp=0xd76ba048 pc=0x805647e Foreign function component_ptr_from_pc
;;;       3: fp=0xd76baa18 pc=0x8065dfd Foreign function (null)
;;;       4: fp=0xd76baa98 pc=0x806615a Foreign function record_backtrace_from_context
;;;       5: fp=0xd76baab8 pc=0x806625e Foreign function sigprof_handler
;;;  * debugger-related tests. I'm not sure why.
(defun disable-profiling ()
  (when (find-package "SB-SPROF")
    (format t "INFO: disabling SB-SPROF~%")
    (funcall (intern "STOP-PROFILING" "SB-SPROF"))))
