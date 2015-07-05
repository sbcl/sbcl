;;;; miscellaneous side-effectful tests of LOAD

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

(defvar *tmp-filename* "load-test.tmp")

;;; Loading from Lisp should set the TOPLEVEL-FORM-NUMBER slot
(with-test (:name :load-lisp-assigns-tlf-num)
  (with-open-file (f *tmp-filename* :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write '(defvar *var0* 'this-is-form-0) :stream f)
    (write '(defvar *var1* 'this-is-form-1) :stream f))
  (load *tmp-filename*)
  (assert (eql 0 (sb-c:definition-source-location-toplevel-form-number
                     (sb-int:info :source-location :variable '*var0*))))
  (assert (eql 1 (sb-c:definition-source-location-toplevel-form-number
                     (sb-int:info :source-location :variable '*var1*))))
  (delete-file *tmp-filename*))

;;; Bug reported by Sean Ross: FASL loader set fill pointer to loaded
;;; simple arrays.

(defvar *array*)

(progn
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *array* #3a(((1 2) (2 1)) ((3 4) (4 3)))) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*array* nil))
             (load tmp-fasl)
             (assert (arrayp *array*))
             (assert (= (array-rank *array*) 3))
             (assert (not (array-has-fill-pointer-p *array*)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

;;; rudimentary external-format test
(dolist (ef '(:default :ascii :latin-1 :utf-8))
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(defun foo (x) (1+ x)) s))
  (fmakunbound 'foo)
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename* :external-format ef))
           (load tmp-fasl)
           (assert (= (foo 1) 2)))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

;;; As reported by David Tolpin *LOAD-PATHNAME* was not merged.
(progn
  (defparameter *saved-load-pathname* nil)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *saved-load-pathname* *load-pathname*) s))
  (unwind-protect
       (progn
         (load *tmp-filename*)
         (assert (equal (merge-pathnames *tmp-filename*) *saved-load-pathname*)))
    (delete-file *tmp-filename*)))

;;; Test many, many variations on LOAD.
(defparameter *counter* 0)
(defparameter *loaded-pathname* nil)
(defparameter *loaded-truename* nil)

(defparameter *test-program-string* (format nil "~
 (incf *counter*)
 (setf *loaded-pathname* *load-pathname*)
 (setf *loaded-truename* *load-truename*)"))

(defmacro load-and-assert (load-argument pathname truename)
  (let ((before (gensym)))
    `(let ((,before *counter*)
           *loaded-pathname* *loaded-truename*)
       (load ,load-argument :print t :verbose t)
       (assert (and (= (1+ ,before) *counter*)
                    #-win32 ;kludge
                    (equal ,(if pathname `(merge-pathnames ,pathname))
                           *loaded-pathname*)
                    #-win32 ;kludge
                    (equal ,(if pathname `(merge-pathnames ,truename))
                           *loaded-truename*))))))

(defmacro with-test-program (source fasl &body body)
  (let ((src (gensym))
        (fsl (gensym)))
    `(let ((,src ,source)
           (,fsl ,fasl))
       (with-open-file (*standard-output* ,src :direction :output
                                          :if-exists :supersede)
         (princ *test-program-string*))
       (when ,fsl
         (compile-file ,src :output-file ,fsl))
       (unwind-protect
            (progn
              ,@body)
         (when (probe-file ,src)
           (delete-file ,src))
         (when (and ,fsl (probe-file ,fsl))
           (delete-file ,fsl))))))

;;; Loading from streams.

;; string-stream
(with-input-from-string (s *test-program-string*)
  (load-and-assert s nil nil))

;; file-stream associated with a source file
(let ((source (pathname "load-impure-test.lisp")))
  (with-test-program source nil
    (with-open-file (stream source)
      (load-and-assert stream source source))))

;; file-stream associated with a fasl file
(let* ((source (pathname "load-impure-test.lisp"))
       (fasl (compile-file-pathname source)))
  (with-test-program source fasl
    (with-open-file (stream fasl :element-type 'unsigned-byte)
      (load-and-assert fasl fasl fasl))))

;; Develop a simple Gray stream to test loading from.
(defclass load-impure-gray-stream (fundamental-character-input-stream)
  ((pointer :initform 0 :accessor load-impure-gray-stream-pointer)))

(defmethod stream-read-char ((stream load-impure-gray-stream))
  (with-accessors ((pointer load-impure-gray-stream-pointer)) stream
    (prog1
        (if (>= pointer (length *test-program-string*))
            :eof
            (char *test-program-string* pointer))
      (incf pointer))))

(defmethod stream-unread-char ((stream load-impure-gray-stream) char)
  (with-accessors ((pointer load-impure-gray-stream-pointer)) stream
    (if (<= pointer 0)
        (error "fibber!  you never read from this stream ~S" stream)
        (decf pointer)))
  nil)

(with-open-stream (stream (make-instance 'load-impure-gray-stream))
  (load-and-assert stream nil nil))

;;; Loading from things named by pathname designators.

;; Test loading a source file by supplying a complete pathname.
(let ((source (pathname "load-impure-test.lisp")))
  (with-test-program source nil
    (load-and-assert source source source)))

;; Test loading a source file when supplying a partial pathname.
(let ((source (pathname "load-impure-test.lisp"))
      (partial (pathname "load-impure-test")))
  (with-test-program source nil
    (load-and-assert partial source source)))

;; Test loading a source file whose name lacks a type when supplying a
;; partial pathname.
(let ((source (make-pathname :type :unspecific
                             :defaults (pathname "load-impure-test")))
      (partial (pathname "load-impure-test")))
  (with-test-program source nil
    (load-and-assert partial partial partial)))

;; Test loading a fasl
(let* ((source (pathname "load-impure-test.lisp"))
       (fasl (compile-file-pathname source)))
  (with-test-program source fasl
    (load-and-assert fasl fasl fasl)))

;; Test loading a fasl when supplying a partial pathname.
(let* ((source  (pathname "load-impure-test.lisp"))
       (fasl (compile-file-pathname source))
       (partial (pathname "load-impure-test")))
  (with-test-program source fasl
    (load-and-assert partial fasl fasl)))

;; Test loading a fasl whose name lacks a type when supplying a
;; partial pathname.
(let* ((source  (pathname "load-impure-test.lisp"))
       (fasl (make-pathname :type :unspecific
                            :defaults (compile-file-pathname source)))
       (partial (pathname "load-impure-test")))
  (with-test-program source fasl
    (load-and-assert partial partial partial)))

;; Test loading a fasl with a strange type
(let* ((source (pathname "load-impure-test.lisp"))
       (fasl (make-pathname :defaults (compile-file-pathname source)
                            :type "compiled-lisp")))
  (with-test-program source fasl
    (load-and-assert fasl fasl fasl)))

;;; Errors

;; Ensure that loading a fasl specified with a type checks for the
;; header.
(let* ((source (pathname "load-impure-test.lisp"))
       (fasl (compile-file-pathname source)))
  (with-test-program source fasl
    (with-open-file (f fasl :direction :io :if-exists :overwrite
                       :element-type '(unsigned-byte 8))
      (write-byte 0 f))
    (handler-case (load fasl)
      (sb-fasl::fasl-header-missing () :ok))))

;; Ensure that loading a fasl specified without a type checks for the
;; header.  Note: this wasn't the behavior in
;; src/code/target-load.lisp v1.40 and earlier (SBCL version 1.0.12.35
;; or so).  If target-load.lisp is reverted to that state eventually,
;; this test should be removed (or that definition of LOAD altered).
(let* ((source (pathname "load-impure-test.lisp"))
       (fasl (compile-file-pathname source))
       (fasl-spec (make-pathname :type nil
                                 :defaults (compile-file-pathname source))))
  (with-test-program source fasl
    (with-open-file (f fasl :direction :io :if-exists :overwrite
                       :element-type '(unsigned-byte 8))
      (write-byte 0 f))
    (handler-case (load fasl-spec)
      (sb-fasl::fasl-header-missing () :ok))))

;; Ensure that we get an error when the source file is newer than the
;; fasl and the supplied argument is an incomplete pathname.
(let* ((source (pathname "load-impure-test.lisp"))
       (fasl (compile-file-pathname source))
       (spec (make-pathname :type nil :defaults source)))
  (with-test-program source fasl
    (sleep 1)
    (with-open-file (*standard-output* source :direction :output
                                       :if-exists :append)
      (write-line ";;comment"))
    (handler-case (load spec)
      ;; IWBNI the error signalled here were more specific than
      ;; SIMPLE-ERROR.
      (error () :|well, we got an error!|))))

;; Ensure that we can invoke the restart SOURCE in the above case.
(let* ((source (pathname "load-impure-test.lisp"))
       (fasl (compile-file-pathname source))
       (spec (make-pathname :type nil :defaults source)))
  (with-test-program source fasl
    (sleep 1)
    (with-open-file (*standard-output* source :direction :output
                                       :if-exists :append)
      (write-line ";;comment"))
    (handler-bind ((error (lambda (error)
                            (declare (ignore error))
                            (when (find-restart 'sb-fasl::source)
                              (invoke-restart 'sb-fasl::source)))))
      (load-and-assert spec source source))))

;; Ensure that we can invoke the restart OBJECT in the above case.
(let* ((source (pathname "load-impure-test.lisp"))
       (fasl (compile-file-pathname source))
       (spec (make-pathname :type nil :defaults source)))
  (with-test-program source fasl
    (sleep 1)
    (with-open-file (*standard-output* source :direction :output
                                       :if-exists :append)
      (write-line ";;comment"))
    (handler-bind ((error (lambda (error)
                            (declare (ignore error))
                            (when (find-restart 'sb-fasl::object)
                              (invoke-restart 'sb-fasl::object)))))
      (load-and-assert spec fasl fasl))))

(with-test (:name :bug-332)
  (flet ((stimulate-sbcl ()
           ;; compile and load the file, then invoke the continue restart on
           ;; the structure redefinition error
           (handler-bind ((error (lambda (c) (continue c))))
             (let ((fasl (compile-file "bug-332.lisp")))
               (load fasl)
               (ignore-errors (delete-file fasl))))))
    (stimulate-sbcl)
    (stimulate-sbcl)
    (stimulate-sbcl)))

(defun load-empty-file (type)
  (let ((pathname (make-pathname :name "load-impure-lisp-empty-temp"
                                 :type type)))
      (unwind-protect
           (progn
             (with-open-file (f pathname
                                :if-exists :supersede
                                :direction :output))
             (handler-case
                 (progn (load pathname) t)
               (error () nil)))
        (ignore-errors (delete-file pathname)))))

(with-test (:name (load :empty.lisp))
  (assert (load-empty-file "lisp")))

(with-test (:name (load :empty.fasl))
  (assert (not (load-empty-file "fasl"))))

;; There is a concurrency bug in ALLOCATE-CODE-OBJECT leading to deadlock.
;; Some changes to the compiler caused it to more often compile a TLF into
;; a callable lamda - as contrasted with a sequence of operations performed
;; entirely by the fasl interpreter - which exacerbated the problem.
;; A meager attempt at a fix of mutex-guarding ALLOCATE-CODE-OBJECT did not
;; resolve the deadlock, and was not ideal anyway.
(with-test (:name :parallel-fasl-load
            :skipped-on :sb-safepoint)
  #+sb-thread
  (let ((lisp #p"parallel-fasl-load-test.lisp")
        (fasl nil)
        (ready nil))
    (unwind-protect
         (progn
           (multiple-value-bind (compiled warned failed)
               (compile-file lisp)
             (setf fasl compiled)
             (assert (not warned))
             (assert (not failed))
             (labels ((load-loop ()
                        (let* ((*standard-output* (make-broadcast-stream))
                               (*error-output* *standard-output*))
                          (sb-ext:wait-for ready)
                          (handler-case
                              (progn
                                (loop repeat 1000
                                      do (load fasl)
                                         (test-it))
                                t)
                            (error (e) e))))
                      (test-it ()
                        (assert (= 1 (one-fun)))
                        (assert (= 2 (two-fun)))
                        (assert (= 42 (symbol-value '*var*)))
                        (assert (= 13 (symbol-value '*quux*)))))
               (let ((t1 (sb-thread:make-thread #'load-loop))
                     (t2 (sb-thread:make-thread #'load-loop))
                     (t3 (sb-thread:make-thread #'load-loop)))
                 (setf ready t)
                 (let ((r1 (sb-thread:join-thread t1))
                       (r2 (sb-thread:join-thread t2))
                       (r3 (sb-thread:join-thread t3)))
                   (unless (and (eq t r1) (eq t r2) (eq t r3))
                     (error "R1: ~A~2%R2: ~A~2%R2: ~A" r1 r2 r3))
                   ;; These ones cannot be tested while redefinitions are running:
                   ;; adding a method implies REMOVE-METHOD, so a call would be racy.
                   (assert (eq :ok (a-slot (make-instance 'a-class :slot :ok))))
                   (assert (eq 'cons (gen-fun '(foo))))
                   (assert (eq 'a-class (gen-fun (make-instance 'a-class)))))
                 (test-it)))))
      (when fasl
        (ignore-errors (delete-file fasl))))))

(defvar *pack*)
#+sb-simd-pack
(with-test (:name :load-simd-pack-int)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-kernel:%make-simd-pack-ub64 2 4)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-kernel:simd-pack integer)))
             (assert (= 2 (sb-kernel:%simd-pack-low *pack*)))
             (assert (= 4 (sb-kernel:%simd-pack-high *pack*)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

#+sb-simd-pack
(with-test (:name :load-simd-pack-single)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-kernel:%make-simd-pack-single 1f0 2f0 3f0 4f0)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-kernel:simd-pack single-float)))
             (assert (equal (multiple-value-list (sb-kernel:%simd-pack-singles *pack*))
                            '(1f0 2f0 3f0 4f0)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

#+sb-simd-pack
(with-test (:name :load-simd-pack-double)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-kernel:%make-simd-pack-double 1d0 2d0)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-kernel:simd-pack double-float)))
             (assert (equal (multiple-value-list (sb-kernel:%simd-pack-doubles *pack*))
                            '(1d0 2d0)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

;; Check that ':load print' on a fasl has some non-null effect
(with-test (:name :fasloader-print)
  (with-open-file (stream *tmp-filename*
                          :direction :output :if-exists :supersede)
    (dolist (form '((defmacro some-fancy-macro (x) `(car ,x))
                    (defvar *some-var* () nil)
                    (deftype my-favorite-type () '(integer -1 8))
                    (defun fred (x) (- x))
                    (push (some-fancy-macro '(a . b)) *some-var*)))
      (write form :stream stream)))
  (let* ((s (make-string-output-stream))
         (output (compile-file *tmp-filename*)))
    (let ((*standard-output* s))
      (load output :print t))
    (delete-file output)
    (assert (string= (get-output-stream-string s)
";; SOME-FANCY-MACRO
;; *SOME-VAR*
;; MY-FAVORITE-TYPE
;; FRED
;; (A)"))
     (delete-file *tmp-filename*)))

(with-test (:name :load-reader-error)
  (unwind-protect
       (block result
         (with-open-file (f *tmp-filename* :direction :output
                            :if-does-not-exist :create :if-exists :supersede)
           (write-string "(defun fool () (nosuchpackage: " f))
         (handler-bind
             ((condition
               (lambda (e)
                 (if (eql (search "READ error during LOAD:"
                                  (write-to-string e :escape nil))
                          0)
                     (return-from result t)
                     (error "Unexpectedly erred: ~S" e)))))
           (load *tmp-filename* :verbose nil)))
    (delete-file *tmp-filename*))
  ;; Not really a test of the bugfix, but a reminder that asdf-dependency-grovel
  ;; uses this internal macro and that we should endeavor not to break the syntax.
  (macroexpand '(sb-c::do-forms-from-info
                 ((myform myindex) my-source-info) (something))))
