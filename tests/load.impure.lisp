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

(defvar *tmp-filename* (scratch-file-name "tmp"))

;;; These tests are essentially the same as in compiler.pure.lisp
;;; They have to be run before we mess up *DEFAULT-PATHNAME-DEFAULTS*
(with-test (:name :load-as-source-error-position-reporting)
  ;; These test errors that occur during READ
  (dolist (input '("data/wonky1.lisp" "data/wonky2.lisp" "data/wonky3.lisp"))
    (let ((expect (with-open-file (f input) (read f))))
      (assert (stringp expect))
      (let ((err-string
             (block foo
               ;; you can't query the stream position with HANDLER-CASE
               ;; because it closes before the condition is formatted.
               (handler-bind ((error (lambda (c)
                                       (return-from foo
                                         (write-to-string c :escape nil)))))
                 (load input)))))
        (assert (search expect err-string)))))

  ;; This tests an error that occur during EVAL
  (let ((s (with-output-to-string (*error-output*)
             (handler-bind ((error #'abort)) (load "data/wonky4.lisp")))))
    (assert (search "While evaluating the form starting at line 16, column 1"
                    s))))

;;; Save this because we're going to mess up the path in the following SETQ.
(defvar *parallel-load-source-file* (truename "parallel-fasl-load-test.lisp"))

;;; Tests in this file make assertions about behavior of LOAD or OPEN when given
;;; partial pathnames. As such, fully qualifying the pathname arguments with the
;;; temp dir would totally defeat the point of the tests. While we can't assume
;;; that the current directory is writable, we can change the defaults to the temp
;;; dir which doesn't affect the meaning of the tests, since they don't care where
;;; their files are, with the one exception being "parallel-load".
;;; I'm not doing this for win32 since I don't know what works there.
#-win32 (setq *default-pathname-defaults*
              (truename (make-pathname :directory
                                       (pathname-directory (scratch-file-name)))))

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
(with-test (:name :load-string-stream)
  (with-input-from-string (s *test-program-string*)
    (load-and-assert s nil nil)))

;; file-stream associated with a source file
(with-test (:name :load-lisp-file-stream)
  (let ((source (scratch-file-name "lisp")))
    (with-test-program source nil
      (with-open-file (stream source)
        (load-and-assert stream source source)))))

;; file-stream associated with a fasl file
(with-test (:name :load-fasl-file-stream)
  (let* ((source (scratch-file-name "lisp"))
         (fasl (compile-file-pathname source)))
    (with-test-program source fasl
      (with-open-file (stream fasl :element-type 'unsigned-byte)
        (load-and-assert fasl fasl fasl)))))

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

(with-test (:name :load-gray-stream)
  (with-open-stream (stream (make-instance 'load-impure-gray-stream))
    (load-and-assert stream nil nil)))

;;; Loading from things named by pathname designators.

(defvar *tmp-lisp-filename* (scratch-file-name "lisp"))

;; Test loading a source file by supplying a complete pathname.
(with-test (:name :load-source-file-full-pathname)
  (let ((source *tmp-lisp-filename*))
    (with-test-program source nil
      (load-and-assert source source source))))

;; Test loading a source file when supplying a partial pathname.
(with-test (:name :load-source-file-partial-pathname)
  (let ((source *tmp-lisp-filename*)
        (partial (make-pathname :defaults *tmp-lisp-filename*
                                 :type nil)))
    (with-test-program source nil
      (load-and-assert partial source source))))

;; Test loading a source file whose name lacks a type when supplying a
;; partial pathname.
(with-test (:name :load-source-file-default-type)
  (let ((source (make-pathname :type :unspecific
                               :defaults *tmp-lisp-filename*))
        (partial (make-pathname :defaults *tmp-lisp-filename*
                                :type nil)))
    (with-test-program source nil
      (load-and-assert partial partial partial))))

;; Test loading a fasl
(with-test (:name :load-fasl-file)
  (let* ((source *tmp-lisp-filename*)
         (fasl (compile-file-pathname source)))
    (with-test-program source fasl
      (load-and-assert fasl fasl fasl))))

;; Test loading a fasl when supplying a partial pathname.
(with-test (:name :load-fasl-file-partial-pathname)
  (let* ((source *tmp-lisp-filename*)
         (fasl (compile-file-pathname source))
         (partial (make-pathname :defaults *tmp-lisp-filename*
                                 :type nil)))
    (with-test-program source fasl
      (load-and-assert partial fasl fasl))))

;; Test loading a fasl whose name lacks a type when supplying a
;; partial pathname.
(with-test (:name :load-fasl-file-defaut-type)
  (let* ((source  *tmp-lisp-filename*)
         (fasl (make-pathname :type :unspecific
                              :defaults (compile-file-pathname source)))
         (partial (make-pathname :defaults *tmp-lisp-filename*
                                 :type nil)))
    (with-test-program source fasl
      (load-and-assert partial partial partial))))

;; Test loading a fasl with a strange type
(with-test (:name :load-fasl-file-strange-type)
  (let* ((source *tmp-lisp-filename*)
         (fasl (make-pathname :defaults (compile-file-pathname source)
                              :type "compiled-lisp")))
    (with-test-program source fasl
      (load-and-assert fasl fasl fasl))))

;;; Errors

;; Ensure that loading a fasl specified with a type checks for the
;; header.
(with-test (:name :load-fasl-header-missing-1)
  (let* ((source *tmp-lisp-filename*)
         (fasl (compile-file-pathname source)))
    (with-test-program source fasl
      (with-open-file (f fasl :direction :io :if-exists :overwrite
                         :element-type '(unsigned-byte 8))
        (write-byte 0 f))
      (handler-case (load fasl)
        (sb-fasl::fasl-header-missing () :ok)))))

;; Ensure that loading a fasl specified without a type checks for the
;; header.  Note: this wasn't the behavior in
;; src/code/target-load.lisp v1.40 and earlier (SBCL version 1.0.12.35
;; or so).  If target-load.lisp is reverted to that state eventually,
;; this test should be removed (or that definition of LOAD altered).
(with-test (:name :load-fasl-header-missing-2)
  (let* ((source *tmp-lisp-filename*)
         (fasl (compile-file-pathname source))
         (fasl-spec (make-pathname :type nil
                                   :defaults (compile-file-pathname source))))
    (with-test-program source fasl
      (with-open-file (f fasl :direction :io :if-exists :overwrite
                         :element-type '(unsigned-byte 8))
        (write-byte 0 f))
      (handler-case (load fasl-spec)
        (sb-fasl::fasl-header-missing () :ok)))))

;; Ensure that we get an error when the source file is newer than the
;; fasl and the supplied argument is an incomplete pathname.
(with-test (:name :load-default-obsolete-fasl)
  (let* ((source *tmp-lisp-filename*)
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
        (error () :|well, we got an error!|)))))

;; Ensure that we can invoke the restart SOURCE in the above case.
(with-test (:name :load-default-obsolete-fasl-restart-source)
  (let* ((source *tmp-lisp-filename*)
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
        (load-and-assert spec source source)))))

;; Ensure that we can invoke the restart OBJECT in the above case.
(with-test (:name :load-defaulted-obsolete-fasl-restart-object)
  (let* ((source *tmp-lisp-filename*)
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
        (load-and-assert spec fasl fasl)))))

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
  (let ((pathname (scratch-file-name type)))
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
  (with-scratch-file (fasl "fasl")
    (let ((ready nil))
      (multiple-value-bind (compiled warned failed)
          (compile-file *parallel-load-source-file* :output-file fasl)
        (assert (not warned))
        (assert (not failed)))
      (labels ((load-loop ()
                 (let* ((*standard-output* (make-broadcast-stream))
                        (*error-output* *standard-output*))
                   (sb-ext:wait-for ready)
                   (handler-case (dotimes (i 1000 t)
                                   (load fasl)
                                   (test-it))
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
          (test-it))))))

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
  (macroexpand '(sb-c:do-forms-from-info
                 ((myform myindex) my-source-info) (something))))
