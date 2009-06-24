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
  (defvar *saved-load-pathname*)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *saved-load-pathname* *load-pathname*) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (load *tmp-filename*)
           (assert (equal (merge-pathnames *tmp-filename*) *saved-load-pathname*)))
      (delete-file *tmp-filename*))))

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
                    (equal ,(if pathname `(merge-pathnames ,pathname))
                           *loaded-pathname*)
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
           (let ((filename (format nil "/tmp/~A.lisp" (gensym))))
             ;; create a file which redefines a structure incompatibly
             (with-open-file (f filename :direction :output :if-exists :supersede)
               (print '(defstruct bug-332 foo) f)
               (print '(defstruct bug-332 foo bar) f))
             ;; compile and load the file, then invoke the continue restart on
             ;; the structure redefinition error
             (handler-bind ((error (lambda (c) (continue c))))
               (load (compile-file filename))))))
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

(with-test (:name (load "empty.lisp"))
  (assert (load-empty-file "lisp")))

(with-test (:name (load "empty.fasl"))
  (assert (not (load-empty-file "fasl"))))
