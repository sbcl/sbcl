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

;;;; DIRECTORY

(defun truename-as-expected-p ()
  #.(and (string= (pathname-name *load-pathname*) (pathname-name *load-truename*))
         (string= (pathname-type *load-pathname*) (pathname-type *load-truename*))))

;;; In sbcl-0.6.9 DIRECTORY failed on paths with :WILD or
;;; :WILD-INFERIORS in their directory components.
(with-test (:name (directory :wild-inferiors))
  (let ((dir (directory "../**/*.*")))
    ;; We know a little bit about the structure of this result;
    ;; let's test to make sure that this test file is in it.
    ;; If the  truename of this file is not as expected, the look for only the
    ;; name+type regardless of directory, treating all parts as essentially random.
    (let ((string-to-find
           (if (truename-as-expected-p)
               "tests/filesys.pure.lisp"
               (namestring (make-pathname :name (pathname-name *load-truename*)
                                          :type (pathname-type *load-truename*))))))
      (assert (find string-to-find dir
                    :test #'search :key #'namestring)))))

;;; In sbcl-0.9.7 DIRECTORY failed on pathnames with character-set
;;; components.
(with-test (:name (directory :character-set :pattern))
  ;; In addition to potential truename randomization,
  ;; do not assume that the current directory is the place to look.
  (let* ((pattern (if (truename-as-expected-p)
                      "[f]*.*"
                      (format nil "~a[~a]*~:[~;.*~]"
                              (namestring
                               (make-pathname :directory (pathname-directory *load-truename*)))
                              (char (pathname-name *load-truename*) 0)
                              (pathname-type *load-truename*))))
         (string-to-find (if (truename-as-expected-p)
                             "filesys.pure.lisp"
                             (pathname-name *load-truename*)))
         (dir (directory pattern)))
    (assert (find string-to-find dir :test #'search :key #'namestring))))

;;; Canonicalization of pathnames for DIRECTORY
(with-test (:name (directory :/.))
  (assert (equal (directory #p".") (directory #p"./")))
  (assert (equal (directory #p".") (directory #p""))))
(with-test (:name (directory :/..))
  (assert (equal (directory #p"..") (directory #p"../"))))
(with-test (:name (directory :unspecific))
  (assert (equal (directory #p".")
                 (directory (make-pathname
                             :name :unspecific
                             :type :unspecific)))))

;;; This used to signal a TYPE-ERROR.
(with-test (:name (directory :..*))
  (directory "somedir/..*"))

;;; DIRECTORY used to treat */** as **.
(with-test (:name (directory :*/**))
  ;; FIXME: this test should be redone to construct a controlled file
  ;; hierarchy for listing. If test files or test runners ever get
  ;; reorganized and it turns out there are no subdirectories under
  ;; the *DEFAULT-PATHNAME-DEFAULTS* when this gets run, this test
  ;; will pass but fail to test the behavior it's supposed to.
  (assert (equal (directory "*/**/*.*")
                 ;; Each call to DIRECTORY sorts its results, but if
                 ;; our files' truenames are random (e.g., if files
                 ;; here are symlinks to hashes of file content), then
                 ;; we need to merge the results of the inner
                 ;; DIRECTORY calls.
                 (reduce
                  (lambda (list1 list2)
                    ;; Depends on all truenames having namestrings.
                    ;; (Which they do; noted for future reference.)
                    (merge 'list list1 list2 'string< :key 'namestring))
                  (mapcar (lambda (directory)
                            (directory (merge-pathnames "**/*.*" directory)))
                          (directory "*/"))))))

(with-test (:name (directory *default-pathname-defaults* :bug-1740563))
  ;; FIXME: this writes into the source directory depending on whether
  ;; TEST_DIRECTORY has been made to point elsewhere or not.
  (let ((test-directory (concatenate 'string (sb-ext:posix-getenv "TEST_DIRECTORY") "/")))
    (ensure-directories-exist test-directory)
    (close (open (merge-pathnames "a.txt" test-directory) :if-does-not-exist :create))
    (close (open (merge-pathnames "b.lisp" test-directory) :if-does-not-exist :create))
    (unwind-protect
         (flet ((directory* (pattern &rest d-p-d-components)
                  (let ((*default-pathname-defaults*
                         (apply #'make-pathname
                                :defaults *default-pathname-defaults*
                                d-p-d-components)))
                    (directory pattern))))
           (let* ((*default-pathname-defaults* (pathname test-directory))
                  (expected-wild (directory "*.*"))
                  (expected-one-file (directory "a.txt"))
                  (cases '((:name nil   :type "txt")
                           (:name nil   :type :wild)
                           (:name "a"   :type nil)
                           (:name "a"   :type "txt")
                           (:name "a"   :type :wild)
                           (:name :wild :type nil)
                           (:name :wild :type :wild)
                           (:name :wild :type "txt"))))
             (dolist (components cases)
               (assert (equal (apply #'directory* "*.*" components)
                              expected-wild))
               (assert (equal (apply #'directory* "a.txt" components)
                              expected-one-file)))
             (assert (equal (directory* "" :name :wild :type :wild)
                            expected-wild))))
      (delete-directory test-directory :recursive t))))

;;;; OPEN

;;; In sbcl-0.6.9 FOO-NAMESTRING functions  returned "" instead of NIL.
(with-test (:name (file-namestring directory-namestring :name))
  (let ((pathname0 (make-pathname :host nil
                                  :directory
                                  (pathname-directory
                                   *default-pathname-defaults*)
                                  :name "getty"))
        (pathname1 (make-pathname :host nil
                                  :directory nil
                                  :name nil)))
    (assert (equal (file-namestring pathname0) "getty"))
    (assert (equal (directory-namestring pathname0)
                   (directory-namestring *default-pathname-defaults*)))
    (assert (equal (file-namestring pathname1) ""))
    (assert (equal (directory-namestring pathname1) ""))))

;;; Set *default-pathname-defaults* to something other than the unix
;;; cwd, to catch functions which access the filesystem without
;;; merging properly.  We should test more functions than just OPEN
;;; here, of course

(with-test (:name (open *default-pathname-defaults*))
  (let ((*default-pathname-defaults*
         (make-pathname :directory
                        (butlast
                         (pathname-directory *default-pathname-defaults*))
                        :defaults *default-pathname-defaults*)))
    ;; SBCL 0.7.1.2 failed to merge on OPEN
    (with-open-file (i "tests/filesys.pure.lisp")
      (assert i))))

;;; OPEN, LOAD and friends should signal an error of type FILE-ERROR
;;; if they are fed wild pathname designators; firstly, with wild
;;; pathnames that don't correspond to any files:
(with-test (:name (open :wild file-error 1))
  (assert-error (open "non-existent*.lisp") file-error))
(with-test (:name (load :wild file-error 1))
  (assert-error (load "non-existent*.lisp") file-error))
;;; then for pathnames that correspond to precisely one:
(with-test (:name (open :wild file-error 2))
  (assert-error (open "filesys.pur*.lisp") file-error))
(with-test (:name (load :wild file-error 2))
  (assert-error (load "filesys.pur*.lisp") file-error))
;;; then for pathnames corresponding to many:
(with-test (:name (open :wild file-error 3))
  (assert-error (open "*.lisp") file-error))
(with-test (:name (load :wild file-error 3))
  (assert-error (load "*.lisp") file-error))

;;; ANSI: FILE-LENGTH should signal an error of type TYPE-ERROR if
;;; STREAM is not a stream associated with a file.
;;;
;;; (Peter Van Eynde's ansi-test suite caught this, and Eric Marsden
;;; reported a fix for CMU CL, which was ported to sbcl-0.6.12.35.)
(with-test (:name (file-length *terminal-io* type-error))
  (assert-error (file-length *terminal-io*) type-error))

(with-test (:name (file-length synonym-stream))
  (with-open-file (*stream* "filesys.pure.lisp" :direction :input)
    (declare (special *stream*))
    (assert (integerp (file-length (make-synonym-stream '*stream*))))
    (let ((*stream2* (make-synonym-stream '*stream*)))
      (declare (special *stream2*))
      (assert (integerp (file-length (make-synonym-stream '*stream2*)))))))

;;; A few cases Windows does have enough marbles to pass right now
(with-test (:name (sb-ext:native-namestring :win32)
                  :skipped-on (not :win32))
  (assert (equal "C:\\FOO" (native-namestring "C:\\FOO")))
  (assert (equal "C:\\FOO" (native-namestring "C:/FOO")))
  (assert (equal "C:\\FOO\\BAR" (native-namestring "C:\\FOO\\BAR")))
  (assert (equal "C:\\FOO\\BAR" (native-namestring "C:\\FOO\\BAR\\" :as-file t))))

(with-test (:name (sb-ext:parse-native-namestring :as-directory :junk-allowed))
  (assert
   (equal
    (parse-native-namestring "foo.lisp" nil *default-pathname-defaults*
                             :as-directory t)
    (parse-native-namestring "foo.lisp" nil *default-pathname-defaults*
                             :as-directory t
                             :junk-allowed t))))

;;; Test for NATIVE-PATHNAME / NATIVE-NAMESTRING stuff
;;;
;;; given only safe characters in the namestring, NATIVE-PATHNAME will
;;; never error, and NATIVE-NAMESTRING on the result will return the
;;; original namestring.
(with-test (:name (sb-ext:native-namestring sb-ext:native-pathname :random))
  (let ((safe-chars
         (coerce
          (cons #\Newline
                (loop for x from 32 to 127 collect (code-char x)))
          'simple-base-string))
        (tricky-sequences #("/../" "../" "/.." "." "/." "./" "/./"
                            "[]" "*" "**" "/**" "**/" "/**/" "?"
                            "\\*" "\\[]" "\\?" "\\*\\*" "*\\*")))
   (labels ((canon (s)
              #+win32
              ;; We canonicalize to \ as the directory separator
              ;; on windows -- though both \ and / are legal.
              (substitute #\\ #\/ s)
              #+unix
              ;; Consecutive separators become a single separator
              (let ((p (search "//" s)))
                (if p
                    (canon (concatenate 'string (subseq s 0 p) (subseq s (1+ p))))
                    s))))
    (loop repeat 1000
          for length = (random 32)
          for native-namestring = (coerce
                                   (loop repeat length
                                         collect
                                         (char safe-chars
                                               (random (length safe-chars))))
                                   'simple-base-string)
          for pathname = (native-pathname native-namestring)
          for nnn = (native-namestring pathname)
          do (setf native-namestring (canon native-namestring))
             (unless (string= nnn native-namestring)
               (error "1: wanted ~S, got ~S" native-namestring nnn)))
    (loop repeat 1000
          for native-namestring = (with-output-to-string (s)
                                    (write-string "mu" s)
                                    (loop
                                      (let ((r (random 1.0)))
                                        (cond
                                          ((< r 1/20) (return))
                                          ((< r 1/2)
                                           (write-char
                                            (char safe-chars
                                                  (random (length safe-chars)))
                                            s))
                                          (t (write-string
                                              (aref tricky-sequences
                                                    (random
                                                     (length tricky-sequences)))
                                              s))))))
          for pathname = (native-pathname native-namestring)
          for tricky-nnn = (native-namestring pathname)
          do (setf native-namestring (canon native-namestring))
             (unless (string= tricky-nnn native-namestring)
               (error "2: wanted ~S, got ~S" native-namestring tricky-nnn))))))

;;; USER-HOMEDIR-PATHNAME and the extension SBCL-HOMEDIR-PATHNAME both
;;; used to call PARSE-NATIVE-NAMESTRING without supplying a HOST
;;; argument, and so would lose when *DEFAULT-PATHNAME-DEFAULTS* was a
;;; logical pathname.
(with-test (:name (user-homedir-pathname :robustness))
  (let ((*default-pathname-defaults* (pathname "SYS:")))
    (assert (not (typep (user-homedir-pathname)
                        'logical-pathname)))))

(with-test (:name (sb-int:sbcl-homedir-pathname :robustness))
  (let ((*default-pathname-defaults* (pathname "SYS:")))
    (assert (not (typep (sb-int:sbcl-homedir-pathname)
                        'logical-pathname)))))

(with-test (:name (file-author stringp))
  #-win32
  (assert (stringp (file-author (user-homedir-pathname))))
  #+win32
  (assert (not (file-author (user-homedir-pathname)))))
(with-test (:name (file-write-date integerp))
  (assert (integerp (file-write-date (user-homedir-pathname)))))

;;; Generated with
;;; (loop for exist in '(nil t)
;;;       append
;;;       (loop for (if-exists if-does-not-exist) in '((nil :error)
;;;                                                    (:error nil)
;;;                                                    (nil nil)
;;;                                                    (:error :error))
;;;             collect (list 'do-open exist if-exists if-does-not-exist)))
(with-test (:name (open :never-openning))
  (flet ((do-open (existing if-exists if-does-not-exist
                   &optional (direction :output))
           (open (if existing
                     #.(or *compile-file-truename* *load-truename*)
                     "a-really-non-existing-file")
                 :direction direction
                 :if-exists if-exists :if-does-not-exist if-does-not-exist)))
    (assert-error (do-open nil nil :error) file-error)
    (assert (not (do-open nil :error nil)))
    (assert (not (do-open t nil :error)))
    (assert-error (do-open t :error nil) file-error)
    (assert (not (do-open nil nil nil)))
    (assert-error (do-open nil :error :error) file-error)
    (assert (not (do-open t nil nil)))
    (assert-error (do-open t :error :error))

    (assert-error (do-open nil nil :error :io) file-error)
    (assert (not (do-open nil :error nil :io)))
    (assert (not (do-open t nil :error :io)))
    (assert-error (do-open t :error nil :io) file-error)
    (assert (not (do-open nil nil nil :io)))
    (assert-error (do-open nil :error :error :io) file-error)
    (assert (not (do-open t nil nil :io)))
    (assert-error (do-open t :error :error :io) file-error)))

(with-test (:name (open :new-version))
  (multiple-value-bind (value error)
      (ignore-errors (open #.(or *compile-file-truename* *load-truename*)
                           :direction :output
                           :if-exists :new-version))
    (assert (not value))
    (assert error)
    (let ((control (simple-condition-format-control error)))
      (assert (search "OPEN :IF-EXISTS :NEW-VERSION is not supported" control))
      (assert (search "when a new version must be created." control)))))

(with-test (:name (open :if-does-not-exist restart))
  (flet ((do-open (restart)
           (let ((filename "does-not-exist"))
             (unwind-protect
                  (handler-bind
                      ((file-does-not-exist
                         (lambda (condition)
                           (let ((restart (find-restart restart condition)))
                             (invoke-restart restart)))))
                    (close (open filename :direction :output)))
               (assert (probe-file filename))
               (delete-file filename)))))
    (do-open 'sb-impl::create)))

(with-test (:name (open :if-exists restart))
  (labels ((read-file (filename)
             (with-open-file (stream filename)
               (let ((result (make-string (file-length stream))))
                 (read-sequence result stream)
                 result)))
           (do-open (restart expected-content)
             (let ((filename "exists"))
               (with-open-file (stream filename :direction :output
                                                :if-does-not-exist :create)
                 (write-string "foo" stream))
               (unwind-protect
                    (progn
                      (handler-bind
                          ((file-exists
                             (lambda (condition)
                               (let ((restart (find-restart restart condition)))
                                 (invoke-restart restart)))))
                        (let ((stream (open filename :direction :output)))
                          (write-string "bar" stream)
                          (close stream)))
                      (assert (equal expected-content (read-file filename))))
                 (delete-file filename)
                 (ignore-errors
                  (delete-file (concatenate 'string filename ".bak")))))))
    (do-open 'sb-impl::supersede "bar")
    (do-open 'sb-impl::overwrite "bar")
    (do-open 'sb-impl::rename "bar")
    (do-open 'append "foobar")))

(with-test (:name (parse-native-namestring :canon) :skipped-on (not :unix))
  (let ((pathname (parse-native-namestring "foo/bar//baz")))
    (assert (string= (car (last (pathname-directory pathname))) "bar"))))


;;;; DELETE-DIRECTORY

(with-test (:name (delete-directory :as-file :complicated-name-or-type :bug-1740624))
  ;; This test creates directories whose names are in some way
  ;; complicated to express as the filename part of a pathname, for
  ;; example by including characters that need escaping in namestrings
  ;; or looking like a :type component without a :name
  ;; component. DELETE-DIRECTORY is applied to pathnames with filename
  ;; parts to delete these directories. The intention is testing the
  ;; translation from filename part to directory component employed by
  ;; DELETE-DIRECTORY.
  (labels ((prepare (string)
             #-win32 (substitute #\\ #\E string)
             #+win32 (substitute #\^ #\E string))
           (make-type (type)
             (make-pathname :type (prepare type)))
           (make-unspecific (namep typep)
             (apply #'make-pathname
                    :directory '(:relative "foo")
                    (append (when namep '(:name :unspecific))
                            (when typep '(:type :unspecific)))))
           (test (as-file as-directory)
             (let* ((test-directory (concatenate
                                     'string
                                     (sb-ext:posix-getenv "TEST_DIRECTORY") "/"))
                    (delete-directory (merge-pathnames
                                       (typecase as-file
                                         (string (prepare as-file))
                                         (pathname as-file))
                                       test-directory)))
               (ensure-directories-exist (merge-pathnames
                                          (prepare as-directory)
                                          test-directory))
               (unwind-protect
                    (progn
                      (delete-directory delete-directory)
                      (assert (not (probe-file (prepare as-directory)))))
                 (delete-directory test-directory :recursive t)))))
    ;; Name component present
    #-win32 (test "aE?b"                    "aE?b/")
    #-win32 (test "aE*b"                    "aE*b/")
            (test "aE[cd]b"                 "aE[cd]b/")
            (test "aEEb"                    "aEEb/")
    ;; Type component
    #-win32 (test (make-type "a?b")         ".aE?b/")
    #-win32 (test (make-type "a*b")         ".aE*b/")
            (test (make-type "a[cd]b")      ".aE[cd]b/")
            (test (make-type "aEb")         ".aEEb/")
    ;; Name and type components present
    #-win32 (test "foo.aE?b"                "foo.aE?b/")
    #-win32 (test "foo.aE*b"                "foo.aE*b/")
            (test "foo.aE[cd]b"             "foo.aE[cd]b/")
            (test "foo.aEEb"                "foo.aEEb/")
    ;; Name and/or type :unspecific
            (test (make-unspecific nil nil) "foo/")
            (test (make-unspecific nil t)   "foo/")
            (test (make-unspecific t   nil) "foo/")
            (test (make-unspecific t   t)   "foo/")))
