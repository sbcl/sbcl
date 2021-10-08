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

(enable-test-parallelism)

(defvar *format-mode*)

(defun format* (format-control &rest arguments)
  (ecase *format-mode*
    (:interpret
     (eval `(format nil ,format-control ,@arguments)))
    (:compile
     (let ((names (sb-int:make-gensym-list (length arguments))))
       (funcall (checked-compile
                 `(lambda ,names (format nil ,format-control ,@names)))
                arguments)))))

(defmacro with-compiled-and-interpreted-format (() &body body)
  `(flet ((run-body (mode)
            (let ((*format-mode* mode))
              (handler-case
                  (progn ,@body)
                (error (condition)
                  (error "~@<Error in ~A FORMAT: ~A~@:>"
                         mode condition))))))
     (run-body :interpret)
     (run-body :compile)))

(defun format-error-format-control-string-p (condition)
  (and (typep condition 'sb-format:format-error)
       (sb-format::format-error-control-string condition)))

(deftype format-error-with-control-string ()
  `(and sb-format:format-error
        (satisfies format-error-format-control-string-p)))

(with-test (:name :combine-directives)
  ;; The scratch buffer for rematerializing a control string after extracting
  ;; user-fun directives (~//) needs to account for the expansion of the input
  ;; if a directive is converted immediately to a run of literal characters.
  (sb-format::extract-user-fun-directives "Oh hello~10%~/f/"))

(with-test (:name (:[-directive :non-integer-argument))
  (with-compiled-and-interpreted-format ()
    (assert-error (format* "~[~]" 1d0) format-error-with-control-string)))

(with-test (:name (:P-directive :no-previous-argument))
  (with-compiled-and-interpreted-format ()
    (assert-error (format* "~@<~:P~@:>" '()) format-error-with-control-string)))

(with-test (:name (:*-directive :out-of-bounds))
  (with-compiled-and-interpreted-format ()
    (assert-error (format* "~2@*" '()) format-error-with-control-string)
    (assert-error (format* "~1:*" '()) format-error-with-control-string)))

(with-test (:name :encapsulated-~/-formatter)
  (let ((s (make-string-output-stream)))
    (declare (notinline format))
    (sb-int:encapsulate 'sb-ext:print-symbol-with-prefix 'test
                        (lambda (f stream obj &rest args)
                          (write-string "{{" stream)
                          (apply f stream obj args)
                          (write-string "}}" stream)))
    (format s "~/sb-ext:print-symbol-with-prefix/" 'cl-user::test)
    (sb-int:unencapsulate 'sb-ext:print-symbol-with-prefix 'test)
    (assert (string= "{{COMMON-LISP-USER::TEST}}" (get-output-stream-string s)))))

(with-test (:name :non-simple-string)
  (let ((control (make-array 2 :element-type 'base-char
                               :initial-element #\A
                               :fill-pointer 1)))
    (checked-compile-and-assert
        ()
        `(lambda () (with-output-to-string (stream)
                      (funcall (formatter ,control) stream)))
      (() "A" :test #'equal))
    (checked-compile-and-assert
        ()
        `(lambda () (format nil ,control))
      (() "A" :test #'equal))
    (checked-compile-and-assert
        ()
        `(lambda () (cerror ,control ,control))
      (() (condition 'simple-error)))
    (checked-compile-and-assert
        ()
        `(lambda () (error ,control))
      (() (condition 'simple-error)))))

(with-test (:name :tokenize-curly-brace-nonliteral-empty)
  (flet ((try (string)
           (let ((tokens (sb-format::tokenize-control-string string)))
             (assert (and (= (length tokens) 3)
                          (string= (second tokens) ""))))))
    ;; Each of these curly-brace-wrapped expressions needs to preserve
    ;; the insides as an empty string so that it does not degenerate
    ;; to the control string "~{~}".
    (try "~{~
~}")
    (try "~{~0|~}")
    (try "~{~0~~}")
    (try "~{~0%~}")
    ;; these are also each three parsed tokens:
    ;; 0 newlines plus an ignored newline
    (try "~{~0%~
~}")
    ;; 0 newlines, an ignored newline, and 0 tildes
    (try "~{~0%~
~0|~0~~}")))

(with-test (:name :no-compiler-notes)
  ;; you should't see optimization notes from compiling format strings.
  ;; (the FORMATTER macro is a heavy user of PRINC)
  (checked-compile
   '(lambda (x) (declare (optimize speed)) (princ x))
   :allow-notes nil)
  (checked-compile
   '(lambda ()  (declare (optimize speed)) (formatter "~:*"))))

(defun format-to-string-stream (thing string-stream)
  (declare (notinline format))
  ;; Tokenizing this string will cons about 224 bytes
  (format string-stream "Test ~D" thing)
  ;; CLEAR-OUTPUT should work, but doesn't
  (file-position string-stream 0))

(with-test (:name :cached-tokenized-string
            :skipped-on :interpreter)
  (let ((stream (make-string-output-stream)))
    (format-to-string-stream 45678 stream)
    (ctu:assert-no-consing (format-to-string-stream 45678 stream))))

(with-test (:name :uncached-tokenized-string)
  (let ((control-string
         ;; super-smart compiler might see that the result of
         ;; this call is constantly "hello ~a"
         (locally (declare (notinline format)) (format nil "hello ~~a"))))
    (let ((s1 (format nil control-string '(1 2)))
          (s2
           (progn
             (setf (aref control-string 0) #\Y)
             (format nil control-string '(1 2)))))
      (assert (string= s1 "hello (1 2)"))
      (assert (string= s2 "Yello (1 2)")))))
