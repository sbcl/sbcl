;;;; This file is for testing external-format functionality for
;;;; little-endian UCS-4, using test machinery which does not have
;;;; side effects.  Note that the tests here reach into unexported
;;;; functionality, and should not be used as a guide for users.

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

#-sb-unicode (invoke-restart 'run-tests::skip-file)

(defvar *test-path* (scratch-file-name))

(macrolet ((input-test (inxf expected &environment env)
             `(progn
                (with-test (:name (,(macroexpand 'name env) :file ,inxf))
                  (with-open-file (s *test-path* :external-format ',inxf)
                    (handler-bind ((sb-int:character-decoding-error
                                    (lambda (c) (use-value "" c))))
                      (let* ((string (make-string 20))
                             (count (read-sequence string s)))
                        (assert (equal (map 'list 'identity (subseq string 0 count)) ,expected))))))
                (with-test (:name (,(macroexpand 'name env) :octets ,inxf))
                  (handler-bind ((sb-int:character-decoding-error
                                  (lambda (c) (use-value "" c))))
                    (let ((octets (coerce bytes '(simple-array (unsigned-byte 8) 1))))
                      (assert (equal (sb-ext:octets-to-string octets :external-format ',inxf)
                                     (coerce ,expected 'string))))))))
           (with-input-bytes ((id bytes) &body body)
             `(let ((bytes ,bytes))
                (with-open-file (s *test-path* :element-type '(unsigned-byte 8)
                                   :direction :output :if-exists :supersede)
                  (dolist (byte bytes)
                    (write-byte byte s)))
                (symbol-macrolet ((name ,id))
                  (macrolet ((test (inxf expected)
                               `(input-test ,inxf ,expected)))
                    ,@body)))))
  (with-input-bytes ((:input :invalid) (list #x35 #x00 #x00 #x00
                                             #xff #xff #xff #xff
                                             #x37 #x00 #x00 #x00))
    (test :ucs-4le '(#\5 #\7))
    (test (:ucs-4le :replacement #\?) '(#\5 #\? #\7)))
  (with-input-bytes ((:input :multiple-invalid) (list #x35 #x00 #x00 #x00
                                                      #xff #xff #xff #xff
                                                      #xff #xff #xff #xff
                                                      #x37 #x00 #x00 #x00))
    (test :ucs-4le '(#\5 #\7))
    (test (:ucs-4le :replacement #\?) '(#\5 #\? #\? #\7)))
  (with-input-bytes ((:input :invalid-units1) (list #x35 #x00 #x00 #x00
                                                    #x00))
    (test :ucs-4le '(#\5))
    (test (:ucs-4le :replacement #\?) '(#\5 #\?)))
  (with-input-bytes ((:input :invalid-units2) (list #x35 #x00 #x00 #x00
                                                    #x00 #x00))
    (test :ucs-4le '(#\5))
    (test (:ucs-4le :replacement #\?) '(#\5 #\?)))
  (with-input-bytes ((:input :invalid-units3) (list #x35 #x00 #x00 #x00
                                                    #x00 #x00 #x00))
    (test :ucs-4le '(#\5))
    (test (:ucs-4le :replacement #\?) '(#\5 #\?)))
  (with-input-bytes ((:input :invalid-then-invalid-units1) (list #xff #xff #xff #xff
                                                                 #x00))
    (test :ucs-4le '())
    (test (:ucs-4le :replacement #\?) '(#\? #\?))))

(delete-file *test-path*)
