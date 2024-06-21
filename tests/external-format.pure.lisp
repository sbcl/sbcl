;;;; This file is for testing external-format functionality, using
;;;; test machinery which might have side effects (e.g.  executing
;;;; DEFUN, writing files).  Note that the tests here reach into
;;;; unexported functionality, and should not be used as a guide for
;;;; users.

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

;;; I have absolutely no idea what's going on with ppc64 little-endian,
;;; but this file's realtime is just ridiculous on the test machine
;;; and it totally dominates the time taken in parallel-exec:
;;;    big-endian:    real        0m3.265s
;;;    little-endian: real        2m53.039s
;;; Whereas, with this one file eliminated on ppc64le, the total wallclock
;;; time for parallel-exec (with 12 workers) is approximately 94 seconds
;;; on either machine. What is so horrible about our external-format codecs
;;; that it antagonizes the little-endian CPU so badly?
#+(and ppc64 little-endian) (invoke-restart 'run-tests::skip-file)

(defmacro do-external-formats ((xf) &body body)
  (let ((nxf (gensym))
        (xfs (gensym))
        (xfsym (gensym)))
    `(sb-int:dovector (,nxf sb-impl::*external-formats*)
       (when ,nxf
         (let* ((,xfs (sb-int:ensure-list ,nxf))
                (,xfsym (first (sb-impl::ef-names (car ,xfs)))))
           (dolist (,xf (append (list ,xfsym)
                                (when (find :cr (cdr ,xfs) :key #'caar)
                                  (list (list ,xfsym :newline :cr)))
                                (when (find :crlf (cdr ,xfs) :key #'caar)
                                  (list (list ,xfsym :newline :crlf)))))
             ,@body))))))

(defmacro with-ef-test (options &rest rest)
  (let* ((test-name (getf options :name))
         (ef-name (car (last test-name))))
    (if (sb-impl::get-external-format ef-name)
        `(with-test ,options ,@rest)
        `(format t "::: INFO: no external format named ~S~%" ',ef-name))))

(defvar *test-path* (scratch-file-name))

(flet ((s2o-file (string &key external-format)
         (with-open-file (s *test-path* :direction :output :if-exists :supersede
                            :external-format external-format)
           (write-sequence string s))
         (with-open-file (s *test-path* :direction :input :element-type '(unsigned-byte 8))
           (let* ((vector (make-array 10 :initial-element 0))
                  (count (read-sequence vector s)))
             (subseq vector 0 count))))
       (o2s-file (vector &key external-format)
         (with-open-file (s *test-path* :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
           (write-sequence vector s))
         (with-open-file (s *test-path* :direction :input :external-format external-format)
           (let* ((string (make-string 10))
                  (count (read-sequence string s)))
             (subseq string 0 count)))))
(with-test (:name (:external-format-options :ascii :file))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (s2o-file string :external-format :ascii) #(63)))
    (assert (equalp (o2s-file octets :external-format :ascii) #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:ascii :replacement "?"))
                    #(63)))
    (assert (equalp (o2s-file octets :external-format '(:ascii :replacement "?"))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:ascii :newline :lf)) #(63)))
    (assert (equalp (o2s-file octets :external-format '(:ascii :newline :lf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:ascii :newline :crlf)) #(63)))
    (assert (equalp (o2s-file octets :external-format '(:ascii :newline :crlf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert-error (s2o-file string :external-format '(:ascii :replacment "?")))
    (assert-error (o2s-file octets :external-format '(:ascii :replacment "?")))
    (assert-error (s2o-file string :external-format '(:ascii :replacement #p"~")))
    (assert-error (o2s-file octets :external-format '(:ascii :replacement #p"~")))
    (assert-error (s2o-file string :external-format '(:ascii :nelwine :crlf)))
    (assert-error (o2s-file octets :external-format '(:ascii :nelwine :crlf)))
    (assert-error (s2o-file string :external-format '(:ascii :newline :clrf)))
    (assert-error (o2s-file octets :external-format '(:ascii :newline :clrf)))))
(with-test (:name (:external-format-options :ascii :octets))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (string-to-octets string :external-format :ascii) #(63)))
    (assert (equalp (octets-to-string octets :external-format :ascii) #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:ascii :replacement "?"))
                    #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:ascii :replacement "?"))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:ascii :newline :lf)) #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:ascii :newline :lf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:ascii :newline :crlf)) #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:ascii :newline :crlf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert-error (string-to-octets string :external-format '(:ascii :replacment "?")))
    (assert-error (octets-to-string octets :external-format '(:ascii :replacment "?")))
    (assert-error (string-to-octets string :external-format '(:ascii :replacement #p"~")))
    (assert-error (octets-to-string octets :external-format '(:ascii :replacement #p"~")))
    (assert-error (string-to-octets string :external-format '(:ascii :nelwine :crlf)))
    (assert-error (octets-to-string octets :external-format '(:ascii :nelwine :crlf)))
    (assert-error (string-to-octets string :external-format '(:ascii :newline :clrf)))
    (assert-error (octets-to-string octets :external-format '(:ascii :newline :clrf)))))

(with-test (:name (:external-format-options :latin-1 :file))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (s2o-file string :external-format :latin-1) #(63)))
    (assert (equalp (o2s-file octets :external-format :latin-1) #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:latin-1 :replacement "?"))
                    #(63)))
    (assert (equalp (o2s-file octets :external-format '(:latin-1 :replacement "?"))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:latin-1 :newline :lf)) #(63)))
    (assert (equalp (o2s-file octets :external-format '(:latin-1 :newline :lf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:latin-1 :newline :crlf)) #(63)))
    (assert (equalp (o2s-file octets :external-format '(:latin-1 :newline :crlf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert-error (s2o-file string :external-format '(:latin-1 :replacment "?")))
    (assert-error (o2s-file octets :external-format '(:latin-1 :replacment "?")))
    (assert-error (s2o-file string :external-format '(:latin-1 :replacement #p"~")))
    (assert-error (o2s-file octets :external-format '(:latin-1 :replacement #p"~")))
    (assert-error (s2o-file string :external-format '(:latin-1 :nelwine :crlf)))
    (assert-error (o2s-file octets :external-format '(:latin-1 :nelwine :crlf)))
    (assert-error (s2o-file string :external-format '(:latin-1 :newline :clrf)))
    (assert-error (o2s-file octets :external-format '(:latin-1 :newline :clrf)))))
(with-test (:name (:external-format-options :latin-1 :octets))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (string-to-octets string :external-format :latin-1) #(63)))
    (assert (equalp (octets-to-string octets :external-format :latin-1) #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:latin-1 :replacement "?"))
                    #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:latin-1 :replacement "?"))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:latin-1 :newline :lf)) #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:latin-1 :newline :lf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:latin-1 :newline :crlf)) #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:latin-1 :newline :crlf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert-error (string-to-octets string :external-format '(:latin-1 :replacment "?")))
    (assert-error (octets-to-string octets :external-format '(:latin-1 :replacment "?")))
    (assert-error (string-to-octets string :external-format '(:latin-1 :replacement #p"~")))
    (assert-error (octets-to-string octets :external-format '(:latin-1 :replacement #p"~")))
    (assert-error (string-to-octets string :external-format '(:latin-1 :nelwine :crlf)))
    (assert-error (octets-to-string octets :external-format '(:latin-1 :nelwine :crlf)))
    (assert-error (string-to-octets string :external-format '(:latin-1 :newline :clrf)))
    (assert-error (octets-to-string octets :external-format '(:latin-1 :newline :clrf)))))

(with-test (:name (:external-format-options :utf-8 :file))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (s2o-file string :external-format :utf-8) #(63)))
    (assert (equalp (o2s-file octets :external-format :utf-8) #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:utf-8 :replacement "?"))
                    #(63)))
    (assert (equalp (o2s-file octets :external-format '(:utf-8 :replacement "?"))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:utf-8 :newline :lf)) #(63)))
    (assert (equalp (o2s-file octets :external-format '(:utf-8 :newline :lf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:utf-8 :newline :crlf)) #(63)))
    (assert (equalp (o2s-file octets :external-format '(:utf-8 :newline :crlf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert-error (s2o-file string :external-format '(:utf-8 :replacment "?")))
    (assert-error (o2s-file octets :external-format '(:utf-8 :replacment "?")))
    (assert-error (s2o-file string :external-format '(:utf-8 :replacement #p"~")))
    (assert-error (o2s-file octets :external-format '(:utf-8 :replacement #p"~")))
    (assert-error (s2o-file string :external-format '(:utf-8 :nelwine :crlf)))
    (assert-error (o2s-file octets :external-format '(:utf-8 :nelwine :crlf)))
    (assert-error (s2o-file string :external-format '(:utf-8 :newline :clrf)))
    (assert-error (o2s-file octets :external-format '(:utf-8 :newline :clrf)))))
(with-test (:name (:external-format-options :utf-8 :octets))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (string-to-octets string :external-format :utf-8) #(63)))
    (assert (equalp (octets-to-string octets :external-format :utf-8) #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:utf-8 :replacement "?"))
                    #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:utf-8 :replacement "?"))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:utf-8 :newline :lf)) #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:utf-8 :newline :lf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:utf-8 :newline :crlf)) #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:utf-8 :newline :crlf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert-error (string-to-octets string :external-format '(:utf-8 :replacment "?")))
    (assert-error (octets-to-string octets :external-format '(:utf-8 :replacment "?")))
    (assert-error (string-to-octets string :external-format '(:utf-8 :replacement #p"~")))
    (assert-error (octets-to-string octets :external-format '(:utf-8 :replacement #p"~")))
    (assert-error (string-to-octets string :external-format '(:utf-8 :nelwine :crlf)))
    (assert-error (octets-to-string octets :external-format '(:utf-8 :nelwine :crlf)))
    (assert-error (string-to-octets string :external-format '(:utf-8 :newline :clrf)))
    (assert-error (octets-to-string octets :external-format '(:utf-8 :newline :clrf)))))

(with-test (:name (:external-format-options :gbk :file)
                  :skipped-on (or (not :sb-unicode) :unicode-lite))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (s2o-file string :external-format :gbk) #(63)))
    (assert (equalp (o2s-file octets :external-format :gbk) #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:gbk :replacement "?"))
                    #(63)))
    (assert (equalp (o2s-file octets :external-format '(:gbk :replacement "?"))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:gbk :newline :lf)) #(63)))
    (assert (equalp (o2s-file octets :external-format '(:gbk :newline :lf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert-error (s2o-file string :external-format '(:gbk :newline :crlf)))
    (assert-error (o2s-file octets :external-format '(:gbk :newline :crlf)))
    (assert-error (s2o-file string :external-format '(:gbk :replacment "?")))
    (assert-error (o2s-file octets :external-format '(:gbk :replacment "?")))
    (assert-error (s2o-file string :external-format '(:gbk :replacement #p"~")))
    (assert-error (o2s-file octets :external-format '(:gbk :replacement #p"~")))
    (assert-error (s2o-file string :external-format '(:gbk :nelwine :crlf)))
    (assert-error (o2s-file octets :external-format '(:gbk :nelwine :crlf)))
    (assert-error (s2o-file string :external-format '(:gbk :newline :clrf)))
    (assert-error (o2s-file octets :external-format '(:gbk :newline :clrf)))))
(with-test (:name (:external-format-options :gbk :octets)
                  :skipped-on (or (not :sb-unicode) :unicode-lite))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (string-to-octets string :external-format :gbk) #(63)))
    (assert (equalp (octets-to-string octets :external-format :gbk) #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:gbk :replacement "?"))
                    #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:gbk :replacement "?"))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:gbk :newline :lf)) #(63)))
    (assert (equalp (octets-to-string octets :external-format '(:gbk :newline :lf))
                    #(#\Nul #\Nul #\Nul #\Nul)))
    (assert-error (string-to-octets string :external-format '(:gbk :newline :crlf)))
    (assert-error (octets-to-string octets :external-format '(:gbk :newline :crlf)))
    (assert-error (string-to-octets string :external-format '(:gbk :replacment "?")))
    (assert-error (octets-to-string octets :external-format '(:gbk :replacment "?")))
    (assert-error (string-to-octets string :external-format '(:gbk :replacement #p"~")))
    (assert-error (octets-to-string octets :external-format '(:gbk :replacement #p"~")))
    (assert-error (string-to-octets string :external-format '(:gbk :nelwine :crlf)))
    (assert-error (octets-to-string octets :external-format '(:gbk :nelwine :crlf)))
    (assert-error (string-to-octets string :external-format '(:gbk :newline :clrf)))
    (assert-error (octets-to-string octets :external-format '(:gbk :newline :clrf)))))

(with-test (:name (:external-format-options :ucs-2le :file)
                  :skipped-on (not :sb-unicode))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (s2o-file string :external-format :ucs-2le) #(63 0)))
    (assert (equalp (o2s-file octets :external-format :ucs-2le) #(#\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:ucs-2le :replacement "?"))
                    #(63 0)))
    (assert (equalp (o2s-file octets :external-format '(:ucs-2le :replacement "?"))
                    #(#\Nul #\Nul)))
    (assert (equalp (s2o-file string :external-format '(:ucs-2le :newline :lf)) #(63 0)))
    (assert (equalp (o2s-file octets :external-format '(:ucs-2le :newline :lf))
                    #(#\Nul #\Nul)))
    (assert-error (s2o-file string :external-format '(:ucs-2le :newline :crlf)))
    (assert-error (o2s-file octets :external-format '(:ucs-2le :newline :crlf)))
    (assert-error (s2o-file string :external-format '(:ucs-2le :replacment "?")))
    (assert-error (o2s-file octets :external-format '(:ucs-2le :replacment "?")))
    (assert-error (s2o-file string :external-format '(:ucs-2le :replacement #p"~")))
    (assert-error (o2s-file octets :external-format '(:ucs-2le :replacement #p"~")))
    (assert-error (s2o-file string :external-format '(:ucs-2le :nelwine :crlf)))
    (assert-error (o2s-file octets :external-format '(:ucs-2le :nelwine :crlf)))
    (assert-error (s2o-file string :external-format '(:ucs-2le :newline :clrf)))
    (assert-error (o2s-file octets :external-format '(:ucs-2le :newline :clrf)))))
(with-test (:name (:external-format-options :ucs-2le :octets)
                  :skipped-on (not :sb-unicode))
  (let ((string "?")
        (octets (coerce '(0 0 0 0) '(vector (unsigned-byte 8)))))
    (assert (equalp (string-to-octets string :external-format :ucs-2le) #(63 0)))
    (assert (equalp (octets-to-string octets :external-format :ucs-2le) #(#\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:ucs-2le :replacement "?"))
                    #(63 0)))
    (assert (equalp (octets-to-string octets :external-format '(:ucs-2le :replacement "?"))
                    #(#\Nul #\Nul)))
    (assert (equalp (string-to-octets string :external-format '(:ucs-2le :newline :lf)) #(63 0)))
    (assert (equalp (octets-to-string octets :external-format '(:ucs-2le :newline :lf))
                    #(#\Nul #\Nul)))
    (assert-error (string-to-octets string :external-format '(:ucs-2le :newline :crlf)))
    (assert-error (octets-to-string octets :external-format '(:ucs-2le :newline :crlf)))
    (assert-error (string-to-octets string :external-format '(:ucs-2le :replacment "?")))
    (assert-error (octets-to-string octets :external-format '(:ucs-2le :replacment "?")))
    (assert-error (string-to-octets string :external-format '(:ucs-2le :replacement #p"~")))
    (assert-error (octets-to-string octets :external-format '(:ucs-2le :replacement #p"~")))
    (assert-error (string-to-octets string :external-format '(:ucs-2le :nelwine :crlf)))
    (assert-error (octets-to-string octets :external-format '(:ucs-2le :nelwine :crlf)))
    (assert-error (string-to-octets string :external-format '(:ucs-2le :newline :clrf)))
    (assert-error (octets-to-string octets :external-format '(:ucs-2le :newline :clrf)))))
) ; FLET

(with-test (:name :end-of-file)
  (do-external-formats (xf)
    (with-open-file (s #-win32 "/dev/null" #+win32 "nul" :direction :input :external-format xf)
      (assert (eq (read-char s nil s) s)))))

;; Output routines must return the written element
(with-test (:name :output-routine-retval :skipped-on :win32)
  (dolist (x sb-impl::*output-routines*)
    (with-open-file (f "/dev/null" :direction :output :if-exists :overwrite)
      (let ((arg (if (eq (car x) 'character) #\z 99))
            (fun (symbol-function (third x))))
        (assert (eql arg (funcall fun f arg)))))))

;;; Test standard character read-write equivalency over all external formats.
(macrolet
    ((frob ()
       (let ((tests nil))
         (do-external-formats (xf)
           (pushnew `(with-test (:name (:standard-character :read-write-equivalency ,xf))
                       (let ((standard-characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~"))
                         (with-open-file (s *test-path* :direction :output
                                            :if-exists :supersede :external-format ',xf)
                           (loop for character across standard-characters
                                 do (write-char character s)))
                         (with-open-file (s *test-path* :direction :input
                                            :external-format ',xf)
                           (loop for character across standard-characters
                                 do (let ((got (read-char s)))
                                      (unless (eql character got)
                                        (error "wanted ~S, got ~S" character got)))))))
                    tests :key #'cadr :test #'equal))
         `(progn ,@tests))))
  (frob))

#-sb-unicode
(progn
  (delete-file *test-path*)
  (test-util:report-test-status)
  (invoke-restart 'run-tests::skip-file))

;;; Test UTF-8 writing and reading of 1, 2, 3 and 4 octet characters with
;;; all possible offsets. Tests for buffer edge bugs. fd-stream buffers are
;;; 8192 wide.
(dotimes (width-1 4)
  (let ((character (code-char (elt '(1 #x81 #x801 #x10001) width-1))))
    (dotimes (offset (+ width-1 1))
      (with-open-file (s *test-path* :direction :output
                       :if-exists :supersede :external-format :utf-8)
        (dotimes (n offset)
          (write-char #\a s))
        (dotimes (n (+ 4 sb-impl::+bytes-per-buffer+))
          (write-char character s)))
      (with-open-file (s *test-path* :direction :input
                       :external-format :utf-8)
        (dotimes (n offset)
          (assert (eql (read-char s) #\a)))
        (dotimes (n (+ 4 sb-impl::+bytes-per-buffer+))
          (let ((got (read-char s)))
            (unless (eql got character)
              (error "wanted ~S, got ~S (~S)" character got n))))
        (assert (eql (read-char s nil s) s))))))

;;; Test character decode restarts.
(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :element-type '(unsigned-byte 8))
  (write-byte 65 s)
  (write-byte 66 s)
  (write-byte #xe0 s)
  (write-byte 67 s))
(with-open-file (s *test-path* :direction :input
                 :external-format :utf-8)
  (let ((count 0))
    (handler-bind
        ((sb-int:character-decoding-error #'(lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (when (> (incf count) 1)
                                                (error "too many errors"))
                                              (invoke-restart
                                               'sb-int:attempt-resync))))
      (assert (equal (read-line s nil s) "ABC"))
      (assert (equal (read-line s nil s) s)))))
(with-open-file (s *test-path* :direction :input
                 :external-format :utf-8)
  (let ((count 0))
    (handler-bind
        ((sb-int:character-decoding-error #'(lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (when (> (incf count) 1)
                                                (error "too many errors"))
                                              (invoke-restart
                                               'sb-int:force-end-of-file))))
      (assert (equal (read-line s nil s) "AB"))
      (setf count 0)
      (assert (equal (read-line s nil s) s)))))

;;; And again with more data to account for buffering (this was briefly)
;;; broken in early 0.9.6.
(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :element-type '(unsigned-byte 8))
  (let ((a (make-array 50
                       :element-type '(unsigned-byte 64)
                       :initial-contents (map 'list #'char-code
                                              "1234567890123456789012345678901234567890123456789."))))
    (setf (aref a 49) (char-code #\Newline))
    (dotimes (i 40)
      (write-sequence a s))
    (write-byte #xe0 s)
    (dotimes (i 40)
      (write-sequence a s))))
(with-test (:name (:character-decode-large :attempt-resync))
  (with-open-file (s *test-path* :direction :input
                     :external-format :utf-8)
    (let ((count 0))
      (handler-bind
          ((sb-int:character-decoding-error (lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (when (> (incf count) 1)
                                                (error "too many errors"))
                                              (invoke-restart
                                               'sb-int:attempt-resync)))
           ;; The failure mode is an infinite loop, add a timeout to
           ;; detetct it.
           (sb-ext:timeout (lambda (condition)
                             (declare (ignore condition))
                             (error "Timeout"))))
        (sb-ext:with-timeout 5
          (dotimes (i 80)
            (assert (equal (read-line s nil s)
                           "1234567890123456789012345678901234567890123456789"))))))))

(with-test (:name (:character-decode-large :force-end-of-file))
  (with-open-file (s *test-path* :direction :input
                     :external-format :utf-8)
    (let ((count 0))
      (handler-bind
          ((sb-int:character-decoding-error (lambda (decoding-error)
                                              (declare (ignore decoding-error))
                                              (when (> (incf count) 1)
                                                (error "too many errors"))
                                              (invoke-restart
                                               'sb-int:force-end-of-file)))
           ;; The failure mode is an infinite loop, add a timeout to detetct it.
           (sb-ext:timeout (lambda (condition)
                             (declare (ignore condition))
                             (error "Timeout"))))
        (sb-ext:with-timeout 5
          (dotimes (i 40)
            (assert (equal (read-line s nil s)
                           "1234567890123456789012345678901234567890123456789")))
          (setf count 0)
          (assert (equal (read-line s nil s) s)))))))

;;; Test character encode restarts.
(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :external-format :latin-1)
  (handler-bind
      ((sb-int:character-encoding-error #'(lambda (encoding-error)
                                            (declare (ignore encoding-error))
                                            (invoke-restart
                                             'sb-impl::output-nothing))))
    (write-char #\A s)
    (write-char #\B s)
    (write-char (code-char 322) s)
    (write-char #\C s)))
(with-open-file (s *test-path* :direction :input
                 :external-format :latin-1)
  (assert (equal (read-line s nil s) "ABC"))
  (assert (equal (read-line s nil s) s)))

(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :external-format :latin-1)
  (handler-bind
      ((sb-int:character-encoding-error #'(lambda (encoding-error)
                                            (declare (ignore encoding-error))
                                            (invoke-restart
                                             'sb-impl::output-nothing))))
    (let ((string (make-array 4 :element-type 'character
                              :initial-contents `(#\A #\B ,(code-char 322)
                                                      #\C))))
      (write-string string s))))
(with-open-file (s *test-path* :direction :input
                 :external-format :latin-1)
  (assert (equal (read-line s nil s) "ABC"))
  (assert (equal (read-line s nil s) s)))

;;; Test skipping character-decode-errors in comments.
(let* ((input (scratch-file-name "lisp"))
       (s (open input :direction :output
                      :if-exists :supersede :external-format :latin-1))
       (output))
  (unwind-protect
       (progn
         (write-string ";;; ABCD" s)
         (write-char (code-char 233) s)
         (terpri s)
         (close s)
         (let ((*error-output* (make-broadcast-stream)))
           (setq output
                 (compile-file input
                               :external-format :utf-8 :verbose nil))))
    (delete-file s)
    (let ((p (probe-file output)))
      (when p
        (delete-file p)))))


;;;; KOI8-R external format
#-unicode-lite
(progn
(with-open-file (s *test-path* :direction :output
                 :if-exists :supersede :external-format :koi8-r)
  (write-char (code-char #xB0) s)
  (assert (eq
           (handler-case
               (progn
                 (write-char (code-char #xBAAD) s)
                 :bad)
             (sb-int:character-encoding-error ()
               :good))
           :good)))
(with-open-file (s *test-path* :direction :input
                 :element-type '(unsigned-byte 8))
  (let ((byte (read-byte s)))
    (assert (= (eval byte) #x9C))))
(with-open-file (s *test-path* :direction :input
                 :external-format :koi8-r)
  (let ((char (read-char s)))
    (assert (= (char-code (eval char)) #xB0))))

(let* ((koi8-r-codes (coerce '(240 210 201 215 197 212 33) '(vector (unsigned-byte 8))))
       (uni-codes #(1055 1088 1080 1074 1077 1090 33))

       (string (octets-to-string koi8-r-codes :external-format :koi8-r))
       (uni-decoded (map 'vector #'char-code string)))
  (declare (ignore uni-decoded))
  (assert (equalp (map 'vector #'char-code (octets-to-string koi8-r-codes :external-format :koi8-r))
                  uni-codes))
  (assert (equalp (string-to-octets (map 'string #'code-char uni-codes) :external-format :koi8-r)
                  koi8-r-codes)))
)


;;; tests of FILE-STRING-LENGTH
(let ((standard-characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~"))
  (do-external-formats (xf)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format xf)
      (loop for x across standard-characters
            for position = (file-position s)
            for char-length = (file-string-length s x)
            do (write-char x s)
            do (assert (= (file-position s) (+ position char-length))))
      (let ((position (file-position s))
            (string-length (file-string-length s standard-characters)))
        (write-string standard-characters s)
        (assert (= (file-position s) (+ position string-length)))))))

(let ((char-codes '(0 1 255 256 511 512 1023 1024 2047 2048 4095 4096
                    8191 8192 16383 16384 32767 32768 65535 65536 131071
                    131072 262143 262144)))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede
                     :external-format :utf-8)
    (dolist (code char-codes)
      (let* ((char (code-char code))
             (position (file-position s))
             (char-length (file-string-length s char)))
        (write-char char s)
        (assert (= (file-position s) (+ position char-length)))))
    (let* ((string (map 'string #'code-char char-codes))
           (position (file-position s))
           (string-length (file-string-length s string)))
      (write-string string s)
      (assert (= (file-position s) (+ position string-length))))))


;;; See sbcl-devel "Subject: Bug in FILE-POSITION on UTF-8-encoded files"
;;; by Lutz Euler on 2006-03-05 for more details.
(with-test (:name (:file-position :utf-8))
  (let ((path *test-path*))
    (with-open-file (s path
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
      ;; Write #\*, encoded in UTF-8, to the file.
      (write-byte 42 s)
      ;; Append #\adiaeresis, encoded in UTF-8, to the file.
      (write-sequence '(195 164) s))
    (with-open-file (s path :external-format :utf-8)
      (read-char s)
      (let ((pos (file-position s))
            (char (read-char s)))
        #+nil
        (format t "read character with code ~a successfully from file position ~a~%"
                (char-code char) pos)
        (file-position s pos)
        #+nil
        (format t "set file position back to ~a, trying to read-char again~%" pos)
        (let ((new-char (read-char s)))
          (assert (char= char new-char)))))
    (values)))

;;; We used to call STREAM-EXTERNAL-FORMAT on the stream in the error
;;; when printing a coding error, but that didn't work if the stream
;;; was closed by the time the error was printed.  See sbcl-devel
;;; "Subject: Printing coding errors for closed streams" by Zach Beane
;;; on 2008-10-16 for more info.
(with-test (:name (:character-coding-error-stream-external-format))
  (flet ((first-file-character ()
           (with-open-file (stream *test-path* :external-format :utf-8)
             (read-char stream))))
    (with-open-file (stream *test-path*
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (write-byte 192 stream))
    (princ-to-string (nth-value 1 (ignore-errors (first-file-character))))))

;;; External format support in SB-ALIEN

(with-test (:name (:sb-alien :vanilla))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      c-string
    (str c-string))
  (assert (equal "foo" (strdup "foo"))))

(with-test (:name (:sb-alien :utf-8 :utf-8))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :utf-8)
    (str (c-string :external-format :utf-8)))
  (assert (equal "foo" (strdup "foo"))))

(with-test (:name (:sb-alien :latin-1 :utf-8))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :latin-1)
    (str (c-string :external-format :utf-8)))
  (assert (= (length (strdup (string (code-char 246))))
             2)))

(with-test (:name (:sb-alien :utf-8 :latin-1))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :utf-8)
    (str (c-string :external-format :latin-1)))
  (assert (equal (string (code-char 228))
                 (strdup (concatenate 'string
                                      (list (code-char 195))
                                      (list (code-char 164)))))))

(with-ef-test (:name (:sb-alien :ebcdic :ebcdic-us))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :ebcdic-us)
    (str (c-string :external-format :ebcdic-us)))
  (assert (equal "foo" (strdup "foo"))))

(with-ef-test (:name (:sb-alien :latin-1 :ebcdic-us))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :latin-1)
    (str (c-string :external-format :ebcdic-us)))
  (assert (not (equal "foo" (strdup "foo")))))

(with-ef-test (:name (:sb-alien :simple-base-string :ebcdic-us))
  (define-alien-routine (#-win32 "strdup" #+win32 "_strdup" strdup)
      (c-string :external-format :ebcdic-us
                :element-type base-char)
    (str (c-string :external-format :ebcdic-us)))
  (assert (typep (strdup "foo") 'simple-base-string)))

(with-test (:name (:input-replacement :at-end-of-file))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (handler-bind ((sb-int:character-decoding-error
                    (lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'sb-impl::input-replacement #\?))))
      (with-open-file (s *test-path* :external-format :utf-8)
        (cond
          ((char= (read-char s) #\?)
           (assert (or (= i (char-code #\?)) (> i 127))))
          (t (assert (and (not (= i (char-code #\?))) (< i 128)))))))))

(with-ef-test (:name (:unibyte-invalid-codepoints :cp857))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format :cp857)
      (handler-case (read-char s)
        (error () (assert (member i '(#xd5 #xe7 #xf2))))
        (:no-error (char) char (assert (not (member i '(#xd5 #xe7 #xf2)))))))))

(with-ef-test (:name (:unibyte-input-replacement :cp857))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:cp857 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert (member i `(,(char-code #\?) #xd5 #xe7 #xf2))))
          (t (assert (not (member i `(,(char-code #\?) #xd5 #xe7 #xf2))))))))))

(with-ef-test (:name (:unibyte-output-replacement :cp857))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:cp857 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:cp857))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i 128)
        (assert (= (char-code (char string i)) i)))
      (assert (= 38 (count #\? string :start 128))))))

(with-test (:name (:unibyte-input-replacement :ascii))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:ascii :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert (or (= i (char-code #\?)) (> i 127))))
          (t (assert (and (< i 128) (not (= i (char-code #\?)))))))))))

(with-test (:name (:unibyte-output-replacement :ascii))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:ascii :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:ascii))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i 128)
        (assert (= (char-code (char string i)) i)))
      (assert (= 128 (count #\? string :start 128))))))

(with-test (:name (:unibyte-input-replacement :latin-1))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-1 :replacement #\?))
      (let ((char (read-char s)))
        (assert (= (char-code char) i))))))

(with-test (:name (:unibyte-output-replacement :latin-1))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-1 :replacement #\?))
    (dotimes (i 257)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-1))
    (let ((string (make-string 257)))
      (read-sequence string s)
      (dotimes (i 256)
        (assert (= (char-code (char string i)) i)))
      (assert (char= #\? (char string 256))))))

;;; latin-2 tests
(with-ef-test (:name (:unibyte-input-replacement :latin-2))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-2 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((< i #xa1) (assert (= (char-code char) i)))
          ;; FIXME: more tests
          )))))

(with-ef-test (:name (:unibyte-output-replacement :latin-2))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-2 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-2))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 57 (count #\? string :start #xa1))))))

;;; latin-3 tests
(with-ef-test (:name (:unibyte-input-replacement :latin-3))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-3 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert #1=(or (= i (char-code #\?))
                          (member i '(#xa5 #xae #xbe #xc3 #xd0 #xe3 #xf0)))))
          (t (assert (not #1#))))))))

(with-ef-test (:name (:unibyte-output-replacement :latin-3))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-3 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-3))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 35 (count #\? string :start #xa1))))))

;;; latin-4 tests
(with-ef-test (:name (:unibyte-input-replacement :latin-4))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-4 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((< i #xa1) (assert (= (char-code char) i)))
          ;; FIXME: more tests
          )))))

(with-ef-test (:name (:unibyte-output-replacement :latin-4))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-4 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-4))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 50 (count #\? string :start #xa1))))))

;;; iso-8859-5 tests
(with-ef-test (:name (:unibyte-input-replacement :iso-8859-5))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-5 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((= (char-code char) i)
           (assert (or (< i #xa1) (= i #xad))))
          (t (assert (and (>= i #xa1) (/= i #xad)))))))))

(with-ef-test (:name (:unibyte-output-replacement :iso-8859-5))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-5 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-5))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 93 (count #\? string :start #xa1))))))

;;; iso-8859-6 tests
(with-ef-test (:name (:unibyte-input-replacement :iso-8859-6))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-6 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert #1=(or (= i (char-code #\?))
                          (<= #xa1 i #xa3) (<= #xa5 i #xab) (<= #xae i #xba)
                          (<= #xbc i #xbe) (= i #xc0) (<= #xdb i #xdf)
                          (<= #xf3 i))))
          (t (assert (not #1#))))))))

(with-ef-test (:name (:unibyte-output-replacement :iso-8859-6))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-6 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-6))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 93 (count #\? string :start #xa1))))))

;;; iso-8859-7 tests
(with-ef-test (:name (:unibyte-input-replacement :iso-8859-7))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-7 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert #1=(or (= i (char-code #\?))
                          (member i '(#xa4 #xa5 #xaa #xae #xd2 #xff)))))
          (t (assert (not #1#))))))))

(with-ef-test (:name (:unibyte-output-replacement :iso-8859-7))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-7 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-7))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 80 (count #\? string :start #xa1))))))

;;; iso-8859-8 tests
(with-ef-test (:name (:unibyte-input-replacement :iso-8859-8))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-8 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert #1=(or (= i (char-code #\?))
                          (= i #xa1) (<= #xbf i #xde) (>= i #xfb))))
          (t (assert (not  #1#))))))))

(with-ef-test (:name (:unibyte-output-replacement :iso-8859-8))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-8 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-8))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 67 (count #\? string :start #xa1))))))

;;; latin-5 tests
(with-ef-test (:name (:unibyte-input-replacement :latin-5))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-5 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (and (= (char-code char) i)
                         (not (member i '(#xd0 #xdd #xde #xf0 #xfd #xfe))))
                    (and (member i '(#xd0 #xdd #xde #xf0 #xfd #xfe))
                         (not (char= char #\?)))))))))

(with-ef-test (:name (:unibyte-output-replacement :latin-5))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-5 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-5))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xd0)
        (assert (= (char-code (char string i)) i)))
      (assert (= 6 (count #\? string :start #xd0))))))

;;; latin-6 tests
(with-ef-test (:name (:unibyte-input-replacement :latin-6))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-6 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (= (char-code char) i)
                    (and (<= #xa1 i #xff)
                         (not (char= char #\?)))))))))

(with-ef-test (:name (:unibyte-output-replacement :latin-6))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-6 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-6))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 46 (count #\? string :start #xa1))))))

;;; iso-8859-11 tests
(with-ef-test (:name (:unibyte-input-replacement :iso-8859-11))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:iso-8859-11 :replacement #\?))
      (let ((char (read-char s)))
        (cond
          ((eq char #\?)
           (assert (member i #1=`(,(char-code #\?) #xdb #xdc #xdd #xde #xfc #xfd #xfe #xff))))
          (t (assert (not (member i #1#)))))))))

(with-ef-test (:name (:unibyte-output-replacement :iso-8859-11))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:iso-8859-11 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:iso-8859-11))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 95 (count #\? string :start #xa1))))))

;;; latin-7 tests
(with-ef-test (:name (:unibyte-input-replacement :latin-7))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-7 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (= (char-code char) i)
                    (and (<= #xa1 i #xff)
                         (not (char= char #\?)))))))))

(with-ef-test (:name (:unibyte-output-replacement :latin-7))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-7 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-7))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (dolist (i '(#xd8 #xc6 #xf8 #xe6))
        (assert (char/= (char string i) #\?)))
      (assert (= 52 (count #\? string :start #xa1))))))

;;; latin-8 tests
(with-ef-test (:name (:unibyte-input-replacement :latin-8))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-8 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (= (char-code char) i)
                    (and (<= #xa1 i #xfe)
                         (not (char= char #\?)))))))))

(with-ef-test (:name (:unibyte-output-replacement :latin-8))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-8 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-8))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa1)
        (assert (= (char-code (char string i)) i)))
      (assert (= 31 (count #\? string :start #xa1))))))

;;; latin-9 tests
(with-ef-test (:name (:unibyte-input-replacement :latin-9))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:latin-9 :replacement #\?))
      (let ((char (read-char s)))
        (assert (or (and (= (char-code char) i)
                         (not (member i '(#xa4 #xa6 #xa8 #xb4 #xb8 #xbc #xbd #xbe))))
                    (and (member i '(#xa4 #xa6 #xa8 #xb4 #xb8 #xbc #xbd #xbe))
                         (not (char= char #\?)))))))))

(with-ef-test (:name (:unibyte-output-replacement :latin-9))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:latin-9 :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:latin-9))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #xa4)
        (assert (= (char-code (char string i)) i)))
      (assert (= 8 (count #\? string :start #xa4))))))

;;; koi8-r tests
(with-ef-test (:name (:unibyte-input-replacement :koi8-r)
                  :skipped-on :unicode-lite)
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:koi8-r :replacement #\?))
      (let ((char (read-char s)))
        (cond ((= (char-code char) i)
               (assert (< i 128)))
              (t (assert (> i 127))))))))

(with-ef-test (:name (:unibyte-output-replacement :koi8-r))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:koi8-r :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:koi8-r))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #x80)
        (assert (= (char-code (char string i)) i)))
      (assert (= 122 (count #\? string :start #x80))))))

;;; koi8-u tests
(with-ef-test (:name (:unibyte-input-replacement :koi8-u))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:koi8-u :replacement #\?))
      (let ((char (read-char s)))
        (cond ((= (char-code char) i)
               (assert (< i 128)))
              (t (assert (> i 127))))))))

(with-ef-test (:name (:unibyte-output-replacement :koi8-u))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:koi8-u :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:koi8-u))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #x80)
        (assert (= (char-code (char string i)) i)))
      (assert (= 122 (count #\? string :start #x80))))))

;;; x-mac-cyrillic tests
(with-ef-test (:name (:unibyte-input-replacement :x-mac-cyrillic))
  (dotimes (i 256)
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte i s))
    (with-open-file (s *test-path* :external-format '(:x-mac-cyrillic :replacement #\?))
      (let ((char (read-char s)))
        (cond ((= (char-code char) i)
               (assert (or (< i 128) (member i '(#xa2 #xa3 #xa9 #xb1 #xb5)))))
              (t (assert (and (> i 127)
                              (not (member i '(#xa2 #xa3 #xa9 #xb1 #xb5)))))))))))

(with-ef-test (:name (:unibyte-output-replacement :x-mac-cyrillic))
  (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:x-mac-cyrillic :replacement #\?))
    (dotimes (i 256)
      (write-char (code-char i) s)))
  (with-open-file (s *test-path* :external-format '(:x-mac-cyrillic))
    (let ((string (make-string 256)))
      (read-sequence string s)
      (dotimes (i #x80)
        (assert (= (char-code (char string i)) i)))
      (assert (= 113 (count #\? string :start #x80))))))

;;; ucs-2 tests
(with-test (:name (:multibyte :ucs2le))
  (let* ((size 120)
         (array (map-into (make-array size :element-type '(unsigned-byte 16))
                          (lambda () (random #x10000)))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dotimes (i size)
        (write-byte (ldb (byte 8 0) (aref array i)) s)
        (write-byte (ldb (byte 8 8) (aref array i)) s)))
    (with-open-file (s *test-path* :external-format :ucs2le)
      (let ((string (make-string size)))
        (read-sequence string s)
        (dotimes (i size)
          (assert (= (char-code (char string i)) (aref array i))))))))

(with-test (:name (:multibyte :ucs2be))
  (let* ((size 120)
         (array (map-into (make-array size :element-type '(unsigned-byte 16))
                          (lambda () (random #x10000)))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dotimes (i size)
        (write-byte (ldb (byte 8 8) (aref array i)) s)
        (write-byte (ldb (byte 8 0) (aref array i)) s)))
    (with-open-file (s *test-path* :external-format :ucs2be)
      (let ((string (make-string size)))
        (read-sequence string s)
        (dotimes (i size)
          (assert (= (char-code (char string i)) (aref array i))))))))

(with-test (:name (:multibyte :output-replacement :ucs2le))
  (let* ((size 1200)
         (string (map-into (make-string size)
                           (lambda () (code-char (random #x10000))))))
    (setf (char string 0) (code-char #x10001)
          (char string (1- size)) (code-char #x10002))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:ucs2le :replacement #\replacement_character))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :ucs2le)
      (let ((new (make-string size)))
        (read-sequence new s)
        (assert (char= (char new 0) #\replacement_character))
        (assert (char= (char new (1- size)) #\replacement_character))
        (assert (string= string new :start1 1 :start2 1 :end1 (1- size) :end2 (1- size)))))))

(with-test (:name (:multibyte :output-replacement :ucs2be))
  (let* ((size 1200)
         (string (map-into (make-string size)
                           (lambda () (code-char (random #x10000))))))
    (setf (char string 0) (code-char #x10001)
          (char string (1- size)) (code-char #x10002))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :external-format '(:ucs2be :replacement #\replacement_character))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :ucs2be)
      (let ((new (make-string size)))
        (read-sequence new s)
        (assert (char= (char new 0) #\replacement_character))
        (assert (char= (char new (1- size)) #\replacement_character))
        (assert (string= string new :start1 1 :start2 1 :end1 (1- size) :end2 (1- size)))))))

(with-test (:name (:multibyte :input-replacement :ucs4le))
  (let ((octets (coerce '(0 1 1 0 1 0 0 1) '(vector (unsigned-byte 8)))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence octets s))
    (with-open-file (s *test-path* :external-format '(:ucs4le :replacement #\replacement_character))
      (let ((string (read-line s)))
        (assert (char= (char string 0) (code-char #x10100)))
        (assert (char= (char string 1) #\replacement_character))))))

(with-test (:name (:multibyte :input-replacement :ucs4le))
  (let ((octets (coerce '(0 1 1 0 1 0 0 1) '(vector (unsigned-byte 8)))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence octets s))
    (with-open-file (s *test-path* :external-format '(:ucs4be :replacement #\replacement_character))
      (let ((string (read-line s)))
        (assert (char= (char string 0) (code-char #x10100)))
        (assert (char= (char string 1) #\replacement_character))))))

;;; utf tests
(with-test (:name (:utf-16le :roundtrip))
  (let ((string (map 'string 'code-char '(#x20 #x200 #x2000 #xfffd #x10fffd))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format :utf-16le)
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-16le)
      (assert (string= string (read-line s))))))
(with-test (:name (:utf-16be :roundtrip))
  (let ((string (map 'string 'code-char '(#x20 #x200 #x2000 #xfffd #x10fffd))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format :utf-16be)
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-16be)
      (assert (string= string (read-line s))))))
(with-test (:name (:utf-16le :encoding-error))
  (let ((string (map 'string 'code-char '(#x20 #xfffe #xdc00 #xd800 #x1fffe #x20))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format '(:utf-16le :replacement #\?))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-16le)
      (assert (string= " ???? " (read-line s))))))
(with-test (:name (:utf-16be :encoding-error))
  (let ((string (map 'string 'code-char '(#x20 #xfffe #xdc00 #xd800 #x1fffe #x20))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format '(:utf-16be :replacement #\?))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-16be)
      (assert (string= " ???? " (read-line s))))))

(with-test (:name (:utf-32le :roundtrip))
  (let ((string (map 'string 'code-char '(#x20 #x200 #x2000 #xfffd #x10fffd))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format :utf-32le)
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-32le)
      (assert (string= string (read-line s))))))
(with-test (:name (:utf-32be :roundtrip))
  (let ((string (map 'string 'code-char '(#x20 #x200 #x2000 #xfffd #x10fffd))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format :utf-32be)
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-32be)
      (assert (string= string (read-line s))))))
(with-test (:name (:utf-32le :encoding-error))
  (let ((string (map 'string 'code-char '(#x20 #xfffe #xdc00 #xd800 #x1fffe #x20))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format '(:utf-32le :replacement #\?))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-32le)
      (assert (string= " ???? " (read-line s))))))
(with-test (:name (:utf-32be :encoding-error))
  (let ((string (map 'string 'code-char '(#x20 #xfffe #xdc00 #xd800 #x1fffe #x20))))
    (with-open-file (s *test-path* :direction :output :if-exists :supersede
                       :external-format '(:utf-32be :replacement #\?))
      (write-string string s))
    (with-open-file (s *test-path* :external-format :utf-32be)
      (assert (string= " ???? " (read-line s))))))

(with-test (:name :invalid-external-format)
  (labels ((test-error (e)
             (assert (typep e 'error))
             (unless (equal "Undefined external-format: :BAD-FORMAT"
                            (princ-to-string e))
               (error "Bad error:~%  ~A" e)))
           (test (direction)
             (test-error
              (handler-case
                  (open #-win32 "/dev/null" #+win32 "nul" :direction direction :external-format :bad-format
                        :if-exists :overwrite)
                (error (e) e)))))
    (test :input)
    (test :output)
    (test :io)
    (test-error
     (handler-case
         (run-program "sh" '() :input :stream :external-format :bad-format)
       (error (e) e)))
    (test-error
     (handler-case
         (string-to-octets "foobar" :external-format :bad-format)
       (error (e) e)))
    (test-error
     (let ((octets (string-to-octets "foobar" :external-format :latin1)))
       (handler-case
           (octets-to-string octets :external-format :bad-format)
         (error (e) e))))))

(with-ef-test (:name (:lp713063 :euc-jp))
  (with-open-file (f *test-path*
                     :direction :output
                     :external-format '(:euc-jp :replacement #\?)
                     :if-exists :supersede)
    (write-string (make-string 3 :initial-element #\horizontal_bar) f))
  (assert (equal "???"
                 (with-open-file (f *test-path*
                                    :direction :input
                                    :external-format :euc-jp)
                   (read-line f)))))

;; test for lp#659107
(with-test (:name :cmdline-setq-external-format
                  :skipped-on (not :sb-unicode))
  (with-scratch-file (script "lisp")
    (with-open-file (stream script :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf16le)
      (format stream "(defvar s \"what? ~A\"~%)" (name-char "GRINNING_FACE"))
      (format stream "(sb-ext:exit :code
 (if (and (string= (subseq s 0 6) \"what? \") (char= (char s 6) #\\grinning_face)) 0 1))~%"))
    (let ((process (run-program
                    sb-ext:*runtime-pathname*
                    (list "--core" sb-int:*core-string*
                          "--noinform" "--no-sysinit" "--no-userinit" "--noprint"
                          "--disable-debugger"
                          "--eval" "(setq *default-external-format* :utf16le)"
                          "--load" script)
                    :error t)))
        (assert (zerop (process-exit-code process))))))

(with-test (:name (open :default-external-format :newline))
  (with-open-file (f *test-path*
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists :supersede)
    (let ((string (format nil "Hello~C~CWorld" #\Return #\Linefeed)))
      (write-sequence (map '(vector (unsigned-byte 8)) 'char-code string) f)))
  (let ((*default-external-format* '(:ascii :newline :crlf)))
    (with-open-file (f *test-path*)
      (let ((string (make-string (length "Hello  World "))))
        (read-sequence string f)
        (assert (string= string (format nil "Hello~CWorld" #\Newline) :end1 11))))))

(with-test (:name (compile-file :default-external-format :newline))
  (let ((path (scratch-file-name "lisp")))
    (with-open-file (f path
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
      (let ((string (format nil "(defun foo (x) (format nil (formatter \"Hello~~~C~C  ~~A World\") x))" #\Return #\Linefeed)))
        (write-sequence (map '(vector (unsigned-byte 8)) 'char-code string) f)))
    (let ((*default-external-format* '(:ascii :newline :crlf))
          (*error-output* (make-broadcast-stream)))
      (multiple-value-bind (output failurep warningsp)
          (compile-file path :verbose nil)
        (declare (ignore output))
        (assert (null failurep))
        (assert (null warningsp))))))

(delete-file *test-path*)
