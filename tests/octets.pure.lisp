;;;; tests of octet/character machinery with no side effects

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

(cl:in-package :cl-user)

(locally
    (declare (optimize debug (speed 0)))

(labels ((ub8 (len-or-seq)
           (if (numberp len-or-seq)
               (make-array len-or-seq :element-type '(unsigned-byte 8) :initial-element 0)
               (coerce len-or-seq '(simple-array (unsigned-byte 8) (*)))))

         (ensure-roundtrip-ascii ()
           (let ((octets (ub8 128)))
             (dotimes (i 128)
               (setf (aref octets i) i))
             (let* ((str (octets-to-string octets :external-format :ascii))
                    (oct2 (string-to-octets str :external-format :ascii)))
               (assert (= (length octets) (length oct2)))
               (assert (every #'= octets oct2))))
           t)

         (ensure-roundtrip-latin (format)
           (let ((octets (ub8 256)))
             (dotimes (i 256)
               (setf (aref octets i) i))
             (let* ((str (octets-to-string octets :external-format format))
                    (oct2 (string-to-octets str :external-format format)))
               (assert (= (length octets) (length oct2)))
               (assert (every #'= octets oct2))))
           t)

         (ensure-roundtrip-latin1 ()
           (ensure-roundtrip-latin :latin1))

         #+sb-unicode
         (ensure-roundtrip-latin9 ()
           (ensure-roundtrip-latin :latin9))

         (ensure-roundtrip-utf8 ()
           (let ((string (make-string char-code-limit)))
             (dotimes (i char-code-limit)
               (setf (char string i) (code-char i)))
             (let ((string2
                    (octets-to-string (string-to-octets string :external-format :utf8)
                                      :external-format :utf8)))
               (assert (= (length string2) (length string)))
               (assert (string= string string2))))
           t)

         (utf8-decode-test (octets expected-results expected-errors)
           (let ((error-count 0))
             (handler-bind ((sb-int:character-decoding-error
                             (lambda (c)
                               (incf error-count)
                               (use-value "?" c))))
               (assert (string= expected-results
                                (octets-to-string (ub8 octets)
                                                  :external-format :utf-8)))
               (assert (= error-count expected-errors)))))

         (utf8-decode-tests (octets expected-results)
           (let ((expected-errors (count #\? expected-results)))
             (utf8-decode-test octets expected-results expected-errors)
             (utf8-decode-test (concatenate 'vector
                                            '(34)
                                            octets
                                            '(34))
                               (format nil "\"~A\"" expected-results)
                               expected-errors))))

  (ensure-roundtrip-ascii)
  (ensure-roundtrip-latin1)
  #+sb-unicode
  (progn
    (ensure-roundtrip-latin9)
    ;; Latin-9 chars; the previous test checked roundtrip from
    ;; octets->char and back, now test that the latin-9 characters did
    ;; in fact appear during that trip.
    (let ((l9c (map 'string #'code-char '(8364 352 353 381 382 338 339 376))))
      (assert
       (string= (octets-to-string (string-to-octets l9c :external-format :latin9)
                                  :external-format :latin9)
                l9c))))
  (ensure-roundtrip-utf8)

  (let ((non-ascii-bytes (make-array 128
                                     :element-type '(unsigned-byte 8)
                                     :initial-contents (loop for i from 128 below 256
                                                             collect i))))
    (handler-bind ((sb-int:character-decoding-error
                    (lambda (c)
                      (use-value "??" c))))
      (assert (string= (octets-to-string non-ascii-bytes :external-format :ascii)
                       (make-string 256 :initial-element #\?)))))
  (let ((non-ascii-chars (make-array 128
                                     :element-type 'character
                                     :initial-contents (loop for i from 128 below 256
                                                             collect (code-char i)))))
    (handler-bind ((sb-int:character-encoding-error
                    (lambda (c)
                      (use-value "??" c))))
      (assert (equalp (string-to-octets non-ascii-chars :external-format :ascii)
                      (make-array 256 :initial-element (char-code #\?))))))

  ;; From Markus Kuhn's UTF-8 test file:
  ;; http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt

  ;; Too-big characters
  #-sb-unicode
  (progn
    (utf8-decode-tests #(#xc4 #x80) "?") ; #x100
    (utf8-decode-tests #(#xdf #xbf) "?") ; #x7ff
    (utf8-decode-tests #(#xe0 #xa0 #x80) "?") ; #x800
    (utf8-decode-tests #(#xef #xbf #xbf) "?") ; #xffff
    (utf8-decode-tests #(#xf0 #x90 #x80 #x80) "?")) ; #x10000
  (utf8-decode-tests #(#xf4 #x90 #x80 #x80) "?") ; #x110000
  (utf8-decode-tests #(#xf7 #xbf #xbf #xbf) "?") ; #x1fffff
  (utf8-decode-tests #(#xf8 #x88 #x80 #x80 #x80) "?") ; #x200000
  (utf8-decode-tests #(#xfb #xbf #xbf #xbf #xbf) "?") ; #x3ffffff
  (utf8-decode-tests #(#xfc #x84 #x80 #x80 #x80 #x80) "?") ; #x4000000
  (utf8-decode-tests #(#xfd #xbf #xbf #xbf #xbf #xbf) "?") ; #x7fffffff

  ;; Unexpected continuation bytes
  (utf8-decode-tests #(#x80) "?")
  (utf8-decode-tests #(#xbf) "?")
  (utf8-decode-tests #(#x80 #xbf) "??")
  (utf8-decode-tests #(#x80 #xbf #x80) "???")
  (utf8-decode-tests #(#x80 #xbf #x80 #xbf) "????")
  (utf8-decode-tests #(#x80 #xbf #x80 #xbf #x80) "?????")
  (utf8-decode-tests #(#x80 #xbf #x80 #xbf #x80 #xbf) "??????")
  (utf8-decode-tests #(#x80 #xbf #x80 #xbf #x80 #xbf #x80) "???????")

  ;; All 64 continuation bytes in a row
  (apply #'utf8-decode-tests
         (loop for i from #x80 to #xbf
               collect i into bytes
               collect #\? into chars
               finally (return (list bytes
                                     (coerce chars 'string)))))

  ;; Lonely start characters
  (flet ((lsc (first last)
           (apply #'utf8-decode-tests
                  (loop for i from first to last
                        nconc (list i 32) into bytes
                        nconc (list #\? #\Space) into chars
                        finally (return (list bytes
                                              (coerce chars 'string)))))
           (apply #'utf8-decode-tests
                  (loop for i from first to last
                        collect i into bytes
                        collect #\? into chars
                        finally (return (list bytes
                                              (coerce chars 'string)))))))
    (lsc #xc0 #xdf) ; 2-byte sequence start chars
    (lsc #xe0 #xef) ; 3-byte
    (lsc #xf0 #xf7) ; 4-byte
    (lsc #xf8 #xfb) ; 5-byte
    (lsc #xfc #xfd)) ; 6-byte

  ;; Otherwise incomplete sequences (last continuation byte missing)
  (utf8-decode-tests #0=#(#xc0) "?")
  (utf8-decode-tests #1=#(#xe0 #x80) "?")
  (utf8-decode-tests #2=#(#xf0 #x80 #x80) "?")
  (utf8-decode-tests #3=#(#xf8 #x80 #x80 #x80) "?")
  (utf8-decode-tests #4=#(#xfc #x80 #x80 #x80 #x80) "?")
  (utf8-decode-tests #5=#(#xdf) "?")
  (utf8-decode-tests #6=#(#xef #xbf) "?")
  (utf8-decode-tests #7=#(#xf7 #xbf #xbf) "?")
  (utf8-decode-tests #8=#(#xfb #xbf #xbf #xbf) "?")
  (utf8-decode-tests #9=#(#xfd #xbf #xbf #xbf #xbf) "?")

  ;; All ten previous tests concatenated
  (utf8-decode-tests (concatenate 'vector #0# #1# #2# #3# #4# #5# #6# #7# #8# #9#)
                     "??????????")

  ;; Random impossible bytes
  (utf8-decode-tests #(#xfe) "?")
  (utf8-decode-tests #(#xff) "?")
  (utf8-decode-tests #(#xfe #xfe #xff #xff) "????")

  ;; Overlong sequences - /
  (utf8-decode-tests #(#xc0 #xaf) "?")
  (utf8-decode-tests #(#xe0 #x80 #xaf) "?")
  (utf8-decode-tests #(#xf0 #x80 #x80 #xaf) "?")
  (utf8-decode-tests #(#xf8 #x80 #x80 #x80 #xaf) "?")
  (utf8-decode-tests #(#xfc #x80 #x80 #x80 #x80 #xaf) "?")

  ;; Overlong sequences - #\Rubout
  (utf8-decode-tests #(#xc1 #xbf) "?")
  (utf8-decode-tests #(#xe0 #x9f #xbf) "?")
  (utf8-decode-tests #(#xf0 #x8f #xbf #xbf) "?")
  (utf8-decode-tests #(#xf8 #x87 #xbf #xbf #xbf) "?")
  (utf8-decode-tests #(#xfc #x83 #xbf #xbf #xbf #xbf) "?")

  ;; Overlong sequences - #\Null
  (utf8-decode-tests #(#xc0 #x80) "?")
  (utf8-decode-tests #(#xe0 #x80 #x80) "?")
  (utf8-decode-tests #(#xf0 #x80 #x80 #x80) "?")
  (utf8-decode-tests #(#xf8 #x80 #x80 #x80 #x80) "?")
  (utf8-decode-tests #(#xfc #x80 #x80 #x80 #x80 #x80) "?")

  ;; Not testing surrogates & characters #xFFFE, #xFFFF; they're
  ;; perfectly good sbcl chars even if they're not actually ISO 10646
  ;; characters, and it's probably a good idea for s-to-o and o-to-s
  ;; to be inverses of each other as far as possible.
  )

)

;; regression test: STRING->UTF8 didn't properly handle a non-zero
;; START argument.
(assert (equalp #(50) (string-to-octets "42" :start 1 :external-format :utf-8)))
