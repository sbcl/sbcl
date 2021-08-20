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
           (let ((string (make-string char-code-limit :initial-element #\nul)))
             (dotimes (i char-code-limit)
               (unless (<= #xd800 i #xdfff)
                 (setf (char string i) (code-char i))))
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
  #+(and sb-unicode (not unicode-lite))
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

  (with-test (:name (:ascii :decoding-error use-value))
    (let ((non-ascii-bytes (make-array 128
                                       :element-type '(unsigned-byte 8)
                                       :initial-contents (loop for i from 128 below 256 collect i)))
        (error-count 0))
      (handler-bind ((sb-int:character-decoding-error
                      (lambda (c)
                        (incf error-count)
                        (use-value "??" c))))
        (assert (string= (octets-to-string non-ascii-bytes :external-format :ascii)
                         (make-string 256 :initial-element #\?)))
        (assert (= error-count 128)))))
  (with-test (:name (:ascii :encoding-error use-value))
    (let ((non-ascii-chars (make-array 128
                                       :element-type 'character
                                       :initial-contents (loop for i from 128 below 256 collect (code-char i))))
          (error-count 0))
      (handler-bind ((sb-int:character-encoding-error
                      (lambda (c)
                        (incf error-count)
                        (use-value "??" c))))
        (assert (equalp (string-to-octets non-ascii-chars :external-format :ascii)
                        (make-array 256 :initial-element (char-code #\?))))
        (assert (= error-count 128)))))

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
  #+nil ; old, 6-byte UTF-8 definition
  (progn
    (utf8-decode-tests #(#xf4 #x90 #x80 #x80) "?") ; #x110000
    (utf8-decode-tests #(#xf7 #xbf #xbf #xbf) "?") ; #x1fffff
    (utf8-decode-tests #(#xf8 #x88 #x80 #x80 #x80) "?") ; #x200000
    (utf8-decode-tests #(#xfb #xbf #xbf #xbf #xbf) "?") ; #x3ffffff
    (utf8-decode-tests #(#xfc #x84 #x80 #x80 #x80 #x80) "?") ; #x4000000
    (utf8-decode-tests #(#xfd #xbf #xbf #xbf #xbf #xbf) "?")) ; #x7fffffff
  (progn ; new, 4-byte (maximum #x10ffff) UTF-8 definition
    (utf8-decode-tests #(#xf4 #x90) "??") ; #x110000
    (utf8-decode-tests #(#xf7 #xbf #xbf #xbf) "????") ; #x1fffff
    (utf8-decode-tests #(#xf8 #x88 #x80 #x80 #x80) "?????") ; #x200000
    (utf8-decode-tests #(#xfb #xbf #xbf #xbf #xbf) "?????") ; #x3ffffff
    (utf8-decode-tests #(#xfc #x84 #x80 #x80 #x80 #x80) "??????") ; #x4000000
    (utf8-decode-tests #(#xfd #xbf #xbf #xbf #xbf #xbf) "??????")) ; #x7fffffff

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
  (utf8-decode-tests #1=#(#xe0 #xa0) "?")
  (utf8-decode-tests #2=#(#xf0 #x90 #x80) "?")
  #+nil
  (utf8-decode-tests #3=#(#xf8 #x80 #x80 #x80) "?")
  #+nil
  (utf8-decode-tests #4=#(#xfc #x80 #x80 #x80 #x80) "?")
  (utf8-decode-tests #5=#(#xdf) "?")
  (utf8-decode-tests #6=#(#xef #xbf) "?")
  #+nil
  (utf8-decode-tests #7=#(#xf7 #xbf #xbf) "?")
  #+nil
  (utf8-decode-tests #8=#(#xfb #xbf #xbf #xbf) "?")
  #+nil
  (utf8-decode-tests #9=#(#xfd #xbf #xbf #xbf #xbf) "?")

  ;; All ten previous tests concatenated
  (utf8-decode-tests (concatenate 'vector #0# #1# #2# #5# #6#)
                     "?????")

  ;; Random impossible bytes
  (utf8-decode-tests #(#xfe) "?")
  (utf8-decode-tests #(#xff) "?")
  (utf8-decode-tests #(#xfe #xfe #xff #xff) "????")

  ;; Overlong sequences - /
  (utf8-decode-tests #(#xc0 #xaf) "??")
  (utf8-decode-tests #(#xe0 #x80 #xaf) "???")
  (utf8-decode-tests #(#xf0 #x80 #x80 #xaf) "????")
  (utf8-decode-tests #(#xf8 #x80 #x80 #x80 #xaf) "?????")
  (utf8-decode-tests #(#xfc #x80 #x80 #x80 #x80 #xaf) "??????")

  ;; Overlong sequences - #\Rubout
  (utf8-decode-tests #(#xc1 #xbf) "??")
  (utf8-decode-tests #(#xe0 #x9f #xbf) "???")
  (utf8-decode-tests #(#xf0 #x8f #xbf #xbf) "????")
  (utf8-decode-tests #(#xf8 #x87 #xbf #xbf #xbf) "?????")
  (utf8-decode-tests #(#xfc #x83 #xbf #xbf #xbf #xbf) "??????")

  ;; Overlong sequences - #\Null
  (utf8-decode-tests #(#xc0 #x80) "??")
  (utf8-decode-tests #(#xe0 #x80 #x80) "???")
  (utf8-decode-tests #(#xf0 #x80 #x80 #x80) "????")
  (utf8-decode-tests #(#xf8 #x80 #x80 #x80 #x80) "?????")
  (utf8-decode-tests #(#xfc #x80 #x80 #x80 #x80 #x80) "??????")

  ;; Not testing surrogates & characters #xFFFE, #xFFFF; they're
  ;; perfectly good sbcl chars even if they're not actually ISO 10646
  ;; characters, and it's probably a good idea for s-to-o and o-to-s
  ;; to be inverses of each other as far as possible.
  )

)

;;; regression test: STRING->UTF8 didn't properly handle a non-zero
;;; START argument.
(assert (equalp #(50) (string-to-octets "42" :start 1 :external-format :utf-8)))

(assert (equalp #() (string-to-octets "" :external-format :utf-8)))
(assert (equalp #() (string-to-octets (make-string 0)
                                      :external-format :utf-8)))
(assert (equalp #() (string-to-octets (make-string 5)
                                      :start 3 :end 3 :external-format :utf-8)))
(assert (equalp #(0) (string-to-octets (make-string 5)
                                       :start 3 :end 3 :null-terminate t
                                       :external-format :utf-8)))

;;; whoops: the iso-8859-2 format referred to an undefined symbol.
#+(and sb-unicode (not unicode-lite))
(assert (equalp #(251) (string-to-octets (string (code-char 369))
                                         :external-format :latin-2)))

(with-test (:name (:euc-jp :decoding-errors) :skipped-on (or (not :sb-unicode) :unicode-lite))
  (handler-bind ((sb-int:character-decoding-error
                  (lambda (c) (use-value #\? c))))
    (assert (string= "?{?"
                     (octets-to-string
                      (coerce #(182 123 253 238) '(vector (unsigned-byte 8)))
                      :external-format :euc-jp)))))

(with-test (:name (:utf-8 :surrogates :encoding-errors) :skipped-on (not :sb-unicode))
  (handler-bind ((sb-int:character-encoding-error
                  (lambda (c) (use-value #\? c))))
    (assert (equalp (string-to-octets (string (code-char #xd800))
                                      :external-format :utf-8)
                    (vector (char-code #\?))))))
(with-test (:name (:utf-8 :surrogates :decoding-errors) :skipped-on (not :sb-unicode))
  (handler-bind ((sb-int:character-decoding-error
                  (lambda (c) (use-value #\? c))))
    (assert (find #\? (octets-to-string
                       (coerce #(237 160 128) '(vector (unsigned-byte 8)))
                       :external-format :utf-8)))))

(with-test (:name (:ucs-2 :out-of-range :encoding-errors) :skipped-on (not :sb-unicode))
  (handler-bind ((sb-int:character-encoding-error
                  (lambda (c) (use-value "???" c))))
    (assert (equalp (string-to-octets (string (code-char #x10001))
                                      :external-format :ucs-2le)
                    #(63 0 63 0 63 0))))
  (handler-bind ((sb-int:character-encoding-error
                  (lambda (c) (use-value "???" c))))
    (assert (equalp (string-to-octets (string (code-char #x10001))
                                      :external-format :ucs-2be)
                    #(0 63 0 63 0 63)))))

(with-test (:name (:ucs-4 :out-of-range :decoding-errors) :skipped-on (not :sb-unicode))
  (handler-bind ((sb-int:character-decoding-error
                  (lambda (c) (use-value "???" c))))
    (assert (equalp (octets-to-string (coerce '(1 2 3 4) '(vector (unsigned-byte 8)))
                                      :external-format :ucs-4le)
                    "???")))
  (assert (equalp (octets-to-string (coerce '(#xff #xff #x10 #x00) '(vector (unsigned-byte 8)))
                                    :external-format :ucs-4le)
                  (string (code-char #x10ffff))))
  (handler-bind ((sb-int:character-decoding-error
                  (lambda (c) (use-value "???" c))))
    (assert (equalp (octets-to-string (coerce '(1 2 3 4) '(vector (unsigned-byte 8)))
                                      :external-format :ucs-4be)
                    "???"))
    (assert (equalp (octets-to-string (coerce '(#x00 #x10 #xff #xff) '(vector (unsigned-byte 8)))
                                      :external-format :ucs-4be)
                    (string (code-char #x10ffff))))))

(with-test (:name (:utf-16le :ensure-roundtrip) :skipped-on (not :sb-unicode))
  (flet ((enc (x)
           (string-to-octets x :external-format :utf-16le))
         (dec (x)
           (octets-to-string (coerce x '(vector (unsigned-byte 8)))
                             :external-format :utf-16le)))
    (let ((string (map 'string 'code-char
                       '(#x20 #x200 #x2000 #x10000 #x10401 #x10fffd)))
          (octets #(#x20 0 0 #x2 0 #x20 0 #xd8 0 #xdc 1 #xd8 1 #xdc #xff #xdb #xfd #xdf)))
      (assert (equalp (enc string) octets))
      (assert (equalp (dec octets) string)))))

(with-test (:name (:utf-16le :encoding-error) :skipped-on (not :sb-unicode))
  (flet ((enc (x)
           (string-to-octets x :external-format '(:utf-16le :replacement #\?)))
         (dec (x)
           (octets-to-string (coerce x '(vector (unsigned-byte 8)))
                             :external-format :utf-16le)))
    (let ((string (map 'string 'code-char '(#xd800 #xdc00 #xfffe #x10ffff))))
      (assert (equalp (enc string) #(63 0 63 0 63 0 63 0))))))

(with-test (:name (:utf-16be :ensure-roundtrip) :skipped-on (not :sb-unicode))
  (flet ((enc (x)
           (string-to-octets x :external-format :utf-16be))
         (dec (x)
           (octets-to-string (coerce x '(vector (unsigned-byte 8)))
                             :external-format :utf-16be)))
    (let ((string (map 'string 'code-char
                       '(#x20 #x200 #x2000 #x10000 #x10401 #x10fffd)))
          (octets #(0 #x20 #x2 0 #x20 0 #xd8 0 #xdc 0 #xd8 1 #xdc 1 #xdb #xff #xdf #xfd)))
      (assert (equalp (enc string) octets))
      (assert (equalp (dec octets) string)))))

(with-test (:name (:utf-16be :encoding-error) :skipped-on (not :sb-unicode))
  (flet ((enc (x)
           (string-to-octets x :external-format '(:utf-16be :replacement #\?)))
         (dec (x)
           (octets-to-string (coerce x '(vector (unsigned-byte 8)))
                             :external-format :utf-16be)))
    (let ((string (map 'string 'code-char '(#xd800 #xdc00 #xfffe #x10ffff))))
      (assert (equalp (enc string) #(0 63 0 63 0 63 0 63))))))


(with-test (:name (:utf-32le :ensure-roundtrip) :skipped-on (not :sb-unicode))
  (flet ((enc (x)
           (string-to-octets x :external-format :utf-32le))
         (dec (x)
           (octets-to-string (coerce x '(vector (unsigned-byte 8)))
                             :external-format :utf-32le)))
    (let ((string (map 'string 'code-char
                       '(#x20 #x200 #x2000 #x10000 #x10401 #x10fffd)))
          (octets #(#x20 0 0 0 0 #x2 0 0 0 #x20 0 0 0 0 1 0 1 4 1 0 #xfd #xff #x10 0)))
      (assert (equalp (enc string) octets))
      (assert (equalp (dec octets) string)))))

(with-test (:name (:utf-32le :encoding-error) :skipped-on (not :sb-unicode))
  (flet ((enc (x)
           (string-to-octets x :external-format '(:utf-32le :replacement #\?)))
         (dec (x)
           (octets-to-string (coerce x '(vector (unsigned-byte 8)))
                             :external-format :utf-32le)))
    (let ((string (map 'string 'code-char '(#xd800 #xdc00 #xfffe #x10ffff))))
      (assert (equalp (enc string) #(63 0 0 0 63 0 0 0 63 0 0 0 63 0 0 0))))))


(with-test (:name (:utf-32be :ensure-roundtrip) :skipped-on (not :sb-unicode))
  (flet ((enc (x)
           (string-to-octets x :external-format :utf-32be))
         (dec (x)
           (octets-to-string (coerce x '(vector (unsigned-byte 8)))
                             :external-format :utf-32be)))
    (let ((string (map 'string 'code-char
                       '(#x20 #x200 #x2000 #x10000 #x10401 #x10fffd)))
          (octets #(0 0 0 #x20 0 0 #x2 0 0 0 #x20 0 0 1 0 0 0 1 4 1 0 #x10 #xff #xfd)))
      (assert (equalp (enc string) octets))
      (assert (equalp (dec octets) string)))))

(with-test (:name (:utf-32be :encoding-error) :skipped-on (not :sb-unicode))
  (flet ((enc (x)
           (string-to-octets x :external-format '(:utf-32be :replacement #\?)))
         (dec (x)
           (octets-to-string (coerce x '(vector (unsigned-byte 8)))
                             :external-format :utf-32be)))
    (let ((string (map 'string 'code-char '(#xd800 #xdc00 #xfffe #x10ffff))))
      (assert (equalp (enc string) #(0 0 0 63 0 0 0 63 0 0 0 63 0 0 0 63))))))

(with-test (:name :compile-file-position-with-encodings
            :skipped-on (not :sb-unicode))
  (with-open-file (f1 "data/compile-file-pos.lisp" :external-format :utf-8)
    (with-open-file (f2 "data/compile-file-pos-utf16be.lisp"
                        :external-format :utf-16be)
      (dotimes (i 3) ; skip three lines
        ;; because a comment line differs, and the function names differ
        (read-line f1)
        (read-line f2))
      (dotimes (i 3) ; compare three lines
        (assert (string= (read-line f1) (read-line f2))))))
  (flet ((compile-and-load (file encoding main-fun)
           (let ((fasl (compile-file file
                                     :output-file (scratch-file-name "fasl")
                                     :external-format encoding
                                     :print nil :verbose nil)))
             (load fasl)
             (delete-file fasl)
             (funcall main-fun))))
    (multiple-value-bind (a1 b1 c1)
        (compile-and-load "data/compile-file-pos.lisp" :utf-8 'cfp-foolz1)
      (multiple-value-bind (a2 b2 c2)
          (compile-and-load "data/compile-file-pos-utf16be.lisp" :utf-16be
                            'cfp-foolz2)
        (assert (string= a1 a2))
        (assert (string= b1 b2))
        ;; COMPILE-FILE-POSITION is insensitive to file encoding.
        (assert (string= c1 c2))))))
