;;;; encodings available regardless of build-time unicode settings

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")


;;; ASCII

(declaim (inline code->ascii-mapper))
(defun code->ascii-mapper (code)
  (declare (optimize speed #.*safety-0*)
           (type char-code code))
  (if (> code 127)
      nil
      code))

(declaim (inline get-ascii-bytes))
(defun get-ascii-bytes (string pos)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range pos))
  (get-latin-bytes #'code->ascii-mapper :ascii string pos))

(defun string->ascii (string sstart send null-padding)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range sstart send))
  (values (string->latin% string sstart send #'get-ascii-bytes null-padding)))

(defmacro define-ascii->string (accessor type)
  (let ((name (make-od-name 'ascii->string accessor)))
    `(progn
      (defun ,name (array astart aend)
        (declare (optimize speed)
                 (type ,type array)
                 (type array-range astart aend))
        ;; Since there is such a thing as a malformed ascii byte, a
        ;; simple "make the string, fill it in" won't do.
        (let ((string (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
          (loop for apos from astart below aend
                do (let* ((code (,accessor array apos))
                          (string-content
                           (if (< code 128)
                               (code-char code)
                               (decoding-error array apos (1+ apos) :ascii
                                               'malformed-ascii apos))))
                     (if (characterp string-content)
                         (vector-push-extend string-content string)
                         (loop for c across string-content
                               do (vector-push-extend c string))))
                finally (return (coerce string 'simple-string))))))))
(instantiate-octets-definition define-ascii->string)

(define-unibyte-external-format :ascii
    (:us-ascii :ansi_x3.4-1968 :iso-646 :iso-646-us :|646|)
  (if (>= bits 128)
      (external-format-encoding-error stream bits)
      (setf (sap-ref-8 sap tail) bits))
  (if (>= byte 128)
      (return-from decode-break-reason 1)
      (code-char byte))
  ascii->string-aref
  string->ascii)

;;; Latin-1

(declaim (inline get-latin1-bytes))
(defun get-latin1-bytes (string pos)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range pos))
  (get-latin-bytes #'identity :latin-1 string pos))

(defun string->latin1 (string sstart send null-padding)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range sstart send))
  (values (string->latin% string sstart send #'get-latin1-bytes null-padding)))

(defmacro define-latin1->string* (accessor type)
  (declare (ignore type))
  (let ((name (make-od-name 'latin1->string* accessor)))
    `(progn
      (defun ,name (string sstart send array astart aend)
        (,(make-od-name 'latin->string* accessor) string sstart send array astart aend #'identity)))))
(instantiate-octets-definition define-latin1->string*)

(defmacro define-latin1->string (accessor type)
  (declare (ignore type))
  `(defun ,(make-od-name 'latin1->string accessor) (array astart aend)
    (,(make-od-name 'latin->string accessor) array astart aend #'identity)))
(instantiate-octets-definition define-latin1->string)

;;; Multiple names for the :ISO{,-}8859-* families are needed because on
;;; FreeBSD (and maybe other BSD systems), nl_langinfo("LATIN-1") will
;;; return "ISO8859-1" instead of "ISO-8859-1".
(define-unibyte-external-format :latin-1 (:latin1 :iso-8859-1 :iso8859-1)
  (if (>= bits 256)
      (external-format-encoding-error stream bits)
      (setf (sap-ref-8 sap tail) bits))
  (code-char byte)
  latin1->string-aref
  string->latin1)


;;; UTF-8

;;; to UTF-8

(declaim (inline char-len-as-utf8))
(defun char-len-as-utf8 (code)
  (declare (optimize speed #.*safety-0*)
           (type (integer 0 (#.char-code-limit)) code))
  (cond ((< code 0) (bug "can't happen"))
        ((< code #x80) 1)
        ((< code #x800) 2)
        ((< code #x10000) 3)
        ((< code #x110000) 4)
        (t (bug "can't happen"))))

(defun string->utf8 (string sstart send null-padding)
  (declare (optimize (speed 3) #.*safety-0*)
           (type simple-string string)
           (type (integer 0 1) null-padding)
           (type array-range sstart send))
  (macrolet ((ascii-bash ()
               ;; KLUDGE: this depends on the fact that we know that
               ;; our arrays are initialized with zeros.
               '(let ((array (make-array (+ null-padding (- send sstart))
                                         :element-type '(unsigned-byte 8))))
                 (loop for i from 0
                       and j from sstart below send
                       do (setf (aref array i) (char-code (char string j))))
                 array))
             (output-code (tag)
               `(case (char-len-as-utf8 code)
                  (1 (add-byte code))
                  (2 (add-byte (logior #xc0 (ldb (byte 5 6) code)))
                     (add-byte (logior #x80 (ldb (byte 6 0) code))))
                  (3 (when (<= #xd800 code #xdfff)
                       (setf error-position i)
                       (go ,tag))
                     (add-byte (logior #xe0 (ldb (byte 4 12) code)))
                     (add-byte (logior #x80 (ldb (byte 6 6) code)))
                     (add-byte (logior #x80 (ldb (byte 6 0) code))))
                  (4 (add-byte (logior #xf0 (ldb (byte 3 18) code)))
                     (add-byte (logior #x80 (ldb (byte 6 12) code)))
                     (add-byte (logior #x80 (ldb (byte 6 6) code)))
                     (add-byte (logior #x80 (ldb (byte 6 0) code)))))))
    (etypecase string
      ((simple-array character (*))
       (let ((utf8-length 0))
         ;; Since it has to fit in a vector, it must be a fixnum!
         (declare (type (and unsigned-byte fixnum) utf8-length))
         (loop for i of-type index from sstart below send
               do (incf utf8-length (char-len-as-utf8 (char-code (char string i)))))
         (if (= utf8-length (- send sstart))
             (ascii-bash)
             (let ((array (make-array (+ null-padding utf8-length)
                                      :element-type '(unsigned-byte 8)))
                   (new-array nil)
                   (error-position 0)
                   (index 0))
               (declare (type index index))
               (tagbody
                :no-error
                  (flet ((add-byte (b)
                           (setf (aref array index) b)
                           (incf index)))
                    (declare (inline add-byte))
                    (loop for i of-type index from sstart below send
                          for code = (char-code (char string i))
                          do (output-code :first-error)
                          finally (return-from string->utf8 array)))
                :first-error
                  (setf new-array (make-array (* index 2) :adjustable t
                                              :element-type '(unsigned-byte 8)
                                              :fill-pointer index))
                  (replace new-array array)
                :error
                  (let ((replacement (encoding-error :utf-8 string index)))
                    (flet ((add-byte (b) (vector-push-extend b new-array)))
                      (dotimes (i (length replacement))
                        (add-byte (aref replacement i)))
                      (loop for i of-type index from (1+ error-position) below send
                            for code = (char-code (char string i))
                            do (output-code :error)
                            finally (return-from string->utf8
                                      (progn
                                        (unless (zerop null-padding)
                                          (vector-push-extend 0 new-array))
                                        (copy-seq new-array)))))))))))
      #+sb-unicode
      ((simple-array base-char (*))
       ;; On unicode builds BASE-STRINGs are limited to ASCII range,
       ;; so we can take a fast path -- and get benefit of the element
       ;; type information. On non-unicode build BASE-CHAR ==
       ;; CHARACTER, handled above.
       (ascii-bash)))))

;;; from UTF-8

(defmacro define-bytes-per-utf8-character (accessor type)
  (let ((name (make-od-name 'bytes-per-utf8-character accessor)))
    `(progn
      ;;(declaim (inline ,name))
      (let ((lexically-max
             (string->utf8 (string (code-char ,(1- char-code-limit)))
                           0 1 0)))
        (declare (type (simple-array (unsigned-byte 8) (#+sb-unicode 4 #-sb-unicode 2)) lexically-max))
        (defun ,name (array pos end)
          (declare (optimize speed #.*safety-0*)
                   (type ,type array)
                   (type array-range pos end))
          ;; returns the number of bytes consumed and nil if it's a
          ;; valid character or the number of bytes consumed and a
          ;; replacement string if it's not.
          (let ((initial-byte (,accessor array pos))
                (reject-reason nil)
                (reject-position pos)
                (remaining-bytes (- end pos)))
            (declare (type array-range reject-position remaining-bytes))
            (labels ((valid-utf8-starter-byte-p (b)
                       (declare (type (unsigned-byte 8) b))
                       (let ((ok (cond
                                   ((zerop (logand b #b10000000)) 1)
                                   ((and (= (logand b #b11100000) #b11000000)
                                         (>= b #xc2)) 2)
                                   ((= (logand b #b11110000) #b11100000) 3)
                                   ((and (= (logand b #b11111000) #b11110000)
                                         (<= b #xf4)) 4)
                                   (t nil))))
                         (unless ok
                           (setf reject-reason 'invalid-utf8-starter-byte))
                         ok))
                     (enough-bytes-left-p (x)
                       (let ((ok (> end (+ pos (1- x)))))
                         (unless ok
                           (setf reject-reason 'end-of-input-in-character))
                         ok))
                     (valid-secondary-p (x)
                       (let* ((idx (the array-range (+ pos x)))
                              (b (,accessor array idx))
                              (ok (= (logand b #b11000000) #b10000000)))
                         (when (and ok (= x 1))
                           (setf ok
                                 (case initial-byte
                                   (#xe0 (>= b #xa0))
                                   (#xed (< b #xa0))
                                   (#xf0 (>= b #x90))
                                   (#xf4 (< b #x90))
                                   (t t))))
                         (unless ok
                           (setf reject-reason 'invalid-utf8-continuation-byte)
                           (setf reject-position idx))
                         ok))
                     (preliminary-ok-for-length (maybe-len len)
                       (and (eql maybe-len len)
                            ;; Has to be done in this order so that
                            ;; certain broken sequences (e.g., the
                            ;; two-byte sequence `"initial (length 3)"
                            ;; "non-continuation"' -- `#xef #x32')
                            ;; signal only part of that sequence as
                            ;; erroneous.
                            (loop for i from 1 below (min len remaining-bytes)
                                  always (valid-secondary-p i))
                            (enough-bytes-left-p len)))
                     (character-below-char-code-limit-p ()
                       ;; This is only called on a four-byte sequence
                       ;; (two in non-unicode builds) to ensure we
                       ;; don't go over SBCL's character limts.
                       (let ((ok (cond ((< (aref lexically-max 0) (,accessor array pos))
                                        nil)
                                       ((> (aref lexically-max 0) (,accessor array pos))
                                        t)
                                       ((< (aref lexically-max 1) (,accessor array (+ pos 1)))
                                        nil)
                                       #+sb-unicode
                                       ((> (aref lexically-max 1) (,accessor array (+ pos 1)))
                                        t)
                                       #+sb-unicode
                                       ((< (aref lexically-max 2) (,accessor array (+ pos 2)))
                                        nil)
                                       #+sb-unicode
                                       ((> (aref lexically-max 2) (,accessor array (+ pos 2)))
                                        t)
                                       #+sb-unicode
                                       ((< (aref lexically-max 3) (,accessor array (+ pos 3)))
                                        nil)
                                       (t t))))
                         (unless ok
                           (setf reject-reason 'character-out-of-range))
                         ok)))
              (declare (inline valid-utf8-starter-byte-p
                               enough-bytes-left-p
                               valid-secondary-p
                               preliminary-ok-for-length))
              (let ((maybe-len (valid-utf8-starter-byte-p initial-byte)))
                (cond ((eql maybe-len 1)
                       (values 1 nil))
                      ((and (preliminary-ok-for-length maybe-len 2)
                            #-sb-unicode (character-below-char-code-limit-p))
                       (values 2 nil))
                      ((and (preliminary-ok-for-length maybe-len 3)
                            #-sb-unicode (not (setf reject-reason 'character-out-of-range)))
                       (values 3 nil))
                      ((and (preliminary-ok-for-length maybe-len 4)
                            #-sb-unicode (not (setf reject-reason 'character-out-of-range))
                            (character-below-char-code-limit-p))
                       (values 4 nil))
                      (t
                       (let* ((bad-end
                               (ecase reject-reason
                                 (invalid-utf8-starter-byte (1+ pos))
                                 (end-of-input-in-character end)
                                 (invalid-utf8-continuation-byte reject-position)
                                 (character-out-of-range (+ pos maybe-len))))
                              (bad-len (- bad-end pos)))
                         (declare (type array-range bad-end bad-len))
                         (let ((replacement (decoding-error array pos bad-end :utf-8 reject-reason reject-position)))
                           (values bad-len replacement)))))))))))))
(instantiate-octets-definition define-bytes-per-utf8-character)

(defmacro define-simple-get-utf8-char (accessor type)
  (let ((name (make-od-name 'simple-get-utf8-char accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (array pos bytes)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range pos)
                 (type (integer 1 4) bytes))
        (flet ((cref (x)
                 (,accessor array (the array-range (+ pos x)))))
          (declare (inline cref))
          (code-char (ecase bytes
                       (1 (cref 0))
                       (2 (logior (ash (ldb (byte 5 0) (cref 0)) 6)
                                  (ldb (byte 6 0) (cref 1))))
                       (3 (logior (ash (ldb (byte 4 0) (cref 0)) 12)
                                  (ash (ldb (byte 6 0) (cref 1)) 6)
                                  (ldb (byte 6 0) (cref 2))))
                       (4 (logior (ash (ldb (byte 3 0) (cref 0)) 18)
                                  (ash (ldb (byte 6 0) (cref 1)) 12)
                                  (ash (ldb (byte 6 0) (cref 2)) 6)
                                  (ldb (byte 6 0) (cref 3)))))))))))
(instantiate-octets-definition define-simple-get-utf8-char)

(defmacro define-utf8->string (accessor type)
  (let ((name (make-od-name 'utf8->string accessor)))
    `(progn
      (defun ,name (array astart aend)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range astart aend))
        (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
          (loop with pos = astart
                while (< pos aend)
                do (multiple-value-bind (bytes invalid)
                       (,(make-od-name 'bytes-per-utf8-character accessor) array pos aend)
                     (declare (type (or null string) invalid))
                     (cond
                       ((null invalid)
                        (vector-push-extend (,(make-od-name 'simple-get-utf8-char accessor) array pos bytes) string))
                       (t
                        (dotimes (i (length invalid))
                          (vector-push-extend (char invalid i) string))))
                     (incf pos bytes)))
          (coerce string 'simple-string))))))
(instantiate-octets-definition define-utf8->string)

(define-external-format/variable-width (:utf-8 :utf8) t
  #+sb-unicode (code-char #xfffd) #-sb-unicode #\?
  (let ((bits (char-code byte)))
    (cond ((< bits #x80) 1)
          ((< bits #x800) 2)
          ((< bits #x10000) 3)
          (t 4)))
  (ecase size
    (1 (setf (sap-ref-8 sap tail) bits))
    (2 (setf (sap-ref-8 sap tail)       (logior #xc0 (ldb (byte 5 6) bits))
             (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 0) bits))))
    (3 (when (<= #xd800 bits #xdfff)
         (external-format-encoding-error stream bits))
       (setf (sap-ref-8 sap tail)       (logior #xe0 (ldb (byte 4 12) bits))
             (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 6) bits))
             (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 0) bits))))
    (4 (setf (sap-ref-8 sap tail)       (logior #xf0 (ldb (byte 3 18) bits))
             (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 12) bits))
             (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 6) bits))
             (sap-ref-8 sap (+ 3 tail)) (logior #x80 (ldb (byte 6 0) bits)))))
  (1 (cond ((< byte #x80) 1)
           ((< byte #xc2) (return-from decode-break-reason 1))
           ((< byte #xe0) 2)
           ((< byte #xf0) 3)
           (t 4)))
  (code-char (ecase size
               (1 byte)
               (2 (let ((byte2 (sap-ref-8 sap (1+ head))))
                    (unless (<= #x80 byte2 #xbf)
                      (return-from decode-break-reason 2))
                    (dpb byte (byte 5 6) byte2)))
               (3 (let ((byte2 (sap-ref-8 sap (1+ head)))
                        (byte3 (sap-ref-8 sap (+ 2 head))))
                    (unless (and (<= #x80 byte2 #xbf)
                                 (<= #x80 byte3 #xbf)
                                 (or (/= byte #xe0) (<= #xa0 byte2 #xbf))
                                 (or (/= byte #xed) (<= #x80 byte2 #x9f)))
                      (return-from decode-break-reason 3))
                    (dpb byte (byte 4 12) (dpb byte2 (byte 6 6) byte3))))
               (4 (let ((byte2 (sap-ref-8 sap (1+ head)))
                        (byte3 (sap-ref-8 sap (+ 2 head)))
                        (byte4 (sap-ref-8 sap (+ 3 head))))
                    (unless (and (<= #x80 byte2 #xbf)
                                 (<= #x80 byte3 #xbf)
                                 (<= #x80 byte4 #xbf)
                                 (or (/= byte #xf0) (<= #x90 byte2 #xbf))
                                 (or (/= byte #xf4) (<= #x80 byte2 #x8f)))
                      (return-from decode-break-reason 4))
                    (dpb byte (byte 3 18)
                         (dpb byte2 (byte 6 12)
                              (dpb byte3 (byte 6 6) byte4)))))))
  utf8->string-aref
  string->utf8
  #+sb-unicode :base-string-direct-mapping #+sb-unicode t)
