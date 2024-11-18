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


;;; General unibyte support

(defmacro define-unibyte-to-octets-functions
    (external-format get-bytes-name string-to-octets-name code->-name)
  (let ((get-bytes-name/cr (symbolicate get-bytes-name '/cr))
        (string-to-octets-name/cr (symbolicate string-to-octets-name '/cr))
        (string-to-octets-name/crlf (symbolicate string-to-octets-name '/crlf)))
    `(progn
       (declaim (inline ,get-bytes-name ,get-bytes-name/cr))
       (defun ,get-bytes-name (string pos replacement)
         (declare (optimize speed #.*safety-0*)
                  (type simple-string string)
                  (type array-range pos))
         (get-latin-bytes #',code->-name ',external-format replacement string pos))
       (defun ,string-to-octets-name (string sstart send null-padding replacement)
         (declare (optimize speed #.*safety-0*)
                  (type simple-string string)
                  (type array-range sstart send))
         (values (string->latin% string sstart send #',get-bytes-name null-padding replacement)))
       (defun ,get-bytes-name/cr (string pos replacement)
         (declare (optimize speed #.*safety-0*)
                  (type simple-string string)
                  (type array-range pos))
         (get-latin-bytes (lambda (code) (,code->-name (if (= code 10) 13 code)))
                          '(,external-format :newline :cr) replacement string pos))
       (defun ,string-to-octets-name/cr (string sstart send null-padding replacement)
         (declare (optimize speed #.*safety-0*)
                  (type simple-string string)
                  (type array-range sstart send))
         (values (string->latin% string sstart send #',get-bytes-name/cr null-padding replacement)))
       (defun ,string-to-octets-name/crlf (string sstart send null-padding replacement)
         (declare (optimize speed #.*safety-0*)
                  (type simple-string string)
                  (type array-range sstart send)
                  (type (integer 0 1) null-padding))
         (let ((array (make-array (+ (* 2 (- send sstart)) null-padding)
                                  :element-type '(unsigned-byte 8)
                                  :fill-pointer 0 :adjustable t)))
           (loop for i from sstart below send
                 do (let* ((code (char-code (char string i)))
                           (byte (,code->-name code)))
                      (cond
                        ((= code 10)
                         (vector-push-extend (,code->-name 13) array)
                         (vector-push-extend byte array))
                        ((null byte)
                         (let ((replacement (encoding-error '(,external-format :newline :crlf)
                                                            replacement string i)))
                           (declare (type (simple-array (unsigned-byte 8) (*)) replacement))
                           (dotimes (j (length replacement))
                             (vector-push-extend (aref replacement j) array))))
                        (t (vector-push-extend byte array)))))
           (dotimes (i null-padding)
             (vector-push-extend 0 array))
           (coerce array '(simple-array (unsigned-byte 8) (*))))))))

(defmacro define-unibyte-to-string-functions
    (external-format octets-to-string-name ->code-name &optional invalid-bytes-p)
  (let ((octets-to-string-name/cr (symbolicate octets-to-string-name '/cr))
        (octets-to-string-name/crlf (symbolicate octets-to-string-name '/crlf))
        (decoding-condition-name (symbolicate 'malformed- external-format)))
    (declare (ignorable decoding-condition-name))
    `(macrolet ((def (accessor type)
                  (declare (ignorable type))
                  `(progn
                     ,@,(when invalid-bytes-p
                          ``((define-condition ,',decoding-condition-name (octet-decoding-error) ())))
                     ,,(if invalid-bytes-p
                           ``(progn
                               (defun ,(make-od-name ',octets-to-string-name accessor) (array astart aend replacement)
                                 (declare (optimize speed)
                                          (type ,type array)
                                          (type array-range astart aend)
                                          (ignorable replacement))
                                 (let ((string (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
                                   (loop for apos from astart below aend
                                         do (let* ((byte (,accessor array apos))
                                                   (code (,',->code-name byte))
                                                   (string-content
                                                    (if code
                                                        (code-char code)
                                                        (decoding-error array apos (1+ apos)
                                                                        ,',external-format
                                                                        replacement
                                                                        ',',decoding-condition-name
                                                                        apos))))
                                              (if (characterp string-content)
                                                  (vector-push-extend string-content string)
                                                  (loop for c across string-content
                                                        do (vector-push-extend c string))))
                                         finally (return (coerce string 'simple-string)))))
                               (defun ,(make-od-name ',octets-to-string-name/cr accessor) (array astart aend replacement)
                                 (declare (optimize speed)
                                          (type ,type array)
                                          (type array-range astart aend)
                                          (ignorable replacement))
                                 (let ((string (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
                                   (loop for apos from astart below aend
                                         do (let* ((byte (,accessor array apos))
                                                   (code (,',->code-name byte))
                                                   (string-content
                                                    (cond
                                                      ((null code)
                                                       (decoding-error array apos (1+ apos)
                                                                       '(,',external-format :newline :cr)
                                                                       replacement
                                                                       ',',decoding-condition-name
                                                                       apos))
                                                      ((= code 13) (code-char 10))
                                                      (t (code-char code)))))
                                              (if (characterp string-content)
                                                  (vector-push-extend string-content string)
                                                  (loop for c across string-content
                                                        do (vector-push-extend c string))))
                                         finally (return (coerce string 'simple-string))))))
                           ``(progn
                               (defun ,(make-od-name ',octets-to-string-name accessor) (array astart aend replacement)
                                 (declare (ignore replacement))
                                 (,(make-od-name 'latin->string accessor) array astart aend #',',->code-name))
                               (defun ,(make-od-name ',octets-to-string-name/cr accessor) (array astart aend replacement)
                                 (declare (ignore replacement))
                                 (,(make-od-name 'latin->string accessor) array astart aend
                                   (lambda (x) (let ((code (,',->code-name x))) (if (= code 13) 10 code)))))))
                     (defun ,(make-od-name ',octets-to-string-name/crlf accessor) (array astart aend replacement)
                       (declare (optimize speed)
                                (type ,type array)
                                (type array-range astart aend)
                                (ignorable replacement))
                       (let ((string (make-array (- aend astart) :element-type 'character
                                                 :fill-pointer 0 :adjustable t)))
                         (loop for apos from astart below aend
                               do (let* ((byte (,accessor array apos))
                                         (code (,',->code-name byte))
                                         (string-content
                                          (cond
                                            ,@',(when invalid-bytes-p
                                                  `(((null code)
                                                     (decoding-error array apos (1+ apos)
                                                                     '(,external-format :newline :crlf)
                                                                     replacement
                                                                     ',decoding-condition-name
                                                                     apos))))
                                            ((= code 13)
                                             (if (= apos (1- aend))
                                                 (code-char 13)
                                                 (let* ((next-byte (,accessor array (1+ apos)))
                                                        (next-code (,',->code-name next-byte)))
                                                   (if (= next-code 10)
                                                       (progn (incf apos) #\Newline)
                                                       (code-char 13)))))
                                            (t (code-char code)))))
                                    (if (characterp string-content)
                                        (vector-push-extend string-content string)
                                        (loop for c across string-content
                                              do (vector-push-extend c string))))
                               finally (return (coerce string 'simple-string))))))))
       (instantiate-octets-definition def))))

(defmacro define-unibyte-external-format-with-newline-variants
    (name other-names
     (->code-name code->-name)
     (->string-name string->name)
     (->string/cr-name string/cr->name)
     (->string/crlf-name string/crlf->name)
     &optional invalid-bytes-p)
  `(progn
     (define-unibyte-external-format ,name ,other-names
       (,code->-name (char-code |ch|))
       (let ((byte (,code->-name bits)))
         (if byte
             (setf (sap-ref-8 sap tail) byte)
             (external-format-encoding-error stream bits)))
       (let ((code (,->code-name byte)))
         ,(if invalid-bytes-p
              '(if code (code-char code) (return-from decode-break-reason 1))
              '(code-char code)))
       ,->string-name
       ,string->name)
     (define-external-format/variable-width (,name)
         t #\? 1
         (let* ((newbits (if (= bits 10) 13 bits))
                (byte (,code->-name newbits)))
           (if byte
               (setf (sap-ref-8 sap tail) byte)
               (external-format-encoding-error stream bits)))
         1
         (let ((code (,->code-name byte)))
           ,(if invalid-bytes-p
                `(if code
                     (if (= code 13) #\Newline (code-char code))
                     (return-from decode-break-reason 1))
                '(if (= code 13) #\Newline (code-char code))))
         ,->string/cr-name
         ,string/cr->name
         :char-encodable-p (,code->-name (char-code |ch|))
         :newline-variant :cr)
     (define-external-format/variable-width (,name)
         t #\?
         (if (char= |ch| #\Newline) 2 1)
         (let ((byte (,code->-name bits)))
           (cond
             ((null byte) (external-format-encoding-error stream bits))
             ((= bits 10)
              ;; FIXME: if we required that CODE->-NAME was required
              ;; to be callable at compile-time, then we could remove
              ;; the LOAD-TIME-VALUEs here and below.
              (setf (sap-ref-8 sap tail) (load-time-value (,code->-name 13) t))
              (setf (sap-ref-8 sap (1+ tail)) byte))
             (t (setf (sap-ref-8 sap tail) byte))))
         ((2 1)
          (cond
            ((= (- tail head) 1) 1) ; one octet away from EOF, can't possibly be CRLF
            ((and (= byte (load-time-value (,code->-name 13) t))
                  (= (sap-ref-8 sap (1+ head)) (load-time-value (,code->-name 10) t)))
             2)
            (t 1)))
         (if (= size 2)
             #\Newline
             (let ((code (,->code-name byte)))
               ,(if invalid-bytes-p
                    '(if code (code-char code) (return-from decode-break-reason 1))
                    '(code-char code))))
         ,->string/crlf-name
         ,string/crlf->name
         :char-encodable-p (,code->-name (char-code |ch|))
         :newline-variant :crlf)))

;;; ASCII

(declaim (inline code->ascii-mapper))
(defun code->ascii-mapper (code)
  (declare (optimize speed #.*safety-0*)
           (%char-code code))
  (if (> code 127) nil code))

(declaim (inline ascii->code-mapper))
(defun ascii->code-mapper (byte)
  (declare (optimize speed #.*safety-0*)
           (type (unsigned-byte 8) byte))
  (if (> byte 127) nil byte))

(define-unibyte-to-octets-functions :ascii get-ascii-bytes string->ascii code->ascii-mapper)
(define-unibyte-to-string-functions :ascii ascii->string ascii->code-mapper t)
(define-unibyte-external-format-with-newline-variants :ascii
    (:us-ascii :ansi_x3.4-1968 :iso-646 :iso-646-us :|646|)
  (ascii->code-mapper code->ascii-mapper)
  (ascii->string-aref string->ascii)
  (ascii->string/cr-aref string->ascii/cr)
  (ascii->string/crlf-aref string->ascii/crlf)
  t)

;;; Latin-1

;;; FIXME: we need this for the CLRF definition, but having this
;;; probably now implies that the check for (< CODE 256) in
;;; GET-LATIN-BYTES is redundant.
(declaim (inline code->latin1-mapper))
(defun code->latin1-mapper (code)
  (declare (optimize speed #.*safety-0*)
           (%char-code code))
  (and (< code 256) code))

(define-unibyte-to-octets-functions :latin-1 get-latin1-bytes string->latin1 code->latin1-mapper)
(define-unibyte-to-string-functions :latin-1 latin1->string identity)
(define-unibyte-external-format-with-newline-variants :latin-1 (:latin1 :iso-8859-1 :iso8859-1)
  (identity code->latin1-mapper)
  (latin1->string-aref string->latin1)
  (latin1->string/cr-aref string->latin1/cr)
  (latin1->string/crlf-aref string->latin1/crlf))

;;; UTF-8

;;; to UTF-8

(declaim (inline char-len-as-utf8))
(defun char-len-as-utf8 (code)
  (declare (optimize speed #.*safety-0*)
           (type (integer 0 (#.char-code-limit)) code))
  (cond ((< code 0) (unreachable))
        ((< code #x80) 1)
        ((< code #x800) 2)
        ((< code #x10000) 3)
        ((< code #x110000) 4)
        (t (unreachable))))

(macrolet ((ascii-bash (newline &optional (bytes nil bytesp))
             (declare (type (member :lf :cr :crlf) newline))
             (case newline
               ((:lf :cr)
                ;; KLUDGE: this depends on the fact that we know that
                ;; our arrays are initialized with zeros.
                `(let ((array (make-array (+ null-padding ,(if bytesp bytes '(- send sstart)))
                                          :element-type '(unsigned-byte 8))))
                   (loop for i from 0
                         and j from sstart below send
                         for code = (char-code (char string j))
                         do (setf (aref array i) ,(if (eql newline :cr) '(if (= code 10) 13 code) 'code)))
                   array))
               (:crlf
                `(let* ((utf8-length ,(if bytesp bytes '(+ (- send sstart) (loop for j from sstart below send count (char= (char string j) #\Newline)))))
                        (array (make-array (+ null-padding utf8-length)
                                           :element-type '(unsigned-byte 8))))
                   (loop for i from 0
                         and j from sstart below send
                         for code = (char-code (char string j))
                         do (setf (aref array i) (if (= code 10) 13 code))
                         if (= code 10) do (setf (aref array (incf i)) 10))
                   array))))
           (output-code (tag newline)
             (declare (type (member :lf :cr :crlf) newline))
             `(case (char-len-as-utf8 code)
                (1 ,@(case newline
                       (:lf '((add-byte code)))
                       (:cr '((add-byte (if (= code 10) 13 code))))
                       (:crlf '((add-byte (if (= code 10) 13 code))
                                (when (= code 10) (add-byte 10))))))
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
                   (add-byte (logior #x80 (ldb (byte 6 0) code))))))
           (def (name newline)
             `(defun ,name (string sstart send null-padding replacement)
                (declare (optimize (speed 3) #.*safety-0*)
                         (type simple-string string)
                         (type (integer 0 1) null-padding)
                         (type array-range sstart send))
                (etypecase string
                  ((simple-array character (*))
                   (let ((utf8-length 0)
                         ,@(when (eql newline :crlf) '((newline-count 0))))
                     ;; Since it has to fit in a vector, it must be a fixnum!
                     (declare (type (and unsigned-byte fixnum) utf8-length))
                     (loop for i of-type index from sstart below send
                           for code = (char-code (char string i))
                           do (incf utf8-length (char-len-as-utf8 code))
                           ,@(when (eql newline :crlf) '(if (= code 10) do (incf newline-count))))
                     (if (= utf8-length (- send sstart))
                         (ascii-bash ,newline ,(if (eql newline :crlf) '(+ newline-count utf8-length) 'utf8-length))
                         (let ((array (make-array (+ null-padding utf8-length ,@(when (eql newline :crlf) '(newline-count)))
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
                                      do (output-code :first-error ,newline)
                                      finally (return-from ,name array)))
                            :first-error
                              (setf new-array (make-array (* index 2) :adjustable t
                                                          :element-type '(unsigned-byte 8)
                                                          :fill-pointer index))
                              (replace new-array array)
                            :error
                              (let ((replacement (encoding-error
                                                  ,(if (eql newline :lf)
                                                       :utf-8
                                                       `'(:utf-8 :newline ,newline))
                                                  replacement string index)))
                                (flet ((add-byte (b) (vector-push-extend b new-array)))
                                  (dotimes (i (length replacement))
                                    (add-byte (aref replacement i)))
                                  (loop for i of-type index from (1+ error-position) below send
                                        for code = (char-code (char string i))
                                        do (output-code :error ,newline)
                                        finally (return-from ,name
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
                   (ascii-bash ,newline))))))
  (def string->utf8 :lf)
  (def string/cr->utf8 :cr)
  (def string/crlf->utf8 :crlf))

;;; from UTF-8

(macrolet ((def (crlfp newline definer-name base-name)
             `(progn
                (defmacro ,definer-name (accessor type)
                  (let ((name (make-od-name ',base-name accessor)))
                    `(progn
                       ;;(declaim (inline ,name))
                       (let ((lexically-max
                              (string->utf8 (string (code-char ,(1- char-code-limit)))
                                            0 1 0 nil)))
                         (declare (type (simple-array (unsigned-byte 8) (#+sb-unicode 4 #-sb-unicode 2)) lexically-max))
                         (defun ,name (array pos end replacement)
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
                                 (cond
                                   ,@,(when crlfp
                                        ``(((= initial-byte 13)
                                            (values
                                             (if (and (> remaining-bytes 1)
                                                      (= (,accessor array (+ pos 1)) 10))
                                                 2
                                                 1)
                                             nil))))
                                   ((eql maybe-len 1)
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
                                      (let ((replacement (decoding-error array pos bad-end
                                                                         ,,(if newline ``'(:utf-8 :newline ,,newline) '':utf-8)
                                                                         replacement reject-reason reject-position)))
                                        (values bad-len replacement)))))))))))))
                (instantiate-octets-definition ,definer-name))))
  (def nil nil define-bytes-per-utf8-character bytes-per-utf8-character)
  (def nil :cr define-bytes-per-utf8-character/clr bytes-per-utf8-character/cr)
  (def t :crlf define-bytes-per-utf8-character/crlf bytes-per-utf8-character/crlf))

(macrolet ((def (newline definer-name base-name)
             `(progn
                (defmacro ,definer-name (accessor type)
                  (let ((name (make-od-name ',base-name accessor)))
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
                                        (1 ,(if (eql ,newline :cr)
                                                '(let ((code (cref 0)))
                                                   (if (= code 13) 10 code))
                                                '(cref 0)))
                                        (2 ,(if (eql ,newline :crlf)
                                                '(let ((code (cref 0)))
                                                   (if (= code 13)
                                                       10
                                                       (logior (ash (ldb (byte 5 0) (cref 0)) 6)
                                                               (ldb (byte 6 0) (cref 1)))))
                                                '(logior (ash (ldb (byte 5 0) (cref 0)) 6)
                                                               (ldb (byte 6 0) (cref 1)))))
                                        (3 (logior (ash (ldb (byte 4 0) (cref 0)) 12)
                                                   (ash (ldb (byte 6 0) (cref 1)) 6)
                                                   (ldb (byte 6 0) (cref 2))))
                                        (4 (logior (ash (ldb (byte 3 0) (cref 0)) 18)
                                                   (ash (ldb (byte 6 0) (cref 1)) 12)
                                                   (ash (ldb (byte 6 0) (cref 2)) 6)
                                                   (ldb (byte 6 0) (cref 3)))))))))))
                (instantiate-octets-definition ,definer-name))))
  (def :lf define-simple-get-utf8-char simple-get-utf8-char)
  (def :cr define-simple-get-utf8-char/cr simple-get-utf8-char/cr)
  (def :crlf define-simple-get-utf8-char/crlf simple-get-utf8-char/crlf))

(macrolet ((def (newline definer-name base-name bytes-per-base-name getter-base-name)
             (declare (ignore newline))
             `(progn
                (defmacro ,definer-name (accessor type)
                  (let ((name (make-od-name ',base-name accessor)))
                    `(progn
                       (defun ,name (array astart aend replacement)
                         (declare (optimize speed #.*safety-0*)
                                  (type ,type array)
                                  (type array-range astart aend))
                         (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
                           (loop with pos = astart
                                 while (< pos aend)
                                 do (multiple-value-bind (bytes invalid)
                                        (,(make-od-name ',bytes-per-base-name accessor) array pos aend replacement)
                                      (declare (type (or null string) invalid))
                                      (cond
                                        ((null invalid)
                                         (vector-push-extend (,(make-od-name ',getter-base-name accessor) array pos bytes) string))
                                        (t
                                         (dotimes (i (length invalid))
                                           (vector-push-extend (char invalid i) string))))
                                      (incf pos bytes)))
                           (coerce string 'simple-string))))))
                (instantiate-octets-definition ,definer-name))))
  (def :lf define-utf8->string utf8->string bytes-per-utf8-character simple-get-utf8-char)
  (def :cr define-utf8->string/cr utf8->string/cr bytes-per-utf8-character simple-get-utf8-char/cr)
  (def :crlf define-utf8-string/crlf utf8->string/crlf bytes-per-utf8-character/crlf simple-get-utf8-char/crlf))

#+(and sb-unicode 64-bit little-endian
       (not (or arm64 x86-64))) ;; have true simd definitions
(defun sb-vm::simd-copy-utf8-to-character-string (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((head (buffer-head ibuf))
           (tail (buffer-tail ibuf))
           (sap (buffer-sap ibuf))
           (left (- end start))
           (result-characters (logand left (- sb-vm:n-word-bytes)))
           (string-bytes (logand (- tail head) (- sb-vm:n-word-bytes)))
           (n (min result-characters string-bytes))
           (repeat (ldb (byte sb-vm:n-word-bits 0) #x0101010101010101))
           (ascii-mask (* 128 repeat))
           (string-sap (vector-sap string))
           (string-offset (truly-the fixnum (* start 4))))
      (declare (optimize sb-c::preserve-single-use-debug-variables
                         sb-c::preserve-constants))
      (loop for ibuf-offset from head below (+ head n) by sb-vm:n-word-bytes
            do
            (let ((word (sap-ref-word sap ibuf-offset)))
              (when (logtest word ascii-mask)
                (setf head ibuf-offset)
                (return))
              (setf (sap-ref-word string-sap string-offset)
                    (dpb (ldb (byte 8 8) word)
                         (byte 8 32)
                         (ldb (byte 8 0) word))
                    (sap-ref-word string-sap (+ string-offset 8))
                    (dpb (ldb (byte 8 24) word)
                         (byte 8 32)
                         (ldb (byte 8 16) word))
                    (sap-ref-word string-sap (+ string-offset 16))
                    (dpb (ldb (byte 8 40) word)
                         (byte 8 32)
                         (ldb (byte 8 32) word))
                    (sap-ref-word string-sap (+ string-offset 24))
                    (dpb (ldb (byte 8 56) word)
                         (byte 8 32)
                         (ldb (byte 8 48) word))))
            (incf string-offset (* 4 sb-vm:n-word-bytes))
            finally (incf head n))
      (setf (buffer-head ibuf) head)
      (truly-the index (values (truncate string-offset 4))))))

#+(and sb-unicode 64-bit little-endian)
(defun sb-vm::simd-copy-utf8-crlf-to-character-string-with-size (start end string ibuf size-buffer)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string size-buffer)
    (let* ((head (buffer-head ibuf))
           (tail (buffer-tail ibuf))
           (sap (buffer-sap ibuf))
           (n (logand (min (- end start)
                           (- (- tail head) 1))
                      (- 2)))
           (repeat (ldb (byte 16 0) #x0101010101010101))
           (ascii-mask (* 128 repeat))
           (string-sap (vector-sap string))
           (size-sap (vector-sap size-buffer))
           (size-offset start)
           (string-offset (* start 4))
           (end (+ head n)))
      (declare (index string-offset size-offset))
      (declare (optimize sb-c::preserve-single-use-debug-variables
                         sb-c::preserve-constants))
      (loop while (< head end)
            do
            (let ((word (sap-ref-16 sap head)))
              (when (logtest word ascii-mask)
                (return))
              (cond ((= word #x0A0D)
                     (setf (sap-ref-32 string-sap string-offset) 10
                           (sap-ref-8 size-sap size-offset) 2)
                     (incf head 2)
                     (incf string-offset 4)
                     (incf size-offset 1))
                    ((= (ash word -8) 13)
                     (setf (sap-ref-32 string-sap string-offset) (ldb (byte 8 0) word)
                           (sap-ref-8 size-sap size-offset) 1)
                     (incf head 1)
                     (incf string-offset 4)
                     (incf size-offset 1))
                    (t
                     (setf (sap-ref-64 string-sap string-offset)
                           (dpb (ldb (byte 8 8) word)
                                (byte 32 32)
                                (ldb (byte 8 0) word))
                           (sap-ref-16 size-sap size-offset) #x0101)
                     (incf head 2)
                     (incf string-offset 8)
                     (incf size-offset 2)))))
      (setf (buffer-head ibuf) head)
      size-offset)))

#+(and sb-unicode 64-bit little-endian
       (not (or arm64 x86-64))) ;; have true simd definitions
(defun sb-vm::simd-copy-utf8-to-base-string (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((head (buffer-head ibuf))
           (tail (buffer-tail ibuf))
           (sap (buffer-sap ibuf))
           (n (logand (min (- end start)
                           (- tail head))
                      (- sb-vm:n-word-bytes)))
           (repeat (ldb (byte sb-vm:n-word-bits 0) #x0101010101010101))
           (ascii-mask (* 128 repeat))
           (string-sap (vector-sap string))
           (string-offset start))
      (declare (optimize sb-c::preserve-single-use-debug-variables
                         sb-c::preserve-constants))
      (loop for ibuf-offset from head below (+ head n) by sb-vm:n-word-bytes
            do
            (let ((word (sap-ref-word sap ibuf-offset)))
              (when (logtest word ascii-mask)
                (setf head ibuf-offset)
                (return))
              (setf (sap-ref-word string-sap string-offset) word))
            (incf string-offset sb-vm:n-word-bytes)
            finally (incf head n))
      (setf (buffer-head ibuf) head)
      (truly-the index string-offset))))

(defmacro utf8-char-loop (&key size-buffer
                               crlf
                               (eof t))
  `(do ()
       ((or (= tail head)
            (= index end)))
     (let ((byte (sap-ref-8 sap head)))
       (setf (aref string index)
             (code-char
              (cond
                ,@(and crlf
                       `(((and (= byte 13)
                               (let ((new-head (+ head 2)))
                                 (cond ((and (<= new-head tail)
                                             (= (sap-ref-8 sap (1+ head)) 10))
                                        (incf head 2)
                                        ,@(when size-buffer
                                            `((setf (aref size-buffer index) 2)))
                                        10)
                                       ,@(and eof
                                          `((eof nil)))
                                       (t
                                        ,@(and eof
                                               `((setf requested-refill t)))
                                        (return))))))))
                ((< byte 128)
                 (incf head)
                 ,@(when size-buffer
                     `((setf (aref size-buffer index) 1)))
                 byte)
                ((< byte 194)
                 (decode-break 1))
                ((< byte 224)
                 (let ((new-head (+ head 2)))
                   (when (> new-head tail)
                     ,@(and eof
                            `((setf incomplete t)))
                     (return))
                   (prog1
                       (let ((byte2 (sap-ref-8 sap (1+ head))))
                         (unless (<= 128 byte2 191)
                           (decode-break 2))
                         (dpb byte (byte 5 6) byte2))
                     ,@(when size-buffer
                         `((setf (aref size-buffer index) 2)))
                     (setf head new-head))))
                ((< byte 240)
                 (let ((new-head (+ head 3)))
                   (when (> new-head tail)
                     ,@(and eof
                            `((setf incomplete t)))
                     (return))
                   (prog1
                       (let ((byte2 (sap-ref-8 sap (1+ head)))
                             (byte3 (sap-ref-8 sap (+ 2 head))))
                         (unless
                             (and (<= 128 byte2 191)
                                  (<= 128 byte3 191)
                                  (or (/= byte 224) (<= 160 byte2 191))
                                  (or (/= byte 237) (<= 128 byte2 159)))
                           (decode-break 3))
                         (dpb byte (byte 4 12)
                              (dpb byte2 (byte 6 6) byte3)))
                     ,@(when size-buffer
                         `((setf (aref size-buffer index) 3)))
                     (setf head new-head))))
                (t
                 (let ((new-head (+ head 4)))
                   (when (> new-head tail)
                     ,@(and eof
                            `((setf incomplete t)))
                     (return))
                   (prog1
                       (let ((byte2 (sap-ref-8 sap (1+ head)))
                             (byte3 (sap-ref-8 sap (+ 2 head)))
                             (byte4 (sap-ref-8 sap (+ 3 head))))
                         (unless
                             (and (<= 128 byte2 191)
                                  (<= 128 byte3 191)
                                  (<= 128 byte4 191)
                                  (or (/= byte 240) (<= 144 byte2 191))
                                  (or (/= byte 244) (<= 128 byte2 143)))
                           (decode-break 4))
                         (dpb byte (byte 3 18)
                              (dpb byte2 (byte 6 12)
                                   (dpb byte3 (byte 6 6) byte4))))
                     ,@(when size-buffer
                         `((setf (aref size-buffer index) 4)))
                     (setf head new-head)))))))
       (incf index))
     (setf (buffer-head ibuf) head)))

(defun fd-stream-read-n-characters/utf-8 (stream string size-buffer start end &aux (index start))
  (declare (type fd-stream stream)
           (type index start end index)
           (type ansi-stream-cin-buffer string)
           (type ansi-stream-csize-buffer size-buffer))
  (when (fd-stream-eof-forced-p stream)
    (setf (fd-stream-eof-forced-p stream) nil)
    (return-from fd-stream-read-n-characters/utf-8 start))
  (do ((instead (fd-stream-instead stream)))
      ((= (fill-pointer instead) 0)
       (setf (fd-stream-listen stream) nil))
    (setf (aref string index) (vector-pop instead))
    (setf (aref size-buffer index) 0)
    (incf index)
    (when (= end index)
      (when (= (fill-pointer instead) 0)
        (setf (fd-stream-listen stream) nil))
      (return-from fd-stream-read-n-characters/utf-8 index)))
  (block outer
    (do ()
        (())
      #+(and sb-unicode 64-bit little-endian)
      (let ((new-index (sb-vm::simd-copy-utf8-to-character-string index end string (fd-stream-ibuf stream))))
        ;; Make sure to change this 1 whenever
        ;; simd-copy-utf8-to-character-string starts processing more than
        ;; just ascii characters.
        (fill size-buffer 1 :start index :end new-index)
        (setf index new-index))
      (let* ((ibuf (fd-stream-ibuf stream))
             (head (buffer-head ibuf))
             (tail (buffer-tail ibuf))
             (sap (buffer-sap ibuf)))
        (declare (type index head tail))
        (flet ((decode-break (reason)
                 (setf (buffer-head ibuf) head)
                 (unless (> index start)
                   (stream-decoding-error-and-handle stream reason 1))
                 (return-from outer index)))
          (utf8-char-loop :size-buffer t
                          :eof nil))
        (when (or (> index start)
                  (null (catch 'eof-input-catcher (refill-input-buffer stream))))
          (return index))))))

(defun fd-stream-read-n-characters/utf-8/crlf (stream string size-buffer start end &aux (index start))
  (declare (type fd-stream stream)
           (type index start end index)
           (type ansi-stream-cin-buffer string)
           (type ansi-stream-csize-buffer size-buffer))
  (when (fd-stream-eof-forced-p stream)
    (setf (fd-stream-eof-forced-p stream) nil)
    (return-from fd-stream-read-n-characters/utf-8/crlf start))
  (do ((instead (fd-stream-instead stream)))
      ((= (fill-pointer instead) 0)
       (setf (fd-stream-listen stream) nil))
    (setf (aref string index) (vector-pop instead))
    (setf (aref size-buffer index) 0)
    (incf index)
    (when (= end index)
      (when (= (fill-pointer instead) 0)
        (setf (fd-stream-listen stream) nil))
      (return-from fd-stream-read-n-characters/utf-8/crlf index)))
  (block outer
    (do ()
        (())
      #+(and sb-unicode 64-bit little-endian)
      (setf index
            (sb-vm::simd-copy-utf8-crlf-to-character-string-with-size index end string (fd-stream-ibuf stream) size-buffer))
      (let* ((ibuf (fd-stream-ibuf stream))
             (head (buffer-head ibuf))
             (tail (buffer-tail ibuf))
             (sap (buffer-sap ibuf)))
        (declare (type index head tail))
        (flet ((decode-break (reason)
                 (setf (buffer-head ibuf) head)
                 (unless (> index start)
                   (stream-decoding-error-and-handle stream reason 1))
                 (return-from outer index)))
          (utf8-char-loop :size-buffer t :crlf t :eof nil))
        (when (or (> index start)
                  (null (catch 'eof-input-catcher (refill-input-buffer stream))))
          (return index))))))

;;; Bypass the character buffer, the caller must ensure that it's empty
(defun fd-stream-read-sequence/utf-8-to-string (stream string start end &aux (index start))
  (declare (type fd-stream stream)
           (type index start end index)
           (type (simple-array character (*)) string))
  (block outer
    (loop
     (do ((instead (fd-stream-instead stream)))
         ((= (fill-pointer instead) 0)
          (setf (fd-stream-listen stream) nil))
       (setf (aref string index) (vector-pop instead))
       (incf index)
       (when (= index end)
         (when (= (fill-pointer instead) 0)
           (setf (fd-stream-listen stream) nil))
         (return index)))
     #+(and sb-unicode 64-bit little-endian)
     (setf index
           (sb-vm::simd-copy-utf8-to-character-string index end string (fd-stream-ibuf stream)))
     (let* ((ibuf (fd-stream-ibuf stream))
            (head (buffer-head ibuf))
            (tail (buffer-tail ibuf))
            (sap (buffer-sap ibuf))
            incomplete)
       (declare (type index head tail))
       (flet ((decode-break (reason)
                (setf (buffer-head ibuf) head)
                (when (stream-decoding-error-and-handle stream reason 1)
                  (return-from outer index))
                (return)))
         (utf8-char-loop))
       (when (= index end)
         (setf (buffer-head ibuf) head)
         (return index))
       (unless (catch 'eof-input-catcher (refill-input-buffer stream))
         (when (or (not incomplete)
                   (stream-decoding-error-and-handle stream (- tail head) 1))
           (return index)))))))

#+sb-unicode
(defun fd-stream-read-sequence/utf-8-to-base-string (stream string start end &aux (index start))
  (declare (type fd-stream stream)
           (type index start end index)
           (type simple-base-string string))
  (block outer
    (loop
     (do ((instead (fd-stream-instead stream)))
         ((= (fill-pointer instead) 0)
          (setf (fd-stream-listen stream) nil))
       (setf (aref string index) (vector-pop instead))
       (incf index)
       (when (= index end)
         (when (= (fill-pointer instead) 0)
           (setf (fd-stream-listen stream) nil))
         (return index)))
     #+(and sb-unicode 64-bit little-endian)
     (setf index
           (sb-vm::simd-copy-utf8-to-base-string index end string (fd-stream-ibuf stream)))
     (let* ((ibuf (fd-stream-ibuf stream))
            (head (buffer-head ibuf))
            (tail (buffer-tail ibuf))
            (sap (buffer-sap ibuf))
            incomplete)
       (declare (type index head tail))
       (flet ((decode-break (reason)
                (setf (buffer-head ibuf) head)
                (when (stream-decoding-error-and-handle stream reason 1)
                  (return-from outer index))
                (return)))
         (utf8-char-loop))
       (when (= index end)
         (setf (buffer-head ibuf) head)
         (return index))
       (unless (catch 'eof-input-catcher (refill-input-buffer stream))
         (when (or (not incomplete)
                   (stream-decoding-error-and-handle stream (- tail head) 1))
           (return index)))))))

#+(and sb-unicode 64-bit little-endian
       (not (or arm64 x86-64)))
(defun sb-vm::simd-copy-character-string-to-utf8 (start end string obuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((tail (buffer-tail obuf))
           (sap (buffer-sap obuf))
           (buffer-left (- (buffer-length obuf) tail))
           (string-left (- end start))
           (n (logand (min buffer-left string-left) -2))
           (ascii-mask (ldb (byte 64 0)
                            (lognot (logior 127 (ash 127 32)))))
           (string-sap (vector-sap string))
           (string-offset (truly-the fixnum (* start 4)))
           (last-newline -1))
      (declare (optimize sb-c::preserve-single-use-debug-variables
                         sb-c::preserve-constants))
      (loop for obuf-offset from tail below (+ tail n) by 2
            do
            (let ((word (sap-ref-word string-sap string-offset)))
              (when (logtest word ascii-mask)
                (setf tail obuf-offset)
                (return))
              (let ((a (ldb (byte 8 0) word))
                    (b (ash word -24)))
                (when (= a 10)
                  (setf last-newline string-offset))
                (when (= b (ash 10 8))
                  (setf last-newline (truly-the index (+ string-offset 4))))
                (incf string-offset 8)
                (setf (sap-ref-16 sap obuf-offset)
                      (logior a b))))
            finally (incf tail n))
      (setf (buffer-tail obuf) tail)
      (values (truly-the index (values (truncate string-offset 4)))
              (ash (truly-the fixnum last-newline) -2)))))

(defun output-bytes/utf-8/lf (stream string start* end)
  (declare (optimize (sb-c:verify-arg-count 0)))
  (let ((start (or start* 0))
        (end (or end (length string)))
        (last-newline -1))
    (declare (type index start end))
    (when (fd-stream-synchronize-output stream)
      (synchronize-stream-output stream))
    (unless (<= 0 start end (length string))
      (sequence-bounding-indices-bad-error string start end))
    (do ()
        ((= end start))
      (let ((obuf (fd-stream-obuf stream)))
        (string-dispatch (simple-base-string (simple-array character (*)) string)
                         string
          #+(and sb-unicode 64-bit little-endian)
          (when (and (typep string '(simple-array character (*)))
                     (>= (- end start) 16))
            (multiple-value-bind (new-start newline)
                (truly-the (values index fixnum &optional) (sb-vm::simd-copy-character-string-to-utf8 start end string obuf))
              (setf start new-start)
              (when (>= newline 0)
                (setf last-newline newline))))
          (let ((len (- (buffer-length obuf) 4))
                (sap (buffer-sap obuf))
                (tail (buffer-tail obuf)))
            (declare (type index tail)
                     (optimize (safety 0)))
            (block output-nothing
              (do* ()
                   ((or (= start end) (>= tail len)))
                (let* ((|ch| (aref string start))
                       (bits (char-code |ch|)))
                  (declare (ignorable bits))
                  (when (char= |ch| #\Newline)
                    (setf last-newline start))
                  (cond ((< bits 128)
                         (setf (sap-ref-8 sap tail) bits) (incf tail))
                        ((< bits 2048)
                         (setf (sap-ref-8 sap tail) (logior 192 (ldb (byte 5 6) bits))
                               (sap-ref-8 sap (+ 1 tail)) (logior 128 (ldb (byte 6 0) bits)))
                         (incf tail 2))
                        ((< bits 65536)
                         (when (<= 55296 bits 57343)
                           (external-format-encoding-error stream bits))
                         (setf (sap-ref-8 sap tail) (logior 224 (ldb (byte 4 12) bits))
                               (sap-ref-8 sap (+ 1 tail)) (logior 128 (ldb (byte 6 6) bits))
                               (sap-ref-8 sap (+ 2 tail)) (logior 128 (ldb (byte 6 0) bits)))
                         (incf tail 3))
                        (t
                         (setf (sap-ref-8 sap tail) (logior 240 (ldb (byte 3 18) bits))
                               (sap-ref-8 sap (+ 1 tail)) (logior 128 (ldb (byte 6 12) bits))
                               (sap-ref-8 sap (+ 2 tail)) (logior 128 (ldb (byte 6 6) bits))
                               (sap-ref-8 sap (+ 3 tail)) (logior 128 (ldb (byte 6 0) bits)))
                         (incf tail 4)))
                  (setf (buffer-tail obuf) tail)
                  (incf start)))
              (go flush))
            (incf start))))
     flush
      (when (< start end)
        (flush-output-buffer stream)))
    (ecase (fd-stream-buffering stream)
      (:full)
      (:line
       (when (>= last-newline 0)
         (flush-output-buffer stream)))
      (:none
       (flush-output-buffer stream)))
    (if (>= last-newline 0)
        (setf (fd-stream-output-column stream) (- end last-newline 1))
        (incf (fd-stream-output-column stream) (- end (truly-the index start*))))))

(define-external-format/variable-width (:utf-8 :utf8) t
  #+sb-unicode (code-char #xfffd) #-sb-unicode #\?
  (let ((bits (char-code |ch|)))
    (cond ((< bits #x80) 1)
          ((< bits #x800) 2)
          ((< bits #x10000) 3)
          (t 4)))
  (cond
    ((< bits #x80)
     (setf (sap-ref-8 sap tail) bits)
     (incf tail))
    ((< bits #x800)
     (setf (sap-ref-8 sap tail)       (logior #xc0 (ldb (byte 5 6) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 2))
    ((< bits #x10000)
     (when (<= #xd800 bits #xdfff)
       (external-format-encoding-error stream bits))
     (setf (sap-ref-8 sap tail)       (logior #xe0 (ldb (byte 4 12) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 6) bits))
           (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 3))
    (t
     (setf (sap-ref-8 sap tail)       (logior #xf0 (ldb (byte 3 18) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 12) bits))
           (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 6) bits))
           (sap-ref-8 sap (+ 3 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 4)))
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
  #+sb-unicode :base-string-direct-mapping #+sb-unicode t
  :fd-stream-read-n-characters fd-stream-read-n-characters/utf-8
  :write-n-bytes-fun output-bytes/utf-8/lf
  :char-encodable-p (let ((bits (char-code |ch|))) (not (<= #xd800 bits #xdfff)))
  :handle-size nil)

(define-external-format/variable-width (:utf-8) t
  #+sb-unicode (code-char #xfffd) #-sb-unicode #\?
  (let ((bits (char-code |ch|)))
    (cond ((< bits #x80) 1)
          ((< bits #x800) 2)
          ((< bits #x10000) 3)
          (t 4)))
  (cond
    ((< bits #x80)
     (setf (sap-ref-8 sap tail) (if (= bits 10) 13 bits))
     (incf tail))
    ((< bits #x800)
     (setf (sap-ref-8 sap tail)       (logior #xc0 (ldb (byte 5 6) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 2))
    ((< bits #x10000)
     (when (<= #xd800 bits #xdfff)
       (external-format-encoding-error stream bits))
     (setf (sap-ref-8 sap tail)       (logior #xe0 (ldb (byte 4 12) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 6) bits))
           (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 3))
    (t
     (setf (sap-ref-8 sap tail)       (logior #xf0 (ldb (byte 3 18) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 12) bits))
           (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 6) bits))
           (sap-ref-8 sap (+ 3 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 4)))
  (1 (cond ((< byte #x80) 1)
           ((< byte #xc2) (return-from decode-break-reason 1))
           ((< byte #xe0) 2)
           ((< byte #xf0) 3)
           (t 4)))
  (code-char (ecase size
               (1 (if (= byte 13) 10 byte))
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
  utf8->string/cr-aref
  string/cr->utf8
  :char-encodable-p (let ((bits (char-code |ch|))) (not (<= #xd800 bits #xdfff)))
  :newline-variant :cr
  :handle-size nil)

(define-external-format/variable-width (:utf-8 :utf8) t
  #+sb-unicode (code-char #xfffd) #-sb-unicode #\?
  (let ((bits (char-code |ch|)))
    (cond ((= bits 10) 2)
          ((< bits #x80) 1)
          ((< bits #x800) 2)
          ((< bits #x10000) 3)
          (t 4)))
  (cond
    ((= bits 10)
     (setf (sap-ref-8 sap tail) 13
           (sap-ref-8 sap (+ 1 tail)) 10)
     (incf tail 2))
    ((< bits #x80)
     (setf (sap-ref-8 sap tail) (if (= bits 10) 13 bits))
     (incf tail))
    ((< bits #x800)
     (setf (sap-ref-8 sap tail)       (logior #xc0 (ldb (byte 5 6) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 2))
    ((< bits #x10000)
     (when (<= #xd800 bits #xdfff)
       (external-format-encoding-error stream bits))
     (setf (sap-ref-8 sap tail)       (logior #xe0 (ldb (byte 4 12) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 6) bits))
           (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 3))
    (t
     (setf (sap-ref-8 sap tail)       (logior #xf0 (ldb (byte 3 18) bits))
           (sap-ref-8 sap (+ 1 tail)) (logior #x80 (ldb (byte 6 12) bits))
           (sap-ref-8 sap (+ 2 tail)) (logior #x80 (ldb (byte 6 6) bits))
           (sap-ref-8 sap (+ 3 tail)) (logior #x80 (ldb (byte 6 0) bits)))
     (incf tail 4)))
  ((2 1)
   (cond
     ((< byte #x80)
      (if (and (= byte 13)
               (> (- tail head) 1)
               (= (sap-ref-8 sap (+ head 1)) 10))
          2
          1))
     ((< byte #xc2) (return-from decode-break-reason 1))
     ((< byte #xe0) 2)
     ((< byte #xf0) 3)
     (t 4)))
  (code-char (ecase size
               (1 byte)
               (2 (if (= byte 13)
                      10
                      (let ((byte2 (sap-ref-8 sap (1+ head))))
                        (unless (<= #x80 byte2 #xbf)
                          (return-from decode-break-reason 2))
                        (dpb byte (byte 5 6) byte2))))
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
  utf8->string/crlf-aref
  string/crlf->utf8
  :char-encodable-p (let ((bits (char-code |ch|))) (not (<= #xd800 bits #xdfff)))
  :fd-stream-read-n-characters fd-stream-read-n-characters/utf-8/crlf
  :newline-variant :crlf
  :handle-size nil)

#+(and sb-unicode 64-bit little-endian
       (not arm64)) ;; have true simd definitions or might be redefined with def-variant
(defun sb-vm::simd-copy-utf8-crlf-to-base-string (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((head (buffer-head ibuf))
           (tail (buffer-tail ibuf))
           (sap (buffer-sap ibuf))
           (n (logand (min (- end start)
                           (- (- tail head) 1))
                      (- 2)))
           (repeat (ldb (byte 16 0) #x0101010101010101))
           (ascii-mask (* 128 repeat))
           (string-sap (vector-sap string))
           (string-start start)
           (string-offset string-start)
           (end (+ head n)))
      (declare (optimize sb-c::preserve-single-use-debug-variables
                         sb-c::preserve-constants))
      (loop while (< head end)
            do
            (let ((word (sap-ref-16 sap head)))
              (when (logtest word ascii-mask)
                (return))
              (cond ((= word #x0A0D)
                     (setf (sap-ref-8 string-sap string-offset) 10)
                     (incf head 2)
                     (incf string-offset 1))
                    ((= (ash word -8) 13)
                     (setf (sap-ref-8 string-sap string-offset) word)
                     (incf head 1)
                     (incf string-offset 1))
                    (t
                     (setf (sap-ref-16 string-sap string-offset) word)
                     (incf head 2)
                     (incf string-offset 2)))))
      (setf (buffer-head ibuf) head)
      (truly-the index string-offset))))

#+(and sb-unicode 64-bit little-endian
       (not arm64)) ;; have true simd definitions or might be redefined with def-variant
(defun sb-vm::simd-copy-utf8-crlf-to-character-string (start end string ibuf)
  (declare (type index start end)
           (optimize speed (safety 0)))
  (with-pinned-objects (string)
    (let* ((head (buffer-head ibuf))
           (tail (buffer-tail ibuf))
           (sap (buffer-sap ibuf))
           (n (logand (min (- end start)
                           (- (- tail head) 1))
                      (- 2)))
           (repeat (ldb (byte 16 0) #x0101010101010101))
           (ascii-mask (* 128 repeat))
           (string-sap (vector-sap string))
           (string-offset (* start 4))
           (end (+ head n)))
      (declare (optimize sb-c::preserve-single-use-debug-variables
                         sb-c::preserve-constants))
      (loop while (< head end)
            do
            (let ((word (sap-ref-16 sap head)))
              (when (logtest word ascii-mask)
                (return))
              (cond ((= word #x0A0D)
                     (setf (sap-ref-32 string-sap string-offset) 10)
                     (incf head 2)
                     (incf string-offset 4))
                    ((= (ash word -8) 13)
                     (setf (sap-ref-32 string-sap string-offset) (ldb (byte 8 0) word))
                     (incf head 1)
                     (incf string-offset 4))
                    (t
                     (setf (sap-ref-64 string-sap string-offset)
                           (dpb (ldb (byte 8 8) word)
                                (byte 32 32)
                                (ldb (byte 8 0) word)))
                     (incf head 2)
                     (incf string-offset 8)))))
      (setf (buffer-head ibuf) head)
      (truly-the index (values (truncate string-offset 4))))))

(defun fd-stream-read-sequence/utf-8-crlf-to-character-string (stream string start end)
  (declare (type fd-stream stream)
           (type index start end)
           (type (simple-array character (*)) string))
  (let (eof
        (index start))
    (declare (index index))
    (block outer
      (loop
       (do ((instead (fd-stream-instead stream)))
           ((= (fill-pointer instead) 0)
            (setf (fd-stream-listen stream) nil))
         (setf (aref string index) (vector-pop instead))
         (incf index)
         (when (= index end)
           (when (= (fill-pointer instead) 0)
             (setf (fd-stream-listen stream) nil))
           (return index)))
       #+(and sb-unicode 64-bit little-endian)
       (setf index
             (sb-vm::simd-copy-utf8-crlf-to-character-string index end string (fd-stream-ibuf stream)))
       (let* ((ibuf (fd-stream-ibuf stream))
              (head (buffer-head ibuf))
              (tail (buffer-tail ibuf))
              (sap (buffer-sap ibuf))
              requested-refill
              incomplete)
         (declare (type index head tail))
         (flet ((decode-break (reason)
                  (setf (buffer-head ibuf) head)
                  (when (stream-decoding-error-and-handle stream reason 1)
                    (return-from outer index))
                  (return)))
           (utf8-char-loop :crlf t))
         (when (or (= end index)
                   eof)
           (setf (buffer-head ibuf) head)
           (return index))
         (unless (catch 'eof-input-catcher (refill-input-buffer stream))
           (if requested-refill
               (setf eof t)
               (when (or (not incomplete)
                         (stream-decoding-error-and-handle stream (- tail head) 1))
                 (return index)))))))))

(defun fd-stream-read-sequence/utf-8-crlf-to-base-string (stream string start end)
  (declare (type fd-stream stream)
           (type index start end)
           (type simple-base-string string))
  (let (eof
        (index start))
    (declare (index index))
    (block outer
      (loop
       (do ((instead (fd-stream-instead stream)))
           ((= (fill-pointer instead) 0)
            (setf (fd-stream-listen stream) nil))
         (setf (aref string index) (vector-pop instead))
         (incf index)
         (when (= index end)
           (when (= (fill-pointer instead) 0)
             (setf (fd-stream-listen stream) nil))
           (return index)))
       #+(and sb-unicode 64-bit little-endian)
       (setf index
             (sb-vm::simd-copy-utf8-crlf-to-base-string index end string (fd-stream-ibuf stream)))
       (let* ((ibuf (fd-stream-ibuf stream))
              (head (buffer-head ibuf))
              (tail (buffer-tail ibuf))
              (sap (buffer-sap ibuf))
              requested-refill
              incomplete)
         (declare (type index head tail))
         (flet ((decode-break (reason)
                  (setf (buffer-head ibuf) head)
                  (when (stream-decoding-error-and-handle stream reason 1)
                    (return-from outer index))
                  (return)))
           (utf8-char-loop :crlf t))
         (when (or (= end index)
                   eof)
           (setf (buffer-head ibuf) head)
           (return index))
         (unless (catch 'eof-input-catcher (refill-input-buffer stream))
           (if requested-refill
               (setf eof t)
               (when (or (not incomplete)
                         (stream-decoding-error-and-handle stream (- tail head) 1))
                 (return index)))))))))
