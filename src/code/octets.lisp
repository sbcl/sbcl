;;;; code for string to octet conversion

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; FIXME: The latin9 stuff is currently #!+sb-unicode, because I
;;; don't like the idea of trying to do CODE-CHAR #x<big>.  Is that a
;;; justified fear?  Can we arrange that it's caught and converted to
;;; a decoding error error?  Or should we just give up on non-Unicode
;;; builds?

(in-package "SB!IMPL")

;;; FIXME: don't we have this somewhere else?
(deftype array-range ()
  "A number that can represent an index into a vector, including
one-past-the-end"
  '(integer 0 #.sb!xc:array-dimension-limit))

;;;; conditions

;;; encoding condition

(define-condition octets-encoding-error (character-encoding-error)
  ((string :initarg :string :reader octets-encoding-error-string)
   (position :initarg :position :reader octets-encoding-error-position)
   (external-format :initarg :external-format
                    :reader octets-encoding-error-external-format))
  (:report (lambda (c s)
             (format s "Unable to encode character ~A as ~S."
                     (char-code (char (octets-encoding-error-string c)
                                      (octets-encoding-error-position c)))
                     (octets-encoding-error-external-format c)))))

(defun read-replacement-character ()
  (format *query-io*
          "Replacement byte, bytes, character, or string (evaluated): ")
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun encoding-error (external-format string pos)
  (restart-case
      (error 'octets-encoding-error
             :external-format external-format
             :string string
             :position pos)
    (use-value (replacement)
      :report "Supply a set of bytes to use in place of the invalid one."
      :interactive read-replacement-character
      (typecase replacement
        ((unsigned-byte 8)
         (make-array 1 :element-type '(unsigned-byte 8) :initial-element replacement))
        (character
         (string-to-octets (string replacement)
                           :external-format external-format))
        (string
         (string-to-octets replacement
                           :external-format external-format))
        (t
         (coerce replacement '(simple-array (unsigned-byte 8) (*))))))))

;;; decoding condition

;;; for UTF8, the specific condition signalled will be a generalized
;;; instance of one of the following:
;;;
;;;   end-of-input-in-character
;;;   character-out-of-range
;;;   invalid-utf8-starter-byte
;;;   invalid-utf8-continuation-byte
;;;   overlong-utf8-sequence
;;;
;;; Of these, the only one truly likely to be of interest to calling
;;; code is end-of-input-in-character (in which case it's likely to
;;; want to make a note of octet-decoding-error-start, supply "" as a
;;; replacement string, and then move that last chunk of bytes to the
;;; beginning of its buffer for the next go round) but they're all
;;; provided on the off chance they're of interest.  The next most
;;; likely interesting option is overlong-utf8-sequence -- the
;;; application, if it cares to, can decode this itself (taking care
;;; to ensure that the result isn't out of range of CHAR-CODE-LIMIT)
;;; and return that result.  This library doesn't provide support for
;;; that as a conforming UTF-8-using program is supposed to treat it
;;; as an error.

(define-condition octet-decoding-error (character-decoding-error)
  ((array :initarg :array :accessor octet-decoding-error-array)
   (start :initarg :start :accessor octet-decoding-error-start)
   (end :initarg :end :accessor octet-decoding-error-end)
   (position :initarg :pos :accessor octet-decoding-bad-byte-position)
   (external-format :initarg :external-format
                    :accessor octet-decoding-error-external-format))
  (:report
   (lambda (condition stream)
     (format stream "Illegal ~S character starting at byte position ~D."
             (octet-decoding-error-external-format condition)
             (octet-decoding-error-start condition)))))

(define-condition end-of-input-in-character (octet-decoding-error) ())
(define-condition character-out-of-range (octet-decoding-error) ())
(define-condition invalid-utf8-starter-byte (octet-decoding-error) ())
(define-condition invalid-utf8-continuation-byte (octet-decoding-error) ())
(define-condition overlong-utf8-sequence (octet-decoding-error) ())

(define-condition malformed-ascii (octet-decoding-error) ())

(defun read-replacement-string ()
  (format *query-io* "Enter a replacement string designator (evaluated): ")
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun decoding-error (array start end external-format reason pos)
  (restart-case
      (error reason
             :external-format external-format
             :array array
             :start start
             :end end
             :pos pos)
    (use-value (s)
      :report "Supply a replacement string designator."
      :interactive read-replacement-string
      (string s))))

;;; Utilities used in both to-string and to-octet conversions

(defmacro instantiate-octets-definition (definer)
  `(progn
    (,definer aref (simple-array (unsigned-byte 8) (*)))
    (,definer sap-ref-8 system-area-pointer)))

;;; maps into TO-SEQ from elements of FROM-SEQ via MAPPER.  MAPPER
;;; returns two values: the number of elments stored in TO-SEQ, and
;;; the number used up from FROM-SEQ.  MAPPER is responsible for
;;; getting out if either sequence runs out of room.
(declaim (inline varimap))
(defun varimap (to-seq to-start to-end from-start from-end mapper)
  (declare (optimize speed (safety 0))
           (type array-range to-start to-end from-start from-end)
           (type function mapper))
  (loop with from-size of-type array-range = 0
        and to-size of-type array-range = 0
        for to-pos of-type array-range = to-start then (+ to-pos to-size)
        for from-pos of-type array-range = from-start then (+ from-pos from-size)
        while (and (< to-pos to-end)
                   (< from-pos from-end))
        do (multiple-value-bind (ts fs) (funcall mapper to-pos from-pos)
             (setf to-size ts
                   from-size fs))
        finally (return (values to-seq to-pos from-pos))))

;;; FIXME: find out why the comment about SYMBOLICATE below is true
;;; and fix it, or else replace with SYMBOLICATE.
;;;
;;; FIXME: this is cute, but is going to prevent greps for def.*<name>
;;; from working for (defun ,(make-od-name ...) ...)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-od-name (sym1 sym2)
    ;; "MAKE-NAME" is too generic, but this doesn't do quite what
    ;; SYMBOLICATE does; MAKE-OD-NAME ("octets definition") it is
    ;; then.
    (intern (concatenate 'string (symbol-name sym1) "-" (symbol-name sym2))
            (symbol-package sym1))))

;;;; to-octets conversions

;;; to latin (including ascii)

;;; Converting bytes to character codes is easy: just use a 256-element
;;; lookup table that maps each possible byte to its corresponding
;;; character code.
;;;
;;; Converting character codes to bytes is a little harder, since the
;;; codes may be spare (e.g. we use codes 0-127, 3490, and 4598).  The
;;; previous version of this macro utilized a gigantic CASE expression
;;; to do the hard work, with the result that the code was huge (since
;;; SBCL's then-current compilation strategy for CASE expressions was
;;; (and still is) converting CASE into COND into if-the-elses--which is
;;; also inefficient unless your code happens to occur very early in the
;;; chain.
;;;
;;; The current strategy is to build a table:
;;;
;;; [ ... code_1 byte_1 code_2 byte_2 ... code_n byte_n ... ]
;;;
;;; such that the codes are sorted in order from lowest to highest.  We
;;; can then binary search the table to discover the appropriate byte
;;; for a character code.  We also implement an optimization: all unibyte
;;; mappings do not remap ASCII (0-127) and some do not remap part of
;;; the range beyond character code 127.  So we check to see if the
;;; character code falls into that range first (a quick check, since
;;; character codes are guaranteed to be positive) and then do the binary
;;; search if not.  This optimization also enables us to cut down on the
;;; size of our lookup table.
(defmacro define-unibyte-mapper (byte-char-name code-byte-name &rest exceptions)
  (let* (;; Build a list of (CODE BYTE) pairs
         (pairs (loop for byte below 256
                   for code = (let ((exception (cdr (assoc byte exceptions))))
                                (cond
                                  ((car exception) (car exception))
                                  ((null exception) byte)
                                  (t nil)))
                   when code collect (list code byte) into elements
                   finally (return elements)))
         ;; Find the smallest character code such that the corresponding
         ;; byte is != to the code.
         (lowest-non-equivalent-code (position-if-not #'(lambda (pair)
                                                          (apply #'= pair))
                                                      pairs))
         ;; Sort them for our lookup table.
         (sorted-pairs (sort (subseq pairs lowest-non-equivalent-code)
                             #'< :key #'car))
         ;; Create the lookup table.
         (sorted-lookup-table
          (reduce #'append sorted-pairs :from-end t :initial-value nil)))
    `(progn
       ; Can't inline it with a non-null lexical environment anyway.
       ;(declaim (inline ,byte-char-name))
       (let ((byte-to-code-table
              ,(make-array 256 :element-type t #+nil 'char-code
                           :initial-contents (loop for byte below 256
                                                collect
                                                (let ((exception (cadr (assoc byte exceptions))))
                                                  (if exception
                                                      exception
                                                      byte)))))
             (code-to-byte-table
              ,(make-array (length sorted-lookup-table)
                           :initial-contents sorted-lookup-table)))
         (defun ,byte-char-name (byte)
           (declare (optimize speed (safety 0))
                    (type (unsigned-byte 8) byte))
           (aref byte-to-code-table byte))
         (defun ,code-byte-name (code)
           (declare (optimize speed (safety 0))
                    (type char-code code))
           (if (< code ,lowest-non-equivalent-code)
               code
               ;; We could toss in some TRULY-THEs if we really needed to
               ;; make this faster...
               (loop with low = 0
                  with high = (- (length code-to-byte-table) 2)
                  while (< low high)
                  do (let ((mid (logandc2 (truncate (+ low high 2) 2) 1)))
                       (if (< code (aref code-to-byte-table mid))
                           (setf high (- mid 2))
                           (setf low mid)))
                  finally (return (if (eql code (aref code-to-byte-table low))
                                      (aref code-to-byte-table (1+ low))
                                      nil)))))))))

#!+sb-unicode
(define-unibyte-mapper
    latin9->code-mapper
    code->latin9-mapper
  (#xA4 #x20AC)
  (#xA6 #x0160)
  (#xA8 #x0161)
  (#xB4 #x017D)
  (#xB8 #x017E)
  (#xBC #x0152)
  (#xBD #x0153)
  (#xBE #x0178))

(declaim (inline get-latin-bytes))
(defun get-latin-bytes (mapper external-format string pos)
  (let ((code (funcall mapper (char-code (char string pos)))))
    (declare (type (or null char-code) code))
    (values (cond
              ((and code (< code 256)) code)
              (t
               (encoding-error external-format string pos)))
            1)))

(declaim (inline code->ascii-mapper))
(defun code->ascii-mapper (code)
  (declare (optimize speed (safety 0))
           (type char-code code))
  (if (> code 127)
      nil
      code))

(declaim (inline get-ascii-bytes))
(defun get-ascii-bytes (string pos)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range pos))
  (get-latin-bytes #'code->ascii-mapper :ascii string pos))

(declaim (inline get-latin1-bytes))
(defun get-latin1-bytes (string pos)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range pos))
  (get-latin-bytes #'identity :latin-1 string pos))

#!+sb-unicode
(progn
  (declaim (inline get-latin9-bytes))
  (defun get-latin9-bytes (string pos)
    (declare (optimize speed (safety 0))
             (type simple-string string)
             (type array-range pos))
    (get-latin-bytes #'code->latin9-mapper :latin-9 string pos)))

(declaim (inline string->latin%))
(defun string->latin% (string sstart send get-bytes null-padding)
  (declare (optimize speed)
           (type simple-string string)
           (type index sstart send)
           (type (integer 0 1) null-padding)
           (type function get-bytes))
  ;; The latin encodings are all unibyte encodings, so just directly
  ;; compute the number of octets we're going to generate.
  (let ((octets (make-array (+ (- send sstart) null-padding)
                            ;; This takes care of any null padding the
                            ;; caller requests.
                            :initial-element 0
                            :element-type '(unsigned-byte 8)))
        (index 0)
        (error-position 0))
    (tagbody
     :no-error
       (loop for pos of-type index from sstart below send
          do (let ((byte (funcall get-bytes string pos)))
               (typecase byte
                 ((unsigned-byte 8)
                  (locally (declare (optimize (sb!c::insert-array-bounds-checks 0)))
                    (setf (aref octets index) byte)))
                 ((simple-array (unsigned-byte 8) (*))
                  ;; KLUDGE: We ran into encoding errors.  Bail and do
                  ;; things the slow way (does anybody actually use this
                  ;; functionality besides our own test suite?).
                  (setf error-position pos)
                  (go :error)))
               (incf index))
          finally (return-from string->latin% octets))
     :error
       ;; We have encoded INDEX octets so far and we ran into an encoding
       ;; error at ERROR-POSITION.
       (let ((new-octets (make-array (* index 2)
                                     :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer index)))
         (replace new-octets octets)
         (loop for pos of-type index from error-position below send
            do (let ((thing (funcall get-bytes string pos)))
                 (typecase thing
                   ((unsigned-byte 8)
                    (vector-push-extend thing new-octets))
                   ((simple-array (unsigned-byte 8) (*))
                    (dotimes (i (length thing))
                      (vector-push-extend (aref thing i) new-octets)))))
            finally (return-from string->latin%
                      (progn
                        (unless (zerop null-padding)
                          (vector-push-extend 0 new-octets))
                        (copy-seq new-octets))))))))

(defun string->ascii (string sstart send null-padding)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range sstart send))
  (values (string->latin% string sstart send #'get-ascii-bytes null-padding)))

(defun string->latin1 (string sstart send null-padding)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range sstart send))
  (values (string->latin% string sstart send #'get-latin1-bytes null-padding)))

#!+sb-unicode
(defun string->latin9 (string sstart send null-padding)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range sstart send))
  (values (string->latin% string sstart send #'get-latin9-bytes null-padding)))

;;; to utf8

(declaim (inline char-len-as-utf8))
(defun char-len-as-utf8 (code)
  (declare (optimize speed (safety 0))
           (type (integer 0 (#.sb!xc:char-code-limit)) code))
  (cond ((< code 0) (bug "can't happen"))
        ((< code #x80) 1)
        ((< code #x800) 2)
        ((< code #x10000) 3)
        ((< code #x110000) 4)
        (t (bug "can't happen"))))

(defun string->utf8 (string sstart send null-padding)
  (declare (optimize (speed 3) (safety 0))
           (type simple-string string)
           (type (integer 0 1) null-padding)
           (type array-range sstart send))
  (macrolet ((ascii-bash ()
               '(let ((array (make-array (+ null-padding (- send sstart))
                                         :element-type '(unsigned-byte 8))))
                 (loop for i from 0
                       and j from sstart below send
                       do (setf (aref array i) (char-code (char string j))))
                 array)))
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
                   (index 0))
               (declare (type index index))
               (flet ((add-byte (b)
                        (setf (aref array index) b)
                        (incf index)))
                 (declare (inline add-byte))
                 (loop for i of-type index from sstart below send
                       do (let ((code (char-code (char string i))))
                            (case (char-len-as-utf8 code)
                              (1
                               (add-byte code))
                              (2
                               (add-byte (logior #b11000000 (ldb (byte 5 6) code)))
                               (add-byte (logior #b10000000 (ldb (byte 6 0) code))))
                              (3
                               (add-byte (logior #b11100000 (ldb (byte 4 12) code)))
                               (add-byte (logior #b10000000 (ldb (byte 6 6) code)))
                               (add-byte (logior #b10000000 (ldb (byte 6 0) code))))
                              (4
                               (add-byte (logior #b11110000 (ldb (byte 3 18) code)))
                               (add-byte (logior #b10000000 (ldb (byte 6 12) code)))
                               (add-byte (logior #b10000000 (ldb (byte 6 6) code)))
                               (add-byte (logior #b10000000 (ldb (byte 6 0) code))))))
                       finally (return array)))))))
      #!+sb-unicode
      ((simple-array base-char (*))
       ;; On unicode builds BASE-STRINGs are limited to ASCII range, so we can take
       ;; a fast path -- and get benefit of the element type information. On non-unicode
       ;; build BASE-CHAR == CHARACTER.
       (ascii-bash))
      ((simple-array nil (*))
       ;; Just get the error...
       (aref string sstart)))))

;;;; to-string conversions

;;; from latin (including ascii)

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

(defmacro define-latin->string* (accessor type)
  (let ((name (make-od-name 'latin->string* accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (string sstart send array astart aend mapper)
        (declare (optimize speed (safety 0))
                 (type simple-string string)
                 (type ,type array)
                 (type array-range sstart send astart aend)
                 (function mapper))
        (varimap string sstart send
                 astart aend
                 (lambda (spos apos)
                   (setf (char string spos) (code-char (funcall mapper (,accessor array apos))))
                   (values 1 1)))))))
(instantiate-octets-definition define-latin->string*)

(defmacro define-latin1->string* (accessor type)
  (declare (ignore type))
  (let ((name (make-od-name 'latin1->string* accessor)))
    `(progn
      (defun ,name (string sstart send array astart aend)
        (,(make-od-name 'latin->string* accessor) string sstart send array astart aend #'identity)))))
(instantiate-octets-definition define-latin1->string*)

#!+sb-unicode
(progn
  (defmacro define-latin9->string* (accessor type)
    (declare (ignore type))
    (let ((name (make-od-name 'latin9->string* accessor)))
      `(progn
        (defun ,name (string sstart send array astart aend)
          (,(make-od-name 'latin->string* accessor) string sstart send array astart aend #'latin9->code-mapper)))))
  (instantiate-octets-definition define-latin9->string*))

(defmacro define-latin->string (accessor type)
  (let ((name (make-od-name 'latin->string accessor)))
    `(progn
      (declaim (inline latin->string))
      (defun ,name (array astart aend mapper)
        (declare (optimize speed (safety 0))
                 (type ,type array)
                 (type array-range astart aend)
                 (type function mapper))
        (let ((length (the array-range (- aend astart))))
          (values (,(make-od-name 'latin->string* accessor) (make-string length) 0 length
                                                            array astart aend
                                                            mapper)))))))
(instantiate-octets-definition define-latin->string)

(defmacro define-latin1->string (accessor type)
  (declare (ignore type))
  `(defun ,(make-od-name 'latin1->string accessor) (array astart aend)
    (,(make-od-name 'latin->string accessor) array astart aend #'identity)))
(instantiate-octets-definition define-latin1->string)

#!+sb-unicode
(progn
  (defmacro define-latin9->string (accessor type)
    (declare (ignore type))
    `(defun ,(make-od-name 'latin9->string accessor) (array astart aend)
      (,(make-od-name 'latin->string accessor) array astart aend #'latin9->code-mapper)))
  (instantiate-octets-definition define-latin9->string))

;;; from utf8

(defmacro define-bytes-per-utf8-character (accessor type)
  (let ((name (make-od-name 'bytes-per-utf8-character accessor)))
    `(progn
      ;;(declaim (inline ,name))
      (let ((lexically-max
             (string->utf8 (string (code-char ,(1- sb!xc:char-code-limit)))
                           0 1 0)))
        (declare (type (simple-array (unsigned-byte 8) (#!+sb-unicode 4 #!-sb-unicode 2)) lexically-max))
        (defun ,name (array pos end)
          (declare (optimize speed (safety 0))
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
                                   ((= (logand b #b11100000) #b11000000)
                                    2)
                                   ((= (logand b #b11110000) #b11100000)
                                    3)
                                   ((= (logand b #b11111000) #b11110000)
                                    4)
                                   ((= (logand b #b11111100) #b11111000)
                                    5)
                                   ((= (logand b #b11111110) #b11111100)
                                    6)
                                   (t
                                    nil))))
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
                     (overlong-chk (x y)
                       (let ((ok (or (/= initial-byte x)
                                     (/= (logior (,accessor array (the array-range (+ pos 1)))
                                                 y)
                                         y))))
                         (unless ok
                           (setf reject-reason 'overlong-utf8-sequence))
                         ok))
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
                                       #!+sb-unicode
                                       ((> (aref lexically-max 1) (,accessor array (+ pos 1)))
                                        t)
                                       #!+sb-unicode
                                       ((< (aref lexically-max 2) (,accessor array (+ pos 2)))
                                        nil)
                                       #!+sb-unicode
                                       ((> (aref lexically-max 2) (,accessor array (+ pos 2)))
                                        t)
                                       #!+sb-unicode
                                       ((< (aref lexically-max 3) (,accessor array (+ pos 3)))
                                        nil)
                                       (t t))))
                         (unless ok
                           (setf reject-reason 'character-out-of-range))
                         ok)))
              (declare (inline valid-utf8-starter-byte-p
                               enough-bytes-left-p
                               valid-secondary-p
                               preliminary-ok-for-length
                               overlong-chk))
              (let ((maybe-len (valid-utf8-starter-byte-p initial-byte)))
                (cond ((eql maybe-len 1)
                       (values 1 nil))
                      ((and (preliminary-ok-for-length maybe-len 2)
                            (overlong-chk #b11000000 #b10111111)
                            (overlong-chk #b11000001 #b10111111)
                            #!-sb-unicode (character-below-char-code-limit-p))
                       (values 2 nil))
                      ((and (preliminary-ok-for-length maybe-len 3)
                            (overlong-chk #b11100000 #b10011111)
                            #!-sb-unicode (not (setf reject-reason 'character-out-of-range)))
                       (values 3 nil))
                      ((and (preliminary-ok-for-length maybe-len 4)
                            (overlong-chk #b11110000 #b10001111)
                            #!-sb-unicode (not (setf reject-reason 'character-out-of-range))
                            (character-below-char-code-limit-p))
                       (values 4 nil))
                      ((and (preliminary-ok-for-length maybe-len 5)
                            (overlong-chk #b11111000 #b10000111)
                            (not (setf reject-reason 'character-out-of-range)))
                       (bug "can't happen"))
                      ((and (preliminary-ok-for-length maybe-len 6)
                            (overlong-chk #b11111100 #b10000011)
                            (not (setf reject-reason 'character-out-of-range)))
                       (bug "can't happen"))
                      (t
                       (let* ((bad-end (ecase reject-reason
                                         (invalid-utf8-starter-byte
                                          (1+ pos))
                                         (end-of-input-in-character
                                          end)
                                         (invalid-utf8-continuation-byte
                                          reject-position)
                                         ((overlong-utf8-sequence character-out-of-range)
                                          (+ pos maybe-len))))
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
        (declare (optimize speed (safety 0))
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
        (declare (optimize speed (safety 0))
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

;;;; external formats

(defvar *default-external-format* nil)

(defun default-external-format ()
  (or *default-external-format*
      ;; On non-unicode, use iso-8859-1 instead of detecting it from
      ;; the locale settings. Defaulting to an external-format which
      ;; can represent characters that the CHARACTER type can't
      ;; doesn't seem very sensible.
      #!-sb-unicode
      (setf *default-external-format* :latin-1)
      (let ((external-format #!-win32 (intern (or (sb!alien:alien-funcall
                                                    (extern-alien
                                                      "nl_langinfo"
                                                      (function (c-string :external-format :latin-1)
                                                                int))
                                                    sb!unix:codeset)
                                                  "LATIN-1")
                                              "KEYWORD")
                             #!+win32 (sb!win32::ansi-codepage)))
        (/show0 "cold-printing defaulted external-format:")
        #!+sb-show
        (cold-print external-format)
        (/show0 "matching to known aliases")
        (dolist (entry *external-formats*
                 (progn
                   ;;; FIXME! This WARN would try to do printing
                   ;;; before the streams have been initialized,
                   ;;; causing an infinite erroring loop. We should
                   ;;; either print it by calling to C, or delay the
                   ;;; warning until later. Since we're in freeze
                   ;;; right now, and the warning isn't really
                   ;;; essential, I'm doing what's least likely to
                   ;;; cause damage, and commenting it out. This
                   ;;; should be revisited after 0.9.17. -- JES,
                   ;;; 2006-09-21
                   #+nil
                   (warn "Invalid external-format ~A; using LATIN-1"
                         external-format)
                   (setf external-format :latin-1)))
          (/show0 "cold printing known aliases:")
          #!+sb-show
          (dolist (alias (first entry)) (cold-print alias))
          (/show0 "done cold-printing known aliases")
          (when (member external-format (first entry))
            (/show0 "matched")
            (return)))
        (/show0 "/default external format ok")
        (setf *default-external-format* external-format))))

;;; FIXME: OAOOM here vrt. DEFINE-EXTERNAL-FORMAT in fd-stream.lisp
(defparameter *external-format-functions* (make-hash-table))

(defun add-external-format-funs (format-names funs)
  (dolist (name format-names (values))
    (setf (gethash name *external-format-functions*) funs)))

(add-external-format-funs
 '(:ascii :us-ascii :ansi_x3.4-1968 :iso-646 :iso-646-us :|646|)
 '(ascii->string-aref string->ascii))
(add-external-format-funs
 '(:latin1 :latin-1 :iso-8859-1 :iso8859-1)
 '(latin1->string-aref string->latin1))
#!+sb-unicode
(add-external-format-funs
 '(:latin9 :latin-9 :iso-8859-15 :iso8859-15)
 '(latin9->string-aref string->latin9))
(add-external-format-funs '(:utf8 :utf-8) '(utf8->string-aref string->utf8))

(defun external-formats-funs (external-format)
  (when (eql external-format :default)
    (setf external-format (default-external-format)))
  (or (gethash external-format *external-format-functions*)
      (error "Unknown external-format ~S" external-format)))

;;;; public interface

(defun octets-to-string (vector &key (external-format :default) (start 0) end)
  (declare (type (vector (unsigned-byte 8)) vector))
  (with-array-data ((vector vector)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (funcall (symbol-function (first (external-formats-funs external-format)))
             vector start end)))

(defun string-to-octets (string &key (external-format :default)
                         (start 0) end null-terminate)
  (declare (type string string))
  (with-array-data ((string string)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (declare (type simple-string string))
    (funcall (symbol-function (second (external-formats-funs external-format)))
             string start end (if null-terminate 1 0))))

#!+sb-unicode
(defvar +unicode-replacement-character+ (string (code-char #xfffd)))
#!+sb-unicode
(defun use-unicode-replacement-char (condition)
  (use-value +unicode-replacement-character+ condition))

;;; Utilities that maybe should be exported

#!+sb-unicode
(defmacro with-standard-replacement-character (&body body)
  `(handler-bind ((octet-encoding-error #'use-unicode-replacement-char))
    ,@body))

(defmacro with-default-decoding-replacement ((c) &body body)
  (let ((cname (gensym)))
  `(let ((,cname ,c))
    (handler-bind
        ((octet-decoding-error (lambda (c)
                                 (use-value ,cname c))))
      ,@body))))
