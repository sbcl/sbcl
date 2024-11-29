;;;; code for string to octet conversion

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; FIXME: The latin9 stuff is currently #+sb-unicode, because I
;;; don't like the idea of trying to do CODE-CHAR #x<big>.  Is that a
;;; justified fear?  Can we arrange that it's caught and converted to
;;; a decoding error error?  Or should we just give up on non-Unicode
;;; builds?

(in-package "SB-IMPL")


;;;; conditions

;;; encoding condition

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *safety-0* '(safety 0)))

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

(defun encoding-replacement-octetify (thing external-format)
  (etypecase thing
    ;; TODO: make sure EXTERNAL-FORMAT here does not have a REPLACEMENT, and handle
    ;; encoding errors explicitly
    (character
     (string-to-octets (string thing) :external-format external-format))
    (string
     (string-to-octets thing :external-format external-format))
    ((unsigned-byte 8) (make-array 1 :element-type '(unsigned-byte 8) :initial-element thing))
    (sequence (coerce thing '(simple-array (unsigned-byte 8) 1)))))

(declaim (ftype (sfunction (t t t t) (simple-array (unsigned-byte 8) 1))
                encoding-error))
(defun encoding-error (external-format replacement string pos)
  (flet ((replacement (replacement) (encoding-replacement-octetify replacement external-format)))
    (if replacement
        (replacement replacement)
        (restart-case
            (error 'octets-encoding-error
                   :external-format external-format
                   :string string
                   :position pos)
          (use-value (replacement)
            :report "Supply a set of bytes to use in place of the invalid one."
            :interactive
            (lambda ()
              (read-evaluated-form
               "Replacement byte, bytes, character, or string (evaluated): "))
            (replacement replacement))))))

;;; decoding condition

;;; for UTF8, the specific condition signalled will be a generalized
;;; instance of one of the following:
;;;
;;;   end-of-input-in-character
;;;   character-out-of-range
;;;   invalid-utf8-starter-byte
;;;   invalid-utf8-continuation-byte
;;;
;;; Of these, the only one truly likely to be of interest to calling
;;; code is end-of-input-in-character (in which case it's likely to
;;; want to make a note of octet-decoding-error-start, supply "" as a
;;; replacement string, and then move that last chunk of bytes to the
;;; beginning of its buffer for the next go round) but they're all
;;; provided on the off chance they're of interest.

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

(defun decoding-replacement-stringify (thing external-format)
  (etypecase thing
    (character (string thing))
    (string thing)
    ;; TODO: make sure EXTERNAL-FORMAT here does not have a REPLACEMENT, and handle
    ;; decoding errors explicitly
    ((unsigned-byte 8)
     (let ((octets (make-array 1 :element-type '(unsigned-byte 8) :initial-element thing)))
       (octets-to-string octets :external-format external-format)))
    (sequence
     (let ((octets (coerce thing '(simple-array (unsigned-byte 8) 1))))
       (octets-to-string octets :external-format external-format)))))

(defun decoding-error (array start end external-format replacement reason pos)
  (flet ((replacement (thing) (decoding-replacement-stringify thing external-format)))
    (if replacement
        (replacement replacement)
        (restart-case
            (error reason
                   :external-format external-format
                   :array array
                   :start start
                   :end end
                   :pos pos)
          (use-value (replacement)
            :report "Supply a replacement string."
            :interactive
            (lambda ()
              (read-evaluated-form
               "Replacement byte, bytes, character, or string (evaluated): "))
            (replacement replacement))))))

;;; Utilities used in both to-string and to-octet conversions

(defmacro instantiate-octets-definition (definer)
  `(progn
    (,definer aref (simple-array (unsigned-byte 8) (*)))
    (,definer sap-ref-8 system-area-pointer)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-od-name (sym1 sym2)
    (package-symbolicate (cl:symbol-package sym1) sym1 "-" sym2)))

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
         (lowest-non-equivalent-code
          (caar (sort (copy-seq exceptions) #'< :key #'car)))
         ;; Sort them for our lookup table.
         (sorted-pairs (sort (subseq pairs lowest-non-equivalent-code)
                             #'< :key #'car))
         ;; Create the lookup table.
         (sorted-lookup-table
          (reduce #'append sorted-pairs :from-end t :initial-value nil)))
    `(progn
         ;; We *could* inline this, but it's not obviously the right thing,
         ;; because each use of the inlined function in a different file
         ;; would be forced to dump the large-ish array. To do things like
         ;; this, you generally want a load-time ref to a global constant.
       ;(declaim (inline ,byte-char-name))
         (defun ,byte-char-name (byte)
           (declare (optimize speed #.*safety-0*)
                    (type (unsigned-byte 8) byte))
           ,(let ((byte-to-code
                   (loop for byte below 256
                      collect (let ((exception (cdr (assoc byte exceptions))))
                                (if exception
                                    (car exception)
                                    byte)))))
              (if (position nil byte-to-code)
                  ;; There are bytes with no translation. Represent "missing"
                  ;; as -1 when stored, convert to NIL when accessed.
                  ;; We could use a single otherwise-unused point to mean NIL,
                  ;; but it would be confusing if in one table #xFFFF represents
                  ;; NIL and another #xF00D represents NIL.
                  `(let ((code (aref ,(sb-c::coerce-to-smallest-eltype
                                       (substitute -1 nil byte-to-code))
                                     byte)))
                     (if (>= code 0) code))
                  ;; Every byte has a translation
                  `(aref ,(sb-c::coerce-to-smallest-eltype byte-to-code)
                         byte))))
         (defun ,code-byte-name (code)
           (declare (optimize speed #.*safety-0*)
                    (%char-code code))
           (if (< code ,lowest-non-equivalent-code)
               code
               (loop with code-to-byte-table =
                    ,(sb-c::coerce-to-smallest-eltype sorted-lookup-table)
                  with low = 0
                  with high = (- (length code-to-byte-table) 2)
                  while (< low high)
                  do (let ((mid (logandc2 (truncate (+ low high 2) 2) 1)))
                       (if (< code (aref code-to-byte-table mid))
                           (setf high (- mid 2))
                           (setf low mid)))
                  finally (return (if (eql code (aref code-to-byte-table low))
                                      (aref code-to-byte-table (1+ low))
                                      nil))))))))

(declaim (inline get-latin-bytes))
(defun get-latin-bytes (mapper external-format replacement string pos)
  (let ((code (funcall mapper (char-code (char string pos)))))
    (declare (type (or null %char-code) code))
    (values (cond
              ((and code (< code 256)) code)
              (t
               (encoding-error external-format replacement string pos)))
            1)))

(declaim (inline string->latin%))
(defun string->latin% (string sstart send get-bytes null-padding replacement)
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
        (error-position 0)
        (error-replacement))
    (tagbody
     :no-error
       (loop for pos of-type index from sstart below send
          do (let ((byte (funcall get-bytes string pos replacement)))
               (typecase byte
                 ((unsigned-byte 8)
                  (locally (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                    (setf (aref octets index) byte)))
                 ((simple-array (unsigned-byte 8) (*))
                  ;; KLUDGE: We ran into encoding errors.  Bail and do
                  ;; things the slow way (does anybody actually use this
                  ;; functionality besides our own test suite?).
                  (setf error-position pos error-replacement byte)
                  (go :error)))
               (incf index))
          finally (return-from string->latin% octets))
     :error
       ;; We have encoded INDEX octets so far and we ran into an
       ;; encoding error at ERROR-POSITION; the user has asked us to
       ;; replace the expected output with ERROR-REPLACEMENT.
       (let ((new-octets (make-array (* index 2)
                                     :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer index)))
         (replace new-octets octets)
         (flet ((extend (thing)
                 (typecase thing
                   ((unsigned-byte 8) (vector-push-extend thing new-octets))
                   ((simple-array (unsigned-byte 8) (*))
                    (dotimes (i (length thing))
                      (vector-push-extend (aref thing i) new-octets))))))
           (extend error-replacement)
           (loop for pos of-type index from (1+ error-position) below send
                 do (extend (funcall get-bytes string pos replacement))
                 finally (return-from string->latin%
                           (progn
                             (unless (zerop null-padding)
                               (vector-push-extend 0 new-octets))
                             (copy-seq new-octets)))))))))

;;;; to-string conversions

;;; from latin (including ascii)

(defmacro define-latin->string* (accessor type)
  (let ((name (make-od-name 'latin->string* accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (string sstart send array astart aend mapper)
        (declare (optimize speed #.*safety-0*)
                 (type simple-string string)
                 (type ,type array)
                 (type array-range sstart send astart aend)
                 (function mapper))
        (loop for spos from sstart below send
           for apos from astart below aend
           do (setf (char string spos)
                    (code-char (funcall mapper (,accessor array apos))))
           finally (return (values string spos apos)))))))
(instantiate-octets-definition define-latin->string*)

(defmacro define-latin->string (accessor type)
  (let ((name (make-od-name 'latin->string accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (array astart aend mapper)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range astart aend)
                 (type function mapper))
        (let ((length (the array-range (- aend astart))))
          (values (,(make-od-name 'latin->string* accessor) (make-string length) 0 length
                                                            array astart aend
                                                            mapper)))))))
(instantiate-octets-definition define-latin->string)

;;;; external formats

(defvar *default-external-format* :utf-8)
(defvar *default-source-external-format*
  #+win32 '(:default :newline :crlf)
  #-win32 :default)

(defun default-external-format ()
  (/show0 "/getting default external format")
  *default-external-format*)

(defun parse-external-format (external-format)
  (let* ((defaulted-external-format (typecase external-format
                                      ((eql :default)
                                       *default-external-format*)
                                      ((cons (eql :default))
                                       (let ((default *default-external-format*)
                                             (options (cdr external-format)))
                                         (if (consp *default-external-format*)
                                             (cons (car *default-external-format*)
                                                   (append options (cdr default)))
                                             (cons default options))))
                                      (t
                                       external-format)))
         (external-format-entry (get-external-format defaulted-external-format))
         (canonized-external-format
           (and external-format-entry (canonize-external-format defaulted-external-format external-format-entry))))
    (values external-format-entry canonized-external-format)))


;;;; public interface

(defun maybe-defaulted-external-format (external-format)
  (get-external-format-or-lose (typecase external-format
                                 ((eql :default)
                                  *default-external-format*)
                                 ((cons (eql :default))
                                  (let ((default *default-external-format*)
                                        (options (cdr external-format)))
                                    (if (consp *default-external-format*)
                                        (cons (car *default-external-format*)
                                              (append options (cdr default)))
                                        (cons default options))))
                                 (t
                                  external-format))))

(declaim (inline %octets-to-string))
(declaim (ftype (sfunction (function (vector (unsigned-byte 8)) index sequence-end t)
                           (or (simple-array character (*))
                               (simple-array base-char (*))))
                %octets-to-string))
(defun %octets-to-string (fun vector start end replacement)
  (declare (explicit-check start end :result))
  (with-array-data ((vector vector)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (funcall fun vector start end replacement)))
(declaim (notinline %octets-to-string))
(defun octets-to-string (vector &key (external-format :default) (start 0) end)
  "Return a string obtained by decoding VECTOR according to EXTERNAL-FORMAT.

If EXTERNAL-FORMAT is given, it must designate an external format.

If given, START and END must be bounding index designators and
designate a subsequence of VECTOR that should be decoded.

If some of the octets of VECTOR (or the subsequence bounded by START
and END) cannot be decoded by EXTERNAL-FORMAT an error of a subtype of
SB-INT:CHARACTER-DECODING-ERROR is signaled.

Note that for some values of EXTERNAL-FORMAT the length of the
returned string may be different from the length of VECTOR (or the
subsequence bounded by START and END)."
  (let* ((ef (maybe-defaulted-external-format external-format))
         (replacement (ef-replacement ef)))
    (declare (inline %octets-to-string))
    (%octets-to-string (ef-octets-to-string-fun ef) vector start end replacement)))

(declaim (inline %string-to-octets))
(declaim (ftype (sfunction (function string index sequence-end t t)
                           (simple-array (unsigned-byte 8) (*)))
                %string-to-octets))
(defun %string-to-octets (fun string start end null-terminate replacement)
  (declare (explicit-check start end :result))
  (with-array-data ((string string)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (declare (type simple-string string))
    (funcall fun string start end null-terminate replacement)))
(declaim (notinline %string-to-octets))
(defun string-to-octets (string &key (external-format :default)
                         (start 0) end null-terminate)
  "Return an octet vector that is STRING encoded according to EXTERNAL-FORMAT.

If EXTERNAL-FORMAT is given, it must designate an external format.

If given, START and END must be bounding index designators and
designate a subsequence of STRING that should be encoded.

If NULL-TERMINATE is true, the returned octet vector ends with an
additional 0 element that does not correspond to any part of STRING.

If some of the characters of STRING (or the subsequence bounded by
START and END) cannot be encoded by EXTERNAL-FORMAT an error of a
subtype of SB-INT:CHARACTER-ENCODING-ERROR is signaled.

Note that for some values of EXTERNAL-FORMAT and NULL-TERMINATE the
length of the returned vector may be different from the length of
STRING (or the subsequence bounded by START and END)."
  (declare (explicit-check start end :result))
  (let* ((ef (maybe-defaulted-external-format external-format))
         (replacement (ef-replacement ef)))
    (declare (inline %string-to-octets))
    (%string-to-octets (ef-string-to-octets-fun ef) string start end
                       (if null-terminate 1 0) replacement)))

;;; Vector of all available EXTERNAL-FORMAT instances. Each format is named
;;; by one or more keyword symbols. The mapping from symbol to index into this
;;; vector is memoized into the symbol's :EXTERNAL-FORMAT property.
(define-load-time-global *external-formats* (make-array 60 :initial-element nil))

(defun register-external-format (names &rest args)
  ;; TODO: compare-and-swap the entry if NAME already has an index
  ;; specifying to demand-load this format from a fasl.
  ;; All synonyms of that name will also references the loaded format.
  (let* ((table *external-formats*)
         (newline-variant (getf args :newline-variant))
         (index (get (car names) :external-format))
         (current-entry (and index (aref table index)))
         (canonical-ef (if (consp current-entry) (car current-entry) current-entry))
         (names (if canonical-ef (ef-names canonical-ef) names))
         (ef (apply #'%make-external-format :names names args))
         (free-index (or index (position nil table))))
    (unless (eql newline-variant :lf)
      (aver index)
      (aver current-entry)
      (unless (consp current-entry)
        (setf current-entry (list current-entry)))
      (setf (aref table index)
            (cons (car current-entry)
                  (acons (list newline-variant) ef (cdr current-entry))))
      (return-from register-external-format))
    (dolist (name names)
      (setf (get name :external-format) free-index))
    (setf (aref table free-index) ef)))

;;; This function was moved from 'fd-stream' because it depends on
;;; the various error classes, two of which are defined just above.
;;; XXX: Why does this get called with :DEFAULT and NIL when neither is
;;; the name of any format? Shouldn't those be handled higher up,
;;; or else this should return the actual default?
(defun get-external-format (external-format)
  (multiple-value-bind (format-name options)
      (if (consp external-format)
          (values (car external-format) (cdr external-format))
          (values external-format nil))
    (unless (symbolp format-name)
      (return-from get-external-format nil))
    (loop for (option value) on options by 'cddr
          unless (or (eql option :newline) (eql option :replacement))
          do (return-from get-external-format nil))
    (binding*
        ((newline (getf options :newline :lf))
         (replacement (getf options :replacement))
         (table-index (get format-name :external-format) :exit-if-null)
         (formats *external-formats*)
         (table-entry
          ;; The table entry can be one of:
          ;;   1. #<external-format>
          ;;   2. (#<external-format> ((:crlf #\char) . #<modified-ef>) ...)
          ;;      for a list of modified formats that alter the
          ;;      newline encoding and choice of replacement character.
          ;;   3. "namestring" - to autoload from a fasl containing
          ;;      the named format.
          (let ((ef (svref formats table-index)))
            (etypecase ef
              ((or instance list) ef)
              #+nil
              (string
               ;; Theoretically allow demand-loading the external-format
               ;; from a fasl of this name. (Not done yet)
               ;; (module-provide-contrib ef)
               (let ((ef (svref formats table-index)))
                 (aver (external-format-p ef))
                 ef)))))
         ((base-format variations)
          (if (listp table-entry)
              (values (car table-entry) (cdr table-entry))
              (values table-entry nil)))
         (newline-base-format
          (if (eql newline :lf)
              base-format
              (cdr (assoc-if (lambda (x)
                               (equal x (list newline))) ;; transformed to not cons
                             variations)))
          :exit-if-null))
      (unless (typep replacement '(or null character string (unsigned-byte 8) (simple-array (unsigned-byte 8) 1)))
        (return-from get-external-format nil))
      (when (or (not newline-base-format) (not replacement))
        (return-from get-external-format newline-base-format))
      (loop
       (let ((key (cons newline replacement)))
         (awhen (assoc key variations :test #'equal)
           (return (cdr it)))
         (let* ((new-ef (let ((copy (copy-structure newline-base-format)))
                          (setf (ef-replacement copy) replacement)
                          copy))
                (new-table-entry
                  (cons base-format (acons key new-ef variations)))
                (old (cas (svref formats table-index) table-entry new-table-entry)))
           (when (eq old table-entry)
             (return new-ef))
           ;; CAS failure -> some other thread added an entry. It's probably
           ;; for the same replacement char which is usually #\ufffd.
           ;; So try again. At worst this conses some more garbage.
           (setq table-entry old)))))))

(push
  `("SB-IMPL"
    char-class char-class2 char-class3
    ,@(let (macros)
        (flet ((ends-with-p (s1 s2)
                 (let ((diff (- (length s1) (length s2))))
                   (and (>= diff 0) (string= s1 s2 :start1 diff)))))
          (do-symbols (s "SB-IMPL" macros)
            (let ((name (symbol-name s)))
              (when (and (macro-function s)
                         (eql (mismatch name "DEFINE-") 7)
                         (or (ends-with-p name "->STRING")
                             (ends-with-p name "->STRING*")))
                (push s macros)))))))
  *!removable-symbols*)
