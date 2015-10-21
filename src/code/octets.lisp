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

(defun encoding-error (external-format string pos)
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

(define-condition malformed-ascii (octet-decoding-error) ())

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
      :interactive
      (lambda ()
        (read-evaluated-form
         "Enter a replacement string designator (evaluated): "))
      (string s))))

;;; Utilities used in both to-string and to-octet conversions

(defmacro instantiate-octets-definition (definer)
  `(progn
    (,definer aref (simple-array (unsigned-byte 8) (*)))
    (,definer sap-ref-8 system-area-pointer)))

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
         (lowest-non-equivalent-code
          (caar (sort (copy-seq exceptions) #'< :key #'car)))
         ;; Sort them for our lookup table.
         (sorted-pairs (sort (subseq pairs lowest-non-equivalent-code)
                             #'< :key #'car))
         ;; Create the lookup table.
         (sorted-lookup-table
          (reduce #'append sorted-pairs :from-end t :initial-value nil)))
    (flet ((pick-type (vector &optional missing-points-p)
             (if missing-points-p
                 (let ((max (reduce #'max (remove nil vector))))
                   (cond ((<= max #x7F) '(signed-byte 8))
                         ((<= max #x7FFF) '(signed-byte 16))
                         (t '(signed-byte 32))))
                 (let ((max (reduce #'max vector)))
                   (cond ((<= max #xFF) '(unsigned-byte 8))
                         ((<= max #xFFFF) '(unsigned-byte 16))
                         (t '(unsigned-byte 32)))))))
      `(progn
         ;; We *could* inline this, but it's not obviously the right thing,
         ;; because each use of the inlined function in a different file
         ;; would be forced to dump the large-ish array. To do things like
         ;; this, you generally want a load-time ref to a global constant.
       ;(declaim (inline ,byte-char-name))
         (defun ,byte-char-name (byte)
           (declare (optimize speed (safety 0))
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
                  `(let ((code (aref ,(!make-specialized-array
                                       256 (pick-type byte-to-code t)
                                       (substitute -1 nil byte-to-code))
                                     byte)))
                     (if (>= code 0) code))
                  ;; Every byte has a translation
                  `(aref ,(!make-specialized-array
                           256 (pick-type byte-to-code) byte-to-code)
                         byte))))
         (defun ,code-byte-name (code)
           (declare (optimize speed (safety 0))
                    (type char-code code))
           (if (< code ,lowest-non-equivalent-code)
               code
               (loop with code-to-byte-table =
                    ,(!make-specialized-array
                      (length sorted-lookup-table)
                      (pick-type sorted-lookup-table)
                      sorted-lookup-table)
                  with low = 0
                  with high = (- (length code-to-byte-table) 2)
                  while (< low high)
                  do (let ((mid (logandc2 (truncate (+ low high 2) 2) 1)))
                       (if (< code (aref code-to-byte-table mid))
                           (setf high (- mid 2))
                           (setf low mid)))
                  finally (return (if (eql code (aref code-to-byte-table low))
                                      (aref code-to-byte-table (1+ low))
                                      nil)))))))))

(declaim (inline get-latin-bytes))
(defun get-latin-bytes (mapper external-format string pos)
  (let ((code (funcall mapper (char-code (char string pos)))))
    (declare (type (or null char-code) code))
    (values (cond
              ((and code (< code 256)) code)
              (t
               (encoding-error external-format string pos)))
            1)))

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
        (error-position 0)
        (error-replacement))
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
                 do (extend (funcall get-bytes string pos))
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
        (declare (optimize speed (safety 0))
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
        (declare (optimize speed (safety 0))
                 (type ,type array)
                 (type array-range astart aend)
                 (type function mapper))
        (let ((length (the array-range (- aend astart))))
          (values (,(make-od-name 'latin->string* accessor) (make-string length) 0 length
                                                            array astart aend
                                                            mapper)))))))
(instantiate-octets-definition define-latin->string)

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
      (let ((external-format #!-win32 (intern (or #!-android
                                                  (alien-funcall
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
        (let ((entry (get-external-format external-format)))
          (cond
            (entry
             (/show0 "matched"))
            (t
             ;; FIXME! This WARN would try to do printing
             ;; before the streams have been initialized,
             ;; causing an infinite erroring loop. We should
             ;; either print it by calling to C, or delay the
             ;; warning until later. Since we're in freeze
             ;; right now, and the warning isn't really
             ;; essential, I'm doing what's least likely to
             ;; cause damage, and commenting it out. This
             ;; should be revisited after 0.9.17. -- JES,
             ;; 2006-09-21
             #+nil
             (warn "Invalid external-format ~A; using LATIN-1"
                   external-format)
             (setf external-format :latin-1))))
        (/show0 "/default external format ok")
        (setf *default-external-format* external-format))))

;;;; public interface

(defun maybe-defaulted-external-format (external-format)
  (get-external-format-or-lose (if (eq external-format :default)
                                   (default-external-format)
                                   external-format)))

(defun octets-to-string (vector &key (external-format :default) (start 0) end)
  (declare (type (vector (unsigned-byte 8)) vector))
  (with-array-data ((vector vector)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (let ((ef (maybe-defaulted-external-format external-format)))
      (funcall (ef-octets-to-string-fun ef) vector start end))))

(defun string-to-octets (string &key (external-format :default)
                         (start 0) end null-terminate)
  (declare (type string string))
  (with-array-data ((string string)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (declare (type simple-string string))
    (let ((ef (maybe-defaulted-external-format external-format)))
      (funcall (ef-string-to-octets-fun ef) string start end
               (if null-terminate 1 0)))))

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

;;; This function was moved from 'fd-stream' because it depends on
;;; the various error classes, two of which are defined just above.
(defun get-external-format (external-format)
  (flet ((keyword-external-format (keyword)
           (declare (type keyword keyword))
           (gethash keyword *external-formats*))
         (replacement-handlerify (entry replacement)
           (when entry
             (wrap-external-format-functions
              entry
              (lambda (fun)
                (and fun
                     (lambda (&rest rest)
                       (declare (dynamic-extent rest))
                       (handler-bind
                           ((stream-decoding-error
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'input-replacement replacement)))
                            (stream-encoding-error
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'output-replacement replacement)))
                            (octets-encoding-error
                             (lambda (c) (use-value replacement c)))
                            (octet-decoding-error
                             (lambda (c) (use-value replacement c))))
                         (apply fun rest)))))))))
    (typecase external-format
      (keyword (keyword-external-format external-format))
      ((cons keyword)
       (let ((entry (keyword-external-format (car external-format)))
             (replacement (getf (cdr external-format) :replacement)))
         (if replacement
             (replacement-handlerify entry replacement)
             entry))))))

(defun unintern-init-only-stuff ()
  (let ((this-package (find-package "SB-IMPL")))
    (dolist (s '(char-class char-class2 char-class3
                 steve-splice))
      (unintern s this-package))
    (flet ((ends-with-p (s1 s2)
             (let ((diff (- (length s1) (length s2))))
               (and (>= diff 0) (string= s1 s2 :start1 diff)))))
      (do-symbols (s this-package)
        (let ((name (symbol-name s)))
          (if (and (macro-function s)
                   (eql (mismatch name "DEFINE-") 7)
                   (or (ends-with-p name "->STRING")
                       (ends-with-p name "->STRING*")))
              (unintern s this-package)))))))
