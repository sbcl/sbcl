;;;; os-independent stream functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; standard streams

;;; The initialization of these streams is performed by
;;; STREAM-COLD-INIT-OR-RESET.
(defvar *terminal-io*)
(setf (documentation '*terminal-io* 'variable) "terminal I/O stream")
(defvar *standard-input*)
(setf (documentation '*standard-input* 'variable) "default input stream")
(defvar *standard-output*)
(setf (documentation '*standard-output* 'variable) "default output stream")
(defvar *error-output*)
(setf (documentation '*error-output* 'variable) "error output stream")
(defvar *query-io*)
(setf (documentation '*query-io* 'variable) "query I/O stream")
(defvar *trace-output*)
(setf (documentation '*trace-output* 'variable) "trace output stream")
(defvar *debug-io*)
(setf (documentation '*debug-io* 'variable) "interactive debugging stream")

(defun stream-element-type-stream-element-mode (element-type)
  (cond ((or (not element-type)
             (eq element-type t)
             (eq element-type :default)) :bivalent)
        ((or (eq element-type 'character)
             (eq element-type 'base-char))
         'character)
        ((memq element-type '(signed-byte unsigned-byte))
         element-type)
        ((and (proper-list-of-length-p element-type 2)
              (memq (car element-type)
                    '(signed-byte unsigned-byte)))
         (car element-type))
        ((not (ignore-errors
               (setf element-type
                     (type-or-nil-if-unknown element-type t))))
         :bivalent)
        ((eq element-type *empty-type*)
         :bivalent)
        ((csubtypep element-type (specifier-type 'character))
         'character)
        ((csubtypep element-type (specifier-type 'unsigned-byte))
         'unsigned-byte)
        ((csubtypep element-type (specifier-type 'signed-byte))
         'signed-byte)
        (t
         :bivalent)))

(defun ill-in (stream &rest ignore)
  (declare (ignore ignore))
  (error 'simple-type-error
         :datum stream
         :expected-type '(satisfies input-stream-p)
         :format-control "~S is not a character input stream."
         :format-arguments (list stream)))
(defun ill-out (stream &rest ignore)
  (declare (ignore ignore))
  (error 'simple-type-error
         :datum stream
         :expected-type '(satisfies output-stream-p)
         :format-control "~S is not a character output stream."
         :format-arguments (list stream)))
(defun ill-bin (stream &rest ignore)
  (declare (ignore ignore))
  (error 'simple-type-error
         :datum stream
         :expected-type '(satisfies input-stream-p)
         :format-control "~S is not a binary input stream."
         :format-arguments (list stream)))
(defun ill-bout (stream &rest ignore)
  (declare (ignore ignore))
  (error 'simple-type-error
         :datum stream
         :expected-type '(satisfies output-stream-p)
         :format-control "~S is not a binary output stream."
         :format-arguments (list stream)))
(defun closed-flame (stream &rest ignore)
  (declare (ignore ignore))
  (error 'closed-stream-error :stream stream))
(defun closed-flame-saved (stream &rest ignore)
  (declare (ignore ignore))
  (error 'closed-saved-stream-error :stream stream))
(defun no-op-placeholder (&rest ignore)
  (declare (ignore ignore)))

;;; stream manipulation functions

(defun maybe-resolve-synonym-stream (stream)
  (labels ((recur (stream)
             (if (synonym-stream-p stream)
                 (recur (symbol-value (synonym-stream-symbol stream)))
                 stream)))
    (recur stream)))

(declaim (inline resolve-synonym-stream))
(defun resolve-synonym-stream (stream)
  (let ((result (symbol-value (synonym-stream-symbol stream))))
    (if (synonym-stream-p result)
        (maybe-resolve-synonym-stream result)
        result)))

(defmethod input-stream-p ((stream ansi-stream))
  (if (synonym-stream-p stream)
      (input-stream-p (resolve-synonym-stream stream))
      (and (not (eq (ansi-stream-in stream) #'closed-flame))
       ;;; KLUDGE: It's probably not good to have EQ tests on function
       ;;; values like this. What if someone's redefined the function?
       ;;; Is there a better way? (Perhaps just VALID-FOR-INPUT and
       ;;; VALID-FOR-OUTPUT flags? -- WHN 19990902
           (or (not (eq (ansi-stream-in stream) #'ill-in))
               (not (eq (ansi-stream-bin stream) #'ill-bin))))))

(defmethod output-stream-p ((stream ansi-stream))
  (if (synonym-stream-p stream)
      (output-stream-p (resolve-synonym-stream stream))
      (and (not (eq (ansi-stream-in stream) #'closed-flame))
           (or (not (eq (ansi-stream-cout stream) #'ill-out))
               (not (eq (ansi-stream-bout stream) #'ill-bout))))))

(defmethod open-stream-p ((stream ansi-stream))
  ;; CLHS 21.1.4 lets us not worry about synonym streams here.
  (let ((in (ansi-stream-in stream)))
    (not (or (eq in (load-time-value #'closed-flame t))
             (eq in (load-time-value #'closed-flame-saved t))))))

(defmethod stream-element-type ((stream ansi-stream))
  (call-ansi-stream-misc stream :element-type))

(defun stream-external-format (stream)
  (stream-api-dispatch (stream)
    :simple (s-%stream-external-format stream)
    :gray (error "~S is not defined for ~S" 'stream-external-format stream)
    :native (call-ansi-stream-misc stream :external-format)))

(defmethod interactive-stream-p ((stream ansi-stream))
  (call-ansi-stream-misc stream :interactive-p))

(defmethod close ((stream ansi-stream) &key abort)
  (unless (eq (ansi-stream-in stream) #'closed-flame)
    (call-ansi-stream-misc stream :close abort))
  t)

(defun set-closed-flame (stream)
  (setf (ansi-stream-in stream) #'closed-flame)
  (setf (ansi-stream-bin stream) #'closed-flame)
  (setf (ansi-stream-n-bin stream) #'closed-flame)
  (setf (ansi-stream-cout stream) #'closed-flame)
  (setf (ansi-stream-bout stream) #'closed-flame)
  (setf (ansi-stream-sout stream) #'closed-flame)
  (setf (ansi-stream-misc stream) #'closed-flame))

(defun set-closed-flame-by-slad (stream)
  (setf (ansi-stream-in stream) #'closed-flame-saved)
  (setf (ansi-stream-bin stream) #'closed-flame-saved)
  (setf (ansi-stream-n-bin stream) #'closed-flame-saved)
  (setf (ansi-stream-cout stream) #'closed-flame-saved)
  (setf (ansi-stream-bout stream) #'closed-flame-saved)
  (setf (ansi-stream-sout stream) #'closed-flame-saved)
  (setf (ansi-stream-misc stream) #'closed-flame-saved))

;;;; for file position and file length
(defun external-format-char-size (external-format)
  (ef-char-size (get-external-format external-format)))

;;; Call the MISC method with the :GET-FILE-POSITION operation.
(declaim (inline !ansi-stream-ftell)) ; named for the stdio inquiry function
(defun !ansi-stream-ftell (stream)
  (declare (type stream stream))
  ;; FIXME: It would be good to comment on the stuff that is done here...
  ;; FIXME: This doesn't look interrupt safe.
  (let ((res (call-ansi-stream-misc stream :get-file-position))
        (delta (- +ansi-stream-in-buffer-length+
                  (ansi-stream-in-index stream))))
    (if (eql delta 0)
        res
        (when res
         (let ((char-size (if (fd-stream-p stream)
                              (fd-stream-char-size stream)
                              (external-format-char-size (stream-external-format stream)))))
           (- res
              (etypecase char-size
                (function
                 (loop with buffer = (ansi-stream-csize-buffer stream)
                       with start = (ansi-stream-in-index stream)
                       for i from start below +ansi-stream-in-buffer-length+
                       sum (aref buffer i)))
                (fixnum
                 (* char-size delta)))))))))

;;; You're not allowed to specify NIL for the position but we were permitting
;;; it, which made it impossible to test for a bad call that tries to assign
;;; the position, versus a inquiry for the current position.
;;; CLHS specifies: file-position stream position-spec => success-p
;;; and position-spec is a "file position designator" which precludes NIL
;;; but the implementation methods can't detect supplied vs unsupplied.
;;; What a fubar API at the CL: layer. Was #'(SETF FILE-POSITION) not invented?
(defun file-position (stream &optional (position 0 suppliedp))
  (if suppliedp
      ;; Setter
      (let ((arg (the (or index (alien sb-unix:unix-offset) (member :start :end))
                      position)))
        (stream-api-dispatch (stream)
          :native (progn
                    (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
                    (call-ansi-stream-misc stream :set-file-position arg))
          ;; The impl method is expected to return a success indication as
          ;; a generalized boolean.
          :simple (s-%file-position stream arg)
          ;; I think our fndb entry is overconstrained - it says that this returns
          ;; either unsigned-byte or strict boolean, however CLHS says that setting
          ;; FILE-POSITION returns a /generalized boolean/.
          ;; A stream-specific method should be allowed to convey more information
          ;; than T/NIL yet we are forced to discard that information,
          ;; lest the return value constraint on this be violated.
          :gray (let ((result (stream-file-position stream arg)))
                  (if (numberp result) result (and result t)))))
      ;; Getter
      (let ((result (stream-api-dispatch (stream)
                      :native (!ansi-stream-ftell stream)
                      :simple (s-%file-position stream nil)
                      :gray (stream-file-position stream))))
        (the (or unsigned-byte null) result))))

(defmethod stream-file-position ((stream ansi-stream) &optional position)
  ;; Excuse me for asking, but why is this even a thing?
  ;; Users are not supposed to call the stream implementation directly,
  ;; they're supposed to call the function in the CL: package
  ;; which indirects to this. But if they do ... make it work.
  ;; And note that inlining of !ansi-stream-file-position would be pointless,
  ;; it's nearly 1K of code.
  ;; Oh, this is srsly wtf now. If POSITION is NIL,
  ;; then you must not call FILE-POSITION with both arguments.
  (if position
      (file-position stream position)
      (file-position stream)))

;;; This is a literal translation of the ANSI glossary entry "stream
;;; associated with a file".
;;;
;;; KLUDGE: Note that since Unix famously thinks "everything is a
;;; file", and in particular stdin, stdout, and stderr are files, we
;;; end up with this test being satisfied for weird things like
;;; *STANDARD-OUTPUT* (to a tty). That seems unlikely to be what the
;;; ANSI spec really had in mind, especially since this is used as a
;;; qualification for operations like FILE-LENGTH (so that ANSI was
;;; probably thinking of something like what Unix calls block devices)
;;; but I can't see any better way to do it. -- WHN 2001-04-14
(defun stream-file-stream (stream)
  "Test for the ANSI concept \"stream associated with a file\".

   Return NIL or the underlying FILE-STREAM."
  (typecase stream
    (file-stream stream)
    (synonym-stream
     (stream-file-stream (resolve-synonym-stream stream)))))

(defun stream-file-stream-or-lose (stream)
  (declare (type stream stream))
  (or (stream-file-stream stream)
      (error 'simple-type-error
             ;; KLUDGE: The ANSI spec for FILE-LENGTH specifically says
             ;; this should be TYPE-ERROR. But what then can we use for
             ;; EXPECTED-TYPE? This SATISFIES type (with a nonstandard
             ;; private predicate function..) is ugly and confusing, but
             ;; I can't see any other way. -- WHN 2001-04-14
             :datum stream
             :expected-type '(satisfies stream-file-stream)
             :format-control
             "~@<The stream ~2I~_~S ~I~_isn't associated with a file.~:>"
             :format-arguments (list stream))))

(defun stream-file-name-or-lose (stream)
  (or (file-name (stream-file-stream-or-lose stream))
      (error "~@<The stream ~2I~_~S ~I~_is not associated with a named file.~:>"
             stream)))

(defun file-string-length (stream object)
  (stream-api-dispatch (stream)
    :gray nil
    :simple (s-%file-string-length stream object)
    :native (call-ansi-stream-misc stream :file-string-length object)))

;;;; input functions

(defun ansi-stream-read-line-from-frc-buffer (stream eof-error-p eof-value)
  (prepare-for-fast-read-char stream
    (declare (ignore %frc-method%))
    (declare (type ansi-stream-cin-buffer %frc-buffer%))
    (let ((chunks-total-length 0)
          (chunks nil))
      (declare (type index chunks-total-length)
               (list chunks))
      (labels ((refill-buffer ()
                 (prog1 (fast-read-char-refill stream nil)
                   (setf %frc-index% (ansi-stream-in-index %frc-stream%))))
               (build-result (pos n-more-chars)
                 (let ((res (make-string (+ chunks-total-length n-more-chars)))
                       (start1 chunks-total-length))
                   (declare (type index start1))
                   (when (>= pos 0)
                     (replace res %frc-buffer%
                              :start1 start1 :start2 %frc-index% :end2 pos)
                     (setf %frc-index% (1+ pos)))
                   (done-with-fast-read-char)
                   (dolist (chunk chunks res)
                     (declare (type (simple-array character (*)) chunk))
                     (decf start1 (length chunk))
                     (replace res chunk :start1 start1)))))
        (declare (inline refill-buffer))
        (if (or (< %frc-index% +ansi-stream-in-buffer-length+) (refill-buffer))
            (loop
             (let ((pos (position #\Newline %frc-buffer%
                                  :test #'char= :start %frc-index%)))
               (when pos
                 (return (values (build-result pos (- pos %frc-index%)) nil)))
               (let ((chunk (subseq %frc-buffer% %frc-index%)))
                 (incf chunks-total-length (length chunk))
                 (push chunk chunks))
               (unless (refill-buffer)
                 (return (values (build-result -1 0) t)))))
          ;; EOF had been reached before we read anything
          ;; at all. Return the EOF value or signal the error.
            (progn (done-with-fast-read-char)
                   (eof-or-lose stream eof-error-p (values eof-value t))))))))

;; to potentially avoid consing a bufer on sucessive calls to read-line
;; (just consing the result string)
(define-load-time-global *read-line-buffers* nil)
(declaim (list *read-line-buffers*))

(declaim (inline ansi-stream-read-line))
(defun ansi-stream-read-line (stream eof-error-p eof-value)
  (if (ansi-stream-cin-buffer stream)
      ;; Stream has a fast-read-char buffer. Copy large chunks directly
      ;; out of the buffer.
      (ansi-stream-read-line-from-frc-buffer stream eof-error-p eof-value)
      ;; Slow path, character by character.
      ;; There is no need to use PREPARE-FOR-FAST-READ-CHAR
      ;; because the CIN-BUFFER is known to be NIL.
      (let ((ch (funcall (ansi-stream-in stream) stream nil 0)))
        (case ch
          (#\newline (values "" nil))
          (0 (values (eof-or-lose stream eof-error-p eof-value) t))
          (t
           (let* ((buffer (or (atomic-pop *read-line-buffers*)
                              (make-string 128)))
                  (res buffer)
                  (len (length res))
                  (eof)
                  (index 0))
             (declare (type (simple-array character (*)) buffer))
             (declare (optimize (sb-c:insert-array-bounds-checks 0)))
             (declare (index index))
             (setf (schar res index) (truly-the character ch))
             (incf index)
             (loop (case (setq ch (funcall (ansi-stream-in stream) stream nil 0))
                     (#\newline (return))
                     (0 (return (setq eof t)))
                     (t
                      (when (= index len)
                        (setq len (* len 2))
                        (let ((new (make-string len)))
                          (replace new res)
                          (setq res new)))
                      (setf (schar res index) (truly-the character ch))
                      (incf index))))
             (if (eq res buffer)
                 (setq res (subseq buffer 0 index))
                 (%shrink-vector res index))
             ;; Do not push an enlarged buffer, only the original one.
             (atomic-push buffer *read-line-buffers*)
             (values res eof)))))))

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value
                            recursive-p)
  (declare (explicit-check))
  (declare (ignore recursive-p))
  (stream-api-dispatch (stream :input)
    :simple (s-%read-line stream eof-error-p eof-value)
    :native (ansi-stream-read-line stream eof-error-p eof-value)
    :gray
        (multiple-value-bind (string eof) (stream-read-line stream)
          (if (and eof (zerop (length string)))
              (values (eof-or-lose stream eof-error-p eof-value) t)
              (values string eof)))))

;;; We proclaim them INLINE here, then proclaim them MAYBE-INLINE
;;; later on, so, except in this file, they are not inline by default,
;;; but they can be.
(declaim (inline read-char unread-char read-byte))

(declaim (inline ansi-stream-read-char))
(defun ansi-stream-read-char (stream eof-error-p eof-value recursive-p)
  (declare (ignore recursive-p))
  (prepare-for-fast-read-char stream
    (prog1
        (fast-read-char eof-error-p eof-value)
      (done-with-fast-read-char))))

(defun read-char (&optional (stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (declare (explicit-check))
  (stream-api-dispatch (stream :input)
    :native (ansi-stream-read-char stream eof-error-p eof-value recursive-p)
    ;; The final T is BLOCKING-P. I removed the ignored recursive-p arg.
    :simple (let ((char (s-%read-char stream eof-error-p eof-value t)))
              (if (eq char eof-value)
                  char
                  (the character char)))
    :gray
        (let ((char (stream-read-char stream)))
          (if (eq char :eof)
              (eof-or-lose stream eof-error-p eof-value)
              (the character char)))))

(declaim (inline ansi-stream-unread-char))
(defun ansi-stream-unread-char (character stream)
  (let ((index (1- (ansi-stream-in-index stream)))
        (buffer (ansi-stream-cin-buffer stream)))
    (declare (fixnum index))
    (when (minusp index) (error "nothing to unread"))
    (cond (buffer
           (setf (aref buffer index) character)
           (setf (ansi-stream-in-index stream) index)
           ;; Ugh. an ANSI-STREAM with a char buffer never gives a chance to
           ;; the stream's misc routine to handle the UNREAD operation.
           (when (ansi-stream-input-char-pos stream)
             (decf (ansi-stream-input-char-pos stream))))
          (t
           (call-ansi-stream-misc stream :unread character)))))

(defun unread-char (character &optional (stream *standard-input*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :input)
    :simple (s-%unread-char stream character)
    :native (ansi-stream-unread-char character stream)
    :gray (stream-unread-char stream character))
  nil)

(declaim (inline %ansi-stream-listen))
(defun %ansi-stream-listen (stream)
  (or (/= (the fixnum (ansi-stream-in-index stream))
          +ansi-stream-in-buffer-length+)
      (call-ansi-stream-misc stream :listen)))

(declaim (inline ansi-stream-listen))
(defun ansi-stream-listen (stream)
  (let ((result (%ansi-stream-listen stream)))
    (if (eq result :eof)
        nil
        result)))

(defun listen (&optional (stream *standard-input*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :input)
    :simple (error "Unimplemented") ; gets redefined
    :native (ansi-stream-listen stream)
    :gray (stream-listen stream)))

(declaim (inline ansi-stream-read-char-no-hang))
(defun ansi-stream-read-char-no-hang (stream eof-error-p eof-value recursive-p)
  (if (%ansi-stream-listen stream)
      ;; On T or :EOF get READ-CHAR to do the work.
      (ansi-stream-read-char stream eof-error-p eof-value recursive-p)
      nil))

(defun read-char-no-hang (&optional (stream *standard-input*)
                                    (eof-error-p t)
                                    eof-value
                                    recursive-p)
  (declare (explicit-check))
  (stream-api-dispatch (stream :input)
    :native
        (ansi-stream-read-char-no-hang stream eof-error-p eof-value
                                       recursive-p)
    ;; Absence of EOF-OR-LOSE here looks a little suspicious
    ;; considering that the impl function didn't use recursive-p
    :simple (s-%read-char-no-hang stream eof-error-p eof-value)
    :gray
        (let ((char (stream-read-char-no-hang stream)))
          (if (eq char :eof)
              (eof-or-lose stream eof-error-p eof-value)
              (the (or character null) char)))))

(declaim (inline ansi-stream-clear-input))
(defun ansi-stream-clear-input (stream)
  (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
  (call-ansi-stream-misc stream :clear-input))

(defun clear-input (&optional (stream *standard-input*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :input)
    :simple (error "Unimplemented") ; gets redefined
    :native (ansi-stream-clear-input stream)
    :gray (stream-clear-input stream))
  nil)

(declaim (inline ansi-stream-read-byte))
(defun ansi-stream-read-byte (stream eof-error-p eof-value recursive-p)
  ;; Why the "recursive-p" parameter?  a-s-r-b is funcall'ed from
  ;; a-s-read-sequence and needs a lambda list that's congruent with
  ;; that of a-s-read-char
  (declare (ignore recursive-p))
  (with-fast-read-byte (t stream eof-error-p eof-value)
    (fast-read-byte)))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (declare (explicit-check))
  (stream-api-dispatch (stream)
    :native (ansi-stream-read-byte stream eof-error-p eof-value nil)
    :simple (let ((byte (s-%read-byte stream eof-error-p eof-value)))
              (if (eq byte eof-value)
                  byte
                  (the integer byte)))
    :gray
      (let ((byte (stream-read-byte stream)))
        (if (eq byte :eof)
            (eof-or-lose stream eof-error-p eof-value)
            (the integer byte)))))

;;; Read NUMBYTES bytes into BUFFER beginning at START, and return the
;;; number of bytes read.
;;;
;;; Note: CMU CL's version of this had a special interpretation of
;;; EOF-ERROR-P which SBCL does not have. (In the EOF-ERROR-P=NIL
;;; case, CMU CL's version would return as soon as any data became
;;; available.) This could be useful behavior for things like pipes in
;;; some cases, but it wasn't being used in SBCL, so it was dropped.
;;; If we ever need it, it could be added later as a new variant N-BIN
;;; method (perhaps N-BIN-ASAP?) or something.
(declaim (inline read-n-bytes))
(defun read-n-bytes (stream buffer start numbytes &optional (eof-error-p t))
  (if (ansi-stream-p stream)
      (ansi-stream-read-n-bytes stream buffer start numbytes eof-error-p)
      ;; We don't need to worry about element-type size here is that
      ;; callers are supposed to have checked everything is kosher.
      (let* ((end (+ start numbytes))
             (read-end (stream-read-sequence stream buffer start end)))
        (eof-or-lose stream (and eof-error-p (< read-end end)) (- read-end start)))))

(defun ansi-stream-read-n-bytes (stream buffer start numbytes eof-error-p)
  (declare (type ansi-stream stream)
           (type index numbytes start)
           (type (or (simple-array * (*)) system-area-pointer) buffer))
  (let ((in-buffer (ansi-stream-in-buffer stream)))
    (unless in-buffer
      (return-from ansi-stream-read-n-bytes
        (funcall (ansi-stream-n-bin stream) stream buffer nil start numbytes eof-error-p)))
    (let* ((index (ansi-stream-in-index stream))
           (num-buffered (- +ansi-stream-in-buffer-length+ index)))
      ;; These bytes are of course actual bytes, i.e. 8-bit octets
      ;; and not variable-length bytes.
      (cond ((<= numbytes num-buffered)
             (%byte-blt in-buffer index buffer start numbytes)
             (setf (ansi-stream-in-index stream) (+ index numbytes))
             numbytes)
            (t
             (%byte-blt in-buffer index buffer start num-buffered)
             (let ((end (+ start num-buffered)))
               (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
               (+ (funcall (ansi-stream-n-bin stream) stream buffer nil
                           end (- numbytes num-buffered) eof-error-p)
                  num-buffered)))))))

;;; the amount of space we leave at the start of the in-buffer for
;;; unreading
;;;
;;; (It's 4 instead of 1 to allow word-aligned copies.)
(defconstant +ansi-stream-in-buffer-extra+
  4) ; FIXME: should be symbolic constant

;;; This function is called by the FAST-READ-CHAR expansion to refill
;;; the IN-BUFFER for text streams. There is definitely an IN-BUFFER,
;;; and hence must be an N-BIN method. It's also called by other stream
;;; functions which directly peek into the frc buffer.
;;; If EOF is hit and EOF-ERROR-P is false, then return NIL,
;;; otherwise return the new index into CIN-BUFFER.
(defun fast-read-char-refill (stream eof-error-p)
  (when (ansi-stream-input-char-pos stream)
    ;; Characters between (ANSI-STREAM-IN-INDEX %FRC-STREAM%)
    ;; and +ANSI-STREAM-IN-BUFFER-LENGTH+ have to be re-scanned.
    (update-input-char-pos stream))
  (let* ((ibuf (ansi-stream-cin-buffer stream))
         (sizebuf (ansi-stream-csize-buffer stream))
         (count (funcall (ansi-stream-n-bin stream)
                         stream
                         ibuf
                         sizebuf
                         +ansi-stream-in-buffer-extra+
                         (- +ansi-stream-in-buffer-length+
                            +ansi-stream-in-buffer-extra+)))
         (start (- +ansi-stream-in-buffer-length+ count)))
    (declare (type index start count))
    (cond ((zerop count)
           ;; An empty count does not necessarily mean that we reached
           ;; the EOF, it's also possible that it's e.g. due to a
           ;; invalid octet sequence in a multibyte stream. To handle
           ;; the resyncing case correctly we need to call the reading
           ;; function and check whether an EOF was really reached. If
           ;; not, we can just fill the buffer by one character, and
           ;; hope that the next refill will not need to resync.
           ;;
           ;; KLUDGE: we can't use FD-STREAM functions (which are the
           ;; only ones which will give us decoding errors) here,
           ;; because this code is generic.  We can't call the N-BIN
           ;; function, because near the end of a real file that can
           ;; legitimately bounce us to the IN function.  So we have
           ;; to call ANSI-STREAM-IN.
           (let ((index (1- +ansi-stream-in-buffer-length+)))
             (multiple-value-bind (value size)
                 (funcall (ansi-stream-in stream) stream nil :eof)
               (cond
                 ;; When not signaling an error, it is important that IN-INDEX
                 ;; be set to +ANSI-STREAM-IN-BUFFER-LENGTH+ here, even though
                 ;; DONE-WITH-FAST-READ-CHAR will do the same, thereby writing
                 ;; the caller's %FRC-INDEX% (= +ANSI-STREAM-IN-BUFFER-LENGTH+)
                 ;; into the slot. But because we've already bumped INPUT-CHAR-POS
                 ;; and scanned characters between the original %FRC-INDEX%
                 ;; and the buffer end (above), we must *not* do that again.
                 ((eql value :eof)
                  ;; definitely EOF now
                  (setf (ansi-stream-in-index stream)
                        +ansi-stream-in-buffer-length+)
                  (eof-or-lose stream eof-error-p nil))
                 ;; we resynced or were given something instead
                 (t
                  (setf (aref ibuf index) value)
                  (setf (aref sizebuf index) size)
                  (setf (ansi-stream-in-index stream) index))))))
          (t
           (when (/= start +ansi-stream-in-buffer-extra+)
             (#.(let* ((n-character-array-bits
                        (sb-vm:saetp-n-bits
                         (find 'character
                               sb-vm:*specialized-array-element-type-properties*
                               :key #'sb-vm:saetp-specifier)))
                       (bash-function (intern (format nil "UB~D-BASH-COPY" n-character-array-bits)
                                              (find-package "SB-KERNEL"))))
                  bash-function)
                ibuf +ansi-stream-in-buffer-extra+
                ibuf start
                count)
             (replace sizebuf sizebuf :start1 start :end1 (+ start count)
                      :start2 +ansi-stream-in-buffer-extra+))
           (setf (ansi-stream-in-index stream) start)))))

;;; This is similar to FAST-READ-CHAR-REFILL, but we don't have to
;;; leave room for unreading.
(defun fast-read-byte-refill (stream eof-error-p eof-value)
  (let* ((ibuf (ansi-stream-in-buffer stream))
         (count (funcall (ansi-stream-n-bin stream) stream
                         ibuf nil 0 +ansi-stream-in-buffer-length+
                         nil))
         (start (- +ansi-stream-in-buffer-length+ count)))
    (declare (type index start count))
    (cond ((zerop count)
           (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
           (funcall (ansi-stream-bin stream) stream eof-error-p eof-value))
          (t
           (unless (zerop start)
             (ub8-bash-copy ibuf 0
                            ibuf start
                            count))
           (setf (ansi-stream-in-index stream) (1+ start))
           (aref ibuf start)))))

;;; output functions

(defun write-char (character &optional (stream *standard-output*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :output)
    :native (return-from write-char (funcall (ansi-stream-cout stream) stream character))
    :simple (s-%write-char stream character)
    :gray (stream-write-char stream character))
  character)

(defun terpri (&optional (stream *standard-output*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :output)
    :native (funcall (ansi-stream-cout stream) stream #\Newline)
    :simple (s-%terpri stream)
    :gray (stream-terpri stream))
  nil)

(defun fresh-line (&optional (stream *standard-output*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :output)
    :native (unless (eql (charpos stream) 0)
              (funcall (ansi-stream-cout stream) stream #\newline)
              t)
    :simple (s-%fresh-line stream)
    :gray (stream-fresh-line stream)))

(macrolet
    ((define (name)
       `(defun ,name (string stream start end)
          (declare (optimize (sb-c:verify-arg-count 0)))
          ;; unclear why the dispatch to simple and gray methods have to receive a simple-string.
          ;; I'm pretty sure the STREAM-foo methods on gray streams are not specified to be
          ;; constrained to receive only simple-string.
          ;; So they must either do their their own "unwrapping" of a complex string, or
          ;; instead just access character-at-a-time or inefficiently call SUBSEQ.
          (with-array-data ((data string) (start start) (end end) :check-fill-pointer t)
            (stream-api-dispatch (stream :output)
              :native (progn (funcall (ansi-stream-sout stream) stream data start end)
                             ,@(when (eq name '%write-line)
                                 '((funcall (ansi-stream-cout stream) stream #\newline))))
              :simple (,(symbolicate "S-" name) stream data start end)
              :gray (progn (stream-write-string stream data start end)
                           ,@(when (eq name '%write-line)
                               '((stream-write-char stream #\newline))))))
          string)))
  (define %write-line)
  (define %write-string))

(defun write-string (string &optional (stream *standard-output*)
                            &key (start 0) end)
  (declare (explicit-check))
  (%write-string string stream start end))

(defun write-line (string &optional (stream *standard-output*)
                   &key (start 0) end)
  (declare (explicit-check))
  (%write-line string stream start end))

(defun charpos (&optional (stream *standard-output*))
  (stream-api-dispatch (stream :output)
    :native (call-ansi-stream-misc stream :charpos)
    :simple (s-%charpos stream)
    :gray (stream-line-column stream)))

(defun line-length (&optional (stream *standard-output*))
  (stream-api-dispatch (stream :output)
    :native (call-ansi-stream-misc stream :line-length)
    :simple (s-%line-length stream)
    :gray (stream-line-length stream)))

(defun finish-output (&optional (stream *standard-output*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :output)
    :native (call-ansi-stream-misc stream :finish-output)
    :simple (s-%finish-output stream)
    :gray (stream-finish-output stream))
  nil)

(defun force-output (&optional (stream *standard-output*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :output)
    :native (call-ansi-stream-misc stream :force-output)
    :simple (s-%force-output stream)
    :gray (stream-force-output stream))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  (declare (explicit-check))
  (stream-api-dispatch (stream :output)
    :native (call-ansi-stream-misc stream :clear-output)
    :simple (s-%clear-output stream)
    :gray (stream-clear-output stream))
  nil)

(defun write-byte (integer stream)
  (declare (explicit-check))
  ;; The STREAM argument is not allowed to be a designator.
  (stream-api-dispatch (stream)
    :native (return-from write-byte (funcall (ansi-stream-bout stream) stream integer))
    :simple (s-%write-byte stream integer)
    :gray (stream-write-byte stream integer))
  integer)


(declaim (maybe-inline read-char unread-char read-byte)) ; too big

;;; This is called from ANSI-STREAM routines that encapsulate CLOS
;;; streams to handle the misc routines and dispatch to the
;;; appropriate SIMPLE- or FUNDAMENTAL-STREAM functions.
(defun stream-misc-dispatch (stream operation arg)
  (if (simple-stream-p stream)

      ;; Dispatch to a simple-stream implementation function
      (stream-misc-case (operation)
        (:listen (listen stream)) ; call the redefined LISTEN
        (:unread (s-%unread-char stream arg))
        (:close (error "Attempted to close inner stream ~S" stream))
        (:clear-input (clear-input stream)) ; call the redefined CLEAR-INPUT
        (:force-output (s-%force-output stream))
        (:finish-output (s-%finish-output stream))
        (:clear-output (s-%clear-output stream))
        ;; All simple-streams use (UNSIGNED-BYTE 8) - it's one of the
        ;; salient distinctions between simple-streams and Gray streams.
        ;; See (DEFMETHOD STREAM-ELEMENT-TYPE ((STREAM SIMPLE-STREAM)) ...)
        (:element-type '(unsigned-byte 8))
        ;; FIXME: All simple-streams are actually bivalent. We historically have
        ;; returned UNSIGNED-BYTE based on element-type. But this is wrong!
        (:element-mode 'UNSIGNED-BYTE)
        ;; This call returns an instance of the format structure defined
        ;; by the SB-SIMPLE-STREAMS package, not the SB-IMPL:: structure.
        ;; This also needs to be fixed.
        (:external-format (s-%stream-external-format stream))
        (:interactive-p (interactive-stream-p stream))
        (:line-length (s-%line-length stream))
        (:charpos (s-%charpos stream))
        (:file-length (s-%file-length stream))
        (:file-string-length (s-%file-string-length stream arg))
        (:set-file-position (s-%file-position stream arg))
        ;; yeesh, this wants a _required_ NIL argument to mean "inquire".
        (:get-file-position (s-%file-position stream nil)))

      ;; else call the generic function
      (stream-misc-case (operation)
       (:listen (stream-listen stream))
       (:unread (stream-unread-char stream arg)) ; specialized arg first
       (:close (error "Attempted to close inner stream ~S" stream))
       (:clear-input (stream-clear-input stream))
       (:force-output (stream-force-output stream))
       (:finish-output (stream-finish-output stream))
       (:clear-output (stream-clear-output stream))
       (:element-type (stream-element-type stream))
       (:element-mode
        (stream-element-type-stream-element-mode (stream-element-type stream)))
       (:interactive-p (interactive-stream-p stream)) ; is generic
       (:line-length (stream-line-length stream))
       (:charpos (stream-line-column stream))
       (:set-file-position (stream-file-position stream arg))
       (:get-file-position (stream-file-position stream))
       ;; This last bunch of pseudo-methods will probably just signal an error
       ;; since they aren't generic and don't work on Gray streams.
       (:external-format (stream-external-format stream))
       (:file-length (file-length stream))
       (:file-string-length (file-string-length stream arg)))))

(declaim (inline stream-element-mode))
(defun stream-element-mode (stream)
  (declare (type stream stream))
  (cond
    ((fd-stream-p stream)
     (fd-stream-element-mode stream))
    ((and (ansi-stream-p stream)
          (call-ansi-stream-misc stream :element-mode)))
    (t
     (stream-element-type-stream-element-mode
      (stream-element-type stream)))))

;;;; broadcast streams

(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (unless (output-stream-p stream)
      (error 'type-error
             :datum stream
             :expected-type '(satisfies output-stream-p))))
  (let ((stream (%make-broadcast-stream streams)))
    (unless streams
      (flet ((out (stream arg)
               (declare (ignore stream)
                        (optimize speed (safety 0)))
               arg)
             (sout (stream string start end)
               (declare (ignore stream string start end)
                        (optimize speed (safety 0)))))
        (setf (broadcast-stream-cout stream) #'out
              (broadcast-stream-bout stream) #'out
              (broadcast-stream-sout stream) #'sout)))
    stream))

(macrolet ((out-fun (name fun args return)
             `(defun ,name (stream ,@args)
                (dolist (stream (broadcast-stream-streams stream) ,return)
                  (,fun ,(car args) stream ,@(cdr args))))))
  (out-fun broadcast-cout write-char (char) char)
  (out-fun broadcast-bout write-byte (byte) byte)
  (out-fun broadcast-sout %write-string (string start end) nil))

(defun broadcast-misc (stream operation arg1)
  (let ((streams (broadcast-stream-streams stream)))
    (stream-misc-case (operation)
      ;; FIXME: This may not be the best place to note this, but I
      ;; think the :CHARPOS protocol needs revision.  Firstly, I think
      ;; this is the last place where a NULL return value was possible
      ;; (before adjusting it to be 0), so a bunch of conditionals IF
      ;; CHARPOS can be removed; secondly, it is my belief that
      ;; FD-STREAMS, when running FILE-POSITION, do not update the
      ;; CHARPOS, and consequently there will be much wrongness.
      ;;
      ;; FIXME: see also TWO-WAY-STREAM treatment of :CHARPOS -- why
      ;; is it testing the :charpos of an input stream?
      ;;
      ;; -- CSR, 2004-02-04
      (:charpos
       (dolist (stream streams 0)
         (let ((charpos (charpos stream)))
           (when charpos
             (return charpos)))))
      (:line-length
       (let ((min nil))
         (dolist (stream streams min)
           (let ((res (line-length stream)))
             (when res (setq min (if min (min res min) res)))))))
      (:element-type
       (let ((last (last streams)))
         (if last
             (stream-element-type (car last))
             t)))
      (:element-mode
       (awhen (last streams)
         (stream-element-mode (car it))))
      (:external-format
       (let ((last (last streams)))
         (if last
             (stream-external-format (car last))
             :default)))
      (:file-length
       (let ((last (last streams)))
         (if last
             (file-length (car last))
             0)))
      (:set-file-position
           (let ((res (or (eql arg1 :start) (eql arg1 0))))
             (dolist (stream streams res)
               (setq res (file-position stream arg1)))))
      (:get-file-position
           (let ((last (last streams)))
             (if last
                 (file-position (car last))
                 0)))
      (:file-string-length
       (let ((last (last streams)))
         (if last
             (file-string-length (car last) arg1)
             1)))
      (:close
         ;; I don't know how something is trying to close the
         ;; universal sink stream, but it is. Stop it from happening.
         (unless (eq stream *null-broadcast-stream*)
           (set-closed-flame stream)))
      (t
       (let ((res nil))
         (dolist (stream streams res)
           (setq res
                 (if (ansi-stream-p stream)
                     (call-ansi-stream-misc stream operation arg1)
                     (stream-misc-dispatch stream operation arg1)))))))))

;;;; synonym streams

(defmethod print-object ((x synonym-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream ":SYMBOL ~S" (synonym-stream-symbol x))))

;;; The output simple output methods just call the corresponding
;;; function on the synonymed stream.
(macrolet ((out-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                (declare (optimize (safety 1) (sb-c:verify-arg-count 0)))
                (let ((syn (symbol-value (synonym-stream-symbol stream))))
                  (,fun ,(car args) syn ,@(cdr args))))))
  (out-fun synonym-out write-char ch)
  (out-fun synonym-bout write-byte n)
  (out-fun synonym-sout %write-string string start end))

;;; For the input methods, we just call the corresponding function on the
;;; synonymed stream. These functions deal with getting input out of
;;; the In-Buffer if there is any.
(macrolet ((in-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                (declare (optimize (safety 1) (sb-c:verify-arg-count 0)))
                ,@(when (member 'sbuffer args) '((declare (ignore sbuffer))))
                (,fun (symbol-value (synonym-stream-symbol stream))
                      ,@(remove 'sbuffer args)))))
  (in-fun synonym-in read-char eof-error-p eof-value)
  (in-fun synonym-bin read-byte eof-error-p eof-value)
  (in-fun synonym-n-bin read-n-bytes buffer sbuffer start numbytes eof-error-p))

(defun synonym-misc (stream operation arg1)
  (declare (optimize (safety 1)))
  ;; CLHS 21.1.4 implies that CLOSE on a synonym stream closes the synonym stream in that
  ;; "The consequences are undefined if the synonym stream symbol is not bound to an open
  ;;  stream from the time of the synonym stream's creation until the time it is closed."
  ;;         The antecent of this "it" is the synonym stream --------------^
  ;; which means that there exist a way to close synonym streams.
  ;; We can presume that CLOSE is that way, despite some text seemingly to the contrary
  ;;  "Any operations on a synonym stream will be performed on the stream that is then
  ;;   the value of the dynamic variable named by the synonym stream symbol."
  ;; so "any" in that sentence mean "almost any, with a notable exception".
  (stream-misc-case (operation)
   (:close
    (set-closed-flame stream))
   (t
    (let ((syn (symbol-value (synonym-stream-symbol stream))))
     (if (ansi-stream-p syn)
        ;; We have to special-case some operations which interact with
        ;; the in-buffer of the wrapped stream, since just calling
        ;; ANSI-STREAM-MISC on them
        (stream-misc-case (operation)
          (:listen (%ansi-stream-listen syn))
          (:clear-input (clear-input syn))
          (:unread (unread-char arg1 syn))
          (t
           (call-ansi-stream-misc syn operation arg1)))
        (stream-misc-dispatch syn operation arg1))))))

;;;; two-way streams

(defstruct (two-way-stream
            (:include ansi-stream
                      (in #'two-way-in)
                      (bin #'two-way-bin)
                      (n-bin #'two-way-n-bin)
                      (cout #'two-way-out)
                      (bout #'two-way-bout)
                      (sout #'two-way-sout)
                      (misc #'two-way-misc))
            (:constructor %make-two-way-stream (input-stream output-stream))
            (:copier nil)
            (:predicate nil))
  (input-stream (missing-arg) :type stream :read-only t)
  (output-stream (missing-arg) :type stream :read-only t))

(defmethod print-object ((x two-way-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream
            ":INPUT-STREAM ~S :OUTPUT-STREAM ~S"
            (two-way-stream-input-stream x)
            (two-way-stream-output-stream x))))

(defun make-two-way-stream (input-stream output-stream)
  "Return a bidirectional stream which gets its input from INPUT-STREAM and
   sends its output to OUTPUT-STREAM."
  (unless (output-stream-p output-stream)
    (error 'type-error
           :datum output-stream
           :expected-type '(satisfies output-stream-p)))
  (unless (input-stream-p input-stream)
    (error 'type-error
           :datum input-stream
           :expected-type '(satisfies input-stream-p)))
  (%make-two-way-stream input-stream output-stream))

(macrolet ((out-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                (let ((syn (two-way-stream-output-stream stream)))
                  (,fun ,(car args) syn ,@(cdr args))))))
  (out-fun two-way-out write-char ch)
  (out-fun two-way-bout write-byte n)
  (out-fun two-way-sout %write-string string start end))

(macrolet ((in-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                ,@(when (member 'sbuffer args) '((declare (ignore sbuffer))))
                (,fun (two-way-stream-input-stream stream) ,@(remove 'sbuffer args)))))
  (in-fun two-way-in read-char eof-error-p eof-value)
  (in-fun two-way-bin read-byte eof-error-p eof-value)
  (in-fun two-way-n-bin read-n-bytes buffer sbuffer start numbytes eof-error-p))

(defun two-way-misc (stream operation arg1)
  (let* ((in (two-way-stream-input-stream stream))
         (out (two-way-stream-output-stream stream))
         (in-ansi-stream-p (ansi-stream-p in))
         (out-ansi-stream-p (ansi-stream-p out)))
    (stream-misc-case (operation)
      (:listen
       (if in-ansi-stream-p
           (%ansi-stream-listen in)
           (listen in)))
      ((:finish-output :force-output :clear-output)
       (if out-ansi-stream-p
           (call-ansi-stream-misc out operation arg1)
           (stream-misc-dispatch out operation arg1)))
      (:clear-input (clear-input in))
      (:unread (unread-char arg1 in))
      (:element-type
       (let ((in-type (stream-element-type in))
             (out-type (stream-element-type out)))
         (if (equal in-type out-type)
             in-type
             `(and ,in-type ,out-type))))
      (:element-mode
       (let ((in-mode (stream-element-mode in))
             (out-mode (stream-element-mode out)))
         (when (equal in-mode out-mode)
           in-mode)))
      (:close
       (set-closed-flame stream))
      (t
       (or (if in-ansi-stream-p
               (call-ansi-stream-misc in operation arg1)
               (stream-misc-dispatch in operation arg1))
           (if out-ansi-stream-p
               (call-ansi-stream-misc out operation arg1)
               (stream-misc-dispatch out operation arg1)))))))

;;;; concatenated streams

(defstruct (concatenated-stream
            (:include ansi-stream
                      (in #'concatenated-in)
                      (bin #'concatenated-bin)
                      (n-bin #'concatenated-n-bin)
                      (misc #'concatenated-misc))
            (:constructor %make-concatenated-stream (list))
            (:copier nil)
            (:predicate nil))
  ;; The car of this is the substream we are reading from now.
  ;; This is not named CONCATENATED-STREAM-STREAMS because user modification
  ;; via (SETF CONCATENATED-STREAM-STREAMS) is not conforming.
  (list nil :type list))
(declaim (freeze-type concatenated-stream))
(defun concatenated-stream-streams (stream) ; standard function
  (concatenated-stream-list stream))

(defmethod print-object ((x concatenated-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream ":STREAMS ~S" (concatenated-stream-list x))))

(defun make-concatenated-stream (&rest streams)
  "Return a stream which takes its input from each of the streams in turn,
   going on to the next at EOF."
  (dolist (stream streams)
    (unless (input-stream-p stream)
      (error 'type-error
             :datum stream
             :expected-type '(satisfies input-stream-p))))
  (%make-concatenated-stream streams))

(macrolet ((in-fun (name fun)
             `(defun ,name (stream eof-error-p eof-value)
                (do ((streams (concatenated-stream-list stream)
                              (cdr streams)))
                    ((null streams)
                     (eof-or-lose stream eof-error-p eof-value))
                  (let* ((stream (car streams))
                         (result (,fun stream nil nil)))
                    (when result (return result)))
                  (pop (concatenated-stream-list stream))))))
  (in-fun concatenated-in read-char)
  (in-fun concatenated-bin read-byte))

(defun concatenated-n-bin (stream buffer sbuffer start numbytes eof-errorp)
  (declare (ignore sbuffer))
  (do ((streams (concatenated-stream-list stream) (cdr streams))
       (current-start start)
       (remaining-bytes numbytes))
      ((null streams)
       (if eof-errorp
           (error 'end-of-file :stream stream)
           (- numbytes remaining-bytes)))
    (let* ((stream (car streams))
           (bytes-read (read-n-bytes stream buffer current-start
                                     remaining-bytes nil)))
      (incf current-start bytes-read)
      (decf remaining-bytes bytes-read)
      (when (zerop remaining-bytes) (return numbytes)))
    (setf (concatenated-stream-list stream) (cdr streams))))

(defun concatenated-misc (stream operation arg1)
  (let* ((left (concatenated-stream-list stream))
         (current (car left)))
    (stream-misc-case (operation)
      (:listen
       (unless left
         (return-from concatenated-misc :eof))
       (loop
        (let ((stuff (if (ansi-stream-p current)
                         (%ansi-stream-listen current)
                         (stream-misc-dispatch current operation arg1))))
          (cond ((eq stuff :eof)
                 ;; Advance STREAMS, and try again.
                 (pop (concatenated-stream-list stream))
                 (setf current (car (concatenated-stream-list stream)))
                 (unless current
                   ;; No further streams. EOF.
                   (return :eof)))
                (stuff
                 ;; Stuff's available.
                 (return t))
                (t
                 ;; Nothing is available yet.
                 (return nil))))))
      (:clear-input (when left (clear-input current)))
      (:unread (when left (unread-char arg1 current)))
      (:close
       (set-closed-flame stream))
      (t
       (when left
         (if (ansi-stream-p current)
             (call-ansi-stream-misc current operation arg1)
             (stream-misc-dispatch current operation arg1)))))))

;;;; echo streams

(defstruct (echo-stream
            (:include two-way-stream
                      (in #'echo-in)
                      (bin #'echo-bin)
                      (misc #'echo-misc)
                      (n-bin #'echo-n-bin))
            (:constructor %make-echo-stream (input-stream output-stream))
            (:copier nil)
            (:predicate nil))
  (unread-stuff nil :type boolean))
(declaim (freeze-type two-way-stream))

(defmethod print-object ((x echo-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream
            ":INPUT-STREAM ~S :OUTPUT-STREAM ~S"
            (two-way-stream-input-stream x)
            (two-way-stream-output-stream x))))

(defun make-echo-stream (input-stream output-stream)
  "Return a bidirectional stream which gets its input from INPUT-STREAM and
   sends its output to OUTPUT-STREAM. In addition, all input is echoed to
   the output stream."
  (unless (output-stream-p output-stream)
    (error 'type-error
           :datum output-stream
           :expected-type '(satisfies output-stream-p)))
  (unless (input-stream-p input-stream)
    (error 'type-error
           :datum input-stream
           :expected-type '(satisfies input-stream-p)))
  (%make-echo-stream input-stream output-stream))

(macrolet ((in-fun (name in-fun out-fun &rest args)
             `(defun ,name (stream ,@args)
                (let* ((unread-stuff-p (echo-stream-unread-stuff stream))
                       (in (echo-stream-input-stream stream))
                       (out (echo-stream-output-stream stream))
                       (result (if eof-error-p
                                   (,in-fun in ,@args)
                                   (,in-fun in nil in))))
                  (setf (echo-stream-unread-stuff stream) nil)
                  (cond
                    ((eql result in) eof-value)
                    ;; If unread-stuff was true, the character read
                    ;; from the input stream was previously echoed.
                    (t (unless unread-stuff-p (,out-fun result out)) result))))))
  (in-fun echo-in read-char write-char eof-error-p eof-value)
  (in-fun echo-bin read-byte write-byte eof-error-p eof-value))

(defun echo-n-bin (stream buffer sbuffer start numbytes eof-error-p)
  (declare (ignore sbuffer))
  (let ((bytes-read 0))
    ;; Note: before ca 1.0.27.18, the logic for handling unread
    ;; characters never could have worked, so probably nobody has ever
    ;; tried doing bivalent block I/O through an echo stream; this may
    ;; not work either.
    (when (echo-stream-unread-stuff stream)
      (let* ((char (read-char stream))
             (octets (string-to-octets
                      (string char)
                      :external-format
                      (stream-external-format
                       (echo-stream-input-stream stream))))
             (octet-count (length octets))
             (blt-count (min octet-count numbytes)))
        (replace buffer octets :start1 start :end1 (+ start blt-count))
        (incf start blt-count)
        (decf numbytes blt-count)))
    (incf bytes-read (read-n-bytes (echo-stream-input-stream stream) buffer
                                   start numbytes nil))
    (cond
      ((not eof-error-p)
       (write-seq-impl buffer (echo-stream-output-stream stream)
                       start (+ start bytes-read))
       bytes-read)
      ((> numbytes bytes-read)
       (write-seq-impl buffer (echo-stream-output-stream stream)
                       start (+ start bytes-read))
       (error 'end-of-file :stream stream))
      (t
       (write-seq-impl buffer (echo-stream-output-stream stream)
                       start (+ start bytes-read))
       (aver (= numbytes (+ start bytes-read)))
       numbytes))))

;;;; STRING-INPUT-STREAM stuff

(defstruct (string-input-stream
             (:include ansi-stream (misc #'string-in-misc))
             (:constructor nil)
             (:copier nil)
             (:predicate nil))
  ;; Indices into STRING
  (index nil :type index)
  (limit nil :type index :read-only t)
  ;; Backing string after following displaced array chain
  (string nil :type simple-string :read-only t)
  ;; So that we know what string index FILE-POSITION 0 correponds to
  (start nil :type index :read-only t))

(declaim (freeze-type string-input-stream))

(defun string-in-misc (stream operation arg1)
  (declare (type string-input-stream stream))
  (stream-misc-case (operation :default nil)
    (:set-file-position
         (setf (string-input-stream-index stream)
               (case arg1
                 (:start (string-input-stream-start stream))
                 (:end (string-input-stream-limit stream))
                 ;; We allow moving position beyond EOF. Errors happen
                 ;; on read, not move.
                 (t (+ (string-input-stream-start stream) arg1)))))
    (:get-file-position
         (- (string-input-stream-index stream)
            (string-input-stream-start stream)))
    ;; According to ANSI: "Should signal an error of type type-error
    ;; if stream is not a stream associated with a file."
    ;; This is checked by FILE-LENGTH, so no need to do it here either.
    ;; (:file-length (length (string-input-stream-string stream)))
    (:unread (setf (string-input-stream-index stream)
                   ;; silently ignore attempts to go backwards too far
                   (max (1- (string-input-stream-index stream))
                        (string-input-stream-start stream))))
    (:close (set-closed-flame stream))
    (:listen (if (< (string-input-stream-index stream)
                    (string-input-stream-limit stream))
                 t :eof))
    (:element-type (array-element-type (string-input-stream-string stream)))
    (:element-mode 'character)))

;;; Since we don't want to insert ~300 bytes of code at every site
;;; of WITH-INPUT-FROM-STRING, and we lack a way to perform partial inline
;;; dx allocation of structures, this'll have to do.
(defun %init-string-input-stream (stream string &optional (start 0) end)
  (declare (explicit-check string))
  (macrolet ((initforms ()
               `(progn
                 ,@(mapcar (lambda (dsd)
                             ;; good thing we have no raw slots in stream structures
                             `(%instance-set stream ,(dsd-index dsd)
                                   ,(case (dsd-name dsd)
                                     ((index start) 'start)
                                     (limit 'end)
                                     (string 'simple-string)
                                     (in 'input-routine)
                                     (t (dsd-default dsd)))))
                           (dd-slots
                            (find-defstruct-description 'string-input-stream)))))
             (char-in (element-type)
               `(let ((index (string-input-stream-index
                              (truly-the string-input-stream stream)))
                      (string (truly-the (simple-array ,element-type (*))
                                         (string-input-stream-string stream))))
                  (cond ((>= index (string-input-stream-limit stream))
                         (eof-or-lose stream eof-error-p eof-value))
                        (t
                         (setf (string-input-stream-index stream) (1+ index))
                         (char string index))))))
    (flet ((base-char-in (stream eof-error-p eof-value)
             (declare (optimize (sb-c::verify-arg-count 0)
                                (sb-c:insert-array-bounds-checks 0)))
             (char-in base-char))
           (character-in (stream eof-error-p eof-value)
             (declare (optimize (sb-c::verify-arg-count 0)
                                (sb-c:insert-array-bounds-checks 0)))
             (char-in character)))
      (let ((input-routine
             (etypecase string
               (base-string #'base-char-in)
               (string #'character-in))))
        (with-array-data ((simple-string string :offset-var offset)
                          (start start)
                          (end end)
                          :check-fill-pointer t)
          (initforms)
          (values (truly-the string-input-stream stream)
                  offset))))))

;;; It's debatable whether we should try to convert
;;;  (let ((s (make-string-input-stream))) (declare (dynamic-extent s)) ...)
;;; into the thing that WITH-INPUT-FROM-STRING does. That's what the macro is for.
(defun make-string-input-stream (string &optional (start 0) end)
  "Return an input stream which will supply the characters of STRING between
  START and END in order."
  (macrolet ((make () `(%make-structure-instance
                        ,(find-defstruct-description 'string-input-stream)
                        nil)))
    ;; kill the secondary value
    (values (%init-string-input-stream (make) string start end))))

;;;; STRING-OUTPUT-STREAM stuff
;;;;
;;;; FIXME: This, like almost none of the stream code is particularly
;;;; interrupt or thread-safe. While it should not be possible to
;;;; corrupt the heap here, it certainly is possible to end up with
;;;; a string-output-stream whose internal state is messed up.
;;;;
(defun %init-string-output-stream (stream buffer wild-result-type)
  (declare (optimize speed (sb-c::verify-arg-count 0)))
  (declare (string buffer)
           (ignorable wild-result-type)) ; if #-sb-unicode
  (macrolet ((initforms ()
               `(progn
                  ,@(mapcar (lambda (dsd)
                              `(%instance-set stream ,(dsd-index dsd)
                                       ,(case (dsd-name dsd)
                                          (sout '#'string-sout) ; global fun
                                          (misc '#'misc) ; local fun
                                          ((element-type unicode-p cout sout-aux buffer)
                                           (dsd-name dsd))
                                          (t (dsd-default dsd)))))
                                (dd-slots
                                 (find-defstruct-description 'string-output-stream)))))
             (cout (elt-type)
               `(let ((pointer (string-output-stream-pointer
                                (truly-the string-output-stream stream)))
                      (buffer (truly-the (simple-array ,elt-type (*))
                                         (string-output-stream-buffer stream)))
                      (index (string-output-stream-index stream)))
                  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                  (when (= pointer (length buffer))
                    ;; The usual doubling technique: the new buffer shall hold as many
                    ;; characters as were already emplaced.
                    (setf buffer (string-output-stream-new-buffer stream index)
                          pointer 0))
                  (setf (string-output-stream-pointer stream) (truly-the index (1+ pointer)))
                  (setf (string-output-stream-index stream) (truly-the index (1+ index)))
                  ;; return the character
                  (setf (aref (truly-the (simple-array ,elt-type (*)) buffer) pointer)
                        char)))
             (sout (elt-type)
               ;; Only one case cares whether the string contains non-base chars.
               ;;  base-string source and buffer : OK
               ;;  base-string source, character-string buffer : OK
               ;;  character-string source, base-string buf verifies base-char on copy
               ;;  character-string source + buf needs a pre-scan for Unicode.
               `(etypecase src
                  #+sb-unicode
                  (simple-character-string
                   ,@(when (eq elt-type 'character)
                       ;; When UNICODE-P is NIL, meaning no non-base chars were seen yet
                       ;; in the input, pre-scan to see whether that still holds.
                       '((when (and (not (string-output-stream-unicode-p
                                          (truly-the string-output-stream stream)))
                                    (input-contains-unicode))
                           (setf (string-output-stream-unicode-p stream) t
                                 ;; no need to keep checking each character
                                 (ansi-stream-cout stream) #'character-out))))
                   ;; There are transforms for all the necessary REPLACE variations.
                   (replace (truly-the (simple-array ,elt-type (*)) dst)
                            (truly-the simple-character-string src)
                            :start1 start1 :start2 start2 :end2 end2))
                  (simple-base-string
                   (replace (truly-the (simple-array ,elt-type (*)) dst)
                            (truly-the simple-base-string src)
                            :start1 start1 :start2 start2 :end2 end2))))
             (input-contains-unicode ()
               ;; For streams with * element-type (producing the most space-efficient
               ;; string that can hold the output), checking whether Unicode characters appear
               ;; in the source material is potentially advantageous versus checking the
               ;; buffer in GET-OUTPUT-STREAM-STRING, because if all source strings are
               ;; BASE-STRING, we needn't check anything.
               ;; Bounds check was already performed
               `(let ((s (truly-the simple-character-string src)))
                  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                  (loop for i from start2 below end2
                        thereis (>= (char-code (aref s i)) base-char-code-limit)))))
    ;; The "wonderful" thing is you never know where type checks have already been done.
    ;; Is CHAR a character for sure? I have no idea. And how about the indices in SOUT?
    (labels ((base-char-out (stream char)
               (cout base-char))
             (character-out (stream char)
               (cout character))
             (default-out (stream char)
               (when (>= (char-code char) base-char-code-limit)
                 (setf (string-output-stream-unicode-p stream) t
                       ;; no need to keep checking each character
                       (ansi-stream-cout stream) #'character-out))
               (cout character))
             (base-string-out (stream dst src start1 start2 end2)
               (declare (ignorable stream) (index start1 start2 end2))
               (sout base-char))
             (char-string-out (stream dst src start1 start2 end2)
               (declare (ignorable stream) (index start1 start2 end2))
               (sout character))
             (reject (&rest args)
               (declare (ignore args))
               (error "Stream can not accept characters"))
             (misc (stream operation arg1)
               ;; Intercept the misc handler to reset the Unicode state
               ;; (since the char handlers are local functions).
               ;; Technically this should be among the actions performed on :CLEAR-OUTPUT,
               ;; which would also include *actually* clearing the output. But we don't.
               (stream-misc-case (operation)
                (:reset-unicode-p
                   (setf (string-output-stream-unicode-p stream) nil
                         ;; resume checking for Unicode characters
                         (ansi-stream-cout stream) #'default-out))
                (t
                   (string-out-misc stream operation arg1)))))
      (multiple-value-bind (element-type unicode-p cout sout-aux)
          (case (%other-pointer-widetag buffer)
            #+sb-unicode
            (#.sb-vm:simple-character-string-widetag
             (if wild-result-type
                 (values '*         nil  #'default-out   #'char-string-out)
                 (values 'character t    #'character-out #'char-string-out)))
            (#.sb-vm:simple-base-string-widetag
             (values 'base-char :ignore #'base-char-out #'base-string-out))
            (t
             (values nil :ignore #'reject #'reject)))
        (initforms)
        (truly-the string-output-stream stream)))))

;;; Constructors used by the transform of MAKE-STRING-OUTPUT-STREAM,
;;; avoiding parsing of the specified element-type at runtime.
(defun %make-base-string-ostream ()
  (%init-string-output-stream (%allocate-string-ostream)
                              (make-array 63 :element-type 'base-char) ; 2w + 64b
                              nil))
(defun %make-character-string-ostream ()
  (%init-string-output-stream (%allocate-string-ostream)
                              (make-array 32 :element-type 'character) ; 2w + 128b
                              nil))

(defun make-string-output-stream (&key (element-type 'character))
  "Return an output stream which will accumulate all output given it for the
benefit of the function GET-OUTPUT-STREAM-STRING."
  (declare (explicit-check))
  ;; No point in optimizing for unsupplied ELEMENT-TYPE.
  ;; Compiler transforms into %MAKE-CHARACTER-STRING-OSTREAM.
  (let ((ctype (specifier-type element-type)))
    (cond ((and (csubtypep ctype (specifier-type 'base-char))
                ;; Let NIL mean "default", i.e. CHARACTER
                (neq ctype *empty-type*))
           (%make-base-string-ostream))
          ((csubtypep ctype (specifier-type 'character))
           (%make-character-string-ostream))
          (t
           (error "~S is not a subtype of CHARACTER" element-type)))))

;;; Pushes the current segment onto the prev-list, and either pops
;;; or allocates a new one.
(defun string-output-stream-new-buffer (stream size)
  (declare (index size))
  (declare (string-output-stream stream))
  (push (string-output-stream-buffer stream)
        (string-output-stream-prev stream))
  (setf (string-output-stream-buffer stream)
        (or (pop (string-output-stream-next stream))
            ;; There may be a fencepost bug lurking here but I don't think so, and in any case
            ;; this errs on the side of caution.  Given the already dubious state of things
            ;; with regard to meaning of the INDEX type - see comment in src/code/early-extensions
            ;; above its DEF!TYPE - it seems like this can't be making things any worse to
            ;; constrain the chars in a string-output-stream to be even _smaller_ than INDEX.
            (let ((maximum-string-length (1- array-dimension-limit))
                  (current-index (string-output-stream-index stream)))
              (when (> (+ current-index size) maximum-string-length)
                (setq size (- maximum-string-length current-index)))
              (when (<= size 0)
                (error "string-output-stream maximum length exceeded"))
              (if (member (string-output-stream-element-type stream) '(base-char nil))
                  (make-array size :element-type 'base-char)
                  (make-array size :element-type 'character))))))

;;; Moves to the end of the next segment or the current one if there are
;;; no more segments. Returns true as long as there are next segments.
(defun string-output-stream-next-buffer (stream)
  (declare (string-output-stream stream))
  (let* ((old (string-output-stream-buffer stream))
         (new (pop (string-output-stream-next stream)))
         (old-size (length old))
         (skipped (- old-size (string-output-stream-pointer stream))))
    (cond (new
           (let ((new-size (length new)))
             (push old (string-output-stream-prev stream))
             (setf (string-output-stream-buffer stream) new
                   (string-output-stream-pointer stream) new-size)
             (incf (string-output-stream-index stream) (+ skipped new-size)))
           t)
          (t
           (setf (string-output-stream-pointer stream) old-size)
           (incf (string-output-stream-index stream) skipped)
           nil))))

;;; Moves to the start of the previous segment or the current one if there
;;; are no more segments. Returns true as long as there are prev segments.
(defun string-output-stream-prev-buffer (stream)
  (declare (string-output-stream stream))
  (let ((old (string-output-stream-buffer stream))
        (new (pop (string-output-stream-prev stream)))
        (skipped (string-output-stream-pointer stream)))
    (cond (new
           (push old (string-output-stream-next stream))
           (setf (string-output-stream-buffer stream) new
                 (string-output-stream-pointer stream) 0)
           (decf (string-output-stream-index stream) (+ skipped (length new)))
           t)
          (t
           (setf (string-output-stream-pointer stream) 0)
           (decf (string-output-stream-index stream) skipped)
           nil))))

(defun string-sout (stream string start end)
  (declare (explicit-check string)
           (type index start end))
  ;; FIXME: this contains about 7 OBJECT-NOT-INDEX error traps.
  ;; We should be able to check once up front that the string-stream will not
  ;; exceed the max number of allowed characters, and then not do any further tests.
  ;; Also, the SOUT-AUX method contains 4 calls to SB-INT:SEQUENCE-BOUNDING-INDICES-BAD-ERROR
  ;; which may be redundant.
  (let* ((full-length (- end start))
         (length full-length)
         (buffer (string-output-stream-buffer stream))
         (pointer (string-output-stream-pointer stream))
         (space (- (length buffer) pointer))
         (here (min space length))
         (stop (+ start here))
         (overflow (- length space)))
    (declare (index length space here stop full-length)
             (fixnum overflow))
    (tagbody
     :more
       (when (plusp here)
         (funcall (string-output-stream-sout-aux stream)
                  stream buffer string pointer start stop)
         (setf (string-output-stream-pointer stream) (+ here pointer)))
       (when (plusp overflow)
         (setf start stop
               length (- end start)
               ;; BUG: doesn't always enlarge as intended. Consider:
               ;;  - initial buffer capacity of 63 characters
               ;;  - WRITE-STRING with 2 characters setting INDEX=2, SPACE=61
               ;;  - another WRITE-STRING with 62 characters. 61 copied, 1 overflow.
               ;;  - then new BUFFER length is (MAX OVERFLOW INDEX) = 2
               buffer (string-output-stream-new-buffer
                       stream (max overflow (string-output-stream-index stream)))
               pointer 0
               space (length buffer)
               here (min space length)
               stop (+ start here)
               ;; there may be more overflow if we used a buffer
               ;; already allocated to the stream
               overflow (- length space))
         (go :more)))
    (incf (string-output-stream-index stream) full-length)))

;;; Factored out of the -misc method due to size.
(defun set-string-output-stream-file-position (stream pos)
  (let* ((index (string-output-stream-index stream))
         (end (max index (string-output-stream-index-cache stream))))
    (declare (index index end))
    (setf (string-output-stream-index-cache stream) end)
    (cond ((eq :start pos)
           (loop while (string-output-stream-prev-buffer stream)))
          ((eq :end pos)
           (loop while (string-output-stream-next-buffer stream))
           (let ((over (- (string-output-stream-index stream) end)))
             (decf (string-output-stream-pointer stream) over))
           (setf (string-output-stream-index stream) end))
          ((< pos index)
           ;; Set INDEX to the start of the current buffer
           (decf (string-output-stream-index stream) (string-output-stream-pointer stream))
           (setf index (string-output-stream-index stream))
           (setf (string-output-stream-pointer stream) 0)
           (loop while (< pos index)
                 do (string-output-stream-prev-buffer stream)
                 (setf index (string-output-stream-index stream)))
           (let ((step (- pos index)))
             (incf (string-output-stream-pointer stream) step)
             (setf (string-output-stream-index stream) pos)))
          ((> pos index)
           ;; We allow moving beyond the end of stream, implicitly
           ;; extending the output stream.
           (let ((next (string-output-stream-buffer stream)))
             ;; Set INDEX to the end of the current buffer.
             (incf index (- (length next) (string-output-stream-pointer stream)))
             (loop while (and next (> pos index))
                   do (setf next (string-output-stream-next-buffer stream)
                            ;; Update after -next-buffer, INDEX is kept pointing at
                            ;; the end of the current buffer.
                            index (string-output-stream-index stream))))
           ;; Allocate new buffer if needed, or step back to
           ;; the desired index and set pointer and index
           ;; correctly.
           (let ((diff (- pos index)))
             (if (plusp diff)
                 (let* ((new (string-output-stream-new-buffer stream diff))
                        (size (length new)))
                   (aver (= pos (+ index size)))
                   (setf (string-output-stream-pointer stream) size
                         (string-output-stream-index stream) pos))
                 (let ((size (length (string-output-stream-buffer stream))))
                   (setf (string-output-stream-pointer stream) (+ size diff)
                         (string-output-stream-index stream) pos))))))))

(defun string-out-misc (stream operation arg1)
  (declare (optimize speed))
  (stream-misc-case (operation :default nil)
    (:charpos
     ;; Keeping this first is a silly micro-optimization: FRESH-LINE
     ;; makes this the most common one.
     (/noshow0 "/string-out-misc charpos")
     (prog ((pointer (string-output-stream-pointer stream))
            (buffer (string-output-stream-buffer stream))
            (prev (string-output-stream-prev stream))
            (base 0))
      :next
      (let ((pos (when buffer ; could be NIL because of SETF below
                   (string-dispatch (simple-base-string
                                     #+sb-unicode simple-character-string)
                                    buffer
                     (position #\newline buffer :from-end t :end pointer)))))
        (when (or pos (not buffer))
          ;; If newline is at index I, and pointer at index I+N, charpos
          ;; is N-1. If there is no newline, and pointer is at index N,
          ;; charpos is N.
          (return (+ base (if pos (- pointer pos 1) pointer))))
        (setf base (+ base pointer)
              buffer (pop prev)
              pointer (length buffer))
        (/noshow0 "/string-out-misc charpos next")
        (go :next))))
    (:set-file-position
     (set-string-output-stream-file-position stream arg1)
     t)
    (:get-file-position
     (string-output-stream-index stream))
    (:close
     (/noshow0 "/string-out-misc close")
     (set-closed-flame stream))
    (:element-type
     (let ((et (string-output-stream-element-type stream)))
       ;; Always return a valid type-specifier
       (if (eq et '*) 'character et)))
    (:element-mode 'character)))

;;; Return a string of all the characters sent to a stream made by
;;; MAKE-STRING-OUTPUT-STREAM since the last call to this function.
(defun get-output-stream-string (stream)
  (declare (type string-output-stream stream))
  (let* ((length (max (string-output-stream-index stream)
                      (string-output-stream-index-cache stream)))
         (prev (nreverse (string-output-stream-prev stream)))
         (this (string-output-stream-buffer stream))
         (next (string-output-stream-next stream))
         (base-string-p (neq (string-output-stream-unicode-p stream) t))
         ;; This used to contain a FIXME about not allocating a result string,
         ;; instead shrinking the final buffer down to size. But given the likelihood
         ;; of a Unicode buffer and ASCII result, that seems inapplicable nowadays.
         ;; Also, how it impacts setting FILE-POSITION on a string stream is unclear.
         ;; (See https://bugs.launchpad.net/sbcl/+bug/1839040)
         (result (if base-string-p
                     (make-string length :element-type 'base-char)
                     (make-string length))))

    (setf (string-output-stream-index stream) 0
          (string-output-stream-index-cache stream) 0
          (string-output-stream-pointer stream) 0
          ;; throw them away for simplicity's sake: this way the rest of the
          ;; implementation can assume that the greater of INDEX and INDEX-CACHE
          ;; is always within the last buffer.
          (string-output-stream-prev stream) nil
          (string-output-stream-next stream) nil)

    ;; Reset UNICODE-P unless it was :IGNORE or element-type is CHARACTER.
    (when (and (eq (string-output-stream-element-type stream) '*)
               (eq (string-output-stream-unicode-p stream) t))
      (call-ansi-stream-misc stream :reset-unicode-p))

    ;; There are exactly 3 cases that we have to deal with when copying:
    ;;  CHARACTER-STRING into BASE-STRING (without type-checking per character)
    ;;  CHARACTER-STRING into CHARACTER-STRING
    ;;  BASE-STRING into BASE-STRING
    ;; BASE-STRING copied into CHARACTER-STRING is not possible.
    ;; The first case occurs when and only when the element type is * and
    ;; only base characters were written. The other two cases can be handled
    ;; using memcpy() with string indices multiplied by either 1 or 4.
    (flet ((copy (fun scale)
             (let ((start 0)) ; index into RESULT
               (declare (index start))
               (dolist (buffer prev)
                 ;; It doesn't look as though we should have to pass RESULT
                 ;; in to FUN to avoid closure consing, but indeed we do.
                 (funcall fun result buffer start scale)
                 (incf start (length buffer)))
               (funcall fun result this start scale)
               (incf start (length this))
               (dolist (buffer next)
                 (funcall fun result buffer start scale)
                 (incf start (length buffer))))))
      (if (and (eq (string-output-stream-element-type stream) '*)
               base-string-p)
          ;; This is the most common case, arising from WRITE-TO-STRING,
          ;; PRINx-TO-STRING, (FORMAT NIL ...), and many other constructs.
          ;; REPLACE will elide the type test per compilation policy
          ;; which is fine because we've already checked that it'll work.
          (copy (lambda (result source start dummy)
                  (declare (optimize speed (sb-c::type-check 0)))
                  (declare (ignore dummy))
                  (replace (the simple-base-string result)
                           (the simple-character-string source)
                           :start1 start))
                0)
          (with-pinned-objects (result)
            (with-alien ((memcpy (function system-area-pointer
                                           system-area-pointer system-area-pointer unsigned)
                                 :extern))
              (copy (lambda (result source start scale)
                      (declare (index start))
                      (let* ((nchars (min (- length start) (length source)))
                             (nbytes (the index (ash (the index nchars) scale))))
                        (with-pinned-objects (source)
                          (alien-funcall memcpy
                                         (sap+ (vector-sap result) (ash start scale))
                                         (vector-sap source)
                                         nbytes))))
                    (if base-string-p 0 2))))))
    result))

;;; Now that we support base-char string-output streams, it may be possible to eliminate
;;; this, though the other benefit it confers is that the buffer never needs to extend,
;;; and we merely shrink it to the proper size when done writing.
(defstruct (finite-base-string-output-stream
            (:include ansi-stream
                      (cout #'finite-base-string-ouch)
                      (sout #'finite-base-string-sout)
                      (misc #'finite-base-string-out-misc))
            (:constructor %make-finite-base-string-output-stream (buffer))
            (:copier nil)
            (:predicate nil))
  (buffer nil :type simple-base-string :read-only t)
  (pointer 0 :type index))
(declaim (freeze-type finite-base-string-output-stream))

(defun finite-base-string-ouch (stream character)
  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
  (let ((pointer (finite-base-string-output-stream-pointer stream))
        (buffer (finite-base-string-output-stream-buffer stream)))
    (cond ((= pointer (length buffer))
           (bug "Should not happen"))
          (t
           (setf (finite-base-string-output-stream-pointer stream)
                 (truly-the index (1+ pointer))
                 ;; return the character
                 (char buffer pointer) (truly-the base-char character))))))

(defun finite-base-string-sout (stream string start end)
  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
  (let* ((pointer (finite-base-string-output-stream-pointer stream))
         (buffer (finite-base-string-output-stream-buffer stream))
         (length (- end start))
         (new-pointer (+ pointer length)))
    (cond ((> new-pointer (length buffer))
           (bug "Should not happen"))
          #+sb-unicode
          ((typep string 'simple-character-string)
           (replace buffer string
                    :start1 pointer :start2 start :end2 end))
          (t
           (replace buffer (the simple-base-string string)
                    :start1 pointer :start2 start :end2 end)))
    (setf (finite-base-string-output-stream-pointer stream) new-pointer)))

(defun finite-base-string-out-misc (stream operation arg1)
  (declare (ignore stream operation arg1))
  (error "finite-base-string-out-misc needs an implementation"))

;;;; fill-pointer streams

;;; Fill pointer STRING-OUTPUT-STREAMs are not explicitly mentioned in
;;; the CLM, but they are required for the implementation of
;;; WITH-OUTPUT-TO-STRING.

;;; FIXME: need to support (VECTOR NIL), ideally without destroying all hope
;;; of efficiency.
(declaim (inline vector-with-fill-pointer-p))
(defun vector-with-fill-pointer-p (x)
  (and (vectorp x)
       (array-has-fill-pointer-p x)))

(deftype string-with-fill-pointer ()
  `(and (or (vector character) (vector base-char))
        (satisfies vector-with-fill-pointer-p)))

;;; FIXME: The stream should refuse to accept more characters than the given
;;; string can hold without adjustment unless expressly adjustable.
;;; This is a portability issue - the fact that all of our fill-pointer vectors
;;; are implicitly adjustable is an implementation detail that should not be leaked.
;;; For comparison, when evaluating:
;;; (let ((s (make-array 5 :fill-pointer 0 :element-type 'base-char)))
;;;       (with-output-to-string (stream s) (dotimes (i 6) (write-char #\a stream))) s)
;;; CLISP:
;;; *** - VECTOR-PUSH-EXTEND works only on adjustable arrays, not on "aaaaa"
;;; CCL:
;;; > Error: "aaaaa" is not an adjustable array.

(defstruct (fill-pointer-output-stream
            (:include ansi-stream
                      (cout #'fill-pointer-ouch)
                      (sout #'fill-pointer-sout)
                      (misc #'fill-pointer-misc))
            (:constructor nil)
            (:copier nil)
            (:predicate nil))
  ;; a string with a fill pointer where we stuff the stuff we write
  (string (missing-arg) :type string-with-fill-pointer :read-only t))

(declaim (freeze-type fill-pointer-output-stream))
;;; TODO: specialize on string type?
(defun %init-fill-pointer-output-stream (stream string element-type)
  (declare (optimize speed (sb-c::verify-arg-count 0)))
  (declare (ignore element-type))
  (unless (and (stringp string)
               (array-has-fill-pointer-p string))
    (error "~S is not a string with a fill-pointer" string))
  (macrolet ((initforms ()
               `(progn ,@(mapcar (lambda (dsd)
                                   `(%instance-set stream ,(dsd-index dsd)
                                       ,(case (dsd-name dsd)
                                          (string 'string)
                                          (t (dsd-default dsd)))))
                                (dd-slots
                                 (find-defstruct-description 'fill-pointer-output-stream))))))
    (initforms)
    (truly-the fill-pointer-output-stream stream)))

(defun fill-pointer-ouch (stream character)
  ;; FIXME: ridiculously inefficient. Can we throw some TRULY-THEs in here?
  ;; I think the prohibition against touching the string - implying that you can't
  ;; decide to re-displace it - means we should be able to just look at the
  ;; underlying vector, at least until we run out of space in it.
  (let* ((buffer (fill-pointer-output-stream-string stream))
         (current (fill-pointer buffer))
         (current+1 (1+ current)))
    (declare (fixnum current))
    (with-array-data ((workspace buffer) (start) (end))
      (string-dispatch (simple-character-string simple-base-string) workspace
        (let ((offset-current (+ start current)))
          (declare (fixnum offset-current))
          (if (= offset-current end)
              (let* ((new-length (1+ (* current 2)))
                     (new-workspace
                      (ecase (array-element-type workspace)
                        (character (make-string new-length
                                                :element-type 'character))
                        (base-char (make-string new-length
                                                :element-type 'base-char)))))
                (replace new-workspace workspace :start2 start :end2 offset-current)
                (setf workspace new-workspace
                      offset-current current)
                (set-array-header buffer workspace new-length
                                  current+1 0 new-length nil nil))
              (setf (fill-pointer buffer) current+1))
          (setf (char workspace offset-current) character)))))
  character)

(defun fill-pointer-sout (stream string start end)
  (declare (fixnum start end))
  (string-dispatch (simple-character-string simple-base-string) string
    (let* ((buffer (fill-pointer-output-stream-string stream))
           (current (fill-pointer buffer))
           (string-len (- end start))
           (dst-end (+ string-len current)))
      (declare (fixnum current dst-end string-len))
      (with-array-data ((workspace buffer) (dst-start) (dst-length))
        (let ((offset-dst-end (+ dst-start dst-end))
              (offset-current (+ dst-start current)))
          (declare (fixnum offset-dst-end offset-current))
          (if (> offset-dst-end dst-length)
              (let* ((new-length (+ (the fixnum (* current 2)) string-len))
                     (new-workspace
                      (ecase (array-element-type workspace)
                        (character (make-string new-length
                                                :element-type 'character))
                        (base-char (make-string new-length
                                                :element-type 'base-char)))))
                (replace new-workspace workspace
                         :start2 dst-start :end2 offset-current)
                (setf workspace new-workspace
                      offset-current current
                      offset-dst-end dst-end)
                (set-array-header buffer workspace new-length
                                  dst-end 0 new-length nil nil))
              (setf (fill-pointer buffer) dst-end))
          (replace workspace string
                   :start1 offset-current :start2 start :end2 end)))
      dst-end)))

(defun fill-pointer-misc (stream operation arg1
                          &aux (buffer (fill-pointer-output-stream-string stream)))
  (stream-misc-case (operation :default nil)
    (:set-file-position
           (setf (fill-pointer buffer)
                 (case arg1
                   (:start 0)
                   ;; Fill-pointer is always at fill-pointer we will
                   ;; make :END move to the end of the actual string.
                   (:end (array-total-size buffer))
                   ;; We allow moving beyond the end of string if the
                   ;; string is adjustable.
                   (t (when (>= arg1 (array-total-size buffer))
                        (if (adjustable-array-p buffer)
                            (adjust-array buffer arg1)
                            (error "Cannot move FILE-POSITION beyond the end ~
                                    of WITH-OUTPUT-TO-STRING stream ~
                                    constructed with non-adjustable string.")))
                      arg1))))
    (:get-file-position
           (fill-pointer buffer))
    (:charpos
     (let ((current (fill-pointer buffer)))
       (with-array-data ((string buffer) (start) (end current))
         (declare (simple-string string))
         (let ((found (position #\newline string :test #'char=
                                                 :start start :end end
                                                 :from-end t)))
           (if found
               (1- (- end found))
               current)))))
    (:element-type
      (array-element-type
       (fill-pointer-output-stream-string stream)))
    (:element-mode 'character)))

;;;; case frobbing streams, used by FORMAT ~(...~)

(defstruct (case-frob-stream
            (:include ansi-stream
                      (misc #'case-frob-misc))
            (:constructor %make-case-frob-stream (target cout sout))
            (:copier nil))
  (target (missing-arg) :type stream :read-only t))

(declaim (freeze-type case-frob-stream))

(defun make-case-frob-stream (target kind)
  "Return a stream that sends all output to the stream TARGET, but modifies
   the case of letters, depending on KIND, which should be one of:
     :UPCASE - convert to upper case.
     :DOWNCASE - convert to lower case.
     :CAPITALIZE - convert the first letter of words to upper case and the
        rest of the word to lower case.
     :CAPITALIZE-FIRST - convert the first letter of the first word to upper
        case and everything else to lower case."
  (declare (type stream target)
           (type (member :upcase :downcase :capitalize :capitalize-first)
                 kind)
           (values stream))
  (if (case-frob-stream-p target)
      ;; If we are going to be writing to a stream that already does
      ;; case frobbing, why bother frobbing the case just so it can
      ;; frob it again?
      target
      (multiple-value-bind (cout sout)
          (ecase kind
            (:upcase
             (values #'case-frob-upcase-out
                     #'case-frob-upcase-sout))
            (:downcase
             (values #'case-frob-downcase-out
                     #'case-frob-downcase-sout))
            (:capitalize
             (values #'case-frob-capitalize-out
                     #'case-frob-capitalize-sout))
            (:capitalize-first
             (values #'case-frob-capitalize-first-out
                     #'case-frob-capitalize-first-sout)))
        (%make-case-frob-stream target cout sout))))

(defun case-frob-misc (stream op arg1)
  (declare (type case-frob-stream stream))
  (case op
    (:close
     (set-closed-flame stream))
    (:element-mode 'character)
    (t
     (let ((target (case-frob-stream-target stream)))
       (if (ansi-stream-p target)
           (call-ansi-stream-misc target op arg1)
           (stream-misc-dispatch target op arg1))))))

;;; FIXME: formatted output into a simple-stream is currently hampered
;;; by the fact that case-frob streams assume that the stream is either ANSI
;;; or Gray. Probably the easiest fix would be to define STREAM-WRITE-CHAR
;;; and STREAM-WRITE-STRING on simple-stream.
(defun case-frob-upcase-out (stream char)
  (declare (type case-frob-stream stream)
           (type character char))
  (let ((target (case-frob-stream-target stream))
        (c (char-upcase char)))
    (if (ansi-stream-p target)
        ;; ansi stream method for character out has to return the original char
        (progn (funcall (ansi-stream-cout target) target c) char)
        (stream-write-char target c))))

(defun case-frob-upcase-sout (stream str start end)
  (declare (type case-frob-stream stream)
           (type simple-string str)
           (type index start)
           (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
         (len (length str))
         (end (or end len))
         (string (if (and (zerop start) (= len end))
                     (string-upcase str)
                     (nstring-upcase (subseq str start end))))
         (string-len (- end start)))
    (if (ansi-stream-p target)
        (funcall (ansi-stream-sout target) target string 0 string-len)
        (stream-write-string target string 0 string-len))))

(defun case-frob-downcase-out (stream char)
  (declare (type case-frob-stream stream)
           (type character char))
  (let ((target (case-frob-stream-target stream))
        (c (char-downcase char)))
    (if (ansi-stream-p target)
        (progn (funcall (ansi-stream-cout target) target c) char)
        (stream-write-char target c))))

(defun case-frob-downcase-sout (stream str start end)
  (declare (type case-frob-stream stream)
           (type simple-string str)
           (type index start)
           (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
         (len (length str))
         (end (or end len))
         (string (if (and (zerop start) (= len end))
                     (string-downcase str)
                     (nstring-downcase (subseq str start end))))
         (string-len (- end start)))
    (if (ansi-stream-p target)
        (funcall (ansi-stream-sout target) target string 0 string-len)
        (stream-write-string target string 0 string-len))))

(defun case-frob-capitalize-out (stream char)
  (declare (type case-frob-stream stream)
           (type character char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
           (let ((char (char-upcase char)))
             (if (ansi-stream-p target)
                 (funcall (ansi-stream-cout target) target char)
                 (stream-write-char target char)))
           (setf (case-frob-stream-cout stream) #'case-frob-capitalize-aux-out)
           (setf (case-frob-stream-sout stream)
                 #'case-frob-capitalize-aux-sout))
          (t
           (if (ansi-stream-p target)
               (funcall (ansi-stream-cout target) target char)
               (stream-write-char target char)))))
  char)

(defun case-frob-capitalize-sout (stream str start end)
  (declare (type case-frob-stream stream)
           (type simple-string str)
           (type index start)
           (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
         (str (subseq str start end))
         (len (length str))
         (inside-word nil))
    (dotimes (i len)
      (let ((char (schar str i)))
        (cond ((not (alphanumericp char))
               (setf inside-word nil))
              (inside-word
               (setf (schar str i) (char-downcase char)))
              (t
               (setf inside-word t)
               (setf (schar str i) (char-upcase char))))))
    (when inside-word
      (setf (case-frob-stream-cout stream) #'case-frob-capitalize-aux-out)
      (setf (case-frob-stream-sout stream) #'case-frob-capitalize-aux-sout))
    (if (ansi-stream-p target)
        (funcall (ansi-stream-sout target) target str 0 len)
        (stream-write-string target str 0 len))))

(defun case-frob-capitalize-aux-out (stream char)
  (declare (type case-frob-stream stream)
           (type character char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
           (let ((char (char-downcase char)))
             (if (ansi-stream-p target)
                 (funcall (ansi-stream-cout target) target char)
                 (stream-write-char target char))))
          (t
           (if (ansi-stream-p target)
               (funcall (ansi-stream-cout target) target char)
               (stream-write-char target char))
           (setf (case-frob-stream-cout stream) #'case-frob-capitalize-out)
           (setf (case-frob-stream-sout stream) #'case-frob-capitalize-sout))))
  char)

(defun case-frob-capitalize-aux-sout (stream str start end)
  (declare (type case-frob-stream stream)
           (type simple-string str)
           (type index start)
           (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
         (str (subseq str start end))
         (len (length str))
         (inside-word t))
    (dotimes (i len)
      (let ((char (schar str i)))
        (cond ((not (alphanumericp char))
               (setf inside-word nil))
              (inside-word
               (setf (schar str i) (char-downcase char)))
              (t
               (setf inside-word t)
               (setf (schar str i) (char-upcase char))))))
    (unless inside-word
      (setf (case-frob-stream-cout stream) #'case-frob-capitalize-out)
      (setf (case-frob-stream-sout stream) #'case-frob-capitalize-sout))
    (if (ansi-stream-p target)
        (funcall (ansi-stream-sout target) target str 0 len)
        (stream-write-string target str 0 len))))

(defun case-frob-capitalize-first-out (stream char)
  (declare (type case-frob-stream stream)
           (type character char))
  (let ((target (case-frob-stream-target stream)))
    (cond ((alphanumericp char)
           (let ((char (char-upcase char)))
             (if (ansi-stream-p target)
                 (funcall (ansi-stream-cout target) target char)
                 (stream-write-char target char)))
           (setf (case-frob-stream-cout stream) #'case-frob-downcase-out)
           (setf (case-frob-stream-sout stream) #'case-frob-downcase-sout))
          (t
           (if (ansi-stream-p target)
               (funcall (ansi-stream-cout target) target char)
               (stream-write-char target char)))))
  char)

(defun case-frob-capitalize-first-sout (stream str start end)
  (declare (type case-frob-stream stream)
           (type simple-string str)
           (type index start)
           (type (or index null) end))
  (let* ((target (case-frob-stream-target stream))
         (str (subseq str start end))
         (len (length str)))
    (dotimes (i len)
      (let ((char (schar str i)))
        (when (alphanumericp char)
          (setf (schar str i) (char-upcase char))
          (do ((i (1+ i) (1+ i)))
              ((= i len))
            (setf (schar str i) (char-downcase (schar str i))))
          (setf (case-frob-stream-cout stream) #'case-frob-downcase-out)
          (setf (case-frob-stream-sout stream) #'case-frob-downcase-sout)
          (return))))
    (if (ansi-stream-p target)
        (funcall (ansi-stream-sout target) target str 0 len)
        (stream-write-string target str 0 len))))

;;;; Shared {READ,WRITE}-SEQUENCE support functions

(declaim (inline stream-compute-io-function
                 compatible-vector-and-stream-element-types-p))

(defun stream-compute-io-function (stream
                                   stream-element-mode sequence-element-type
                                   character-io binary-io bivalent-io)
  (ecase stream-element-mode
    (character
     character-io)
    ((unsigned-byte signed-byte)
     binary-io)
    (:bivalent
     (cond
       ((member sequence-element-type '(nil t))
        bivalent-io)
       ;; Pick off common subtypes.
       ((eq sequence-element-type 'character)
        character-io)
       ((or (equal sequence-element-type '(unsigned-byte 8))
            (equal sequence-element-type '(signed-byte 8)))
        binary-io)
       ;; Proper subtype tests.
       ((subtypep sequence-element-type 'character)
        character-io)
       ((subtypep sequence-element-type 'integer)
        binary-io)
       (t
        (error "~@<Cannot select IO functions to use for bivalent ~
                stream ~S and a sequence with element-type ~S.~@:>"
                stream sequence-element-type))))))

(defun compatible-vector-and-stream-element-types-p (vector stream)
  (declare (type vector vector)
           (type ansi-stream stream))
  (or (and (typep vector '(simple-array (unsigned-byte 8) (*)))
           (memq (stream-element-mode stream) '(unsigned-byte :bivalent))
           t)
      (and (typep vector '(simple-array (signed-byte 8) (*)))
           (eq (stream-element-mode stream) 'signed-byte))))

;;;; READ-SEQUENCE

(defun read-sequence (seq stream &key (start 0) end)
  "Destructively modify SEQ by reading elements from STREAM.
  That part of SEQ bounded by START and END is destructively modified by
  copying successive elements into it from STREAM. If the end of file
  for STREAM is reached before copying all elements of the subsequence,
  then the extra elements near the end of sequence are not updated, and
  the index of the next element is returned."
  (declare (explicit-check))
  (stream-api-dispatch (stream)
    :simple (error "Unimplemented") ; gets redefined
    :native (ansi-stream-read-sequence seq stream start end)
    :gray (stream-read-sequence stream (the sequence seq) (the index start) (the sequence-end end))))

(declaim (maybe-inline read-sequence/read-function))
(defun read-sequence/read-function (seq stream start %end
                                    stream-element-mode
                                    character-read-function binary-read-function)
  (declare (type stream stream)
           (type index start)
           (type sequence-end %end)
           (type stream-element-mode stream-element-mode)
           (type function character-read-function binary-read-function)
           (values index &optional))
  (let ((end (or %end (length seq))))
    (declare (type index end))
    (labels ((compute-read-function (sequence-element-type)
               (stream-compute-io-function
                stream
                stream-element-mode sequence-element-type
                character-read-function binary-read-function
                character-read-function))
             (read-list (read-function)
               (do ((rem (nthcdr start seq) (rest rem))
                    (i start (1+ i)))
                   ((or (endp rem) (>= i end)) i)
                 (declare (type list rem)
                          (type index i))
                 (let ((el (funcall read-function stream nil :eof nil)))
                   (when (eq el :eof)
                     (return i))
                   (setf (first rem) el))))
             (read-vector/fast (data offset-start)
               (let* ((numbytes (- end start))
                      (bytes-read (read-n-bytes
                                   stream data offset-start numbytes nil)))
                 (if (< bytes-read numbytes)
                     (+ start bytes-read)
                     end)))
             (read-vector (read-function data offset-start offset-end)
               (do ((i offset-start (1+ i)))
                   ((>= i offset-end) end)
                 (declare (type index i))
                 (let ((el (funcall read-function stream nil :eof nil)))
                   (when (eq el :eof)
                     (return (+ start (- i offset-start))))
                   (setf (aref data i) el))))
             (read-generic-sequence (read-function)
               (multiple-value-bind (iterator limit from-end step endp elt set-elt)
                   (sb-sequence:make-sequence-iterator seq :start start :end end)
                 (declare (ignore elt)
                          (type function step endp set-elt))
                 (loop for i of-type index from start
                       until (funcall endp seq iterator limit from-end)
                       do (let ((object (funcall read-function stream nil :eof nil)))
                            (funcall set-elt object seq iterator))
                          (setf iterator (funcall step seq iterator from-end))
                       finally (return i)))))
      (declare (dynamic-extent #'compute-read-function
                               #'read-list #'read-vector/fast #'read-vector
                               #'read-generic-sequence))
      (cond
        ((typep seq 'list)
         (read-list (compute-read-function nil)))
        ((and (ansi-stream-p stream)
              (ansi-stream-cin-buffer stream)
              (typep seq 'simple-string))
         (ansi-stream-read-string-from-frc-buffer seq stream start %end))
        ((typep seq 'vector)
         (with-array-data ((data seq) (offset-start start) (offset-end end)
                           :check-fill-pointer t)
           (if (and (ansi-stream-p stream)
                    (compatible-vector-and-stream-element-types-p data stream))
               (read-vector/fast data offset-start)
               (read-vector (compute-read-function (array-element-type data))
                            data offset-start offset-end))))
        (t
         (read-generic-sequence (compute-read-function nil)))))))

(defun ansi-stream-read-sequence (seq stream start %end)
  (declare (type ansi-stream stream)
           (type index start)
           (type sequence-end %end)
           (inline read-sequence/read-function)
           (values index &optional))
  (read-sequence/read-function
   seq stream start %end (stream-element-mode stream)
   #'ansi-stream-read-char #'ansi-stream-read-byte))

(defun ansi-stream-read-string-from-frc-buffer (seq stream start %end)
  (declare (type simple-string seq)
           (type ansi-stream stream)
           (type index start)
           (type (or null index) %end))
  (let ((needed (- (or %end (length seq))
                   start))
        (read 0))
    (prepare-for-fast-read-char stream
      (declare (ignore %frc-method%))
      (unless %frc-buffer%
        (return-from ansi-stream-read-string-from-frc-buffer nil))
      ;; Read directly into the string when possible
      (or (and (= %frc-index% +ansi-stream-in-buffer-length+)
               (>= needed (/ +ansi-stream-in-buffer-length+ 2))
               (fd-stream-p stream)
               (fd-stream-ibuf stream)
               (cond #+sb-unicode
                     ((typep seq 'simple-base-string)
                      (cond
                        ((eq (ansi-stream-n-bin stream) #'fd-stream-read-n-characters/utf-8)
                         (fd-stream-read-sequence/utf-8-to-base-string stream seq start needed))
                        ((eq (ansi-stream-n-bin stream) #'fd-stream-read-n-characters/utf-8/crlf)
                         (fd-stream-read-sequence/utf-8-crlf-to-base-string stream seq start needed))))
                     ((eq (ansi-stream-n-bin stream) #'fd-stream-read-n-characters/utf-8)
                      (fd-stream-read-sequence/utf-8-to-string stream seq start needed))
                     ((eq (ansi-stream-n-bin stream) #'fd-stream-read-n-characters/utf-8/crlf)
                      (fd-stream-read-sequence/utf-8-crlf-to-character-string stream seq start needed))))
          (labels ((refill-buffer ()
                     (prog1 (fast-read-char-refill stream nil)
                       (setf %frc-index% (ansi-stream-in-index %frc-stream%))))
                   (add-chunk ()
                     (let* ((end (length %frc-buffer%))
                            (len (min (- end %frc-index%)
                                      (- needed read))))
                       (declare (type index end len read needed))
                       (string-dispatch (simple-base-string simple-character-string)
                                        seq
                         (replace seq %frc-buffer%
                                  :start1 (+ start read)
                                  :end1 (+ start read len)
                                  :start2 %frc-index%
                                  :end2 (+ %frc-index% len)))
                       (incf read len)
                       (incf %frc-index% len)
                       (when (or (eql needed read) (not (refill-buffer)))
                         (done-with-fast-read-char)
                         (return-from ansi-stream-read-string-from-frc-buffer
                           (+ start read))))))
            (declare (inline refill-buffer))
            (when (and (= %frc-index% +ansi-stream-in-buffer-length+)
                       (not (refill-buffer)))
              ;; EOF had been reached before we read anything
              ;; at all. But READ-SEQUENCE never signals an EOF error.
              (done-with-fast-read-char)
              (return-from ansi-stream-read-string-from-frc-buffer start))
            (loop (add-chunk)))))))


;;;; WRITE-SEQUENCE

(defun write-sequence (seq stream &key (start 0) (end nil))
  "Write the elements of SEQ bounded by START and END to STREAM."
  (declare (explicit-check seq))
  (let* ((length (length seq))
         (end (or end length)))
    (unless (<= start end length)
      (sequence-bounding-indices-bad-error seq start end)))
  (write-seq-impl seq stream start end))

;;; This macro allows sharing code between
;;; WRITE-SEQUENCE/WRITE-FUNCTION and SB-GRAY:STREAM-WRITE-STRING.
(defmacro write-sequence/vector ((seq type) stream start end write-function)
  (once-only ((seq seq) (stream stream) (start start) (end end)
              (write-function write-function))
    `(locally
         (declare (type ,type ,seq)
                  (type index ,start ,end)
                  (type function ,write-function))
       (do ((i ,start (1+ i)))
           ((>= i ,end))
         (declare (type index i))
         (funcall ,write-function ,stream (aref ,seq i))))))

(declaim (maybe-inline write-sequence/write-function))
(defun write-sequence/write-function (seq stream start %end
                                      stream-element-mode
                                      character-write-function
                                      binary-write-function)
  (declare (type stream stream)
           (type index start)
           (type sequence-end %end)
           (type stream-element-mode stream-element-mode)
           (type function character-write-function binary-write-function))
  (let ((end (or %end (length seq))))
    (declare (type index end))
    (labels ((compute-write-function (sequence-element-type)
               (stream-compute-io-function
                stream
                stream-element-mode sequence-element-type
                character-write-function binary-write-function
                #'write-element/bivalent))
             (write-element/bivalent (stream object)
               (if (characterp object)
                   (funcall character-write-function stream object)
                   (funcall binary-write-function stream object)))
             (write-list (write-function)
               (do ((rem (nthcdr start seq) (rest rem))
                    (i start (1+ i)))
                   ((or (endp rem) (>= i end)))
                 (declare (type list rem)
                          (type index i))
                 (funcall write-function stream (first rem))))
             (write-vector (data start end write-function)
               (write-sequence/vector
                (data (simple-array * (*))) stream start end write-function))
             (write-generic-sequence (write-function)
               (multiple-value-bind (iterator limit from-end step endp element)
                   (sb-sequence:make-sequence-iterator seq :start start :end end)
                 (declare (type function step endp element))
                 (loop until (funcall endp seq iterator limit from-end)
                       for object = (funcall element seq iterator)
                       do (funcall write-function stream object)
                          (setf iterator (funcall step seq iterator from-end))))))
      (declare (dynamic-extent #'compute-write-function
                               #'write-element/bivalent #'write-list
                               #'write-vector  #'write-generic-sequence))
      (etypecase seq
        (list
         (write-list (compute-write-function nil)))
        (string
         (if (ansi-stream-p stream)
             (with-array-data ((data seq) (start start) (end end)
                               :check-fill-pointer t)
               (funcall (ansi-stream-sout stream) stream data start end))
             (stream-write-string stream seq start end)))
        (vector
         (with-array-data ((data seq) (offset-start start) (offset-end end)
                           :check-fill-pointer t)
           (cond ((not (and (fd-stream-p stream)
                            (compatible-vector-and-stream-element-types-p data stream)))
                  (write-vector data offset-start offset-end
                                (compute-write-function
                                 (array-element-type seq))))
                 ((eq (fd-stream-buffering stream) :none)
                  (write-or-buffer-output stream data offset-start offset-end))
                 (t
                  (buffer-output stream data offset-start offset-end)))))
        (sequence
         (write-generic-sequence (compute-write-function nil)))))))

;;; This takes any kind of stream, not just ansi streams, because of recursion.
;;; It's basically just the non-keyword-accepting entry for WRITE-SEQUENCE.
(defun write-seq-impl (seq stream start %end)
  (declare (type stream stream)
           (type index start)
           (type sequence-end %end)
           (inline write-sequence/write-function))
  (stream-api-dispatch (stream)
    :simple (s-%write-sequence stream (the sequence seq) start (or %end (length seq)))
    :gray (stream-write-sequence stream (the sequence seq) start %end)
    :native
    (typecase stream
    ;; Don't merely extract one layer of composite stream, because a synonym stream
    ;; may redirect to a broadcast stream which wraps a two-way-stream etc etc.
    (synonym-stream
     (write-seq-impl seq (symbol-value (synonym-stream-symbol stream)) start %end))
    (broadcast-stream
     (dolist (s (broadcast-stream-streams stream) seq)
       (write-seq-impl seq s start %end)))
    (two-way-stream ; handles ECHO-STREAM also
     (write-seq-impl seq (two-way-stream-output-stream stream) start %end))
    ;; file, string, pretty, and case-frob streams all fall through to the default,
    ;; which has special logic for fd-stream.
    (t
     (write-sequence/write-function
      seq stream start %end (stream-element-mode stream)
      (ansi-stream-cout stream) (ansi-stream-bout stream)))))
  seq)

;;; like FILE-POSITION, only using :FILE-LENGTH
(defun file-length (stream)
  ;; The description for FILE-LENGTH says that an error must be raised
  ;; for streams not associated with files (which broadcast streams
  ;; aren't according to the glossary). However, the behaviour of
  ;; FILE-LENGTH for broadcast streams is explicitly described in the
  ;; BROADCAST-STREAM entry.
  (stream-api-dispatch (stream)
    :simple (s-%file-length stream)
    ;; Perhaps if there were a generic function to obtain the pathname?
    :gray (error "~S is not defined for ~S" 'file-length stream)
    :native (progn
              (unless (typep stream 'broadcast-stream)
                (stream-file-stream-or-lose stream))
              (call-ansi-stream-misc stream :file-length))))

;; Placing this definition (formerly in "toplevel") after the important
;; stream types are known produces smaller+faster code than it did before.
(defun stream-output-stream (stream)
  (typecase stream
    (fd-stream
     stream)
    (synonym-stream
     (stream-output-stream (resolve-synonym-stream stream)))
    (two-way-stream
     (stream-output-stream
      (two-way-stream-output-stream stream)))
    (t
     stream)))

;;; STREAM-ERROR-STREAM is supposed to return a stream even if the
;;; stream has dynamic-extent. While technically a stream,
;;; this object is not actually usable as such - it's only for error reporting.
(defstruct (stub-stream
            (:include ansi-stream)
            (:constructor %make-stub-stream (direction string)))
  (direction nil :read-only t)
  (string nil :read-only t)) ; string or nil

(defun make-stub-stream (underlying-stream)
  (multiple-value-bind (direction string)
      (etypecase underlying-stream
        (string-input-stream
         (values :input (string-input-stream-string underlying-stream)))
        (string-output-stream
         (values :output nil))
        (fill-pointer-output-stream
         (values :output (fill-pointer-output-stream-string underlying-stream))))
    (%make-stub-stream direction string)))

(defmethod print-object ((stub stub-stream) stream)
  (print-unreadable-object (stub stream)
    (let ((direction (stub-stream-direction stub))
          (string (stub-stream-string stub)))
      (format stream "dynamic-extent ~A (unavailable)~@[ ~A ~S~]"
              (if (eq direction :input) 'string-input-stream 'string-output-stream)
              (if string (if (eq direction :input) "from" "to"))
              (if (> (length string) 10)
                  (concatenate 'string (subseq string 0 8) "...")
                  string)))))


;;;; initialization

;;; the stream connected to the controlling terminal, or NIL if there is none
(defvar *tty*)

;;; the stream connected to the standard input (file descriptor 0)
(defvar *stdin*)

;;; the stream connected to the standard output (file descriptor 1)
(defvar *stdout*)

;;; the stream connected to the standard error output (file descriptor 2)
(defvar *stderr*)

;;; This is called when the cold load is first started up, and may also
;;; be called in an attempt to recover from nested errors.
(defun stream-cold-init-or-reset ()
  (stream-reinit)
  (setf *terminal-io* (make-synonym-stream '*tty*))
  (setf *standard-output* (make-synonym-stream '*stdout*))
  (setf *standard-input* (make-synonym-stream '*stdin*))
  (setf *error-output* (make-synonym-stream '*stderr*))
  (setf *query-io* (make-synonym-stream '*terminal-io*))
  (setf *debug-io* *query-io*)
  (setf *trace-output* *standard-output*)
  (values))

(defun stream-deinit ()
  (setq *tty* nil *stdin* nil *stdout* nil *stderr* nil)
  ;; Unbind to make sure we're not accidently dealing with it
  ;; before we're ready (or after we think it's been deinitialized).
  ;; This uses the internal %MAKUNBOUND because the CL: function would
  ;; rightly complain that *AVAILABLE-BUFFERS* is proclaimed always bound.
  (%makunbound '*available-buffers*))

(defvar *streams-closed-by-slad*)

(defun restore-fd-streams ()
  (loop for (stream in bin n-bin cout bout sout misc) in *streams-closed-by-slad*
        do
        (setf (ansi-stream-in stream) in)
        (setf (ansi-stream-bin stream) bin)
        (setf (ansi-stream-n-bin stream) n-bin)
        (setf (ansi-stream-cout stream) cout)
        (setf (ansi-stream-bout stream) bout)
        (setf (ansi-stream-sout stream) sout)
        (setf (ansi-stream-misc stream) misc)))

(defun stdstream-external-format (fd)
  (declare (ignorable fd))
  (let* ((keyword (cond #+(and win32 sb-unicode)
                        ((sb-win32::console-handle-p fd)
                         :ucs-2)
                        (t
                         (external-format-keyword (default-external-format)))))
         (ef (get-external-format keyword))
         (replacement (ef-default-replacement-character ef)))
    `(,keyword :replacement ,replacement)))

;;; This is called whenever a saved core is restarted.
(defun stream-reinit (&optional init-buffers-p)
  (when init-buffers-p
    ;; Use the internal %BOUNDP for similar reason to that cited above-
    ;; BOUNDP on a known global transforms to the constant T.
    (aver (not (%boundp '*available-buffers*)))
    (setf *available-buffers* nil))
  (%with-output-to-string (*error-output*)
    (multiple-value-bind (in out err)
        #-win32 (values 0 1 2)
        #+win32 (sb-win32::get-std-handles)
      (labels (#+win32
               (nul-stream (name inputp outputp)
                 (let ((nul-handle
                         (cond
                           ((and inputp outputp)
                            (sb-win32:unixlike-open "NUL" sb-unix:o_rdwr))
                           (inputp
                            (sb-win32:unixlike-open "NUL" sb-unix:o_rdonly))
                           (outputp
                            (sb-win32:unixlike-open "NUL" sb-unix:o_wronly))
                           (t
                            ;; Not quite sure what to do in this case.
                            nil))))
                   (make-fd-stream
                    nul-handle
                    :name name
                    :input inputp
                    :output outputp
                    :buffering :line
                    :element-type :default
                    :serve-events inputp
                    :auto-close t
                    :external-format (stdstream-external-format nul-handle))))
               (stdio-stream (handle name inputp outputp)
                 (cond
                   #+win32
                   ((null handle)
                    ;; If no actual handle was present, create a stream to NUL
                    (nul-stream name inputp outputp))
                   (t
                    (make-fd-stream
                     handle
                     :name name
                     :input inputp
                     :output outputp
                     :buffering :line
                     :element-type :default
                     :serve-events inputp
                     :external-format (stdstream-external-format handle))))))
        (setf *stdin*  (stdio-stream in  "standard input"  t   nil)
              *stdout* (stdio-stream out "standard output" nil t)
              *stderr* (stdio-stream err "standard error"  nil t))))
    #+win32
    (setf *tty* (make-two-way-stream *stdin* *stdout*))
    #-win32
    (let ((tty (sb-unix:unix-open "/dev/tty" sb-unix:o_rdwr #o666)))
      (setf *tty*
            (if tty
                (make-fd-stream tty :name "the terminal"
                                    :input t :output t :buffering :line
                                    :external-format (stdstream-external-format tty)
                                    :serve-events t
                                    :auto-close t)
                (make-two-way-stream *stdin* *stdout*))))
    (princ (get-output-stream-string *error-output*) *stderr*))
  (values))
