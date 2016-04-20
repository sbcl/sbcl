;;;; os-independent stream functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; standard streams

;;; The initialization of these streams is performed by
;;; STREAM-COLD-INIT-OR-RESET.
(defvar *terminal-io* () #!+sb-doc "terminal I/O stream")
(defvar *standard-input* () #!+sb-doc "default input stream")
(defvar *standard-output* () #!+sb-doc "default output stream")
(defvar *error-output* () #!+sb-doc "error output stream")
(defvar *query-io* () #!+sb-doc "query I/O stream")
(defvar *trace-output* () #!+sb-doc "trace output stream")
(defvar *debug-io* () #!+sb-doc "interactive debugging stream")

(defun stream-element-type-stream-element-mode (element-type)
  (when (eq element-type :default)
    (return-from stream-element-type-stream-element-mode :bivalent))

  (unless (valid-type-specifier-p element-type)
    (return-from stream-element-type-stream-element-mode :bivalent))

  (let* ((characterp (subtypep element-type 'character))
         (unsigned-byte-p (subtypep element-type 'unsigned-byte))
         ;; Every UNSIGNED-BYTE subtype is a SIGNED-BYTE
         ;; subtype. Therefore explicitly check for intersection with
         ;; the negative integers.
         (signed-byte-p (and (subtypep element-type 'signed-byte)
                             (not (subtypep `(and ,element-type (integer * -1))
                                            nil)))))
    (cond
      ((and characterp (not unsigned-byte-p) (not signed-byte-p))
       'character)
      ((and (not characterp) unsigned-byte-p (not signed-byte-p))
       'unsigned-byte)
      ((and (not characterp) (not unsigned-byte-p) signed-byte-p)
       'signed-byte)
      (t
       :bivalent))))

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
(defun no-op-placeholder (&rest ignore)
  (declare (ignore ignore)))

;;; stream manipulation functions

;;; SYNONYM-STREAM type is needed by ANSI-STREAM-{INPUT,OUTPUT}-STREAM-P
(defstruct (synonym-stream (:include ansi-stream
                                     (in #'synonym-in)
                                     (bin #'synonym-bin)
                                     (n-bin #'synonym-n-bin)
                                     (out #'synonym-out)
                                     (bout #'synonym-bout)
                                     (sout #'synonym-sout)
                                     (misc #'synonym-misc))
                           (:constructor make-synonym-stream (symbol))
                           (:copier nil))
  ;; This is the symbol, the value of which is the stream we are synonym to.
  (symbol nil :type symbol :read-only t))
(declaim (freeze-type synonym-stream))

(defun ansi-stream-input-stream-p (stream)
  (declare (type ansi-stream stream))
  (if (synonym-stream-p stream)
      (input-stream-p (symbol-value (synonym-stream-symbol stream)))
      (and (not (eq (ansi-stream-in stream) #'closed-flame))
       ;;; KLUDGE: It's probably not good to have EQ tests on function
       ;;; values like this. What if someone's redefined the function?
       ;;; Is there a better way? (Perhaps just VALID-FOR-INPUT and
       ;;; VALID-FOR-OUTPUT flags? -- WHN 19990902
           (or (not (eq (ansi-stream-in stream) #'ill-in))
               (not (eq (ansi-stream-bin stream) #'ill-bin))))))

;;; Temporary definition that gets overwritten by pcl/gray-streams
(defun input-stream-p (stream)
  (declare (type stream stream))
  (and (ansi-stream-p stream)
       (ansi-stream-input-stream-p stream)))

(defun ansi-stream-output-stream-p (stream)
  (declare (type ansi-stream stream))
  (if (synonym-stream-p stream)
      (output-stream-p (symbol-value (synonym-stream-symbol stream)))
      (and (not (eq (ansi-stream-in stream) #'closed-flame))
           (or (not (eq (ansi-stream-out stream) #'ill-out))
               (not (eq (ansi-stream-bout stream) #'ill-bout))))))

;;; Temporary definition that gets overwritten by pcl/gray-streams
(defun output-stream-p (stream)
  (declare (type stream stream))
  (and (ansi-stream-p stream)
       (ansi-stream-output-stream-p stream)))

(declaim (inline ansi-stream-open-stream-p))
(defun ansi-stream-open-stream-p (stream)
  (declare (type ansi-stream stream))
  ;; CLHS 22.1.4 lets us not worry about synonym streams here.
  (not (eq (ansi-stream-in stream) #'closed-flame)))

(defun open-stream-p (stream)
  (ansi-stream-open-stream-p stream))

(declaim (inline ansi-stream-element-type))
(defun ansi-stream-element-type (stream)
  (declare (type ansi-stream stream))
  (funcall (ansi-stream-misc stream) stream :element-type))

(defun stream-element-type (stream)
  (ansi-stream-element-type stream))

(defun stream-external-format (stream)
  (funcall (ansi-stream-misc stream) stream :external-format))

(defun interactive-stream-p (stream)
  (declare (type stream stream))
  (funcall (ansi-stream-misc stream) stream :interactive-p))

(declaim (inline ansi-stream-close))
(defun ansi-stream-close (stream abort)
  (declare (type ansi-stream stream))
  (when (open-stream-p stream)
    (funcall (ansi-stream-misc stream) stream :close abort))
  t)

(defun close (stream &key abort)
  (ansi-stream-close stream abort))

(defun set-closed-flame (stream)
  (setf (ansi-stream-in stream) #'closed-flame)
  (setf (ansi-stream-bin stream) #'closed-flame)
  (setf (ansi-stream-n-bin stream) #'closed-flame)
  (setf (ansi-stream-out stream) #'closed-flame)
  (setf (ansi-stream-bout stream) #'closed-flame)
  (setf (ansi-stream-sout stream) #'closed-flame)
  (setf (ansi-stream-misc stream) #'closed-flame))

;;;; for file position and file length
(defun external-format-char-size (external-format)
  (ef-char-size (get-external-format external-format)))

;;; Call the MISC method with the :FILE-POSITION operation.
#!-sb-fluid (declaim (inline ansi-stream-file-position))
(defun ansi-stream-file-position (stream position)
  (declare (type stream stream))
  (declare (type (or index (alien sb!unix:unix-offset) (member nil :start :end))
                 position))
  ;; FIXME: It would be good to comment on the stuff that is done here...
  ;; FIXME: This doesn't look interrupt safe.
  (cond
    (position
     (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
     (funcall (ansi-stream-misc stream) stream :file-position position))
    (t
     (let ((res (funcall (ansi-stream-misc stream) stream :file-position nil)))
       (when res
         #!-sb-unicode
         (- res
            (- +ansi-stream-in-buffer-length+
               (ansi-stream-in-index stream)))
         #!+sb-unicode
         (let ((char-size (if (fd-stream-p stream)
                              (fd-stream-char-size stream)
                              (external-format-char-size (stream-external-format stream)))))
           (- res
              (etypecase char-size
                (function
                 (loop with buffer = (ansi-stream-cin-buffer stream)
                       with start = (ansi-stream-in-index stream)
                       for i from start below +ansi-stream-in-buffer-length+
                       sum (funcall char-size (aref buffer i))))
                (fixnum
                 (* char-size
                    (- +ansi-stream-in-buffer-length+
                       (ansi-stream-in-index stream))))))))))))

(defun file-position (stream &optional position)
  (if (ansi-stream-p stream)
      (ansi-stream-file-position stream position)
      (stream-file-position stream position)))

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
(defun stream-associated-with-file-p (x)
  #!+sb-doc
  "Test for the ANSI concept \"stream associated with a file\"."
  (or (typep x 'file-stream)
      (and (synonym-stream-p x)
           (stream-associated-with-file-p (symbol-value
                                           (synonym-stream-symbol x))))))

(defun stream-must-be-associated-with-file (stream)
  (declare (type stream stream))
  (unless (stream-associated-with-file-p stream)
    (error 'simple-type-error
           ;; KLUDGE: The ANSI spec for FILE-LENGTH specifically says
           ;; this should be TYPE-ERROR. But what then can we use for
           ;; EXPECTED-TYPE? This SATISFIES type (with a nonstandard
           ;; private predicate function..) is ugly and confusing, but
           ;; I can't see any other way. -- WHN 2001-04-14
           :datum stream
           :expected-type '(satisfies stream-associated-with-file-p)
           :format-control
           "~@<The stream ~2I~_~S ~I~_isn't associated with a file.~:>"
           :format-arguments (list stream))))

(defun file-string-length (stream object)
  (funcall (ansi-stream-misc stream) stream :file-string-length object))

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

#!-sb-fluid (declaim (inline ansi-stream-read-line))
(defun ansi-stream-read-line (stream eof-error-p eof-value recursive-p)
  (declare (ignore recursive-p))
  (if (ansi-stream-cin-buffer stream)
      ;; Stream has a fast-read-char buffer. Copy large chunks directly
      ;; out of the buffer.
      (ansi-stream-read-line-from-frc-buffer stream eof-error-p eof-value)
      ;; Slow path, character by character.
      (prepare-for-fast-read-char stream
        (let ((res (make-string 80))
              (len 80)
              (index 0))
          (loop
             (let ((ch (fast-read-char nil nil)))
               (cond (ch
                      (when (char= ch #\newline)
                        (done-with-fast-read-char)
                        (return (values (%shrink-vector res index) nil)))
                      (when (= index len)
                        (setq len (* len 2))
                        (let ((new (make-string len)))
                          (replace new res)
                          (setq res new)))
                      (setf (schar res index) ch)
                      (incf index))
                     ((zerop index)
                      (done-with-fast-read-char)
                      (return (values (eof-or-lose stream
                                                   eof-error-p
                                                   eof-value)
                                      t)))
                     ;; Since FAST-READ-CHAR already hit the eof char, we
                     ;; shouldn't do another READ-CHAR.
                     (t
                      (done-with-fast-read-char)
                      (return (values (%shrink-vector res index) t))))))))))

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value
                            recursive-p)
  (declare (explicit-check))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-read-line stream eof-error-p eof-value recursive-p)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (multiple-value-bind (string eof) (stream-read-line stream)
          (if (and eof (zerop (length string)))
              (values (eof-or-lose stream eof-error-p eof-value) t)
              (values string eof))))))

;;; We proclaim them INLINE here, then proclaim them NOTINLINE later on,
;;; so, except in this file, they are not inline by default, but they can be.
#!-sb-fluid (declaim (inline read-char unread-char read-byte listen))

#!-sb-fluid (declaim (inline ansi-stream-read-char))
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
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-read-char stream eof-error-p eof-value recursive-p)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (let ((char (stream-read-char stream)))
          (if (eq char :eof)
              (eof-or-lose stream eof-error-p eof-value)
              (the character char))))))

#!-sb-fluid (declaim (inline ansi-stream-unread-char))
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
           (funcall (ansi-stream-misc stream) stream
                    :unread character)))))

(defun unread-char (character &optional (stream *standard-input*))
  (declare (explicit-check))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-unread-char character stream)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (stream-unread-char stream character)))
  nil)

#!-sb-fluid (declaim (inline ansi-stream-listen))
(defun ansi-stream-listen (stream)
  (or (/= (the fixnum (ansi-stream-in-index stream))
          +ansi-stream-in-buffer-length+)
      ;; Handle :EOF return from misc methods specially
      (let ((result (funcall (ansi-stream-misc stream) stream :listen)))
        (if (eq result :eof)
            nil
            result))))

(defun listen (&optional (stream *standard-input*))
  (declare (explicit-check))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-listen stream)
        ;; Fall through to Gray streams FUNDAMENTAL-STREAM case.
        (stream-listen stream))))

#!-sb-fluid (declaim (inline ansi-stream-read-char-no-hang))
(defun ansi-stream-read-char-no-hang (stream eof-error-p eof-value recursive-p)
  (if (funcall (ansi-stream-misc stream) stream :listen)
      ;; On T or :EOF get READ-CHAR to do the work.
      (ansi-stream-read-char stream eof-error-p eof-value recursive-p)
      nil))

(defun read-char-no-hang (&optional (stream *standard-input*)
                                    (eof-error-p t)
                                    eof-value
                                    recursive-p)
  (declare (explicit-check))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-read-char-no-hang stream eof-error-p eof-value
                                       recursive-p)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (let ((char (stream-read-char-no-hang stream)))
          (if (eq char :eof)
              (eof-or-lose stream eof-error-p eof-value)
              (the (or character null) char))))))

#!-sb-fluid (declaim (inline ansi-stream-clear-input))
(defun ansi-stream-clear-input (stream)
  (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
  (funcall (ansi-stream-misc stream) stream :clear-input))

(defun clear-input (&optional (stream *standard-input*))
  (declare (explicit-check))
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-clear-input stream)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (stream-clear-input stream)))
  nil)

#!-sb-fluid (declaim (inline ansi-stream-read-byte))
(defun ansi-stream-read-byte (stream eof-error-p eof-value recursive-p)
  ;; Why the "recursive-p" parameter?  a-s-r-b is funcall'ed from
  ;; a-s-read-sequence and needs a lambda list that's congruent with
  ;; that of a-s-read-char
  (declare (ignore recursive-p))
  (with-fast-read-byte (t stream eof-error-p eof-value)
    ;; FIXME: the overhead of the UNWIND-PROTECT inserted by
    ;; WITH-FAST-READ-BYTE significantly impacts the time taken
    ;; by single byte reads. For this use-case we could
    ;; probably just change it to a PROG1.
    (fast-read-byte)))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (declare (explicit-check))
  (if (ansi-stream-p stream)
      (ansi-stream-read-byte stream eof-error-p eof-value nil)
      ;; must be Gray streams FUNDAMENTAL-STREAM
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
#!-sb-fluid (declaim (inline read-n-bytes))
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
  (let* ((in-buffer (ansi-stream-in-buffer stream))
         (index (ansi-stream-in-index stream))
         (num-buffered (- +ansi-stream-in-buffer-length+ index)))
    (declare (fixnum index num-buffered))
    (cond
     ((not in-buffer)
      (funcall (ansi-stream-n-bin stream)
               stream
               buffer
               start
               numbytes
               eof-error-p))
     ((<= numbytes num-buffered)
      #+nil
      (let ((copy-function (typecase buffer
                             ((simple-array * (*)) #'ub8-bash-copy)
                             (system-area-pointer #'copy-ub8-to-system-area))))
        (funcall copy-function in-buffer index buffer start numbytes))
      (%byte-blt in-buffer index
                 buffer start (+ start numbytes))
      (setf (ansi-stream-in-index stream) (+ index numbytes))
      numbytes)
     (t
      (let ((end (+ start num-buffered)))
        #+nil
        (let ((copy-function (typecase buffer
                             ((simple-array * (*)) #'ub8-bash-copy)
                             (system-area-pointer #'copy-ub8-to-system-area))))
          (funcall copy-function in-buffer index buffer start num-buffered))
        (%byte-blt in-buffer index buffer start end)
        (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
        (+ (funcall (ansi-stream-n-bin stream)
                    stream
                    buffer
                    end
                    (- numbytes num-buffered)
                    eof-error-p)
           num-buffered))))))

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
         (count (funcall (ansi-stream-n-bin stream)
                         stream
                         ibuf
                         +ansi-stream-in-buffer-extra+
                         (- +ansi-stream-in-buffer-length+
                            +ansi-stream-in-buffer-extra+)
                         nil))
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
           (let* ((index (1- +ansi-stream-in-buffer-length+))
                  (value (funcall (ansi-stream-in stream) stream nil :eof)))
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
                (setf (ansi-stream-in-index stream) index)))))
          (t
           (when (/= start +ansi-stream-in-buffer-extra+)
             (#.(let* ((n-character-array-bits
                        (sb!vm:saetp-n-bits
                         (find 'character
                               sb!vm:*specialized-array-element-type-properties*
                               :key #'sb!vm:saetp-specifier)))
                       (bash-function (intern (format nil "UB~D-BASH-COPY" n-character-array-bits)
                                              (find-package "SB!KERNEL"))))
                  bash-function)
                ibuf +ansi-stream-in-buffer-extra+
                ibuf start
                count))
           (setf (ansi-stream-in-index stream) start)))))

;;; This is similar to FAST-READ-CHAR-REFILL, but we don't have to
;;; leave room for unreading.
(defun fast-read-byte-refill (stream eof-error-p eof-value)
  (let* ((ibuf (ansi-stream-in-buffer stream))
         (count (funcall (ansi-stream-n-bin stream) stream
                         ibuf 0 +ansi-stream-in-buffer-length+
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
  (with-out-stream stream (ansi-stream-out character)
                   (stream-write-char character))
  character)

(defun terpri (&optional (stream *standard-output*))
  (declare (explicit-check))
  (with-out-stream stream (ansi-stream-out #\newline) (stream-terpri))
  nil)

#!-sb-fluid (declaim (inline ansi-stream-fresh-line))
(defun ansi-stream-fresh-line (stream)
  (when (/= (or (charpos stream) 1) 0)
    (funcall (ansi-stream-out stream) stream #\newline)
    t))

(defun fresh-line (&optional (stream *standard-output*))
  (declare (explicit-check))
  (let ((stream (out-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-fresh-line stream)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (stream-fresh-line stream))))

#!-sb-fluid (declaim (inline ansi-stream-write-string))
(defun ansi-stream-write-string (string stream start end)
  (with-array-data ((data string) (offset-start start)
                    (offset-end end)
                    :check-fill-pointer t)
    (funcall (ansi-stream-sout stream)
             stream data offset-start offset-end)))

(defun %write-string (string stream start end)
  (let ((stream (out-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-write-string string stream start end)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (stream-write-string stream string start end)))
  string)

(defun write-string (string &optional (stream *standard-output*)
                            &key (start 0) end)
  (declare (type string string))
  (declare (type stream-designator stream))
  (declare (explicit-check))
  (%write-string string stream start end))

(defun write-line (string &optional (stream *standard-output*)
                   &key (start 0) end)
  (declare (type string string))
  (declare (type stream-designator stream))
  (declare (explicit-check))
  (let ((stream (out-synonym-of stream)))
    (cond ((ansi-stream-p stream)
           (ansi-stream-write-string string stream start end)
           (funcall (ansi-stream-out stream) stream #\newline))
          (t
           (stream-write-string stream string start end)
           (stream-write-char stream #\newline))))
  string)

(defun charpos (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :charpos) (stream-line-column)))

(defun line-length (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :line-length)
                   (stream-line-length)))

(defun finish-output (&optional (stream *standard-output*))
  (declare (explicit-check))
  (with-out-stream stream (ansi-stream-misc :finish-output)
                   (stream-finish-output))
  nil)

(defun force-output (&optional (stream *standard-output*))
  (declare (explicit-check))
  (with-out-stream stream (ansi-stream-misc :force-output)
                   (stream-force-output))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  (declare (explicit-check))
  (with-out-stream stream (ansi-stream-misc :clear-output)
                   (stream-clear-output))
  nil)

(defun write-byte (integer stream)
  (declare (explicit-check))
  (with-out-stream/no-synonym stream (ansi-stream-bout integer)
                              (stream-write-byte integer))
  integer)


;;; Meta: the following comment is mostly true, but gray stream support
;;;   is already incorporated into the definitions within this file.
;;;   But these need to redefinable, otherwise the relative order of
;;;   loading sb-simple-streams and any user-defined code which executes
;;;   (F #'read-char ...) is sensitive to the order in which those
;;;   are loaded, though insensitive at compile-time.
;;; (These were inline throughout this file, but that's not appropriate
;;; globally.  And we must not inline them in the rest of this file if
;;; dispatch to gray or simple streams is to work, since both redefine
;;; these functions later.)
(declaim (notinline read-char unread-char read-byte listen))

;;; This is called from ANSI-STREAM routines that encapsulate CLOS
;;; streams to handle the misc routines and dispatch to the
;;; appropriate SIMPLE- or FUNDAMENTAL-STREAM functions.
(defun stream-misc-dispatch (stream operation &optional arg1 arg2)
  (declare (type stream stream) (ignore arg2))
  (ecase operation
    (:listen
     ;; Return T if input available, :EOF for end-of-file, otherwise NIL.
     (let ((char (read-char-no-hang stream nil :eof)))
       (when (characterp char)
         (unread-char char stream))
       char))
    (:unread
     (unread-char arg1 stream))
    (:close
     (close stream))
    (:clear-input
     (clear-input stream))
    (:force-output
     (force-output stream))
    (:finish-output
     (finish-output stream))
    (:element-type
     (stream-element-type stream))
    (:stream-external-format
     (stream-external-format stream))
    (:interactive-p
     (interactive-stream-p stream))
    (:line-length
     (line-length stream))
    (:charpos
     (charpos stream))
    (:file-length
     (file-length stream))
    (:file-string-length
     (file-string-length stream arg1))
    (:file-position
     (file-position stream arg1))))

;;;; broadcast streams

(defstruct (broadcast-stream (:include ansi-stream
                                       (out #'broadcast-out)
                                       (bout #'broadcast-bout)
                                       (sout #'broadcast-sout)
                                       (misc #'broadcast-misc))
                             (:constructor %make-broadcast-stream
                                           (streams))
                             (:copier nil)
                             (:predicate nil))
  ;; a list of all the streams we broadcast to
  (streams () :type list :read-only t))

(declaim (freeze-type broadcast-stream))

(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (unless (output-stream-p stream)
      (error 'type-error
             :datum stream
             :expected-type '(satisfies output-stream-p))))
  (%make-broadcast-stream streams))

(macrolet ((out-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                (dolist (stream (broadcast-stream-streams stream))
                  (,fun ,(car args) stream ,@(cdr args))))))
  (out-fun broadcast-out write-char char)
  (out-fun broadcast-bout write-byte byte)
  (out-fun broadcast-sout %write-string string start end))

(defun broadcast-misc (stream operation &optional arg1 arg2)
  (let ((streams (broadcast-stream-streams stream)))
    (case operation
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
      (:file-position
       (if arg1
           (let ((res (or (eql arg1 :start) (eql arg1 0))))
             (dolist (stream streams res)
               (setq res (file-position stream arg1))))
           (let ((last (last streams)))
             (if last
                 (file-position (car last))
                 0))))
      (:file-string-length
       (let ((last (last streams)))
         (if last
             (file-string-length (car last) arg1)
             1)))
      (:close
       (set-closed-flame stream))
      (t
       (let ((res nil))
         (dolist (stream streams res)
           (setq res
                 (if (ansi-stream-p stream)
                     (funcall (ansi-stream-misc stream) stream operation
                              arg1 arg2)
                     (stream-misc-dispatch stream operation arg1 arg2)))))))))

;;;; synonym streams

(defmethod print-object ((x synonym-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream ":SYMBOL ~S" (synonym-stream-symbol x))))

;;; The output simple output methods just call the corresponding
;;; function on the synonymed stream.
(macrolet ((out-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                (declare (optimize (safety 1)))
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
                (declare (optimize (safety 1)))
                (,fun (symbol-value (synonym-stream-symbol stream))
                      ,@args))))
  (in-fun synonym-in read-char eof-error-p eof-value)
  (in-fun synonym-bin read-byte eof-error-p eof-value)
  (in-fun synonym-n-bin read-n-bytes buffer start numbytes eof-error-p))

(defun synonym-misc (stream operation &optional arg1 arg2)
  (declare (optimize (safety 1)))
  (let ((syn (symbol-value (synonym-stream-symbol stream))))
    (if (ansi-stream-p syn)
        ;; We have to special-case some operations which interact with
        ;; the in-buffer of the wrapped stream, since just calling
        ;; ANSI-STREAM-MISC on them
        (case operation
          (:listen (or (/= (the fixnum (ansi-stream-in-index syn))
                           +ansi-stream-in-buffer-length+)
                       (funcall (ansi-stream-misc syn) syn :listen)))
          (:clear-input (clear-input syn))
          (:unread (unread-char arg1 syn))
          (t
           (funcall (ansi-stream-misc syn) syn operation arg1 arg2)))
        (stream-misc-dispatch syn operation arg1 arg2))))

;;;; two-way streams

(defstruct (two-way-stream
            (:include ansi-stream
                      (in #'two-way-in)
                      (bin #'two-way-bin)
                      (n-bin #'two-way-n-bin)
                      (out #'two-way-out)
                      (bout #'two-way-bout)
                      (sout #'two-way-sout)
                      (misc #'two-way-misc))
            (:constructor %make-two-way-stream (input-stream output-stream))
            (:copier nil)
            (:predicate nil))
  (input-stream (missing-arg) :type stream :read-only t)
  (output-stream (missing-arg) :type stream :read-only t))

(defprinter (two-way-stream) input-stream output-stream)

(defun make-two-way-stream (input-stream output-stream)
  #!+sb-doc
  "Return a bidirectional stream which gets its input from INPUT-STREAM and
   sends its output to OUTPUT-STREAM."
  ;; FIXME: This idiom of the-real-stream-of-a-possibly-synonym-stream
  ;; should be encapsulated in a function, and used here and most of
  ;; the other places that SYNONYM-STREAM-P appears.
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
                (,fun (two-way-stream-input-stream stream) ,@args))))
  (in-fun two-way-in read-char eof-error-p eof-value)
  (in-fun two-way-bin read-byte eof-error-p eof-value)
  (in-fun two-way-n-bin read-n-bytes buffer start numbytes eof-error-p))

(defun two-way-misc (stream operation &optional arg1 arg2)
  (let* ((in (two-way-stream-input-stream stream))
         (out (two-way-stream-output-stream stream))
         (in-ansi-stream-p (ansi-stream-p in))
         (out-ansi-stream-p (ansi-stream-p out)))
    (case operation
      (:listen
       (if in-ansi-stream-p
           (or (/= (the fixnum (ansi-stream-in-index in))
                   +ansi-stream-in-buffer-length+)
               (funcall (ansi-stream-misc in) in :listen))
           (listen in)))
      ((:finish-output :force-output :clear-output)
       (if out-ansi-stream-p
           (funcall (ansi-stream-misc out) out operation arg1 arg2)
           (stream-misc-dispatch out operation arg1 arg2)))
      (:clear-input (clear-input in))
      (:unread (unread-char arg1 in))
      (:element-type
       (let ((in-type (stream-element-type in))
             (out-type (stream-element-type out)))
         (if (equal in-type out-type)
             in-type `(and ,in-type ,out-type))))
      (:close
       (set-closed-flame stream))
      (t
       (or (if in-ansi-stream-p
               (funcall (ansi-stream-misc in) in operation arg1 arg2)
               (stream-misc-dispatch in operation arg1 arg2))
           (if out-ansi-stream-p
               (funcall (ansi-stream-misc out) out operation arg1 arg2)
               (stream-misc-dispatch out operation arg1 arg2)))))))

;;;; concatenated streams

(defstruct (concatenated-stream
            (:include ansi-stream
                      (in #'concatenated-in)
                      (bin #'concatenated-bin)
                      (n-bin #'concatenated-n-bin)
                      (misc #'concatenated-misc))
            (:constructor %make-concatenated-stream (streams))
            (:copier nil)
            (:predicate nil))
  ;; The car of this is the substream we are reading from now.
  (streams nil :type list))

(declaim (freeze-type concatenated-stream))

(defmethod print-object ((x concatenated-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream
            ":STREAMS ~S"
            (concatenated-stream-streams x))))

(defun make-concatenated-stream (&rest streams)
  #!+sb-doc
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
                (do ((streams (concatenated-stream-streams stream)
                              (cdr streams)))
                    ((null streams)
                     (eof-or-lose stream eof-error-p eof-value))
                  (let* ((stream (car streams))
                         (result (,fun stream nil nil)))
                    (when result (return result)))
                  (pop (concatenated-stream-streams stream))))))
  (in-fun concatenated-in read-char)
  (in-fun concatenated-bin read-byte))

(defun concatenated-n-bin (stream buffer start numbytes eof-errorp)
  (do ((streams (concatenated-stream-streams stream) (cdr streams))
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
    (setf (concatenated-stream-streams stream) (cdr streams))))

(defun concatenated-misc (stream operation &optional arg1 arg2)
  (let* ((left (concatenated-stream-streams stream))
         (current (car left)))
    (case operation
      (:listen
       (unless left
         (return-from concatenated-misc :eof))
       (loop
        (let ((stuff (if (ansi-stream-p current)
                         (funcall (ansi-stream-misc current) current
                                  :listen)
                         (stream-misc-dispatch current :listen))))
          (cond ((eq stuff :eof)
                 ;; Advance STREAMS, and try again.
                 (pop (concatenated-stream-streams stream))
                 (setf current
                       (car (concatenated-stream-streams stream)))
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
             (funcall (ansi-stream-misc current) current operation arg1 arg2)
             (stream-misc-dispatch current operation arg1 arg2)))))))

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

(declaim (freeze-type echo-stream))

(defmethod print-object ((x echo-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (format stream
            ":INPUT-STREAM ~S :OUTPUT-STREAM ~S"
            (two-way-stream-input-stream x)
            (two-way-stream-output-stream x))))

(defun make-echo-stream (input-stream output-stream)
  #!+sb-doc
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

(defun echo-n-bin (stream buffer start numbytes eof-error-p)
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
       (write-sequence buffer (echo-stream-output-stream stream)
                       :start start :end (+ start bytes-read))
       bytes-read)
      ((> numbytes bytes-read)
       (write-sequence buffer (echo-stream-output-stream stream)
                       :start start :end (+ start bytes-read))
       (error 'end-of-file :stream stream))
      (t
       (write-sequence buffer (echo-stream-output-stream stream)
                       :start start :end (+ start bytes-read))
       (aver (= numbytes (+ start bytes-read)))
       numbytes))))

;;;; STRING-INPUT-STREAM stuff

(defstruct (string-input-stream
             (:include ansi-stream
                       (in #'string-inch)
                       (misc #'string-in-misc))
             (:constructor %make-string-input-stream
                           (string current end))
             (:copier nil)
             (:predicate nil))
  (string (missing-arg) :type simple-string :read-only t)
  (current (missing-arg) :type index)
  (end (missing-arg) :type index))

(declaim (freeze-type string-input-stream))

(defun string-inch (stream eof-error-p eof-value)
  (declare (type string-input-stream stream))
  (let ((string (string-input-stream-string stream))
        (index (string-input-stream-current stream)))
    (cond ((>= index (the index (string-input-stream-end stream)))
           (eof-or-lose stream eof-error-p eof-value))
          (t
           (setf (string-input-stream-current stream) (1+ index))
           (char string index)))))

(defun string-binch (stream eof-error-p eof-value)
  (declare (type string-input-stream stream))
  (let ((string (string-input-stream-string stream))
        (index (string-input-stream-current stream)))
    (cond ((>= index (the index (string-input-stream-end stream)))
           (eof-or-lose stream eof-error-p eof-value))
          (t
           (setf (string-input-stream-current stream) (1+ index))
           (char-code (char string index))))))

(defun string-stream-read-n-bytes (stream buffer start requested eof-error-p)
  (declare (type string-input-stream stream)
           (type index start requested))
  (let* ((string (string-input-stream-string stream))
         (index (string-input-stream-current stream))
         (available (- (string-input-stream-end stream) index))
         (copy (min available requested)))
    (declare (type simple-string string))
    (when (plusp copy)
      (setf (string-input-stream-current stream)
            (truly-the index (+ index copy)))
      ;; FIXME: why are we VECTOR-SAP'ing things here?  what's the point?
      ;; and are there SB-UNICODE issues here as well?  --njf, 2005-03-24
      (with-pinned-objects (string buffer)
        (system-area-ub8-copy (vector-sap string)
                              index
                              (if (typep buffer 'system-area-pointer)
                                  buffer
                                  (vector-sap buffer))
                              start
                              copy)))
    (if (and (> requested copy) eof-error-p)
        (error 'end-of-file :stream stream)
        copy)))

(defun string-in-misc (stream operation &optional arg1 arg2)
  (declare (type string-input-stream stream)
           (ignore arg2))
  (case operation
    (:file-position
     (if arg1
         (setf (string-input-stream-current stream)
               (case arg1
                 (:start 0)
                 (:end (string-input-stream-end stream))
                 ;; We allow moving position beyond EOF. Errors happen
                 ;; on read, not move.
                 (t arg1)))
         (string-input-stream-current stream)))
    ;; According to ANSI: "Should signal an error of type type-error
    ;; if stream is not a stream associated with a file."
    ;; This is checked by FILE-LENGTH, so no need to do it here either.
    ;; (:file-length (length (string-input-stream-string stream)))
    (:unread (decf (string-input-stream-current stream)))
    (:close (set-closed-flame stream))
    (:listen (or (/= (the index (string-input-stream-current stream))
                     (the index (string-input-stream-end stream)))
                 :eof))
    (:element-type (array-element-type (string-input-stream-string stream)))))

(defun make-string-input-stream (string &optional (start 0) end)
  #!+sb-doc
  "Return an input stream which will supply the characters of STRING between
  START and END in order."
  (declare (type string string)
           (type index start)
           (type (or index null) end))
  ;; FIXME: very inefficient if the input string is, say a 100000-character
  ;; adjustable string but (- END START) is 100 characters. We should use
  ;; SUBSEQ instead of coercing the whole string. And if STRING is non-simple
  ;; but has element type CHARACTER, wouldn't it work to just use the
  ;; underlying simple-string since %MAKE-STRING-INPUT-STREAM accepts bounding
  ;; indices that can be fudged to deal with any offset?
  ;; And (for unicode builds) if the input is BASE-STRING, we should use
  ;; MAKE-ARRAY and REPLACE to coerce just the specified piece.
  (let* ((string (coerce string '(simple-array character (*)))))
    ;; Why WITH-ARRAY-DATA, since the array is already simple?
    ;; because it's a nice abstract way to check the START and END.
    (with-array-data ((string string) (start start) (end end))
      (%make-string-input-stream
       string ;; now simple
       start end))))

;;;; STRING-OUTPUT-STREAM stuff
;;;;
;;;; FIXME: This, like almost none of the stream code is particularly
;;;; interrupt or thread-safe. While it should not be possible to
;;;; corrupt the heap here, it certainly is possible to end up with
;;;; a string-output-stream whose internal state is messed up.
;;;;
;;;; FIXME: It would be nice to support space-efficient
;;;; string-output-streams with element-type base-char. This would
;;;; mean either a separate subclass, or typecases in functions.

(defparameter *string-output-stream-buffer-initial-size* 64)

(defstruct (string-output-stream
            (:include ansi-stream
                      (out #'string-ouch)
                      (sout #'string-sout)
                      (misc #'string-out-misc))
            (:constructor %make-string-output-stream (element-type))
            (:copier nil)
            (:predicate nil))
  ;; The string we throw stuff in.
  (buffer (make-string
           *string-output-stream-buffer-initial-size*)
   :type (simple-array character (*)))
  ;; Chains of buffers to use
  (prev nil :type list)
  (next nil :type list)
  ;; Index of the next location to use in the current string.
  (pointer 0 :type index)
  ;; Global location in the stream
  (index 0 :type index)
  ;; Index cache: when we move backwards we save the greater of this
  ;; and index here, so the greater of index and this is always the
  ;; end of the stream.
  (index-cache 0 :type index)
  ;; Requested element type
  ;; FIXME: there seems to be no way to skip the type-check in the ctor,
  ;; which is redundant with the check in MAKE-STRING-OUTPUT-STREAM.
  (element-type 'character :type type-specifier
                           :read-only t))

(declaim (freeze-type string-output-stream))
(defun make-string-output-stream (&key (element-type 'character))
  #!+sb-doc
  "Return an output stream which will accumulate all output given it for the
benefit of the function GET-OUTPUT-STREAM-STRING."
  (declare (explicit-check))
  (if (csubtypep (specifier-type element-type) (specifier-type 'character))
      (%make-string-output-stream element-type)
      (error "~S is not a subtype of CHARACTER" element-type)))

;;; Pushes the current segment onto the prev-list, and either pops
;;; or allocates a new one.
(defun string-output-stream-new-buffer (stream size)
  (declare (index size))
  (/noshow0 "/string-output-stream-new-buffer")
  (push (string-output-stream-buffer stream)
        (string-output-stream-prev stream))
  (setf (string-output-stream-buffer stream)
        (or (pop (string-output-stream-next stream))
            ;; FIXME: This would be the correct place to detect that
            ;; more than FIXNUM characters are being written to the
            ;; stream, and do something about it.
            (make-string size))))

;;; Moves to the end of the next segment or the current one if there are
;;; no more segments. Returns true as long as there are next segments.
(defun string-output-stream-next-buffer (stream)
  (/noshow0 "/string-output-stream-next-buffer")
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
  (/noshow0 "/string-output-stream-prev-buffer")
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

(defun string-ouch (stream character)
  (/noshow0 "/string-ouch")
  (let ((pointer (string-output-stream-pointer stream))
        (buffer (string-output-stream-buffer stream))
        (index (string-output-stream-index stream)))
    (cond ((= pointer (length buffer))
           (setf buffer (string-output-stream-new-buffer stream index)
                 (aref buffer 0) character
                 (string-output-stream-pointer stream) 1))
          (t
           (setf (aref buffer pointer) character
                 (string-output-stream-pointer stream) (1+ pointer))))
    (setf (string-output-stream-index stream) (1+ index))))

(defun string-sout (stream string start end)
  (declare (type simple-string string)
           (type index start end))
  (let* ((full-length (- end start))
         (length full-length)
         (buffer (string-output-stream-buffer stream))
         (pointer (string-output-stream-pointer stream))
         (space (- (length buffer) pointer))
         (here (min space length))
         (stop (+ start here))
         (overflow (- length space)))
    (declare (index length space here stop full-length)
             (fixnum overflow)
             (type (simple-array character (*)) buffer))
    (tagbody
     :more
       (when (plusp here)
         (etypecase string
           ((simple-array character (*))
            (replace buffer string :start1 pointer :start2 start :end2 stop))
           (simple-base-string
            (replace buffer string :start1 pointer :start2 start :end2 stop))
           ((simple-array nil (*))
            (replace buffer string :start1 pointer :start2 start :end2 stop)))
         (setf (string-output-stream-pointer stream) (+ here pointer)))
       (when (plusp overflow)
         (setf start stop
               length (- end start)
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
           (loop while (< pos index)
                 do (string-output-stream-prev-buffer stream)
                 (setf index (string-output-stream-index stream)))
           (let ((step (- pos index)))
             (incf (string-output-stream-pointer stream) step)
             (setf (string-output-stream-index stream) pos)))
          ((> pos index)
           ;; We allow moving beyond the end of stream, implicitly
           ;; extending the output stream.
           (let ((next (string-output-stream-next-buffer stream)))
             ;; Update after -next-buffer, INDEX is kept pointing at
             ;; the end of the current buffer.
             (setf index (string-output-stream-index stream))
             (loop while (and next (> pos index))
                   do (setf next (string-output-stream-next-buffer stream)
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

(defun string-out-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (declare (optimize speed))
  (case operation
    (:charpos
     ;; Keeping this first is a silly micro-optimization: FRESH-LINE
     ;; makes this the most common one.
     (/noshow0 "/string-out-misc charpos")
     (prog ((pointer (string-output-stream-pointer stream))
            (buffer (string-output-stream-buffer stream))
            (prev (string-output-stream-prev stream))
            (base 0))
        (declare (type (or null (simple-array character (*))) buffer))
      :next
      (let ((pos (when buffer
                   (position #\newline buffer :from-end t :end pointer))))
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
    (:file-position
     (/noshow0 "/string-out-misc file-position")
     (when arg1
       (set-string-output-stream-file-position stream arg1))
     (string-output-stream-index stream))
    (:close
     (/noshow0 "/string-out-misc close")
     (set-closed-flame stream))
    (:element-type (string-output-stream-element-type stream))))

;;; Return a string of all the characters sent to a stream made by
;;; MAKE-STRING-OUTPUT-STREAM since the last call to this function.
(defun get-output-stream-string (stream)
  (declare (type string-output-stream stream))
  (let* ((length (max (string-output-stream-index stream)
                      (string-output-stream-index-cache stream)))
         (element-type (string-output-stream-element-type stream))
         (prev (nreverse (string-output-stream-prev stream)))
         (this (string-output-stream-buffer stream))
         (next (string-output-stream-next stream))
         (result
          (case element-type
            ;; overwhelmingly common case: can be inlined
            ;;
            ;; FIXME: If we were willing to use %SHRINK-VECTOR here,
            ;; and allocate new strings the size of 2 * index in
            ;; STRING-SOUT, we would not need to allocate one here in
            ;; the common case, but could just use the last one
            ;; allocated, and chop it down to size..
            ;;
            ((character) (make-string length))
            ;; slightly less common cases: inline it anyway
            ((base-char standard-char)
             (make-string length :element-type 'base-char))
            (t
             (make-string length :element-type element-type)))))

    (setf (string-output-stream-index stream) 0
          (string-output-stream-index-cache stream) 0
          (string-output-stream-pointer stream) 0
          ;; throw them away for simplicity's sake: this way the rest of the
          ;; implementation can assume that the greater of INDEX and INDEX-CACHE
          ;; is always within the last buffer.
          (string-output-stream-prev stream) nil
          (string-output-stream-next stream) nil)

    (flet ((replace-all (fun)
             (let ((start 0))
               (declare (index start))
               (dolist (buffer prev)
                 (funcall fun buffer start)
                 (incf start (length buffer)))
               (funcall fun this start)
               (incf start (length this))
               (dolist (buffer next)
                 (funcall fun buffer start)
                 (incf start (length buffer)))
               ;; Hack: erase the pointers to strings, to make it less
               ;; likely that the conservative GC will accidentally
               ;; retain the buffers.
               (fill prev nil)
               (fill next nil))))
      (macrolet ((frob (type)
                   `(replace-all (lambda (buffer from)
                                   (declare (type ,type result)
                                            (type (simple-array character (*))
                                                  buffer))
                                   (replace result buffer :start1 from)))))
        (etypecase result
          ((simple-array character (*))
           (frob (simple-array character (*))))
          (simple-base-string
           (frob simple-base-string))
          ((simple-array nil (*))
           (frob (simple-array nil (*)))))))

    result))

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

(defstruct (fill-pointer-output-stream
            (:include ansi-stream
                      (out #'fill-pointer-ouch)
                      (sout #'fill-pointer-sout)
                      (misc #'fill-pointer-misc))
            (:constructor make-fill-pointer-output-stream (string))
            (:copier nil)
            (:predicate nil))
  ;; a string with a fill pointer where we stuff the stuff we write
  (string (missing-arg) :type string-with-fill-pointer :read-only t))

(declaim (freeze-type fill-pointer-output-stream))

(defun fill-pointer-ouch (stream character)
  (let* ((buffer (fill-pointer-output-stream-string stream))
         (current (fill-pointer buffer))
         (current+1 (1+ current)))
    (declare (fixnum current))
    (with-array-data ((workspace buffer) (start) (end))
      (string-dispatch
          ((simple-array character (*))
           (simple-array base-char (*)))
          workspace
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
          (setf (char workspace offset-current) character))))
    current+1))

(defun fill-pointer-sout (stream string start end)
  (declare (fixnum start end))
  (string-dispatch
      ((simple-array character (*))
       (simple-array base-char (*)))
      string
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

(defun fill-pointer-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:file-position
     (let ((buffer (fill-pointer-output-stream-string stream)))
       (if arg1
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
                      arg1)))
           (fill-pointer buffer))))
    (:charpos
     (let* ((buffer (fill-pointer-output-stream-string stream))
            (current (fill-pointer buffer)))
       (with-array-data ((string buffer) (start) (end current))
         (declare (simple-string string) (ignore start))
         (let ((found (position #\newline string :test #'char=
                                :end end :from-end t)))
           (if found
               (- end (the fixnum found))
               current)))))
     (:element-type
      (array-element-type
       (fill-pointer-output-stream-string stream)))))

;;;; case frobbing streams, used by FORMAT ~(...~)

(defstruct (case-frob-stream
            (:include ansi-stream
                      (misc #'case-frob-misc))
            (:constructor %make-case-frob-stream (target out sout))
            (:copier nil))
  (target (missing-arg) :type stream :read-only t))

(declaim (freeze-type case-frob-stream))

(defun make-case-frob-stream (target kind)
  #!+sb-doc
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
      (multiple-value-bind (out sout)
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
        (%make-case-frob-stream target out sout))))

(defun case-frob-misc (stream op &optional arg1 arg2)
  (declare (type case-frob-stream stream))
  (case op
    (:close
     (set-closed-flame stream))
    (t
     (let ((target (case-frob-stream-target stream)))
       (if (ansi-stream-p target)
           (funcall (ansi-stream-misc target) target op arg1 arg2)
           (stream-misc-dispatch target op arg1 arg2))))))

(defun case-frob-upcase-out (stream char)
  (declare (type case-frob-stream stream)
           (type character char))
  (let ((target (case-frob-stream-target stream))
        (char (char-upcase char)))
    (if (ansi-stream-p target)
        (funcall (ansi-stream-out target) target char)
        (stream-write-char target char))))

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
        (char (char-downcase char)))
    (if (ansi-stream-p target)
        (funcall (ansi-stream-out target) target char)
        (stream-write-char target char))))

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
                 (funcall (ansi-stream-out target) target char)
                 (stream-write-char target char)))
           (setf (case-frob-stream-out stream) #'case-frob-capitalize-aux-out)
           (setf (case-frob-stream-sout stream)
                 #'case-frob-capitalize-aux-sout))
          (t
           (if (ansi-stream-p target)
               (funcall (ansi-stream-out target) target char)
               (stream-write-char target char))))))

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
      (setf (case-frob-stream-out stream)
            #'case-frob-capitalize-aux-out)
      (setf (case-frob-stream-sout stream)
            #'case-frob-capitalize-aux-sout))
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
                 (funcall (ansi-stream-out target) target char)
                 (stream-write-char target char))))
          (t
           (if (ansi-stream-p target)
               (funcall (ansi-stream-out target) target char)
               (stream-write-char target char))
           (setf (case-frob-stream-out stream)
                 #'case-frob-capitalize-out)
           (setf (case-frob-stream-sout stream)
                 #'case-frob-capitalize-sout)))))

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
      (setf (case-frob-stream-out stream)
            #'case-frob-capitalize-out)
      (setf (case-frob-stream-sout stream)
            #'case-frob-capitalize-sout))
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
                 (funcall (ansi-stream-out target) target char)
                 (stream-write-char target char)))
           (setf (case-frob-stream-out stream)
                 #'case-frob-downcase-out)
           (setf (case-frob-stream-sout stream)
                 #'case-frob-downcase-sout))
          (t
           (if (ansi-stream-p target)
               (funcall (ansi-stream-out target) target char)
               (stream-write-char target char))))))

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
          (setf (case-frob-stream-out stream)
                #'case-frob-downcase-out)
          (setf (case-frob-stream-sout stream)
                #'case-frob-downcase-sout)
          (return))))
    (if (ansi-stream-p target)
        (funcall (ansi-stream-sout target) target str 0 len)
        (stream-write-string target str 0 len))))

;;;; Shared {READ,WRITE}-SEQUENCE support functions

(declaim (inline stream-element-mode
                 stream-compute-io-function
                 compatible-vector-and-stream-element-types-p))

(defun stream-element-mode (stream)
  (declare (type stream stream))
  (if (fd-stream-p stream)
      (fd-stream-element-mode stream)
      (stream-element-type-stream-element-mode
       (stream-element-type stream))))

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
           (eq (stream-element-mode stream) 'unsigned-byte))
      (and (typep vector '(simple-array (signed-byte 8) (*)))
           (eq (stream-element-mode stream) 'signed-byte))))

;;;; READ-SEQUENCE

(defun read-sequence (seq stream &key (start 0) end)
  #!+sb-doc
  "Destructively modify SEQ by reading elements from STREAM.
  That part of SEQ bounded by START and END is destructively modified by
  copying successive elements into it from STREAM. If the end of file
  for STREAM is reached before copying all elements of the subsequence,
  then the extra elements near the end of sequence are not updated, and
  the index of the next element is returned."
  (declare (type sequence seq)
           (type stream stream)
           (type index start)
           (type sequence-end end)
           (values index))
  (if (ansi-stream-p stream)
      (ansi-stream-read-sequence seq stream start end)
      ;; must be Gray streams FUNDAMENTAL-STREAM
      (stream-read-sequence stream seq start end)))

(declaim (inline read-sequence/read-function))
(defun read-sequence/read-function (seq stream start %end
                                    stream-element-mode
                                    character-read-function binary-read-function)
  (declare (type sequence seq)
           (type stream stream)
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
               (declare (ignore read-function))
               (error "~@<~A does not yet support generic sequences.~@:>"
                      'read-sequence)))
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
(declaim (notinline read-sequence/read-function))

(defun ansi-stream-read-sequence (seq stream start %end)
  (declare (type sequence seq)
           (type ansi-stream stream)
           (type index start)
           (type sequence-end %end)
           (values index &optional))
  (locally (declare (inline read-sequence/read-function))
    (read-sequence/read-function
     seq stream start %end (stream-element-mode stream)
     #'ansi-stream-read-char #'ansi-stream-read-byte)))

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
      (labels ((refill-buffer ()
                 (prog1 (fast-read-char-refill stream nil)
                   (setf %frc-index% (ansi-stream-in-index %frc-stream%))))
               (add-chunk ()
                 (let* ((end (length %frc-buffer%))
                        (len (min (- end %frc-index%)
                                  (- needed read))))
                   (declare (type index end len read needed))
                   (string-dispatch (simple-base-string
                                     (simple-array character (*)))
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
        (loop (add-chunk))))))


;;;; WRITE-SEQUENCE

(defun write-sequence (seq stream &key (start 0) (end nil))
  #!+sb-doc
  "Write the elements of SEQ bounded by START and END to STREAM."
  (declare (type sequence seq)
           (type stream stream)
           (type index start)
           (type sequence-end end)
           (values sequence))
  (if (ansi-stream-p stream)
      (ansi-stream-write-sequence seq stream start end)
      ;; must be Gray-streams FUNDAMENTAL-STREAM
      (stream-write-sequence stream seq start end)))

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

(declaim (inline write-sequence/write-function))
(defun write-sequence/write-function (seq stream start %end
                                      stream-element-mode
                                      character-write-function
                                      binary-write-function)
  (declare (type sequence seq)
           (type stream stream)
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
               (declare (ignore write-function))
               (error "~@<~A does not yet support generic sequences.~@:>"
                      'write-sequence)))
      (declare (dynamic-extent #'compute-write-function
                               #'write-element/bivalent #'write-list
                               #'write-vector  #'write-generic-sequence))
      (etypecase seq
        (list
         (write-list (compute-write-function nil)))
        (string
         (if (ansi-stream-p stream)
             (ansi-stream-write-string seq stream start end)
             (stream-write-string stream seq start end)))
        (vector
         (with-array-data ((data seq) (offset-start start) (offset-end end)
                           :check-fill-pointer t)
           (if (and (fd-stream-p stream)
                    (compatible-vector-and-stream-element-types-p data stream))
               (buffer-output stream data offset-start offset-end)
               (write-vector data offset-start offset-end
                             (compute-write-function
                              (array-element-type seq))))))
        (sequence
         (write-generic-sequence (compute-write-function nil)))))))
(declaim (notinline write-sequence/write-function))

(defun ansi-stream-write-sequence (seq stream start %end)
  (declare (type sequence seq)
           (type ansi-stream stream)
           (type index start)
           (type sequence-end %end)
           (values sequence))
  (locally (declare (inline write-sequence/write-function))
    (write-sequence/write-function
     seq stream start %end (stream-element-mode stream)
     (ansi-stream-out stream) (ansi-stream-bout stream)))
  seq)

;;; like FILE-POSITION, only using :FILE-LENGTH
(defun file-length (stream)
  ;; FIXME: the FIXME following this one seems wrong on 2 counts:
  ;;  1. since when does cross-compiler hangup occur on undefined types?
  ;;  2. why is that the correct set of types to check for?
  ;; FIXME: The following declaration uses yet undefined types, which
  ;; cause cross-compiler hangup.
  ;;
  ;; (declare (type (or file-stream synonym-stream) stream))
  ;;
  ;; The description for FILE-LENGTH says that an error must be raised
  ;; for streams not associated with files (which broadcast streams
  ;; aren't according to the glossary). However, the behaviour of
  ;; FILE-LENGTH for broadcast streams is explicitly described in the
  ;; BROADCAST-STREAM entry.
  (unless (typep stream 'broadcast-stream)
    (stream-must-be-associated-with-file stream))
  (funcall (ansi-stream-misc stream) stream :file-length))

;; Placing this definition (formerly in "toplevel") after the important
;; stream types are known produces smaller+faster code than it did before.
(defun stream-output-stream (stream)
  (typecase stream
    (fd-stream
     stream)
    (synonym-stream
     (stream-output-stream
      (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (stream-output-stream
      (two-way-stream-output-stream stream)))
    (t
     stream)))

;;;; etc.
