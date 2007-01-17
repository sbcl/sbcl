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
  (error "~S is closed." stream))
(defun no-op-placeholder (&rest ignore)
  (declare (ignore ignore)))

;;; stream manipulation functions

(declaim (inline ansi-stream-input-stream-p))
(defun ansi-stream-input-stream-p (stream)
  (declare (type ansi-stream stream))

  (when (synonym-stream-p stream)
    (setf stream
          (symbol-value (synonym-stream-symbol stream))))

  (and (not (eq (ansi-stream-in stream) #'closed-flame))
       ;;; KLUDGE: It's probably not good to have EQ tests on function
       ;;; values like this. What if someone's redefined the function?
       ;;; Is there a better way? (Perhaps just VALID-FOR-INPUT and
       ;;; VALID-FOR-OUTPUT flags? -- WHN 19990902
       (or (not (eq (ansi-stream-in stream) #'ill-in))
           (not (eq (ansi-stream-bin stream) #'ill-bin)))))

(defun input-stream-p (stream)
  (declare (type stream stream))
  (and (ansi-stream-p stream)
       (ansi-stream-input-stream-p stream)))

(declaim (inline ansi-stream-output-stream-p))
(defun ansi-stream-output-stream-p (stream)
  (declare (type ansi-stream stream))

  (when (synonym-stream-p stream)
    (setf stream (symbol-value
                  (synonym-stream-symbol stream))))

  (and (not (eq (ansi-stream-in stream) #'closed-flame))
       (or (not (eq (ansi-stream-out stream) #'ill-out))
           (not (eq (ansi-stream-bout stream) #'ill-bout)))))

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
  (setf (ansi-stream-in stream) #'closed-flame)
  (setf (ansi-stream-out stream) #'closed-flame)
  (setf (ansi-stream-bout stream) #'closed-flame)
  (setf (ansi-stream-sout stream) #'closed-flame)
  (setf (ansi-stream-misc stream) #'closed-flame))

;;;; file position and file length

;;; Call the MISC method with the :FILE-POSITION operation.
#!-sb-fluid (declaim (inline ansi-stream-file-position))
(defun ansi-stream-file-position (stream position)
  (declare (type stream stream))
  (declare (type (or index (alien sb!unix:off-t) (member nil :start :end))
                 position))
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
         (let* ((external-format (stream-external-format stream))
                (ef-entry (find-external-format external-format))
                (variable-width-p (variable-width-external-format-p ef-entry))
                (char-len (bytes-for-char-fun ef-entry)))
           (- res
              (if variable-width-p
                  (loop with buffer = (ansi-stream-cin-buffer stream)
                        with start = (ansi-stream-in-index stream)
                        for i from start below +ansi-stream-in-buffer-length+
                        sum (funcall char-len (aref buffer i)))
                  (* (funcall char-len #\x)  ; arbitrary argument
                     (- +ansi-stream-in-buffer-length+
                        (ansi-stream-in-index stream)))))))))))

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

;;; like FILE-POSITION, only using :FILE-LENGTH
(defun file-length (stream)
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

(defun file-string-length (stream object)
  (funcall (ansi-stream-misc stream) stream :file-string-length object))

;;;; input functions

#!-sb-fluid (declaim (inline ansi-stream-read-line))
(defun ansi-stream-read-line (stream eof-error-p eof-value recursive-p)
  (declare (ignore recursive-p))
  (prepare-for-fast-read-char stream
    ;; Check whether the FAST-READ-CHAR buffer contains a newline. If it
    ;; does, we can do things quickly by just copying the line from the
    ;; buffer instead of doing repeated calls to FAST-READ-CHAR.
    (when %frc-buffer%
      (locally
          ;; For %FIND-POSITION transform
          (declare (optimize (speed 2)))
        (let ((pos (position #\Newline %frc-buffer%
                             :test #'char=
                             :start %frc-index%)))
          (when pos
            (let* ((len (- pos %frc-index%))
                   (res (make-string len)))
              (replace res %frc-buffer% :start2 %frc-index% :end2 pos)
              (setf %frc-index% (1+ pos))
              (done-with-fast-read-char)
              (return-from ansi-stream-read-line res))))))
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
                  (return (values (%shrink-vector res index) t)))))))))

(defun read-line (&optional (stream *standard-input*) (eof-error-p t) eof-value
                            recursive-p)
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
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-read-char stream eof-error-p eof-value recursive-p)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (let ((char (stream-read-char stream)))
          (if (eq char :eof)
              (eof-or-lose stream eof-error-p eof-value)
              char)))))

#!-sb-fluid (declaim (inline ansi-stream-unread-char))
(defun ansi-stream-unread-char (character stream)
  (let ((index (1- (ansi-stream-in-index stream)))
        (buffer (ansi-stream-cin-buffer stream)))
    (declare (fixnum index))
    (when (minusp index) (error "nothing to unread"))
    (cond (buffer
           (setf (aref buffer index) character)
           (setf (ansi-stream-in-index stream) index))
          (t
           (funcall (ansi-stream-misc stream) stream
                    :unread character)))))

(defun unread-char (character &optional (stream *standard-input*))
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
  (let ((stream (in-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-read-char-no-hang stream eof-error-p eof-value
                                       recursive-p)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (let ((char (stream-read-char-no-hang stream)))
          (if (eq char :eof)
              (eof-or-lose stream eof-error-p eof-value)
              char)))))

#!-sb-fluid (declaim (inline ansi-stream-clear-input))
(defun ansi-stream-clear-input (stream)
  (setf (ansi-stream-in-index stream) +ansi-stream-in-buffer-length+)
  (funcall (ansi-stream-misc stream) stream :clear-input))

(defun clear-input (&optional (stream *standard-input*))
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
  (prepare-for-fast-read-byte stream
    (prog1
        (fast-read-byte eof-error-p eof-value t)
      (done-with-fast-read-byte))))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (if (ansi-stream-p stream)
      (ansi-stream-read-byte stream eof-error-p eof-value nil)
      ;; must be Gray streams FUNDAMENTAL-STREAM
      (let ((char (stream-read-byte stream)))
        (if (eq char :eof)
            (eof-or-lose stream eof-error-p eof-value)
            char))))

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
(defun read-n-bytes (stream buffer start numbytes &optional (eof-error-p t))
  (declare (type ansi-stream stream)
           (type index numbytes start)
           (type (or (simple-array * (*)) system-area-pointer) buffer))
  (let* ((stream (in-synonym-of stream ansi-stream))
         (in-buffer (ansi-stream-in-buffer stream))
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
;;; and hence must be an N-BIN method.
(defun fast-read-char-refill (stream eof-error-p eof-value)
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
           (setf (ansi-stream-in-index stream)
                 +ansi-stream-in-buffer-length+)
           (funcall (ansi-stream-in stream) stream eof-error-p eof-value))
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
           (setf (ansi-stream-in-index stream) (1+ start))
           (aref ibuf start)))))

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
  (with-out-stream stream (ansi-stream-out character)
                   (stream-write-char character))
  character)

(defun terpri (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-out #\newline) (stream-terpri))
  nil)

#!-sb-fluid (declaim (inline ansi-stream-fresh-line))
(defun ansi-stream-fresh-line (stream)
  (when (/= (or (charpos stream) 1) 0)
    (funcall (ansi-stream-out stream) stream #\newline)
    t))

(defun fresh-line (&optional (stream *standard-output*))
  (let ((stream (out-synonym-of stream)))
    (if (ansi-stream-p stream)
        (ansi-stream-fresh-line stream)
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (stream-fresh-line stream))))

(defun write-string (string &optional (stream *standard-output*)
                            &key (start 0) end)
  (declare (type string string))
  ;; Note that even though you might expect, based on the behavior of
  ;; things like AREF, that the correct upper bound here is
  ;; (ARRAY-DIMENSION STRING 0), the ANSI glossary definitions for
  ;; "bounding index" and "length" indicate that in this case (i.e.
  ;; for the ANSI-specified functions WRITE-STRING [and WRITE-LINE]),
  ;; (LENGTH STRING) is the required upper bound. A foolish
  ;; consistency is the hobgoblin of lesser languages..
  (%write-string string stream start (%check-vector-sequence-bounds
                                      string start end))
  string)

#!-sb-fluid (declaim (inline ansi-stream-write-string))
(defun ansi-stream-write-string (string stream start end)
  (declare (type string string))
  (declare (type ansi-stream stream))
  (declare (type index start end))
  (if (array-header-p string)
      (with-array-data ((data string) (offset-start start)
                        (offset-end end))
        (funcall (ansi-stream-sout stream)
                 stream data offset-start offset-end))
      (funcall (ansi-stream-sout stream) stream string start end))
  string)

(defun %write-string (string stream start end)
  (declare (type string string))
  (declare (type stream-designator stream))
  (declare (type index start end))
  (let ((stream (out-synonym-of stream)))
    (if(ansi-stream-p stream)
       (ansi-stream-write-string string stream start end)
       ;; must be Gray streams FUNDAMENTAL-STREAM
       (stream-write-string stream string start end))))

;;; A wrapper function for all those (MACROLET OUT-FUN) definitions,
;;; which cannot deal with keyword arguments.
(declaim (inline write-string-no-key))
(defun write-string-no-key (string stream start end)
  (write-string string stream :start start :end end))

(defun write-line (string &optional (stream *standard-output*)
                          &key (start 0) end)
  (declare (type string string))
  ;; FIXME: Why is there this difference between the treatments of the
  ;; STREAM argument in WRITE-STRING and WRITE-LINE?
  (let ((defaulted-stream (out-synonym-of stream)))
    (%write-string string defaulted-stream start (%check-vector-sequence-bounds
                                                  string start end))
    (write-char #\newline defaulted-stream))
  string)

(defun charpos (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :charpos) (stream-line-column)))

(defun line-length (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :line-length)
                   (stream-line-length)))

(defun finish-output (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :finish-output)
                   (stream-finish-output))
  nil)

(defun force-output (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :force-output)
                   (stream-force-output))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  (with-out-stream stream (ansi-stream-misc :clear-output)
                   (stream-force-output))
  nil)

(defun write-byte (integer stream)
  (with-out-stream/no-synonym stream (ansi-stream-bout integer)
                              (stream-write-byte integer))
  integer)


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
                                           (&rest streams))
                             (:copier nil))
  ;; a list of all the streams we broadcast to
  (streams () :type list :read-only t))

(defun make-broadcast-stream (&rest streams)
  (dolist (stream streams)
    (unless (output-stream-p stream)
      (error 'type-error
             :datum stream
             :expected-type '(satisfies output-stream-p))))
  (apply #'%make-broadcast-stream streams))

(macrolet ((out-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                (dolist (stream (broadcast-stream-streams stream))
                  (,fun ,(car args) stream ,@(cdr args))))))
  (out-fun broadcast-out write-char char)
  (out-fun broadcast-bout write-byte byte)
  (out-fun broadcast-sout write-string-no-key string start end))

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
           (if charpos (return charpos)))))
      (:line-length
       (let ((min nil))
         (dolist (stream streams min)
           (let ((res (line-length stream)))
             (when res (setq min (if min (min res min) res)))))))
      (:element-type
       #+nil ; old, arguably more logical, version
       (let (res)
         (dolist (stream streams (if (> (length res) 1) `(and ,@res) t))
           (pushnew (stream-element-type stream) res :test #'equal)))
       ;; ANSI-specified version (under System Class BROADCAST-STREAM)
       (let ((res t))
         (do ((streams streams (cdr streams)))
             ((null streams) res)
           (when (null (cdr streams))
             (setq res (stream-element-type (car streams)))))))
      (:external-format
       (let ((res :default))
         (dolist (stream streams res)
           (setq res (stream-external-format stream)))))
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
           (let ((res 0))
             (dolist (stream streams res)
               (setq res (file-position stream))))))
      (:file-string-length
       (let ((res 1))
         (dolist (stream streams res)
           (setq res (file-string-length stream arg1)))))
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
(def!method print-object ((x synonym-stream) stream)
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
  (out-fun synonym-sout write-string-no-key string start end))

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
            (:copier nil))
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
  (funcall #'%make-two-way-stream input-stream output-stream))

(macrolet ((out-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                (let ((syn (two-way-stream-output-stream stream)))
                  (,fun ,(car args) syn ,@(cdr args))))))
  (out-fun two-way-out write-char ch)
  (out-fun two-way-bout write-byte n)
  (out-fun two-way-sout write-string-no-key string start end))

(macrolet ((in-fun (name fun &rest args)
             `(defun ,name (stream ,@args)
                (force-output (two-way-stream-output-stream stream))
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
            (:constructor %make-concatenated-stream (&rest streams))
            (:copier nil))
  ;; The car of this is the substream we are reading from now.
  (streams nil :type list))
(def!method print-object ((x concatenated-stream) stream)
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
  (apply #'%make-concatenated-stream streams))

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
            (:copier nil))
  unread-stuff)
(def!method print-object ((x echo-stream) stream)
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
  (funcall #'%make-echo-stream input-stream output-stream))

(macrolet ((in-fun (name in-fun out-fun &rest args)
             `(defun ,name (stream ,@args)
                (or (pop (echo-stream-unread-stuff stream))
                    (let* ((in (echo-stream-input-stream stream))
                           (out (echo-stream-output-stream stream))
                           (result (if eof-error-p
                                       (,in-fun in ,@args)
                                       (,in-fun in nil in))))
                      (cond
                        ((eql result in) eof-value)
                        (t (,out-fun result out) result)))))))
  (in-fun echo-in read-char write-char eof-error-p eof-value)
  (in-fun echo-bin read-byte write-byte eof-error-p eof-value))

(defun echo-n-bin (stream buffer start numbytes eof-error-p)
  (let ((new-start start)
        (read 0))
    (loop
     (let ((thing (pop (echo-stream-unread-stuff stream))))
       (cond
         (thing
          (setf (aref buffer new-start) thing)
          (incf new-start)
          (incf read)
          (when (= read numbytes)
            (return-from echo-n-bin numbytes)))
         (t (return nil)))))
    (let ((bytes-read (read-n-bytes (echo-stream-input-stream stream) buffer
                                    new-start (- numbytes read) nil)))
      (cond
        ((not eof-error-p)
         (write-sequence buffer (echo-stream-output-stream stream)
                         :start new-start :end (+ new-start bytes-read))
         (+ bytes-read read))
        ((> numbytes (+ read bytes-read))
         (write-sequence buffer (echo-stream-output-stream stream)
                         :start new-start :end (+ new-start bytes-read))
         (error 'end-of-file :stream stream))
        (t
         (write-sequence buffer (echo-stream-output-stream stream)
                         :start new-start :end (+ new-start bytes-read))
         (aver (= numbytes (+ new-start bytes-read)))
         numbytes)))))

;;;; STRING-INPUT-STREAM stuff

(defstruct (string-input-stream
             (:include ansi-stream
                       (in #'string-inch)
                       (bin #'ill-bin)
                       (n-bin #'ill-bin)
                       (misc #'string-in-misc))
             (:constructor internal-make-string-input-stream
                           (string current end))
             (:copier nil))
  (string (missing-arg) :type simple-string)
  (current (missing-arg) :type index)
  (end (missing-arg) :type index))

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
      (sb!sys:without-gcing
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
                 ;; on read, not move -- or the user may extend the
                 ;; input string.
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
  (let* ((string (coerce string '(simple-array character (*))))
         (end (%check-vector-sequence-bounds string start end)))
    (with-array-data ((string string) (start start) (end end))
      (internal-make-string-input-stream
       string ;; now simple
       start
       end))))

;;;; STRING-OUTPUT-STREAM stuff

(defstruct (string-output-stream
            (:include ansi-stream
                      (out #'string-ouch)
                      (sout #'string-sout)
                      (misc #'string-out-misc))
            (:constructor make-string-output-stream
                          (&key (element-type 'character)
                           &aux (string (make-string 40))))
            (:copier nil))
  ;; The string we throw stuff in.
  (string (missing-arg) :type (simple-array character (*)))
  ;; Index of the next location to use.
  (index 0 :type fixnum)
  ;; Index cache for string-output-stream-last-index
  (index-cache 0 :type fixnum)
  ;; Requested element type
  (element-type 'character))

#!+sb-doc
(setf (fdocumentation 'make-string-output-stream 'function)
  "Return an output stream which will accumulate all output given it for
   the benefit of the function GET-OUTPUT-STREAM-STRING.")

(defun string-output-stream-last-index (stream)
  (max (string-output-stream-index stream)
       (string-output-stream-index-cache stream)))

(defun string-ouch (stream character)
  (let ((current (string-output-stream-index stream))
        (workspace (string-output-stream-string stream)))
    (declare (type (simple-array character (*)) workspace)
             (type fixnum current))
    (if (= current (the fixnum (length workspace)))
        (let ((new-workspace (make-string (* current 2))))
          (replace new-workspace workspace)
          (setf (aref new-workspace current) character
                (string-output-stream-string stream) new-workspace))
        (setf (aref workspace current) character))
    (setf (string-output-stream-index stream) (1+ current))))

(defun string-sout (stream string start end)
  (declare (type simple-string string)
           (type fixnum start end))
  (let* ((string (if (typep string '(simple-array character (*)))
                     string
                     (coerce string '(simple-array character (*)))))
         (current (string-output-stream-index stream))
         (length (- end start))
         (dst-end (+ length current))
         (workspace (string-output-stream-string stream)))
    (declare (type (simple-array character (*)) workspace string)
             (type fixnum current length dst-end))
    (if (> dst-end (the fixnum (length workspace)))
        (let ((new-workspace (make-string (+ (* current 2) length))))
          (replace new-workspace workspace :end2 current)
          (replace new-workspace string
                   :start1 current :end1 dst-end
                   :start2 start :end2 end)
          (setf (string-output-stream-string stream) new-workspace))
        (replace workspace string
                 :start1 current :end1 dst-end
                 :start2 start :end2 end))
    (setf (string-output-stream-index stream) dst-end)))

(defun string-out-misc (stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:file-position
     (if arg1
         (let ((end (string-output-stream-last-index stream)))
           (setf (string-output-stream-index-cache stream) end
                 (string-output-stream-index stream)
                 (case arg1
                   (:start 0)
                   (:end end)
                   (t
                    ;; We allow moving beyond the end of stream,
                    ;; implicitly extending the output stream.
                    (let ((buffer (string-output-stream-string stream)))
                      (when (> arg1 (length buffer))
                        (setf (string-output-stream-string stream)
                              (make-string
                               arg1 :element-type (array-element-type buffer))
                              (subseq (string-output-stream-string stream)
                                      0 end)
                              (subseq buffer 0 end))))
                      arg1))))
         (string-output-stream-index stream)))
    (:close (set-closed-flame stream))
    (:charpos
     (do ((index (1- (the fixnum (string-output-stream-index stream)))
                 (1- index))
          (count 0 (1+ count))
          (string (string-output-stream-string stream)))
         ((< index 0) count)
       (declare (type (simple-array character (*)) string)
                (type fixnum index count))
       (if (char= (schar string index) #\newline)
           (return count))))
    (:element-type (array-element-type (string-output-stream-string stream)))))

;;; Return a string of all the characters sent to a stream made by
;;; MAKE-STRING-OUTPUT-STREAM since the last call to this function.
(defun get-output-stream-string (stream)
  (declare (type string-output-stream stream))
  (let* ((length (string-output-stream-last-index stream))
         (element-type (string-output-stream-element-type stream))
         (result
          (case element-type
            ;; overwhelmingly common case: can be inlined
            ((character) (make-string length))
            ;; slightly less common cases: inline it anyway
            ((base-char standard-char)
             (make-string length :element-type 'base-char))
            (t (make-string length :element-type element-type)))))
    ;; For the benefit of the REPLACE transform, let's do this, so
    ;; that the common case isn't ludicrously expensive.
    (etypecase result
      ((simple-array character (*))
       (replace result (string-output-stream-string stream)))
      (simple-base-string
       (replace result (string-output-stream-string stream)))
      ((simple-array nil (*))
       (replace result (string-output-stream-string stream))))
    (setf (string-output-stream-index stream) 0
          (string-output-stream-index-cache stream) 0)
    result))

;;; Dump the characters buffer up in IN-STREAM to OUT-STREAM as
;;; GET-OUTPUT-STREAM-STRING would return them.
(defun dump-output-stream-string (in-stream out-stream)
  (%write-string (string-output-stream-string in-stream)
                 out-stream
                 0
                 (string-output-stream-last-index in-stream))
  (setf (string-output-stream-index in-stream) 0
        (string-output-stream-index-cache in-stream) 0))

;;;; fill-pointer streams

;;; Fill pointer STRING-OUTPUT-STREAMs are not explicitly mentioned in
;;; the CLM, but they are required for the implementation of
;;; WITH-OUTPUT-TO-STRING.

;;; FIXME: need to support (VECTOR BASE-CHAR) and (VECTOR NIL),
;;; ideally without destroying all hope of efficiency.
(deftype string-with-fill-pointer ()
  '(and (vector character)
        (satisfies array-has-fill-pointer-p)))

(defstruct (fill-pointer-output-stream
            (:include ansi-stream
                      (out #'fill-pointer-ouch)
                      (sout #'fill-pointer-sout)
                      (misc #'fill-pointer-misc))
            (:constructor make-fill-pointer-output-stream (string))
            (:copier nil))
  ;; a string with a fill pointer where we stuff the stuff we write
  (string (missing-arg) :type string-with-fill-pointer :read-only t))

(defun fill-pointer-ouch (stream character)
  (let* ((buffer (fill-pointer-output-stream-string stream))
         (current (fill-pointer buffer))
         (current+1 (1+ current)))
    (declare (fixnum current))
    (with-array-data ((workspace buffer) (start) (end))
      (declare (type (simple-array character (*)) workspace))
      (let ((offset-current (+ start current)))
        (declare (fixnum offset-current))
        (if (= offset-current end)
            (let* ((new-length (1+ (* current 2)))
                   (new-workspace (make-string new-length)))
              (declare (type (simple-array character (*)) new-workspace))
              (replace new-workspace workspace
                       :start2 start :end2 offset-current)
              (setf workspace new-workspace
                    offset-current current)
              (set-array-header buffer workspace new-length
                                current+1 0 new-length nil))
            (setf (fill-pointer buffer) current+1))
        (setf (schar workspace offset-current) character)))
    current+1))

(defun fill-pointer-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (let* ((string (if (typep string '(simple-array character (*)))
                     string
                     (coerce string '(simple-array character (*)))))
         (buffer (fill-pointer-output-stream-string stream))
         (current (fill-pointer buffer))
         (string-len (- end start))
         (dst-end (+ string-len current)))
    (declare (fixnum current dst-end string-len))
    (with-array-data ((workspace buffer) (dst-start) (dst-length))
      (declare (type (simple-array character (*)) workspace))
      (let ((offset-dst-end (+ dst-start dst-end))
            (offset-current (+ dst-start current)))
        (declare (fixnum offset-dst-end offset-current))
        (if (> offset-dst-end dst-length)
            (let* ((new-length (+ (the fixnum (* current 2)) string-len))
                   (new-workspace (make-string new-length)))
              (declare (type (simple-array character (*)) new-workspace))
              (replace new-workspace workspace
                       :start2 dst-start :end2 offset-current)
              (setf workspace new-workspace
                    offset-current current
                    offset-dst-end dst-end)
              (set-array-header buffer workspace new-length
                                dst-end 0 new-length nil))
            (setf (fill-pointer buffer) dst-end))
        (replace workspace string
                 :start1 offset-current :start2 start :end2 end)))
    dst-end))

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
     (:element-type (array-element-type
                     (fill-pointer-output-stream-string stream)))))

;;;; indenting streams

(defstruct (indenting-stream (:include ansi-stream
                                       (out #'indenting-out)
                                       (sout #'indenting-sout)
                                       (misc #'indenting-misc))
                             (:constructor make-indenting-stream (stream))
                             (:copier nil))
  ;; the stream we're based on
  stream
  ;; how much we indent on each line
  (indentation 0))

#!+sb-doc
(setf (fdocumentation 'make-indenting-stream 'function)
 "Return an output stream which indents its output by some amount.")

;;; INDENTING-INDENT writes the correct number of spaces needed to indent
;;; output on the given STREAM based on the specified SUB-STREAM.
(defmacro indenting-indent (stream sub-stream)
  ;; KLUDGE: bare magic number 60
  `(do ((i 0 (+ i 60))
        (indentation (indenting-stream-indentation ,stream)))
       ((>= i indentation))
     (%write-string
      #.(make-string 60 :initial-element #\Space)
      ,sub-stream
      0
      (min 60 (- indentation i)))))

;;; INDENTING-OUT writes a character to an indenting stream.
(defun indenting-out (stream char)
  (let ((sub-stream (indenting-stream-stream stream)))
    (write-char char sub-stream)
    (if (char= char #\newline)
        (indenting-indent stream sub-stream))))

;;; INDENTING-SOUT writes a string to an indenting stream.
(defun indenting-sout (stream string start end)
  (declare (simple-string string) (fixnum start end))
  (do ((i start)
       (sub-stream (indenting-stream-stream stream)))
      ((= i end))
    (let ((newline (position #\newline string :start i :end end)))
      (cond (newline
             (%write-string string sub-stream i (1+ newline))
             (indenting-indent stream sub-stream)
             (setq i (+ newline 1)))
            (t
             (%write-string string sub-stream i end)
             (setq i end))))))

;;; INDENTING-MISC just treats just the :LINE-LENGTH message
;;; differently. INDENTING-CHARPOS says the charpos is the charpos of
;;; the base stream minus the stream's indentation.
(defun indenting-misc (stream operation &optional arg1 arg2)
  (let ((sub-stream (indenting-stream-stream stream)))
    (if (ansi-stream-p sub-stream)
        (let ((method (ansi-stream-misc sub-stream)))
          (case operation
            (:line-length
             (let ((line-length (funcall method sub-stream operation)))
               (if line-length
                   (- line-length (indenting-stream-indentation stream)))))
            (:charpos
             (let ((charpos (funcall method sub-stream operation)))
               (if charpos
                   (- charpos (indenting-stream-indentation stream)))))
            (t
             (funcall method sub-stream operation arg1 arg2))))
        ;; must be Gray streams FUNDAMENTAL-STREAM
        (case operation
          (:line-length
           (let ((line-length (stream-line-length sub-stream)))
             (if line-length
                 (- line-length (indenting-stream-indentation stream)))))
          (:charpos
           (let ((charpos (stream-line-column sub-stream)))
             (if charpos
                 (- charpos (indenting-stream-indentation stream)))))
          (t
           (stream-misc-dispatch sub-stream operation arg1 arg2))))))

(declaim (maybe-inline read-char unread-char read-byte listen))

;;;; case frobbing streams, used by FORMAT ~(...~)

(defstruct (case-frob-stream
            (:include ansi-stream
                      (misc #'case-frob-misc))
            (:constructor %make-case-frob-stream (target out sout))
            (:copier nil))
  (target (missing-arg) :type stream))

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

(declaim (inline compatible-vector-and-stream-element-types-p))
(defun compatible-vector-and-stream-element-types-p (vector stream)
  (declare (type vector vector)
           (type ansi-stream stream))
  (or (and (typep vector '(simple-array (unsigned-byte 8) (*)))
           (subtypep (stream-element-type stream) '(unsigned-byte 8)))
      (and (typep vector '(simple-array (signed-byte 8) (*)))
           (subtypep (stream-element-type stream) '(signed-byte 8)))))

(defun ansi-stream-read-sequence (seq stream start %end)
  (declare (type sequence seq)
           (type ansi-stream stream)
           (type index start)
           (type sequence-end %end)
           (values index))
  (let ((end (or %end (length seq))))
    (declare (type index end))
    (etypecase seq
      (list
       (let ((read-function
              (if (subtypep (stream-element-type stream) 'character)
                  #'ansi-stream-read-char
                  #'ansi-stream-read-byte)))
         (do ((rem (nthcdr start seq) (rest rem))
              (i start (1+ i)))
             ((or (endp rem) (>= i end)) i)
           (declare (type list rem)
                    (type index i))
           (let ((el (funcall read-function stream nil :eof nil)))
             (when (eq el :eof)
               (return i))
             (setf (first rem) el)))))
      (vector
       (with-array-data ((data seq) (offset-start start) (offset-end end))
         (if (compatible-vector-and-stream-element-types-p data stream)
             (let* ((numbytes (- end start))
                    (bytes-read (read-n-bytes stream data offset-start
                                              numbytes nil)))
               (if (< bytes-read numbytes)
                   (+ start bytes-read)
                   end))
             (let ((read-function
                    (if (subtypep (stream-element-type stream) 'character)
                        ;; If the stream-element-type is CHARACTER,
                        ;; this might be a bivalent stream. If the
                        ;; sequence is a specialized unsigned-byte
                        ;; vector, try to read use binary IO. It'll
                        ;; signal an error if stream is an pure
                        ;; character stream.
                        (if (subtypep (array-element-type data)
                                      'unsigned-byte)
                            #'ansi-stream-read-byte
                            #'ansi-stream-read-char)
                        #'ansi-stream-read-byte)))
               (do ((i offset-start (1+ i)))
                   ((>= i offset-end) end)
                 (declare (type index i))
                 (let ((el (funcall read-function stream nil :eof nil)))
                   (when (eq el :eof)
                     (return (+ start (- i offset-start))))
                   (setf (aref data i) el))))))))))

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

(defun ansi-stream-write-sequence (seq stream start %end)
  (declare (type sequence seq)
           (type ansi-stream stream)
           (type index start)
           (type sequence-end %end)
           (values sequence))
  (let ((end (or %end (length seq))))
    (declare (type index end))
    (etypecase seq
      (list
       (let ((write-function
              (if (subtypep (stream-element-type stream) 'character)
                  (ansi-stream-out stream)
                  (ansi-stream-bout stream))))
         (do ((rem (nthcdr start seq) (rest rem))
              (i start (1+ i)))
             ((or (endp rem) (>= i end)))
           (declare (type list rem)
                    (type index i))
           (funcall write-function stream (first rem)))))
      (string
       (%write-string seq stream start end))
      (vector
       (with-array-data ((data seq) (offset-start start) (offset-end end))
         (labels
             ((output-seq-in-loop ()
                (let ((write-function
                       (if (subtypep (stream-element-type stream) 'character)
                           (lambda (stream object)
                             ;; This might be a bivalent stream, so we need
                             ;; to dispatch on a per-element basis, rather
                             ;; than just based on the sequence or stream
                             ;; element types.
                             (if (characterp object)
                                 (funcall (ansi-stream-out stream)
                                          stream object)
                                 (funcall (ansi-stream-bout stream)
                                          stream object)))
                           (ansi-stream-bout stream))))
                  (do ((i offset-start (1+ i)))
                      ((>= i offset-end))
                    (declare (type index i))
                    (funcall write-function stream (aref data i))))))
           (if (and (fd-stream-p stream)
                    (compatible-vector-and-stream-element-types-p data stream))
               (output-raw-bytes stream data offset-start offset-end)
               (output-seq-in-loop)))))))
  seq)

;;;; etc.
