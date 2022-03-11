;;; -*- lisp -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;

;;; Sbcl port by Rudi Schlatte.

(in-package "SB-SIMPLE-STREAMS")

;;;
;;; **********************************************************************
;;;
;;; Strategy functions for base simple-stream classes

;;;; Helper functions

(defun refill-buffer (stream blocking)
  (declare (type blocking blocking))
  (with-stream-class (simple-stream stream)
    (let* ((unread (sm last-char-read-size stream))
           (buffer (sm buffer stream))
           (bufptr (sm buffer-ptr stream)))
      (unless (or (zerop unread) (zerop bufptr))
        (buffer-copy buffer (- bufptr unread) buffer 0 unread))
      (let ((bytes (device-read stream nil unread nil blocking)))
        (declare (type fixnum bytes))
        (setf (sm buffpos stream) unread
              (sm buffer-ptr stream) (if (plusp bytes)
                                         (+ bytes unread)
                                         unread))
        bytes))))

(defun sc-set-dirty (stream)
  (with-stream-class (single-channel-simple-stream stream)
    (setf (sm mode stream)
          (if (<= (sm buffpos stream)
                  (sm buffer-ptr stream))
              3    ; read-modify
              1    ; write
              ))))

(defun sc-set-clean (stream)
  (with-stream-class (single-channel-simple-stream stream)
    (setf (sm mode stream) 0)))

(defun sc-dirty-p (stream)
  (with-stream-class (single-channel-simple-stream stream)
    (> (sm mode stream) 0)))

(defun flush-buffer (stream blocking)
  (with-stream-class (single-channel-simple-stream stream)
    (let ((ptr 0)
          (bytes (sm buffpos stream)))
      (declare (type fixnum ptr bytes))
      (when (and (> (sm mode stream) 0) (> (sm buffer-ptr stream) 0))
        ;; The data read in from the file could have been changed if
        ;; the stream is opened in read-write mode -- write back
        ;; everything in the buffer at the correct position just in
        ;; case.
        (setf (device-file-position stream)
              (- (device-file-position stream) (sm buffer-ptr stream))))
      (loop
        (when (>= ptr bytes) (setf (sm buffpos stream) 0) (setf (sm mode stream) 0) (return 0))
        (let ((bytes-written (device-write stream nil ptr nil blocking)))
          (declare (fixnum bytes-written))
          (when (minusp bytes-written)
            (error "DEVICE-WRITE error."))
          (incf ptr bytes-written))))))

(defun flush-out-buffer (stream blocking)
  (with-stream-class (dual-channel-simple-stream stream)
    (let ((ptr 0)
          (bytes (sm outpos stream)))
      (declare (type fixnum ptr bytes))
      (loop
        (when (>= ptr bytes) (setf (sm outpos stream) 0) (return 0))
        (let ((bytes-written (device-write stream nil ptr nil blocking)))
          (declare (fixnum bytes-written))
          (when (minusp bytes-written)
            (error "DEVICE-WRITE error."))
          (incf ptr bytes-written))))))

(defun read-byte-internal (stream eof-error-p eof-value blocking)
  (with-stream-class (simple-stream stream)
    (let ((ptr (sm buffpos stream)))
      (when (>= ptr (sm buffer-ptr stream))
        (let ((bytes (device-read stream nil 0 nil blocking)))
          (declare (type fixnum bytes))
          (if (plusp bytes)
              (setf (sm buffer-ptr stream) bytes
                    ptr 0)
              (return-from read-byte-internal
                (sb-impl::eof-or-lose stream eof-error-p eof-value)))))
      (setf (sm buffpos stream) (1+ ptr))
      (setf (sm last-char-read-size stream) 0)
      (setf (sm charpos stream) nil)
      (bref (sm buffer stream) ptr))))

;;;; Single-Channel-Simple-Stream strategy functions


(declaim (ftype j-listen-fn sc-listen-ef))
(defun sc-listen-ef (stream)
  (with-stream-class (simple-stream stream)
    (let ((lcrs (sm last-char-read-size stream))
          (buffer (sm buffer stream))
          (buffpos (sm buffpos stream))
          (cnt 0)
          (char nil))
      (declare (ignorable cnt))
      (unwind-protect
           (flet ((input ()
                    (when (>= buffpos (sm buffer-ptr stream))
                      (let ((bytes (refill-buffer stream nil)))
                        (cond ((= bytes 0)
                               (return-from sc-listen-ef nil))
                              ((< bytes 0)
                               (return-from sc-listen-ef t))
                              (t
                               (setf buffpos (sm buffpos stream))))))
                    (incf (sm last-char-read-size stream))
                    (prog1 (bref buffer buffpos)
                      (incf buffpos)))
                  (unput (n)
                    (decf buffpos n)))
             (setq char (octets-to-char (sm external-format stream)
                                        (sm oc-state stream)
                                        cnt #'input #'unput))
             (characterp char))
        (setf (sm last-char-read-size stream) lcrs)))))

(declaim (ftype j-read-char-fn sc-read-char-ef))
(defun sc-read-char-ef (stream eof-error-p eof-value blocking)
  #|(declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))|#
  (with-stream-class (simple-stream stream)
    (let* ((buffer (sm buffer stream))
           (buffpos (sm buffpos stream))
           (ctrl (sm control-in stream))
           (ef (sm external-format stream))
           (state (sm oc-state stream)))
      (flet ((input ()
               (when (>= buffpos (sm buffer-ptr stream))
                 (when (and (not (any-stream-instance-flags stream :dual :string))
                            (sc-dirty-p stream))
                   (flush-buffer stream t))
                 (let ((bytes (refill-buffer stream blocking)))
                   (cond ((= bytes 0)
                          (return-from sc-read-char-ef nil))
                         ((minusp bytes)
                          (return-from sc-read-char-ef
                            (sb-impl::eof-or-lose stream eof-error-p eof-value)))
                         (t
                          (setf buffpos (sm buffpos stream))))))
               (incf (sm last-char-read-size stream))
               (prog1 (bref buffer buffpos)
                 (incf buffpos)))
             (unput (n)
               (decf buffpos n)))
        (let* ((cnt 0)
               (char (octets-to-char ef state cnt #'input #'unput))
               (code (char-code char)))
          (setf (sm buffpos stream) buffpos
                (sm last-char-read-size stream) cnt
                (sm oc-state stream) state)
          (when (and (< code 32) ctrl (svref ctrl code))
            (setq char (funcall (the (or symbol function) (svref ctrl code))
                                stream char)))
          (if (null char)
              (sb-impl::eof-or-lose stream eof-error-p eof-value)
              char))))))


(declaim (ftype j-read-char-fn sc-read-char-ef-mapped))
(defun sc-read-char-ef-mapped (stream eof-error-p eof-value blocking)
  #|(declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))|#
  (declare (ignore blocking))
  (with-stream-class (simple-stream stream)
    (let* ((buffer (sm buffer stream))
           (buffpos (sm buffpos stream))
           (ctrl (sm control-in stream))
           (ef (sm external-format stream))
           (state (sm oc-state stream)))
      (flet ((input ()
               (when (>= buffpos (sm buffer-ptr stream))
                 (return-from sc-read-char-ef-mapped
                   (sb-impl::eof-or-lose stream eof-error-p eof-value)))
               (incf (sm last-char-read-size stream))
               (prog1 (bref buffer buffpos)
                 (incf buffpos)))
             (unput (n)
               (decf buffpos n)))
        (let* ((cnt 0)
               (char (octets-to-char ef state cnt #'input #'unput))
               (code (char-code char)))
          (setf (sm buffpos stream) buffpos
                (sm last-char-read-size stream) cnt
                (sm oc-state stream) state)
          (when (and (< code 32) ctrl (svref ctrl code))
            (setq char (funcall (the (or symbol function) (svref ctrl code))
                                stream char)))
          (if (null char)
              (sb-impl::eof-or-lose stream eof-error-p eof-value)
              char))))))


(declaim (ftype j-read-chars-fn sc-read-chars-ef))
(defun sc-read-chars-ef (stream string search start end blocking)
  ;; string is filled from START to END, or until SEARCH is found
  ;; Return two values: count of chars read and
  ;;  NIL if SEARCH was not found
  ;;  T if SEARCH was found
  ;;  :EOF if eof encountered before end
  (declare (type simple-stream stream)
           (type string string)
           (type (or null character) search)
           (type fixnum start end)
           (type boolean blocking)
           #|(optimize (speed 3) (space 2) (safety 0) (debug 0))|#)
  (with-stream-class (simple-stream stream)
    (when (and (not (any-stream-instance-flags stream :dual :string))
               (sc-dirty-p stream))
      (flush-buffer stream t))
    (do ((buffer (sm buffer stream))
         (buffpos (sm buffpos stream))
         (buffer-ptr (sm buffer-ptr stream))
         (lcrs 0)
         (ctrl (sm control-in stream))
         (ef (sm external-format stream))
         (state (sm oc-state stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((>= posn end)
         (setf (sm buffpos stream) buffpos
               (sm last-char-read-size stream) lcrs
               (sm oc-state stream) state)
         (values count nil))
      (declare (type sb-int:index buffpos buffer-ptr posn count))
      (flet ((input ()
               (when (>= buffpos buffer-ptr)
                 (setf (sm last-char-read-size stream) lcrs)
                 (let ((bytes (refill-buffer stream blocking)))
                   (declare (type fixnum bytes))
                   (setf buffpos (sm buffpos stream)
                         buffer-ptr (sm buffer-ptr stream))
                   (unless (plusp bytes)
                     (setf (sm buffpos stream) buffpos
                           (sm last-char-read-size stream) lcrs
                           (sm oc-state stream) state)
                     (if (zerop bytes)
                         (return (values count nil))
                         (return (values count :eof))))))
               (prog1 (bref buffer buffpos)
                 (incf buffpos)
                 (incf lcrs)))
             (unput (n)
               (decf buffpos n)))
        (let* ((cnt 0)
               (char (octets-to-char ef state cnt #'input #'unput))
               (code (char-code char)))
          (setq lcrs cnt)
          (when (and (< code 32) ctrl (svref ctrl code))
            (setq char (funcall (the (or symbol function) (svref ctrl code))
                                stream char)))
          (cond ((null char)
                 (setf (sm buffpos stream) buffpos
                       (sm last-char-read-size stream) lcrs
                       (sm oc-state stream) state)
                 (return (values count :eof)))
                ((and search (char= char search))
                 (setf (sm buffpos stream) buffpos
                       (sm last-char-read-size stream) lcrs
                       (sm oc-state stream) state)
                 (return (values count t)))
                (t
                 (setf (char string posn) char))))))))


(declaim (ftype j-read-chars-fn sc-read-chars-ef-mapped))
(defun sc-read-chars-ef-mapped (stream string search start end blocking)
  ;; string is filled from START to END, or until SEARCH is found
  ;; Return two values: count of chars read and
  ;;  NIL if SEARCH was not found
  ;;  T if SEARCH was found
  ;;  :EOF if eof encountered before end
  (declare (type simple-stream stream)
           (type string string)
           (type (or null character) search)
           (type fixnum start end)
           (type boolean blocking)
           (ignore blocking)
           #|(optimize (speed 3) (space 2) (safety 0) (debug 0))|#)
  (with-stream-class (simple-stream stream)
    ;; if stream is single-channel and mode == 3, flush buffer (if dirty)
    (do ((buffer (sm buffer stream))
         (buffpos (sm buffpos stream))
         (buffer-ptr (sm buffer-ptr stream))
         (lcrs 0)
         (ctrl (sm control-in stream))
         (ef (sm external-format stream))
         (state (sm oc-state stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((>= posn end)
         (setf (sm buffpos stream) buffpos
               (sm last-char-read-size stream) lcrs
               (sm oc-state stream) state)
         (values count nil))
      (declare (type sb-int:index buffpos buffer-ptr posn count))
      (flet ((input ()
               (when (>= buffpos buffer-ptr)
                 (return (values count :eof)))
               (prog1 (bref buffer buffpos)
                 (incf buffpos)
                 (incf lcrs)))
             (unput (n)
               (decf buffpos n)))
        (let* ((cnt 0)
               (char (octets-to-char ef state cnt #'input #'unput))
               (code (char-code char)))
          (setq lcrs cnt)
          (when (and (< code 32) ctrl (svref ctrl code))
            (setq char (funcall (the (or symbol function) (svref ctrl code))
                                stream char)))
          (cond ((null char)
                 (setf (sm buffpos stream) buffpos
                       (sm last-char-read-size stream) lcrs
                       (sm oc-state stream) state)
                 (return (values count :eof)))
                ((and search (char= char search))
                 (setf (sm buffpos stream) buffpos
                       (sm last-char-read-size stream) lcrs
                       (sm oc-state stream) state)
                 (return (values count t)))
                (t
                 (setf (char string posn) char))))))))


(declaim (ftype j-unread-char-fn sc-unread-char-ef))
(defun sc-unread-char-ef (stream relaxed)
  (declare (ignore relaxed))
  (with-stream-class (simple-stream stream)
    (let ((unread (sm last-char-read-size stream)))
      (if (>= (sm buffpos stream) unread)
          (decf (sm buffpos stream) unread)
          (error "This shouldn't happen.")))))

(declaim (ftype j-write-char-fn sc-write-char-ef))
(defun sc-write-char-ef (character stream)
  (when character
    (with-stream-class (single-channel-simple-stream stream)
      (let ((buffer (sm buffer stream))
            (buffpos (sm buffpos stream))
            (buf-len (sm buf-len stream))
            (code (char-code character))
            (ctrl (sm control-out stream)))
        (when (and (< code 32) ctrl (svref ctrl code)
                   (funcall (the (or symbol function) (svref ctrl code))
                            stream character))
          (return-from sc-write-char-ef character))
        (flet ((output (byte)
                 (when (>= buffpos buf-len)
                   (setf (sm buffpos stream) buffpos)
                   (setq buffpos (flush-buffer stream t)))
                 (setf (bref buffer buffpos) byte)
                 (incf buffpos)))
          (char-to-octets (sm external-format stream) character
                          (sm co-state stream) #'output))
        (setf (sm buffpos stream) buffpos)
        (sc-set-dirty stream)
        (if (sm charpos stream) (incf (sm charpos stream))))))
  character)

(declaim (ftype j-write-chars-fn sc-write-chars-ef))
(defun sc-write-chars-ef (string stream start end)
  (with-stream-class (single-channel-simple-stream stream)
    (do ((buffer (sm buffer stream))
         (buffpos (sm buffpos stream))
         (buf-len (sm buf-len stream))
         (ef (sm external-format stream))
         (ctrl (sm control-out stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((>= posn end) (setf (sm buffpos stream) buffpos) count)
      (declare (type fixnum buffpos buf-len posn count))
      (let* ((char (char string posn))
             (code (char-code char)))
        (unless (and (< code 32) ctrl (svref ctrl code)
                     (funcall (the (or symbol function) (svref ctrl code))
                              stream char))
          (flet ((output (byte)
                   (when (>= buffpos buf-len)
                     (setf (sm buffpos stream) buffpos)
                     (setq buffpos (flush-buffer stream t)))
                   (setf (bref buffer buffpos) byte)
                   (incf buffpos)))
            (char-to-octets ef char (sm co-state stream) #'output))
          (setf (sm buffpos stream) buffpos)
          (if (sm charpos stream) (incf (sm charpos stream)))
          (sc-set-dirty stream))))))


;;;; Dual-Channel-Simple-Stream strategy functions

;; single-channel read-side functions work for dual-channel streams too

(declaim (ftype j-write-char-fn dc-write-char-ef))
(defun dc-write-char-ef (character stream)
  (when character
    (with-stream-class (dual-channel-simple-stream stream)
      (let ((out-buffer (sm out-buffer stream))
            (outpos (sm outpos stream))
            (max-out-pos (sm max-out-pos stream))
            (code (char-code character))
            (ctrl (sm control-out stream)))
        (when (and (< code 32) ctrl (svref ctrl code)
                   (funcall (the (or symbol function) (svref ctrl code))
                            stream character))
          (return-from dc-write-char-ef character))
        (flet ((output (byte)
                 (when (>= outpos max-out-pos)
                   (setf (sm outpos stream) outpos)
                   (setq outpos (flush-out-buffer stream t)))
                 (setf (bref out-buffer outpos) byte)
                 (incf outpos)))
          (char-to-octets (sm external-format stream) character
                          (sm co-state stream) #'output))
        (setf (sm outpos stream) outpos)
        (if (sm charpos stream) (incf (sm charpos stream))))))
  character)


(declaim (ftype j-write-chars-fn dc-write-chars-ef))
(defun dc-write-chars-ef (string stream start end)
  (with-stream-class (dual-channel-simple-stream stream)
    (do ((buffer (sm out-buffer stream))
         (outpos (sm outpos stream))
         (max-out-pos (sm max-out-pos stream))
         (ef (sm external-format stream))
         (ctrl (sm control-out stream))
         (posn start (1+ posn))
         (count 0 (1+ count)))
        ((>= posn end) (setf (sm outpos stream) outpos) count)
      (declare (type fixnum outpos max-out-pos posn count))
      (let* ((char (char string posn))
             (code (char-code char)))
        (unless (and (< code 32) ctrl (svref ctrl code)
                     (funcall (the (or symbol function) (svref ctrl code))
                              stream char))
          (flet ((output (byte)
                   (when (>= outpos max-out-pos)
                     (setf (sm outpos stream) outpos)
                     (setq outpos (flush-out-buffer stream t)))
                   (setf (bref buffer outpos) byte)
                   (incf outpos)))
            (char-to-octets ef char (sm co-state stream) #'output))
          (setf (sm outpos stream) outpos)
          (if (sm charpos stream) (incf (sm charpos stream))))))))

;;;; String-Simple-Stream strategy functions

(declaim (ftype j-read-char-fn str-read-char))
(defun str-read-char (stream eof-error-p eof-value blocking)
  (declare (type string-input-simple-stream stream) (ignore blocking)
           #|(optimize (speed 3) (space 2) (safety 0) (debug 0))|#
           )
  (with-stream-class (string-input-simple-stream stream)
    (when (any-stream-instance-flags stream :eof)
      (sb-impl::eof-or-lose stream eof-error-p eof-value))
    (let* ((ptr (sm buffpos stream))
           (char (if (< ptr (sm buffer-ptr stream))
                     (schar (sm buffer stream) ptr)
                     nil)))
      (if (null char)
          (sb-impl::eof-or-lose stream eof-error-p eof-value)
          (progn
            (setf (sm last-char-read-size stream) 1)
            ;; do string-streams do control-in processing?
            #|(let ((column (sm charpos stream)))
              (declare (type (or null fixnum) column))
              (when column
                (setf (sm charpos stream) (1+ column))))
            |#
            char)))))

(declaim (ftype j-listen-fn str-listen-e-crlf))
(defun str-listen-e-crlf (stream)
  (with-stream-class (composing-stream stream)
    ;; if this says there's a character available, it may be #\Return,
    ;; in which case read-char will only return if there's a following
    ;; #\Linefeed, so this really has to read the char...
    ;; but without precluding the later unread-char of a character which
    ;; has already been read.
    (funcall-stm-handler j-listen (sm melded-stream stream))))

(declaim (ftype j-read-char-fn str-read-char-e-crlf))
(defun str-read-char-e-crlf (stream eof-error-p eof-value blocking)
  (with-stream-class (composing-stream stream)
    (let* ((encap (sm melded-stream stream))
           (ctrl (sm control-in stream))
           (char (funcall-stm-handler j-read-char encap nil stream blocking)))
      ;; if CHAR is STREAM, we hit EOF; if NIL, blocking is NIL and no
      ;; character was available...
      (when (eql char #\Return)
        (let ((next (funcall-stm-handler j-read-char encap nil stream blocking)))
          ;; if NEXT is STREAM, we hit EOF, so we should just return the
          ;; #\Return (and mark the stream :EOF?  At least unread if we
          ;; got a soft EOF, from a terminal, etc.
          ;; if NEXT is NIL, blocking is NIL and there's a CR but no
          ;; LF available on the stream: have to unread the CR and
          ;; return NIL, letting the CR be reread later.
          ;;
          ;; If we did get a linefeed, adjust the last-char-read-size
          ;; so that an unread of the resulting newline will unread both
          ;; the linefeed _and_ the carriage return.
          (if (eql next #\Linefeed)
              (setq char #\Newline)
              (funcall-stm-handler j-unread-char encap nil))))
      (when (characterp char)
        (let ((code (char-code char)))
          (when (and (< code 32) ctrl (svref ctrl code))
            (setq char (funcall (the (or symbol function) (svref ctrl code))
                                stream char)))))
      (if (eq char stream)
          (sb-impl::eof-or-lose stream eof-error-p eof-value)
          char))))

(declaim (ftype j-unread-char-fn str-unread-char-e-crlf))
(defun str-unread-char-e-crlf (stream relaxed)
  (declare (ignore relaxed))
  (with-stream-class (composing-stream stream)
    (funcall-stm-handler j-unread-char (sm melded-stream stream) nil)))


;;; Functions to install the strategy functions in the appropriate slots

(defun melding-stream (stream)
  (with-stream-class (simple-stream)
    (do ((stm stream (sm melded-stream stm)))
        ((eq (sm melded-stream stm) stream) stm))))

(defun meld (stream encap)
  (with-stream-class (simple-stream)
    (setf (sm melding-base encap) (sm melding-base stream))
    (setf (sm melded-stream encap) (sm melded-stream stream))
    (setf (sm melded-stream stream) encap)
    (rotatef (sm j-listen encap) (sm j-listen stream))
    (rotatef (sm j-read-char encap) (sm j-read-char stream))
    (rotatef (sm j-read-chars encap) (sm j-read-chars stream))
    (rotatef (sm j-unread-char encap) (sm j-unread-char stream))
    (rotatef (sm j-write-char encap) (sm j-write-char stream))
    (rotatef (sm j-write-chars encap) (sm j-write-chars stream))))

(defun unmeld (stream)
  (with-stream-class (simple-stream)
    (let ((encap (sm melded-stream stream)))
      (unless (eq encap (sm melding-base stream))
        (setf (sm melding-base encap) encap)
        (setf (sm melded-stream stream) (sm melded-stream encap))
        (setf (sm melded-stream encap) encap)
        (rotatef (sm j-listen stream) (sm j-listen encap))
        (rotatef (sm j-read-char encap) (sm j-read-char stream))
        (rotatef (sm j-read-chars stream) (sm j-read-chars encap))
        (rotatef (sm j-unread-char stream) (sm j-unread-char encap))
        (rotatef (sm j-write-char stream) (sm j-write-char encap))
        (rotatef (sm j-write-chars stream) (sm j-write-chars encap))))))

;;; In cmucl, this is done with define-function-name-syntax (lists as
;;; function names), we make do with symbol frobbing.
(defun %sf (kind name format &optional access)
  (flet ((find-strategy-function (&rest args)
           (let ((name
                  (find-symbol (format nil "~{~A~^-~}" (mapcar #'string args))
                               #.*package*)))
             (if (fboundp name) (fdefinition name) nil))))
    (or (find-strategy-function kind name format access)
        (find-strategy-function kind name format)
        (find-strategy-function kind name :ef access)
        (find-strategy-function kind name :ef))))


(defun install-single-channel-character-strategy (stream external-format
                                                         access)
  (let ((format (find-external-format external-format)))
    ;; ACCESS is usually NIL
    ;; May be "undocumented" values: stream::buffer, stream::mapped
    ;;   to install strategies suitable for direct buffer streams
    ;;   (i.e., ones that call DEVICE-EXTEND instead of DEVICE-READ)
    ;; (Avoids checking "mode" flags by installing special strategy)
    (with-stream-class (simple-stream stream)
      (setf (sm j-listen stream)
          (%sf 'sc 'listen (ef-name format) access)
            (sm j-read-char stream)
          (%sf 'sc 'read-char (ef-name format) access)
            (sm j-read-chars stream)
          (%sf 'sc 'read-chars (ef-name format) access)
            (sm j-unread-char stream)
          (%sf 'sc 'unread-char (ef-name format) access)
            (sm j-write-char stream)
          (%sf 'sc 'write-char (ef-name format) access)
            (sm j-write-chars stream)
          (%sf 'sc 'write-chars (ef-name format) access))))
  stream)

(defun install-dual-channel-character-strategy (stream external-format)
  (let ((format (find-external-format external-format)))
    (with-stream-class (simple-stream stream)
      (setf (sm j-listen stream)
          (%sf 'sc 'listen (ef-name format))
            (sm j-read-char stream)
          (%sf 'sc 'read-char (ef-name format))
            (sm j-read-chars stream)
          (%sf 'sc 'read-chars (ef-name format))
            (sm j-unread-char stream)
          (%sf 'sc 'unread-char (ef-name format))
            (sm j-write-char stream)
          (%sf 'dc 'write-char (ef-name format))
            (sm j-write-chars stream)
          (%sf 'dc 'write-chars (ef-name format)))))
  stream)

(defun install-string-character-strategy (stream)
  (when (any-stream-instance-flags stream :input)
    (install-string-input-character-strategy stream))
  (when (any-stream-instance-flags stream :output)
    (install-string-output-character-strategy stream))
  stream)

(declaim (sb-ext:deprecated
          :early ("SBCL" "1.2.15")
          (function install-string-character-strategy
                    :replacement (install-string-input-character-strategy
                                  install-string-output-character-strategy))))

(defun install-string-input-character-strategy (stream)
  #| implement me |#
  (with-stream-class (simple-stream stream)
    (setf (sm j-read-char stream) #'str-read-char))
  stream)

(defun install-string-output-character-strategy (stream)
  #| implement me |#
  stream)

(defun install-composing-format-character-strategy (stream composing-format)
  (let ((format composing-format))
    (with-stream-class (simple-stream stream)
      (case format
        (:e-crlf (setf (sm j-read-char stream) #'str-read-char-e-crlf
                       (sm j-unread-char stream) #'str-unread-char-e-crlf))))
    #| implement me |#)
  stream)

(defun compose-encapsulating-streams (stream external-format)
  (when (consp external-format)
    (with-stream-class (simple-stream)
      (let ((encap (if (eq (sm melded-stream stream) stream)
                       nil
                       (sm melded-stream stream))))
        (when (null encap)
          (setq encap (make-instance 'composing-stream))
          (meld stream encap))
        (setf (stream-external-format encap) (car (last external-format)))
        (setf (sm external-format stream) external-format)
        (install-composing-format-character-strategy stream
                                                     (butlast external-format))
        ))))

(defmethod (setf stream-external-format) (ef (stream simple-stream))
  (with-stream-class (simple-stream stream)
    (setf (sm external-format stream) (find-external-format ef)))
  ef)

;;;
;;; NULL STRATEGY FUNCTIONS
;;;

(declaim (ftype j-read-char-fn null-read-char))
(defun null-read-char (stream eof-error-p eof-value blocking)
  (declare (ignore blocking))
  (sb-impl::eof-or-lose stream eof-error-p eof-value))

(declaim (ftype j-read-chars-fn null-read-chars))
(defun null-read-chars (stream string search start end blocking)
  (declare (ignore stream string search start end blocking))
  (values 0 :eof))

(declaim (ftype j-unread-char-fn null-unread-char))
(defun null-unread-char (stream relaxed)
  (declare (ignore stream relaxed)))

(declaim (ftype j-write-char-fn null-write-char))
(defun null-write-char (character stream)
  (declare (ignore stream))
  character)

(declaim (ftype j-write-chars-fn null-write-chars))
(defun null-write-chars (string stream start end)
  (declare (ignore string stream))
  (- end start))

(declaim (ftype j-listen-fn null-listen))
(defun null-listen (stream)
  (declare (ignore stream))
  nil)
