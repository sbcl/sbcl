;;; -*- lisp -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

(in-package "SB-SIMPLE-STREAMS")



(defun sc-refill-buffer (stream blocking)
  (with-stream-class (single-channel-simple-stream stream)
    (when (any-stream-instance-flags stream :dirty)
      ;; FIXME: Implement flush-buffer failure protocol instead of
      ;; blocking here
      (sc-flush-buffer stream t))
    (let* ((unread (sm last-char-read-size stream))
           (buffer (sm buffer stream)))
      (unless (zerop unread)
        (buffer-copy buffer (- (sm buffer-ptr stream) unread) buffer 0 unread))
      (let ((bytes (device-read stream nil unread nil blocking)))
        (declare (type fixnum bytes))
        (setf (sm buffpos stream) unread
              (sm buffer-ptr stream) (if (plusp bytes)
                                         (+ bytes unread)
                                         unread))
        bytes))))


(defun sc-flush-buffer (stream blocking)
  (with-stream-class (single-channel-simple-stream stream)
    (let ((ptr 0)
	  (bytes (sm buffpos stream)))
      (declare (type fixnum ptr bytes))
      ;; Seek to the left before flushing buffer -- the user could
      ;; have set the file-position, and scribbled something in the
      ;; data that was read from the file.
      (when (> (sm buffer-ptr stream) 0)
        (setf (device-file-position stream)
              (- (device-file-position stream) (sm buffer-ptr stream))))
      (loop
	(when (>= ptr bytes)
          (setf (sm buffpos stream) 0
                (sm buffer-ptr stream) 0)
          (remove-stream-instance-flags stream :dirty)
          (return 0))
	(let ((bytes-written (device-write stream nil ptr nil blocking)))
	  (declare (fixnum bytes-written))
	  (when (minusp bytes-written)
	    (error "DEVICE-WRITE error."))
	  (incf ptr bytes-written))))))

(defun dc-refill-buffer (stream blocking)
  (with-stream-class (dual-channel-simple-stream stream)
    (let* ((unread (sm last-char-read-size stream))
           (buffer (sm buffer stream)))
      (unless (zerop unread)
        (buffer-copy buffer (- (sm buffer-ptr stream) unread) buffer 0 unread))
      (let ((bytes (device-read stream nil unread nil blocking)))
        (declare (type fixnum bytes))
        (setf (sm buffpos stream) unread
              (sm buffer-ptr stream) (if (plusp bytes)
                                         (+ bytes unread)
                                         unread))
        bytes))))

(defun dc-flush-buffer (stream blocking)
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

;;;
;;; SINGLE-CHANNEL STRATEGY FUNCTIONS
;;;

(declaim (ftype j-read-char-fn sc-read-char))
(defun sc-read-char (stream eof-error-p eof-value blocking)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (with-stream-class (single-channel-simple-stream stream)
    ;; if stream is open for read-write, may need to flush the buffer
    (let* ((buffer (sm buffer stream))
	   (ptr (sm buffpos stream))
	   (code (if (< ptr (sm buffer-ptr stream))
		     (progn
		       (setf (sm buffpos stream) (1+ ptr))
		       (bref buffer ptr))
		     (let ((bytes (sc-refill-buffer stream blocking)))
		       (declare (type fixnum bytes))
		       (unless (minusp bytes)
			 (let ((ptr (sm buffpos stream)))
			   (setf (sm buffpos stream) (1+ ptr))
			   (bref buffer ptr))))))
	   (char (if code (code-char code) nil))
	   (ctrl (sm control-in stream)))
      (when code
	(setf (sm last-char-read-size stream) 1)
	(when (and (< code 32) ctrl (svref ctrl code))
	  ;; Does this have to be a function, or can it be a symbol?
	  (setq char (funcall (the (or symbol function) (svref ctrl code))
			      stream char))))
      (if (null char)
	  (sb-impl::eof-or-lose stream eof-error-p eof-value)
	  char))))

(declaim (ftype j-read-char-fn sc-read-char--buffer))
(defun sc-read-char--buffer (stream eof-error-p eof-value blocking)
  (declare (ignore blocking)) ;; everything is already in the buffer
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (with-stream-class (single-channel-simple-stream stream)
    (let* ((buffer (sm buffer stream))
	   (ptr (sm buffpos stream))
	   (code (when (< ptr (sm buffer-ptr stream))
		   (setf (sm buffpos stream) (1+ ptr))
		   (bref buffer ptr)))
	   (char (if code (code-char code) nil))
	   (ctrl (sm control-in stream)))
      (when code
	(setf (sm last-char-read-size stream) 1)
	(when (and (< code 32) ctrl (svref ctrl code))
	  ;; Does this have to be a function, or can it be a symbol?
	  (setq char (funcall (the (or symbol function) (svref ctrl code))
			      stream char))))
      (if (null char)
	  (sb-impl::eof-or-lose stream eof-error-p eof-value)
	  char))))

(declaim (ftype j-read-chars-fn sc-read-chars))
(defun sc-read-chars (stream string search start end blocking)
  ;; string is filled from START to END, or until SEARCH is found
  ;; Return two values: count of chars read and
  ;;  NIL if SEARCH was not found
  ;;  T is SEARCH was found
  ;;  :EOF if eof encountered before end
  (declare (type simple-stream stream)
	   (type string string)
	   (type (or null character) search)
	   (type fixnum start end)
	   (type boolean blocking)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (with-stream-class (single-channel-simple-stream stream)
    (setf (sm last-char-read-size stream) 0)
    ;; FIXME: Should arrange for the last character to be unreadable
    (do ((buffer (sm buffer stream))
	 (ptr (sm buffpos stream))
	 (max (sm buffer-ptr stream))
	 (posn start (1+ posn))
	 (count 0 (1+ count)))
	((= posn end) (setf (sm buffpos stream) ptr) (values count nil))
      (declare (type fixnum ptr max posn count))
      (let* ((code (if (< ptr max)
		       (prog1
			   (bref buffer ptr)
			 (incf ptr))
		       (let ((bytes (sc-refill-buffer stream blocking)))
			 (declare (type fixnum bytes))
			 (setf ptr (sm buffpos stream)
			       max (sm buffer-ptr stream))
			 (when (plusp bytes)
			   (prog1
			       (bref buffer ptr)
			     (incf ptr))))))
	     (char (if code (code-char code) nil))
	     (ctrl (sm control-in stream)))
	(when (and code (< code 32) ctrl (svref ctrl code))
	  (setq char (funcall (the (or symbol function) (svref ctrl code))
			      stream char)))
	(cond ((null char)
	       (setf (sm buffpos stream) ptr)
	       (return (values count :eof)))
	      ((and search (char= char search))
	       (setf (sm buffpos stream) ptr)
	       (return (values count t)))
	      (t
	       (setf (char string posn) char)))))))

(declaim (ftype j-read-chars-fn sc-read-chars--buffer))
(defun sc-read-chars--buffer (stream string search start end blocking)
  (declare (type simple-stream stream)
	   (type string string)
	   (type (or null character) search)
	   (type fixnum start end)
	   (type boolean blocking)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (declare (ignore blocking))           ; everything is in the buffer
  (with-stream-class (single-channel-simple-stream stream)
    (do ((buffer (sm buffer stream))
	 (ptr (sm buffpos stream))
	 (max (sm buffer-ptr stream))
	 (posn start (1+ posn))
	 (count 0 (1+ count)))
	((= posn end)
	 (setf (sm buffpos stream) ptr)
	 (unless (zerop count) (setf (sm last-char-read-size stream) 1))
	 (values count nil))
      (declare (type fixnum ptr max posn count))
      (let* ((code (when (< ptr max)
		     (prog1
			 (bref buffer ptr)
		       (incf ptr))))
	     (char (if code (code-char code) nil))
	     (ctrl (sm control-in stream)))
	(when (and code (< code 32) ctrl (svref ctrl code))
	  (setq char (funcall (the (or symbol function) (svref ctrl code))
			      stream char)))
	(cond ((null char)
	       (setf (sm buffpos stream) ptr)
	       (unless (zerop count) (setf (sm last-char-read-size stream) 1))
	       (return (values count :eof)))
	      ((and search (char= char search))
	       (setf (sm buffpos stream) ptr)
	       ;; Unread of last char must unread the search character, too
	       ;; If no characters were read, just add the length of the
	       ;; search char to that of the previously read char.
	       (if (zerop count)
		   (incf (sm last-char-read-size stream))
		   (setf (sm last-char-read-size stream) 2))
	       (return (values count t)))
	      (t
	       (setf (char string posn) char)))))))

(declaim (ftype j-unread-char-fn sc-unread-char))
(defun sc-unread-char (stream relaxed)
  (declare (ignore relaxed))
  (with-stream-class (single-channel-simple-stream stream)
    (let ((unread (sm last-char-read-size stream)))
      (if (>= (sm buffpos stream) unread)
	  (decf (sm buffpos stream) unread)
	  (error "Unreading needs work"))
      (setf (sm last-char-read-size stream) 0))))

(declaim (ftype j-write-char-fn sc-write-char))
(defun sc-write-char (character stream)
  (with-stream-class (single-channel-simple-stream stream)
    (let* ((buffer (sm buffer stream))
	   (ptr (sm buffpos stream))
	   (code (char-code character))
	   (ctrl (sm control-out stream)))
      (when (and (< code 32) ctrl (svref ctrl code)
		 (funcall (the (or symbol function) (svref ctrl code))
			  stream character))
	(return-from sc-write-char character))
      (when (>= ptr (sm buf-len stream))
        (setf ptr (sc-flush-buffer stream t)))
      (setf (bref buffer ptr) code)
      (setf (sm buffpos stream) (1+ ptr))
      (add-stream-instance-flags stream :dirty)))
  character)

(declaim (ftype j-write-chars-fn sc-write-chars))
(defun sc-write-chars (string stream start end)
  (with-stream-class (single-channel-simple-stream stream)
    (do ((buffer (sm buffer stream))
	 (ptr (sm buffpos stream))
	 (max (sm buf-len stream))
	 (ctrl (sm control-out stream))
	 (posn start (1+ posn))
	 (count 0 (1+ count)))
	((>= posn end)
         (setf (sm buffpos stream) ptr)
         (add-stream-instance-flags stream :dirty)
         count)
      (declare (type fixnum ptr max posn count))
      (let* ((char (char string posn))
	     (code (char-code char)))
        ;; FIXME: Can functions in the control-out table side-effect
        ;; the stream?  Section 9.0 prohibits this only for control-in
        ;; functions.  If they can, update (sm buffpos stream) here,
        ;; like around the call to sc-flush-buffer below
	(unless (and (< code 32) ctrl (svref ctrl code)
		     (funcall (the (or symbol function) (svref ctrl code))
			      stream char))
	  (unless (< ptr max)
            ;; need to update buffpos before control leaves this
            ;; function in any way
            (setf (sm buffpos stream) ptr)
            (sc-flush-buffer stream t)
            (setf ptr (sm buffpos stream)))
          (setf (bref buffer ptr) code)
          (incf ptr))))))

(declaim (ftype j-listen-fn sc-listen))
(defun sc-listen (stream)
  (with-stream-class (single-channel-simple-stream stream)
    (or (< (sm buffpos stream) (sm buffer-ptr stream))
	(case (device-read stream nil 0 0 nil)
	  ((0 -2) nil)
	  (-1 #| latch EOF |# nil)
	  (-3 t)
	  (t (error "DEVICE-READ error."))))))

;;; SC-READ-BYTE doesn't actually live in a strategy slot
(defun sc-read-byte (stream eof-error-p eof-value blocking)
  (with-stream-class (single-channel-simple-stream stream)
    ;; @@1
    (let ((ptr (sm buffpos stream)))
      (when (>= ptr (sm buffer-ptr stream))
        (let ((bytes (device-read stream nil 0 nil blocking)))
          (declare (type fixnum bytes))
          (if (plusp bytes)
              (setf (sm buffer-ptr stream) bytes
                    ptr 0)
              (return-from sc-read-byte
                (sb-impl::eof-or-lose stream eof-error-p eof-value)))))
      (setf (sm buffpos stream) (1+ ptr))
      (setf (sm last-char-read-size stream) 0)
      (bref (sm buffer stream) ptr))))

;;;
;;; DUAL-CHANNEL STRATEGY FUNCTIONS
;;;

(declaim (ftype j-read-char-fn dc-read-char))
(defun dc-read-char (stream eof-error-p eof-value blocking)
  ;;(declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (with-stream-class (dual-channel-simple-stream stream)
    ;; if interactive flag is set, finish-output first
    (let* ((buffer (sm buffer stream))
	   (ptr (sm buffpos stream))
	   (code (if (< ptr (sm buffer-ptr stream))
		     (progn
		       (setf (sm buffpos stream) (1+ ptr))
		       (bref buffer ptr))
		     (let ((bytes (dc-refill-buffer stream blocking)))
		       (declare (type fixnum bytes))
		       (unless (minusp bytes)
			 (let ((ptr (sm buffpos stream)))
			   (setf (sm buffpos stream) (1+ ptr))
			   (bref buffer ptr))))))
	   (char (if code (code-char code) nil))
	   (ctrl (sm control-in stream)))
      (when code
	(setf (sm last-char-read-size stream) 1)
	(when (and (< code 32) ctrl (svref ctrl code))
	  ;; Does this have to be a function, or can it be a symbol?
	  (setq char (funcall (the (or symbol function) (svref ctrl code))
			      stream char)))
	#|(let ((column (sm charpos stream)))
	  (declare (type (or null fixnum) column))
	  (when column
	    (setf (sm charpos stream) (1+ column))))|#)
      (if (null char)
	  (sb-impl::eof-or-lose stream eof-error-p eof-value)
	  char))))

(declaim (ftype j-read-chars-fn dc-read-chars))
(defun dc-read-chars (stream string search start end blocking)
  (declare (type dual-channel-simple-stream stream)
	   (type string string)
	   (type (or null character) search)
	   (type fixnum start end)
	   (type boolean blocking)
	   #|(optimize (speed 3) (space 2) (safety 0) (debug 0))|#)
  (with-stream-class (dual-channel-simple-stream stream)
    ;; if interactive flag is set, finish-output first
    (setf (sm last-char-read-size stream) 0)
    ;; Should arrange for the last character to be unreadable
    (do ((buffer (sm buffer stream))
	 (ptr (sm buffpos stream))
	 (max (sm buffer-ptr stream))
	 (posn start (1+ posn))
	 (count 0 (1+ count)))
	((>= posn end) (setf (sm buffpos stream) ptr) (values count nil))
      (declare (type fixnum ptr max posn count))
      (let* ((code (if (< ptr max)
		       (prog1
			   (bref buffer ptr)
			 (incf ptr))
		       (let ((bytes (dc-refill-buffer stream blocking)))
			 (declare (type fixnum bytes))
			 (setf ptr (sm buffpos stream)
			       max (sm buffer-ptr stream))
			 (when (plusp bytes)
			   (prog1
			       (bref buffer ptr)
			     (incf ptr))))))
	     (char (if code (code-char code) nil))
	     (ctrl (sm control-in stream)))
	(when (and code (< code 32) ctrl (svref ctrl code))
	  (setq char (funcall (the (or symbol function) (svref ctrl code))
			      stream char)))
	#|(let ((column (sm charpos stream)))
	  (declare (type (or null fixnum) column))
	  (when column
	    (setf (sm charpos stream) (1+ column))))|#
	(cond ((null char)
	       (setf (sm buffpos stream) ptr)
	       (return (values count :eof)))
	      ((and search (char= char search))
	       (setf (sm buffpos stream) ptr)
	       (return (values count t)))
	      (t
	       (setf (char string posn) char)))))))

(declaim (ftype j-unread-char-fn dc-unread-char))
(defun dc-unread-char (stream relaxed)
  (declare (ignore relaxed))
  (with-stream-class (dual-channel-simple-stream stream)
    (let ((unread (sm last-char-read-size stream)))
      (if (>= (sm buffpos stream) unread)
	  (decf (sm buffpos stream) unread)
	  (error "Unreading needs work"))
      (setf (sm last-char-read-size stream) 0))))

(declaim (ftype j-write-char-fn dc-write-char))
(defun dc-write-char (character stream)
  (when character
    (with-stream-class (dual-channel-simple-stream stream)
      (let* ((buffer (sm out-buffer stream))
             (ptr (sm outpos stream))
             (code (char-code character))
             (ctrl (sm control-out stream)))
        (when (and (< code 32) ctrl (svref ctrl code)
                   (funcall (the (or symbol function) (svref ctrl code))
                            stream character))
          (return-from dc-write-char character))
        (when (>= ptr (sm max-out-pos stream))
          (setq ptr (dc-flush-buffer stream t)))
        (setf (bref buffer ptr) code)
        (setf (sm outpos stream) (1+ ptr)))))
  character)

(declaim (ftype j-write-chars-fn dc-write-chars))
(defun dc-write-chars (string stream start end)
  (with-stream-class (dual-channel-simple-stream stream)
    (do ((buffer (sm out-buffer stream))
	 (ptr (sm outpos stream))
	 (max (sm max-out-pos stream))
	 (ctrl (sm control-out stream))
	 (posn start (1+ posn))
	 (count 0 (1+ count)))
	((>= posn end) (setf (sm outpos stream) ptr) count)
      (declare (type fixnum ptr max posn count))
      (let* ((char (char string posn))
	     (code (char-code char)))
	(unless (and (< code 32) ctrl (svref ctrl code)
		     (funcall (the (or symbol function) (svref ctrl code))
			      stream char))
	  (unless (< ptr max)
            (setf (sm outpos stream) ptr)
            (dc-flush-buffer stream t)
            (setf ptr (sm outpos stream)))
          (setf (bref buffer ptr) code)
          (incf ptr))
        ))))

(declaim (ftype j-listen-fn dc-listen))
(defun dc-listen (stream)
  (with-stream-class (dual-channel-simple-stream stream)
    (or (< (sm buffpos stream) (sm buffer-ptr stream))
	(case (device-read stream nil 0 0 nil)
	  ((0 -2) nil)
	  (-1 #| latch EOF |# nil)
	  (-3 t)
	  (t (error "DEVICE-READ error."))))))

;;; DC-READ-BYTE doesn't actually live in a strategy slot
(defun dc-read-byte (stream eof-error-p eof-value blocking)
  (with-stream-class (dual-channel-simple-stream stream)
    (let ((ptr (sm buffpos stream)))
      (when (>= ptr (sm buffer-ptr stream))
        (let ((bytes (device-read stream nil 0 nil blocking)))
          (declare (type fixnum bytes))
          (if (plusp bytes)
              (setf (sm buffer-ptr stream) bytes
                    ptr 0)
              (return-from dc-read-byte
                (sb-impl::eof-or-lose stream eof-error-p eof-value)))))
      (setf (sm buffpos stream) (1+ ptr))
      (setf (sm last-char-read-size stream) 0)
      (bref (sm buffer stream) ptr))))

;;;
;;; STRING STRATEGY FUNCTIONS
;;;

(declaim (ftype j-read-char-fn string-read-char))
(defun string-read-char (stream eof-error-p eof-value blocking)
  (declare (type string-input-simple-stream stream) (ignore blocking)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
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
		(setf (sm charpos stream) (1+ column))))|#
	    char)))))


(declaim (ftype j-read-char-fn composing-crlf-read-char))
(defun composing-crlf-read-char (stream eof-error-p eof-value blocking)
  ;; TODO: what about the eof-error-p parameter?
  (declare (ignore eof-error-p eof-value))
  (with-stream-class (simple-stream stream)
    (let* ((melded-stream (sm melded-stream stream))
	   (char (funcall-stm-handler j-read-char melded-stream nil stream
				      blocking)))
      ;; if CHAR is STREAM, we hit EOF; if NIL, blocking is NIL and no
      ;; character was available...
      (when (eql char #\Return)
	(let ((next (funcall-stm-handler j-read-char melded-stream
					 nil stream blocking)))
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
	      (funcall-stm-handler j-unread-char melded-stream nil))))
      ;; do control-in processing on whatever character we've got
      char)))

(declaim (ftype j-unread-char-fn composing-crlf-unread-char))
(defun composing-crlf-unread-char (stream relaxed)
  (declare (ignore relaxed))
  (with-stream-class (simple-stream stream)
    (funcall-stm-handler j-unread-char (sm melded-stream stream) nil)))

;;;
;;; Functions to install the strategy functions in the appropriate slots
;;;

(defun %find-topmost-stream (stream)
  ;; N.B.: the topmost stream in the chain of encapsulations is actually
  ;; the bottommost in the "melding" chain
  (with-stream-class (simple-stream)
    (loop
      (when (eq (sm melded-stream stream) (sm melding-base stream))
	(return stream))
      (setq stream (sm melded-stream stream)))))

(defun install-single-channel-character-strategy (stream external-format
							 access)
  (find-external-format external-format)
  (let ((stream (%find-topmost-stream stream)))
    ;; ACCESS is usually NIL
    ;; May be "undocumented" values: stream::buffer, stream::mapped
    ;;   to install strategies suitable for direct buffer streams
    ;;   (i.e., ones that call DEVICE-EXTEND instead of DEVICE-READ)
    ;; (Avoids checking "mode" flags by installing special strategy)
    (with-stream-class (single-channel-simple-stream stream)
      (if (or (eq access 'buffer) (eq access 'mapped))
          (setf (sm j-read-char stream) #'sc-read-char--buffer
                (sm j-read-chars stream) #'sc-read-chars--buffer
                (sm j-unread-char stream) #'sc-unread-char
                (sm j-write-char stream) #'sc-write-char
                (sm j-write-chars stream) #'sc-write-chars
                (sm j-listen stream) #'sc-listen)
          (setf (sm j-read-char stream) #'sc-read-char
                (sm j-read-chars stream) #'sc-read-chars
                (sm j-unread-char stream) #'sc-unread-char
                (sm j-write-char stream) #'sc-write-char
                (sm j-write-chars stream) #'sc-write-chars
                (sm j-listen stream) #'sc-listen))))
  stream)

(defun install-dual-channel-character-strategy (stream external-format)
  (find-external-format external-format)
  (let ((stream (%find-topmost-stream stream)))
    (with-stream-class (dual-channel-simple-stream stream)
      (setf (sm j-read-char stream) #'dc-read-char
            (sm j-read-chars stream) #'dc-read-chars
            (sm j-unread-char stream) #'dc-unread-char
            (sm j-write-char stream) #'dc-write-char
            (sm j-write-chars stream) #'dc-write-chars
            (sm j-listen stream) #'dc-listen)))
  stream)

(defun install-string-input-character-strategy (stream)
  #| implement me |#
  (let ((stream (%find-topmost-stream stream)))
    (with-stream-class (simple-stream stream)
      (setf (sm j-read-char stream) #'string-read-char)))
  stream)

(defun install-string-output-character-strategy (stream)
  #| implement me |#
  stream)

(defun compose-encapsulating-streams (stream external-format)
  (when (consp external-format)
    (with-stream-class (simple-stream)
      (dolist (fmt (butlast external-format))
	(let ((encap (make-instance 'composing-stream :composing-format fmt)))
	  (setf (sm melding-base encap) stream)
	  (setf (sm melded-stream encap) (sm melded-stream stream))
	  (setf (sm melded-stream stream) encap)
	  (rotatef (sm j-listen encap) (sm j-listen stream))
	  (rotatef (sm j-read-char encap) (sm j-read-char stream))
	  (rotatef (sm j-read-chars encap) (sm j-read-chars stream))
	  (rotatef (sm j-unread-char encap) (sm j-unread-char stream))
	  (rotatef (sm j-write-char encap) (sm j-write-char stream))
	  (rotatef (sm j-write-chars encap) (sm j-write-chars stream)))))))

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
