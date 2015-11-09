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
;;; Implementations of standard Common Lisp functions for simple-streams

(defun %uninitialized (stream)
  (error "~S has not been initialized." stream))

(defun %check (stream kind)
  (declare (type simple-stream stream)
           (optimize (speed 3) (space 1) (debug 0) (safety 0)))
  (with-stream-class (simple-stream stream)
    (cond ((not (any-stream-instance-flags stream :simple))
           (%uninitialized stream))
          ((and (eq kind :open)
                (not (any-stream-instance-flags stream :input :output)))
           (sb-kernel:closed-flame stream))
          ((and (or (eq kind :input) (eq kind :io))
                (not (any-stream-instance-flags stream :input)))
           (sb-kernel:ill-in stream))
          ((and (or (eq kind :output) (eq kind :io))
                (not (any-stream-instance-flags stream :output)))
           (sb-kernel:ill-out stream)))))

(defmethod input-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :input))

(defmethod output-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :output))

(defmethod open-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :input :output))

;;; From the simple-streams documentation: "A generic function implies
;;; a specialization capability that does not exist for
;;; simple-streams; simple-stream specializations should be on
;;; device-close."  So don't do it.
(defmethod close ((stream simple-stream) &key abort)
  (device-close stream abort))

(defun %file-position (stream position)
  (declare (type simple-stream stream)
           (type (or (integer 0 *) (member nil :start :end)) position))
  (with-stream-class (simple-stream stream)
    (%check stream :open)
    (if position
        ;; Adjust current position
        (let ((position (case position (:start 0) (:end -1)
                              (otherwise position))))
          (etypecase stream
            (single-channel-simple-stream
             (when (sc-dirty-p stream)
               (flush-buffer stream t)))
            (dual-channel-simple-stream
             (with-stream-class (dual-channel-simple-stream stream)
               (when (> (sm outpos stream) 0)
                 (device-write stream :flush 0 nil t))))
            (string-simple-stream
             nil))

          (setf (sm last-char-read-size stream) 0)
          (setf (sm buffpos stream) 0   ; set pointer to 0 to force a read
                (sm buffer-ptr stream) 0)
          (setf (sm charpos stream) nil)
          (remove-stream-instance-flags stream :eof)
          (setf (device-file-position stream) position))
        ;; Just report current position
        (let ((posn (device-file-position stream)))
          (when posn
            (when (sm handler stream)
              (dolist (queued (sm pending stream))
                (incf posn (- (the sb-int:index (third queued))
                              (the sb-int:index (second queued))))))
            (etypecase stream
              (single-channel-simple-stream
               (case (sm mode stream)
                 ((0 3)         ; read, read-modify
                  ;; Note that posn can increase here if we wrote
                  ;; past the end of previously-read data
                  (decf posn (- (sm buffer-ptr stream) (sm buffpos stream))))
                 (1                     ; write
                  (incf posn (sm buffpos stream)))))
              (dual-channel-simple-stream
               (with-stream-class (dual-channel-simple-stream stream)
                 (incf posn (sm outpos stream))
                 (when (>= (sm buffer-ptr stream) 0)
                   (decf posn (- (sm buffer-ptr stream) (sm buffpos stream))))))
              (string-simple-stream
               nil)))
          posn))))

(defun %file-length (stream)
  (declare (type simple-stream stream))
  (%check stream :open)
  (device-file-length stream))


(defun %file-name (stream)
  (declare (type simple-stream stream))
  (%check stream nil)
  (typecase stream
    (file-simple-stream
     (with-stream-class (file-simple-stream stream)
       (sm pathname stream)))
    (probe-simple-stream
     (with-stream-class (probe-simple-stream stream)
       (sm pathname stream)))
    (otherwise
     nil)))


(defun %file-rename (stream new-name)
  (declare (type simple-stream stream))
  (%check stream nil)
  (if (typep stream 'file-simple-stream)
      (with-stream-class (file-simple-stream stream)
        (setf (sm pathname stream) new-name)
        (setf (sm filename stream) (%file-namestring new-name))
        t)
      nil))


(defun %file-string-length (stream object)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    ;; FIXME: need to account for compositions on the stream...
    (let ((count 0))
      (flet ((fn (octet)
               (declare (ignore octet))
               (incf count)))
        (etypecase object
          (character
           (let ((x nil))
             (char-to-octets (sm external-format stream) object x #'fn)))
          (string
           (let ((x nil)
                 (ef (sm external-format stream)))
             (dotimes (i (length object))
               (declare (type sb-int:index i))
               (char-to-octets ef (char object i) x #'fn))))))
      count)))


(defun %read-line (stream eof-error-p eof-value recursive-p)
  (declare (optimize (speed 3) (space 1) (safety 0) (debug 0))
           (type simple-stream stream)
           (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %read-line
        (sb-impl::eof-or-lose stream eof-error-p eof-value)))
    ;; for interactive streams, finish output first to force prompt
    (when (and (any-stream-instance-flags stream :output)
               (any-stream-instance-flags stream :interactive))
      (%finish-output stream))
    (let* ((encap (sm melded-stream stream)) ; encapsulating stream
           (cbuf (make-string 80))      ; current buffer
           (bufs (list cbuf))           ; list of buffers
           (tail bufs)                  ; last cons of bufs list
           (index 0)                    ; current index in current buffer
           (total 0))                   ; total characters
      (declare (type simple-stream encap)
               (type simple-string cbuf)
               (type cons bufs tail)
               (type sb-int:index index total))
      (loop
        (multiple-value-bind (chars done)
            (funcall-stm-handler j-read-chars encap cbuf
                                 #\Newline index (length cbuf) t)
          (declare (type sb-int:index chars))
          (incf index chars)
          (incf total chars)
          (when (and (eq done :eof) (zerop total))
            (if eof-error-p
                (error 'end-of-file :stream stream)
                (return (values eof-value t))))
          (when done
            ;; If there's only one buffer in use, return it directly
            (when (null (cdr bufs))
              (return (values (sb-kernel:shrink-vector cbuf total)
                              (eq done :eof))))
            ;; If total fits in final buffer, use it
            (when (<= total (length cbuf))
              (replace cbuf cbuf :start1 (- total index) :end2 index)
              (let ((idx 0))
                (declare (type sb-int:index idx))
                (do ((list bufs (cdr list)))
                    ((eq list tail))
                  (let ((buf (car list)))
                    (declare (type simple-string buf))
                    (replace cbuf buf :start1 idx)
                    (incf idx (length buf)))))
              (return (values (sb-kernel:shrink-vector cbuf total)
                              (eq done :eof))))
            ;; Allocate new string of appropriate length
            (let ((string (make-string total))
                  (index 0))
              (declare (type sb-int:index index))
              (dolist (buf bufs)
                (declare (type simple-string buf))
                (replace string buf :start1 index)
                (incf index (length buf)))
              (return  (values string (eq done :eof)))))
          (when (>= index (length cbuf))
            (setf cbuf (make-string (the sb-int:index (* 2 index))))
            (setf index 0)
            (setf (cdr tail) (cons cbuf nil))
            (setf tail (cdr tail))))))))

(defun %read-char (stream eof-error-p eof-value recursive-p blocking-p)
  (declare (type simple-stream stream)
           (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %read-char
        (sb-impl::eof-or-lose stream eof-error-p eof-value)))
    ;; for interactive streams, finish output first to force prompt
    (when (and (any-stream-instance-flags stream :output)
               (any-stream-instance-flags stream :interactive))
      (%finish-output stream))
    (funcall-stm-handler j-read-char (sm melded-stream stream)
                         eof-error-p eof-value blocking-p)))


(defun %unread-char (stream character)
  (declare (type simple-stream stream) (ignore character))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (if (zerop (sm last-char-read-size stream))
        (error "Nothing to unread.")
        (progn
          (funcall-stm-handler j-unread-char (sm melded-stream stream) nil)
          (remove-stream-instance-flags stream :eof)
          (setf (sm last-char-read-size stream) 0)))))


(defun %peek-char (stream peek-type eof-error-p eof-value recursive-p)
  (declare (type simple-stream stream)
           (ignore recursive-p))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %peek-char
        (sb-impl::eof-or-lose stream eof-error-p eof-value)))
    (let* ((encap (sm melded-stream stream))
           (char (funcall-stm-handler j-read-char encap
                                     eof-error-p stream t)))
      (cond ((eq char stream) eof-value)
            ((characterp peek-type)
             (do ((char char (funcall-stm-handler j-read-char encap
                                                  eof-error-p
                                                  stream t)))
                 ((or (eq char stream) (char= char peek-type))
                  (unless (eq char stream)
                    (funcall-stm-handler j-unread-char encap t))
                  (if (eq char stream) eof-value char))))
            ((eq peek-type t)
             (do ((char char (funcall-stm-handler j-read-char encap
                                                  eof-error-p
                                                  stream t)))
                 ((or (eq char stream)
                      (not (sb-impl::whitespace[2]p char)))
                  (unless (eq char stream)
                    (funcall-stm-handler j-unread-char encap t))
                  (if (eq char stream) eof-value char))))
            (t
             (funcall-stm-handler j-unread-char encap t)
             char)))))

(defun %listen (stream width)
  (declare (type simple-stream stream))
  ;; WIDTH is number of octets which must be available; any value
  ;; other than 1 is treated as 'character.
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %listen nil))
    (if (not (or (eql width 1) (null width)))
        (funcall-stm-handler j-listen (sm melded-stream stream))
        (or (< (sm buffpos stream) (sm buffer-ptr stream))
            ;; Attempt buffer refill
            (when (and (not (any-stream-instance-flags stream :dual :string))
                       (>= (sm mode stream) 0))
              ;; single-channel stream dirty -> write data before reading
              (flush-buffer stream nil))
            (>= (refill-buffer stream nil) width)))))

(defun %clear-input (stream buffer-only)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (setf (sm buffpos stream) 0
          (sm buffer-ptr stream) 0
          (sm last-char-read-size stream) 0
          #|(sm unread-past-soft-eof stream) nil|#)
    #| (setf (sm reread-count stream) 0)  on dual-channel streams? |#
    )
  (device-clear-input stream buffer-only))


(defun %read-byte (stream eof-error-p eof-value)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (if (any-stream-instance-flags stream :eof)
        (sb-impl::eof-or-lose stream eof-error-p eof-value)
        (etypecase stream
          (single-channel-simple-stream
           (read-byte-internal stream eof-error-p eof-value t))
          (dual-channel-simple-stream
           (read-byte-internal stream eof-error-p eof-value t))
          (string-simple-stream
           (with-stream-class (string-simple-stream stream)
             (let ((encap (sm input-handle stream)))
               (unless encap
                 (error 'simple-type-error
                        :datum stream
                        :expected-type 'stream
                        :format-control "Can't read-byte on string streams"
                        :format-arguments '()))
               (prog1
                   (read-byte encap eof-error-p eof-value)
                 (setf (sm last-char-read-size stream) 0
                       (sm encapsulated-char-read-size stream) 0)))))))))


(defun %write-char (stream character)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (funcall-stm-handler-2 j-write-char character (sm melded-stream stream))))


(defun %fresh-line (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (when (/= (or (sm charpos stream) 1) 0)
      (funcall-stm-handler-2 j-write-char #\Newline (sm melded-stream stream))
      t)))


(defun %write-string (stream string start end)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (funcall-stm-handler-2 j-write-chars string (sm melded-stream stream)
                           start end)))


(defun %line-length (stream)
  (declare (type simple-stream stream))
  (%check stream :output)
  ;; implement me
  nil)


(defun %finish-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (when (sm handler stream)
      (do ()
          ((null (sm pending stream)))
        (sb-sys:serve-all-events)))
    (etypecase stream
      (single-channel-simple-stream
       ;(when (and (> (sm mode stream) 0) (> (sm buffer-ptr stream) 0))
       ;  (setf (device-file-position stream)
       ;        (- (device-file-position stream) (sm buffer-ptr stream))))
       ;(device-write stream :flush 0 nil t)
       (flush-buffer stream t)
       (setf (sm buffpos stream) 0))
      (dual-channel-simple-stream
       (with-stream-class (dual-channel-simple-stream stream)
         (device-write stream :flush 0 nil t)
         (setf (sm outpos stream) 0)))
      (string-simple-stream
           (device-write stream :flush 0 nil t))))
  nil)


(defun %force-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (etypecase stream
      (single-channel-simple-stream
       ;(when (> (sm buffer-ptr stream) 0)
       ;  (setf (device-file-position stream)
       ;        (- (device-file-position stream) (sm buffer-ptr stream))))
       ;(device-write stream :flush 0 nil nil)
       (flush-buffer stream nil)
       (setf (sm buffpos stream) 0))
      (dual-channel-simple-stream
       (with-stream-class (dual-channel-simple-stream stream)
         (device-write stream :flush 0 nil nil)
         (setf (sm outpos stream) 0)))
      (string-simple-stream
       (device-write stream :flush 0 nil nil))))
  nil)


(defun %clear-output (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (when (sm handler stream)
      (sb-sys:remove-fd-handler (sm handler stream))
      (setf (sm handler stream) nil
            (sm pending stream) nil))
    (etypecase stream
      (single-channel-simple-stream
       (with-stream-class (single-channel-simple-stream stream)
         (case (sm mode stream)
           (1 (setf (sm buffpos stream) 0))
           (3 (setf (sm mode stream) 0)))))
      (dual-channel-simple-stream
       (setf (sm outpos stream) 0))
      (string-simple-stream
       nil))
    (device-clear-output stream)))


(defun %write-byte (stream integer)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (etypecase stream
      (single-channel-simple-stream
       (with-stream-class (single-channel-simple-stream stream)
         (let ((ptr (sm buffpos stream)))
          (when (>= ptr (sm buf-len stream))
            (setf ptr (flush-buffer stream t)))
          (setf (sm buffpos stream) (1+ ptr))
          (setf (sm charpos stream) nil)
          (setf (bref (sm buffer stream) ptr) integer)
          (sc-set-dirty stream))))
      (dual-channel-simple-stream
       (with-stream-class (dual-channel-simple-stream stream)
         (let ((ptr (sm outpos stream)))
           (when (>= ptr (sm max-out-pos stream))
             (setf ptr (flush-out-buffer stream t)))
           (setf (sm outpos stream) (1+ ptr))
           (setf (sm charpos stream) nil)
           (setf (bref (sm out-buffer stream) ptr) integer))))
      (string-simple-stream
       (with-stream-class (string-simple-stream stream)
         (let ((encap (sm output-handle stream)))
           (unless encap
             (error 'simple-type-error
                    :datum stream
                    :expected-type 'stream
                    :format-control "Can't write-byte on string streams."
                    :format-arguments '()))
           (write-byte integer encap)))))))


(defun %read-sequence (stream seq start end partial-fill)
  (declare (type simple-stream stream)
           (type sequence seq)
           (type sb-int:index start end)
           (type boolean partial-fill))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from %read-sequence 0))
    (when (and (not (any-stream-instance-flags stream :dual :string))
               (sc-dirty-p stream))
      (flush-buffer stream t))
    (etypecase seq
      (string
       (funcall-stm-handler j-read-chars (sm melded-stream stream) seq nil
                            start end
                            (if partial-fill :bnb t)))
      ((or (simple-array (unsigned-byte 8) (*))
           (simple-array (signed-byte 8) (*)))
       (when (any-stream-instance-flags stream :string)
         (error "Can't read into byte sequence from a string stream."))
       ;; "read-vector" equivalent, but blocking if partial-fill is NIL
       ;; FIXME: this could be implemented faster via buffer-copy
       (loop with encap = (sm melded-stream stream)
            for index from start below end
            for byte = (read-byte-internal encap nil nil t)
              then (read-byte-internal encap nil nil partial-fill)
            while byte
            do (setf (bref seq index) byte)
            finally (return index)))
      ;; extend to work on other sequences: repeated read-byte
      )))

(defun %write-sequence (stream seq start end)
  (declare (type simple-stream stream)
           (type sequence seq)
           (type sb-int:index start end))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (etypecase seq
      (string
       (funcall-stm-handler-2 j-write-chars seq (sm melded-stream stream)
                              start end))
      ((or (simple-array (unsigned-byte 8) (*))
           (simple-array (signed-byte 8) (*)))
       ;; "write-vector" equivalent
       (setf (sm charpos stream) nil)
       (etypecase stream
         (single-channel-simple-stream
          (with-stream-class (single-channel-simple-stream stream)
            (loop with max-ptr fixnum = (sm buf-len stream)
                  for src-pos fixnum = start then (+ src-pos count)
                  for src-rest fixnum = (- end src-pos)
                  while (> src-rest 0) ; FIXME: this is non-ANSI
                  for ptr fixnum = (let ((ptr (sm buffpos stream)))
                                     (if (>= ptr max-ptr)
                                         (flush-buffer stream t)
                                         ptr))
                  for buf-rest fixnum = (- max-ptr ptr)
                  for count fixnum = (min buf-rest src-rest)
                  do (progn (setf (sm mode stream) 1)
                            (setf (sm buffpos stream) (+ ptr count))
                            (buffer-copy seq src-pos (sm buffer stream) ptr count)))))
         (dual-channel-simple-stream
          (with-stream-class (dual-channel-simple-stream stream)
            (loop with max-ptr fixnum = (sm max-out-pos stream)
                  for src-pos fixnum = start then (+ src-pos count)
                  for src-rest fixnum = (- end src-pos)
                  while (> src-rest 0) ; FIXME: this is non-ANSI
                  for ptr fixnum = (let ((ptr (sm outpos stream)))
                                     (if (>= ptr max-ptr)
                                         (flush-out-buffer stream t)
                                         ptr))
                  for buf-rest fixnum = (- max-ptr ptr)
                  for count fixnum = (min buf-rest src-rest)
                  do (progn (setf (sm outpos stream) (+ ptr count))
                            (buffer-copy seq src-pos (sm out-buffer stream) ptr count)))))
         (string-simple-stream
          (error 'simple-type-error
                 :datum stream
                 :expected-type 'stream
                 :format-control "Can't write a byte sequence to a string stream."
                 :format-arguments '())))
       )
      ;; extend to work on other sequences: repeated write-byte
      ))
  seq)


(defun read-vector (vector stream &key (start 0) end (endian-swap :byte-8))
  (declare (type (sb-kernel:simple-unboxed-array (*)) vector)
           (type stream stream))
  ;; START and END are octet offsets, not vector indices!  [Except for strings]
  ;; Return value is index of next octet to be read into (i.e., start+count)
  (etypecase stream
    (simple-stream
     (with-stream-class (simple-stream stream)
       (cond ((stringp vector)
              (let* ((start (or start 0))
                     (end (or end (length vector)))
                     (encap (sm melded-stream stream))
                     (char (funcall-stm-handler j-read-char encap nil nil t)))
                (when char
                  (setf (schar vector start) char)
                  (incf start)
                  (+ start (funcall-stm-handler j-read-chars encap vector nil
                                                start end nil)))))
             ((any-stream-instance-flags stream :string)
              (error "Can't READ-BYTE on string streams."))
             (t
              (do* ((encap (sm melded-stream stream))
                    (index (or start 0) (1+ index))
                    (end (or end (* (length vector) (vector-elt-width vector))))
                    (endian-swap (endian-swap-value vector endian-swap))
                    (flag t nil))
                   ((>= index end) index)
                (let ((byte (read-byte-internal encap nil nil flag)))
                  (unless byte
                    (return index))
                  (setf (bref vector (logxor index endian-swap)) byte)))))))
    ((or ansi-stream fundamental-stream)
     (unless (typep vector '(or string
                             (simple-array (signed-byte 8) (*))
                             (simple-array (unsigned-byte 8) (*))))
       (error "Wrong vector type for read-vector on stream not of type simple-stream."))
     (read-sequence vector stream :start (or start 0) :end end))))


;;;
;;; USER-LEVEL FUNCTIONS
;;;

(defmethod open-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :input :output))

(defmethod input-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :input))

(defmethod output-stream-p ((stream simple-stream))
  (any-stream-instance-flags stream :output))

(defmethod stream-element-type ((stream simple-stream))
  '(unsigned-byte 8))

(defun interactive-stream-p (stream)
  "Return true if Stream does I/O on a terminal or other interactive device."
  (etypecase stream
    (simple-stream
     (%check stream :open)
     (any-stream-instance-flags stream :interactive))
    (ansi-stream
     (funcall (sb-kernel:ansi-stream-misc stream) stream :interactive-p))
    (fundamental-stream
     nil)))

(defun (setf interactive-stream-p) (flag stream)
  (typecase stream
    (simple-stream
     (%check stream :open)
     (if flag
         (add-stream-instance-flags stream :interactive)
         (remove-stream-instance-flags stream :interactive)))
    (t
     (error 'simple-type-error
            :datum stream
            :expected-type 'simple-stream
            :format-control "Can't set interactive flag on ~S."
            :format-arguments (list stream)))))

(defun file-string-length (stream object)
  (declare (type (or string character) object) (type stream stream))
  "Return the delta in STREAM's FILE-POSITION that would be caused by writing
   OBJECT to STREAM. Non-trivial only in implementations that support
   international character sets."
  (typecase stream
    (simple-stream (%file-string-length stream object))
    (t
     (etypecase object
       (character 1)
       (string (length object))))))

(defun stream-external-format (stream)
  "Returns Stream's external-format."
  (etypecase stream
    (simple-stream
     (with-stream-class (simple-stream)
       (%check stream :open)
       (sm external-format stream)))
    (ansi-stream
     :default)
    (fundamental-stream
     :default)))

(defun open (filename &rest options
             &key (direction :input)
             (element-type 'character element-type-given)
             if-exists if-does-not-exist
             (external-format :default)
             class mapped input-handle output-handle
             &allow-other-keys)
  "Return a stream which reads from or writes to Filename.
  Defined keywords:
   :direction - one of :input, :output, :io, or :probe
   :element-type - type of object to read or write, default BASE-CHAR
   :if-exists - one of :error, :new-version, :rename, :rename-and-delete,
                       :overwrite, :append, :supersede or NIL
   :if-does-not-exist - one of :error, :create or NIL
   :external-format - :default
  See the manual for details.

  The following are simple-streams-specific additions:
   :class - class of stream object to be created
   :mapped - T to open a memory-mapped file
   :input-handle - a stream or Unix file descriptor to read from
   :output-handle - a stream or Unix file descriptor to write to"
  (declare (ignore element-type external-format input-handle output-handle
                   if-exists if-does-not-exist))
  (let ((class (or class 'sb-sys:fd-stream))
        (options (copy-list options))
        (filespec (merge-pathnames filename)))
    (cond ((subtypep class 'sb-sys:fd-stream)
           (remf options :mapped)
           (remf options :input-handle)
           (remf options :output-handle)
           (apply #'open-fd-stream filespec options))
          ((subtypep class 'simple-stream)
           (when element-type-given
             (cerror "Do it anyway."
                     "Can't create simple-streams with an element-type."))
           (when (and (eq class 'file-simple-stream) mapped)
             (setq class 'mapped-file-simple-stream)
             (setf (getf options :class) 'mapped-file-simple-stream))
           (when (subtypep class 'file-simple-stream)
             (when (eq direction :probe)
               (setq class 'probe-simple-stream)))
           (apply #'make-instance class :filename filespec options))
          ((subtypep class 'sb-gray:fundamental-stream)
           (remf options :class)
           (remf options :mapped)
           (remf options :input-handle)
           (remf options :output-handle)
           (make-instance class :lisp-stream
                          (apply #'open-fd-stream filespec options)))
          (t (error "Don't know how to handle the stream class ~A"
                    (etypecase class
                      (symbol (find-class class t))
                      (class class)))))))


;; These are not normally inlined.
;; READ-CHAR is 1K of code, etc. This was probably either a brute-force
;; way to optimize IN-SYNONYM-OF and/or optimize for known sub-hierarchy
;; at compile-time, but how likely is that to help?
(declaim (inline read-byte read-char read-char-no-hang unread-char))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  "Returns the next byte of the Stream."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (let ((byte (%read-byte stream eof-error-p eof-value)))
         (if (eq byte eof-value)
             byte
             (the integer byte))))
      (ansi-stream
       (sb-impl::ansi-stream-read-byte stream eof-error-p eof-value nil))
      (fundamental-stream
       (let ((byte (sb-gray:stream-read-byte stream)))
         (if (eq byte :eof)
             (sb-impl::eof-or-lose stream eof-error-p eof-value)
             (the integer byte)))))))

(defun read-char (&optional (stream *standard-input*) (eof-error-p t)
                            eof-value recursive-p)
  "Inputs a character from Stream and returns it."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (let ((char (%read-char stream eof-error-p eof-value recursive-p t)))
         (if (eq char eof-value)
             char
             (the character char))))
      (ansi-stream
       (sb-impl::ansi-stream-read-char stream eof-error-p eof-value
                                       recursive-p))
      (fundamental-stream
       (let ((char (sb-gray:stream-read-char stream)))
         (if (eq char :eof)
             (sb-impl::eof-or-lose stream eof-error-p eof-value)
             (the character char)))))))

(defun read-char-no-hang (&optional (stream *standard-input*) (eof-error-p t)
                                    eof-value recursive-p)
  "Returns the next character from the Stream if one is availible, or nil."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check stream :input)
       (let ((char
              (with-stream-class (simple-stream)
                (funcall-stm-handler j-read-char stream eof-error-p eof-value nil))))
         (if (or (eq char eof-value) (not char))
             char
             (the character char))))
      (ansi-stream
       (sb-impl::ansi-stream-read-char-no-hang stream eof-error-p eof-value
                                               recursive-p))
      (fundamental-stream
       (let ((char (sb-gray:stream-read-char-no-hang stream)))
         (if (eq char :eof)
             (sb-impl::eof-or-lose stream eof-error-p eof-value)
             (the (or character null) char)))))))

(defun unread-char (character &optional (stream *standard-input*))
  "Puts the Character back on the front of the input Stream."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%unread-char stream character))
      (ansi-stream
       (sb-impl::ansi-stream-unread-char character stream))
      (fundamental-stream
       (sb-gray:stream-unread-char stream character))))
  nil)

(declaim (notinline read-byte read-char read-char-no-hang unread-char))

(defun peek-char (&optional (peek-type nil) (stream *standard-input*)
                            (eof-error-p t) eof-value recursive-p)
  "Peeks at the next character in the input Stream.  See manual for details."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (let ((char
              (%peek-char stream peek-type eof-error-p eof-value recursive-p)))
         (if (eq char eof-value)
             char
             (the character char))))
      ;; FIXME: Broken on ECHO-STREAM (cf internal implementation?) --
      ;; CSR, 2004-01-19
      (ansi-stream
       (sb-impl::ansi-stream-peek-char peek-type stream eof-error-p eof-value
                                       recursive-p))
      (fundamental-stream
       ;; This seems to duplicate all the code of GENERALIZED-PEEKING-MECHANISM
       (cond ((characterp peek-type)
              (do ((char (sb-gray:stream-read-char stream)
                         (sb-gray:stream-read-char stream)))
                  ((or (eq char :eof) (char= char peek-type))
                   (cond ((eq char :eof)
                          (sb-impl::eof-or-lose stream eof-error-p eof-value))
                         (t
                          (sb-gray:stream-unread-char stream char)
                          char)))))
             ((eq peek-type t)
              (do ((char (sb-gray:stream-read-char stream)
                         (sb-gray:stream-read-char stream)))
                  ((or (eq char :eof) (not (sb-impl::whitespace[2]p char)))
                   (cond ((eq char :eof)
                          (sb-impl::eof-or-lose stream eof-error-p eof-value))
                         (t
                          (sb-gray:stream-unread-char stream char)
                          char)))))
             (t
              (let ((char (sb-gray:stream-peek-char stream)))
                (if (eq char :eof)
                    (sb-impl::eof-or-lose stream eof-error-p eof-value)
                    (the character char)))))))))

(defun listen (&optional (stream *standard-input*) (width 1))
  "Returns T if WIDTH octets are available on STREAM.  If WIDTH is
given as 'CHARACTER, check for a character.  Note: the WIDTH argument
is supported only on simple-streams."
  (declare (sb-int:explicit-check))
  ;; WIDTH is number of octets which must be available; any value
  ;; other than 1 is treated as 'character.
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%listen stream width))
      (ansi-stream
       (sb-impl::ansi-stream-listen stream))
      (fundamental-stream
       (sb-gray:stream-listen stream)))))


(defun read-line (&optional (stream *standard-input*) (eof-error-p t)
                            eof-value recursive-p)
  "Returns a line of text read from the Stream as a string, discarding the
  newline character."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%read-line stream eof-error-p eof-value recursive-p))
      (ansi-stream
       (sb-impl::ansi-stream-read-line stream eof-error-p eof-value
                                       recursive-p))
      (fundamental-stream
       (multiple-value-bind (string eof) (sb-gray:stream-read-line stream)
         (if (and eof (zerop (length string)))
             (values (sb-impl::eof-or-lose stream eof-error-p eof-value) t)
             (values string eof)))))))

(defun read-sequence (seq stream &key (start 0) (end nil) partial-fill)
  "Destructively modify SEQ by reading elements from STREAM.
  SEQ is bounded by START and END. SEQ is destructively modified by
  copying successive elements into it from STREAM. If the end of file
  for STREAM is reached before copying all elements of the subsequence,
  then the extra elements near the end of sequence are not updated, and
  the index of the next element is returned."
  (let ((stream (sb-impl::in-synonym-of stream))
        (end (or end (length seq))))
    (etypecase stream
      (simple-stream
       (with-stream-class (simple-stream stream)
         (%read-sequence stream seq start end partial-fill)))
      (ansi-stream
       (sb-impl::ansi-stream-read-sequence seq stream start end))
      (fundamental-stream
       (sb-gray:stream-read-sequence stream seq start end)))))

(defun clear-input (&optional (stream *standard-input*) buffer-only)
  "Clears any buffered input associated with the Stream."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%clear-input stream buffer-only))
      (ansi-stream
       (sb-impl::ansi-stream-clear-input stream))
      (fundamental-stream
       (sb-gray:stream-clear-input stream))))
  nil)

(defun write-byte (integer stream)
  "Outputs an octet to the Stream."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%write-byte stream integer))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-bout stream) stream integer))
      (fundamental-stream
       (sb-gray:stream-write-byte stream integer))))
  integer)

(defun write-char (character &optional (stream *standard-output*))
  "Outputs the Character to the Stream."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%write-char stream character))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-out stream) stream character))
      (fundamental-stream
       (sb-gray:stream-write-char stream character))))
  character)

(defun write-string (string &optional (stream *standard-output*)
                            &key (start 0) (end nil))
  "Outputs the String to the given Stream."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream))
        (end (sb-impl::%check-vector-sequence-bounds string start end)))
    (etypecase stream
      (simple-stream
       (%write-string stream string start end)
       string)
      (ansi-stream
       (sb-impl::ansi-stream-write-string string stream start end))
      (fundamental-stream
       (sb-gray:stream-write-string stream string start end)))))

(defun write-line (string &optional (stream *standard-output*)
                          &key (start 0) end)
  (declare (type string string))
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream))
        (end (sb-impl::%check-vector-sequence-bounds string start end)))
    (etypecase stream
      (simple-stream
       (%check stream :output)
       (with-stream-class (simple-stream stream)
         (funcall-stm-handler-2 j-write-chars string stream start end)
         (funcall-stm-handler-2 j-write-char #\Newline stream)))
      (ansi-stream
       (sb-impl::ansi-stream-write-string string stream start end)
       (funcall (sb-kernel:ansi-stream-out stream) stream #\Newline))
      (fundamental-stream
       (sb-gray:stream-write-string stream string start end)
       (sb-gray:stream-terpri stream))))
  string)

(defun write-sequence (seq stream &key (start 0) (end nil))
  "Write the elements of SEQ bounded by START and END to STREAM."
  (let ((stream (sb-impl::out-synonym-of stream))
        (end (or end (length seq))))
    (etypecase stream
      (simple-stream
       (%write-sequence stream seq start end))
      (ansi-stream
       (sb-impl::ansi-stream-write-sequence seq stream start end))
      (fundamental-stream
       (sb-gray:stream-write-sequence stream seq start end)))))

(defun terpri (&optional (stream *standard-output*))
  "Outputs a new line to the Stream."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check stream :output)
       (with-stream-class (simple-stream stream)
         (funcall-stm-handler-2 j-write-char #\Newline stream)))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-out stream) stream #\Newline))
      (fundamental-stream
       (sb-gray:stream-terpri stream))))
  nil)

(defun fresh-line (&optional (stream *standard-output*))
  "Outputs a new line to the Stream if it is not positioned at the beginning of
   a line.  Returns T if it output a new line, nil otherwise."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%fresh-line stream))
      (ansi-stream
       (sb-impl::ansi-stream-fresh-line stream))
      (fundamental-stream
       (sb-gray:stream-fresh-line stream)))))

(defun finish-output (&optional (stream *standard-output*))
  "Attempts to ensure that all output sent to the Stream has reached its
   destination, and only then returns."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%finish-output stream))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :finish-output))
      (fundamental-stream
       (sb-gray:stream-finish-output stream))))
  nil)

(defun force-output (&optional (stream *standard-output*))
  "Attempts to force any buffered output to be sent."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%force-output stream))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :force-output))
      (fundamental-stream
       (sb-gray:stream-force-output stream))))
  nil)

(defun clear-output (&optional (stream *standard-output*))
  "Clears the given output Stream."
  (declare (sb-int:explicit-check))
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%clear-output stream))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :clear-output))
      (fundamental-stream
       (sb-gray:stream-clear-output stream))))
  nil)


(defun file-position (stream &optional position)
  "With one argument returns the current position within the file
   File-Stream is open to.  If the second argument is supplied, then
   this becomes the new file position.  The second argument may also
   be :start or :end for the start and end of the file, respectively."
  (declare (type (or sb-int:index (member nil :start :end)) position))
  (etypecase stream
    (simple-stream
     (%file-position stream position))
    (ansi-stream
     (sb-impl::ansi-stream-file-position stream position))))

(defun file-length (stream)
  "This function returns the length of the file that File-Stream is open to."
  (etypecase stream
    (simple-stream
     (%file-length stream))
    (ansi-stream
     (sb-impl::stream-must-be-associated-with-file stream)
     (funcall (sb-kernel:ansi-stream-misc stream) stream :file-length))))

(defun charpos (&optional (stream *standard-output*))
  "Returns the number of characters on the current line of output of the given
  Stream, or Nil if that information is not availible."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (with-stream-class (simple-stream stream)
         (%check stream :open)
         (sm charpos stream)))
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :charpos))
      (fundamental-stream
       (sb-gray:stream-line-column stream)))))

(defun line-length (&optional (stream *standard-output*))
  "Returns the number of characters in a line of output of the given
  Stream, or Nil if that information is not availible."
  (let ((stream (sb-impl::out-synonym-of stream)))
    (etypecase stream
      (simple-stream
       (%check stream :output)
       ;; TODO (sat 2003-04-02): a way to specify a line length would
       ;; be good, I suppose.  Returning nil here means
       ;; sb-pretty::default-line-length is used.
       nil)
      (ansi-stream
       (funcall (sb-kernel:ansi-stream-misc stream) stream :line-length))
      (fundamental-stream
       (sb-gray:stream-line-length stream)))))

(defun wait-for-input-available (stream &optional timeout)
  "Waits for input to become available on the Stream and returns T.  If
  Timeout expires, Nil is returned."
  (let ((stream (sb-impl::in-synonym-of stream)))
    (etypecase stream
      (fixnum
       (sb-sys:wait-until-fd-usable stream :input timeout))
      (simple-stream
       (%check stream :input)
       (with-stream-class (simple-stream stream)
         (or (< (sm buffpos stream) (sm buffer-ptr stream))
             (wait-for-input-available (sm input-handle stream) timeout))))
      (two-way-stream
       (wait-for-input-available (two-way-stream-input-stream stream) timeout))
      (synonym-stream
       (wait-for-input-available (symbol-value (synonym-stream-symbol stream))
                                 timeout))
      (sb-sys:fd-stream
       (or (< (sb-impl::fd-stream-in-index stream)
              (length (sb-impl::fd-stream-in-buffer stream)))
           (wait-for-input-available (sb-sys:fd-stream-fd stream) timeout))))))

;; Make PATHNAME and NAMESTRING work
(defun sb-int:file-name (stream &optional new-name)
  (typecase stream
    (file-simple-stream
     (with-stream-class (file-simple-stream stream)
       (cond (new-name
              (%file-rename stream new-name))
             (t
              (%file-name stream)))))
    (sb-sys:fd-stream
     (cond (new-name
            (setf (sb-impl::fd-stream-pathname stream) new-name)
            (setf (sb-impl::fd-stream-file stream)
                  (%file-namestring new-name))
            t)
           (t
            (sb-impl::fd-stream-pathname stream))))))
