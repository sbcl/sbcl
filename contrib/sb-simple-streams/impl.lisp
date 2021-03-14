;;; -*- lisp -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;

;;; Sbcl port by Rudi Schlatte.

(in-package "SB-SIMPLE-STREAMS")

(eval-when (:compile-toplevel)
  (defun optional+key-style-warning-p (condition)
    (and (typep condition '(and simple-condition style-warning))
         (stringp (simple-condition-format-control condition))
         (search "&OPTIONAL and &KEY found"
                 (simple-condition-format-control condition))))
  (proclaim '(sb-ext:muffle-conditions (satisfies optional+key-style-warning-p))))

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

(defun sb-impl::s-%file-position (stream position)
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

(defun sb-impl::s-%file-length (stream)
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


(defun sb-impl::s-%file-string-length (stream object)
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


(defun sb-impl::s-%read-line (stream eof-error-p eof-value)
  (declare (optimize (speed 3) (space 1) (safety 0) (debug 0))
           (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from sb-impl::s-%read-line
        (sb-impl::eof-or-lose stream eof-error-p eof-value)))
    ;; for interactive streams, finish output first to force prompt
    (when (and (any-stream-instance-flags stream :output)
               (any-stream-instance-flags stream :interactive))
      (sb-impl::s-%finish-output stream))
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

(defun sb-impl::s-%read-char (stream eof-error-p eof-value blocking-p)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from sb-impl::s-%read-char
        (sb-impl::eof-or-lose stream eof-error-p eof-value)))
    ;; for interactive streams, finish output first to force prompt
    (when (and (any-stream-instance-flags stream :output)
               (any-stream-instance-flags stream :interactive))
      (sb-impl::s-%finish-output stream))
    (funcall-stm-handler j-read-char (sm melded-stream stream)
                         eof-error-p eof-value blocking-p)))


(defun sb-impl::s-%unread-char (stream character)
  (declare (type simple-stream stream) (ignore character))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (if (zerop (sm last-char-read-size stream))
        (error "Nothing to unread.")
        (progn
          (funcall-stm-handler j-unread-char (sm melded-stream stream) nil)
          (remove-stream-instance-flags stream :eof)
          (setf (sm last-char-read-size stream) 0)))))


(defun sb-impl::s-%peek-char (stream peek-type eof-error-p eof-value)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :input)
    (when (any-stream-instance-flags stream :eof)
      (return-from sb-impl::s-%peek-char
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


(defun sb-impl::s-%read-byte (stream eof-error-p eof-value)
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


(defun sb-impl::s-%write-char (stream character)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (funcall-stm-handler-2 j-write-char character (sm melded-stream stream))))


(defun sb-impl::s-%fresh-line (stream)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (when (/= (or (sm charpos stream) 1) 0)
      (funcall-stm-handler-2 j-write-char #\Newline (sm melded-stream stream))
      t)))


(defun sb-impl::s-%write-string (stream string start end)
  (declare (type simple-stream stream))
  (with-stream-class (simple-stream stream)
    (%check stream :output)
    (funcall-stm-handler-2 j-write-chars string (sm melded-stream stream)
                           start end)))


(defun sb-impl::s-%line-length (stream)
  (declare (type simple-stream stream))
  (%check stream :output)
  ;; implement me
  nil)


(defun sb-impl::s-%finish-output (stream)
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


(defun sb-impl::s-%force-output (stream)
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


(defun sb-impl::%clear-output (stream)
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


(defun sb-impl::s-%write-byte (stream integer)
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

(defun sb-impl::s-%write-sequence (stream seq start end)
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

(defmethod interactive-stream-p ((stream simple-stream))
     (%check stream :open)
     (any-stream-instance-flags stream :interactive))

(defmethod (setf interactive-stream-p) (flag (stream simple-stream))
     (%check stream :open)
     (if flag
         (add-stream-instance-flags stream :interactive)
         (remove-stream-instance-flags stream :interactive)))

(defun sb-impl::s-%stream-external-format (stream)
     (with-stream-class (simple-stream)
       (%check stream :open)
       (sm external-format stream)))

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

(defun sb-impl::s-%read-char-no-hang (stream eof-error-p eof-value)
       (%check stream :input)
       (let ((char
              (with-stream-class (simple-stream)
                (funcall-stm-handler j-read-char stream eof-error-p eof-value nil))))
         (if (or (eq char eof-value) (not char))
             char
             (the character char))))

(defun listen (&optional (stream *standard-input*) (width 1))
  "Returns T if WIDTH octets are available on STREAM.  If WIDTH is
given as 'CHARACTER, check for a character.  Note: the WIDTH argument
is supported only on simple-streams."
  (declare (sb-int:explicit-check))
  ;; WIDTH is number of octets which must be available; any value
  ;; other than 1 is treated as 'character.
  (let ((stream (in-stream-from-designator stream)))
    (etypecase stream
      (simple-stream
       (%listen stream width))
      (ansi-stream
       (sb-impl::ansi-stream-listen stream))
      (fundamental-stream
       (sb-gray:stream-listen stream)))))

(defun read-sequence (seq stream &key (start 0) (end nil) partial-fill)
  "Destructively modify SEQ by reading elements from STREAM.
  SEQ is bounded by START and END. SEQ is destructively modified by
  copying successive elements into it from STREAM. If the end of file
  for STREAM is reached before copying all elements of the subsequence,
  then the extra elements near the end of sequence are not updated, and
  the index of the next element is returned."
  (let ((stream (in-stream-from-designator stream))
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
  (let ((stream (in-stream-from-designator stream)))
    (etypecase stream
      (simple-stream
       (%clear-input stream buffer-only))
      (ansi-stream
       (sb-impl::ansi-stream-clear-input stream))
      (fundamental-stream
       (sb-gray:stream-clear-input stream))))
  nil)

(defun sb-impl::s-%write-line (stream string start end)
  (declare (type simple-stream stream))
       (%check stream :output)
       (with-stream-class (simple-stream stream)
         (funcall-stm-handler-2 j-write-chars string stream start end)
         (funcall-stm-handler-2 j-write-char #\Newline stream)))

(defun sb-impl::s-%terpri (stream)
       (%check stream :output)
       (with-stream-class (simple-stream stream)
         (funcall-stm-handler-2 j-write-char #\Newline stream)))

(defun sb-impl::s-%charpos (stream)
       (with-stream-class (simple-stream stream)
         (%check stream :open)
         (sm charpos stream)))

(defun wait-for-input-available (stream &optional timeout)
  "Waits for input to become available on the Stream and returns T.  If
  Timeout expires, Nil is returned."
  (let ((stream (in-stream-from-designator stream)))
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
(defun sb-impl::s-%file-name (stream new-name)
     (with-stream-class (file-simple-stream stream)
       (cond (new-name
              (%file-rename stream new-name))
             (t
              (%file-name stream)))))
