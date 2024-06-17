;;;; streams for UNIX file descriptors

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; BUFFER
;;;;
;;;; Streams hold BUFFER objects, which contain a SAP, size of the
;;;; memory area the SAP stands for (LENGTH bytes), and HEAD and TAIL
;;;; indexes which delimit the "valid", or "active" area of the
;;;; memory. HEAD is inclusive, TAIL is exclusive.
;;;;
;;;; Buffers get allocated lazily, and are recycled by returning them
;;;; to the *AVAILABLE-BUFFERS* list. Every buffer has its own
;;;; finalizer, to take care of releasing the SAP memory when a stream
;;;; is not properly closed.
;;;;
;;;; The code aims to provide a limited form of thread and interrupt
;;;; safety: parallel writes and reads may lose output or input, cause
;;;; interleaved IO, etc -- but they should not corrupt memory. The
;;;; key to doing this is to read buffer state once, and update the
;;;; state based on the read state:
;;;;
;;;; (let ((tail (buffer-tail buffer)))
;;;;   ...
;;;;   (setf (buffer-tail buffer) (+ tail n)))
;;;;
;;;; NOT
;;;;
;;;; (let ((tail (buffer-tail buffer)))
;;;;   ...
;;;;  (incf (buffer-tail buffer) n))
;;;;

(defstruct (buffer (:constructor !make-buffer (sap length))
                   (:copier nil))
  (sap (missing-arg) :type system-area-pointer :read-only t)
  (length (missing-arg) :type index :read-only t)
  (head 0 :type index)
  (tail 0 :type index)
  (prev-head 0 :type index))
(declaim (freeze-type buffer))

(define-load-time-global *available-buffers* ()
  "List of available buffers.")

(defconstant +bytes-per-buffer+ (* 32 1024)
  "Default number of bytes per buffer.")

(defun alloc-buffer (&optional (size +bytes-per-buffer+))
  (declare (sb-c::tlab :system)
           (inline allocate-system-memory) ; so the SAP gets heap-consed
           (inline !make-buffer))
  ;; Don't want to allocate & unwind before the finalizer is in place.
  (without-interrupts
    (let* ((sap (allocate-system-memory size))
           (buffer (!make-buffer sap size)))
      (when (zerop (sap-int sap))
        (error "Could not allocate ~D bytes for buffer." size))
      (finalize buffer (lambda ()
                         (deallocate-system-memory sap size))
                :dont-save t)
      buffer)))

(defun get-buffer ()
  (or (and *available-buffers* (atomic-pop *available-buffers*))
      (alloc-buffer)))

(declaim (inline reset-buffer))
(defun reset-buffer (buffer)
  (setf (buffer-head buffer) 0
        (buffer-tail buffer) 0)
  buffer)

(defun release-buffer (buffer)
  (declare (sb-c::tlab :system))
  (reset-buffer buffer)
  (atomic-push buffer *available-buffers*))


;;;; the FD-STREAM structure

;;; Coarsely characterizes the element type of an FD-STREAM w.r.t.
;;; its SUBTYPEP relations to the relevant CHARACTER and
;;; ([UN]SIGNED-BYTE 8) types. This coarse characterization enables
;;; dispatching on the element type as needed by {READ,WRITE}-SEQUENCE
;;; without calling SUBTYPEP.
(deftype stream-element-mode ()
  '(member character unsigned-byte signed-byte :bivalent))

(defstruct (fd-stream
            (:constructor %make-fd-stream)
            (:conc-name fd-stream-)
            (:predicate fd-stream-p)
            (:include ansi-stream
             ;; FIXME: would a type constraint on IN-BUFFER
             ;; and/or CIN-BUFFER improve anything?
                      (misc #'fd-stream-misc-routine))
            (:copier nil))

  ;; the name of this stream
  (name nil)
  ;; the file this stream is for
  (file nil)
  ;; the backup file namestring for the old file, for :IF-EXISTS
  ;; :RENAME or :RENAME-AND-DELETE.
  (original nil :type (or simple-string null))
  (delete-original nil)       ; for :if-exists :rename-and-delete
  ;;; the number of bytes per element
  (element-size 1 :type index)
  ;; the type of element being transfered
  (element-type 'base-char)
  ;; coarse characterization of the element type. see description of
  ;; STREAM-ELEMENT-MODE type.
  (element-mode :bivalent :type stream-element-mode)
  ;; the Unix file descriptor
  (fd -1 :type #-win32 fixnum #+win32 sb-vm:signed-word)
  ;; What do we know about the FD?
  (fd-type :unknown :type keyword)
  ;; controls when the output buffer is flushed
  (buffering :full :type (member :full :line :none))
  ;; controls whether the input buffer must be cleared before output
  ;; (must be done for files, not for sockets, pipes and other data
  ;; sources where input and output aren't related).
  (synchronize-output nil)
  ;; character position if known -- this may run into bignums, but
  ;; we probably should flip it into null then for efficiency's sake...
  (output-column nil :type (or (and unsigned-byte
                                    #+64-bit index)
                               null))
  ;; T if input is waiting on FD. :EOF if we hit EOF.
  (listen nil :type (member nil t :eof))
  ;; T if serve-event is allowed when this stream blocks
  (serve-events nil :type boolean)

  ;; the input buffer
  (instead (make-array 0 :element-type 'character :adjustable t :fill-pointer t) :type (array character (*)))
  (ibuf nil :type (or buffer null))
  (eof-forced-p nil :type (member t nil))

  ;; the output buffer
  (obuf nil :type (or buffer null))

  ;; output flushed, but not written due to non-blocking io?
  (output-queue nil)
  (handler nil)
  ;; timeout specified for this stream as seconds or NIL if none
  (timeout nil :type (or single-float null))
  ;; pathname of the file this stream is opened to (returned by PATHNAME)
  (pathname nil :type (or pathname null))
  ;; Not :DEFAULT, because we want to match CHAR-SIZE!
  (external-format :latin-1)
  ;; fixed width, or function to call with a character
  (char-size 1 :type (or fixnum function))
  (replacement nil :type (or null character string (simple-array (unsigned-byte 8) 1)))
  (output-bytes #'ill-out :type function))

(defun fd-stream-bivalent-p (stream)
  (eq (fd-stream-element-mode stream) :bivalent))

(defmethod print-object ((fd-stream fd-stream) stream)
  (declare (type stream stream))
  (print-unreadable-object (fd-stream stream :type t :identity t)
    (format stream "for ~S" (fd-stream-name fd-stream))))

;;; Release all of FD-STREAM's buffers. Originally the intent of this
;;; was to grab a mutex once only, but the buffer pool is lock-free now.
(defun release-fd-stream-buffers (fd-stream)
  (awhen (fd-stream-ibuf fd-stream)
    (setf (fd-stream-ibuf fd-stream) nil)
    (release-buffer it))
  (awhen (fd-stream-obuf fd-stream)
    (setf (fd-stream-obuf fd-stream) nil)
    (release-buffer it))
  (dolist (buf (fd-stream-output-queue fd-stream))
    (when (buffer-p buf)
      (release-buffer buf)))
  (setf (fd-stream-output-queue fd-stream) nil))

;;;; FORM-TRACKING-STREAM

;; The compiler uses this to record for each input subform the start and
;; end positions as character offsets. Measuring in characters rather than
;; bytes is both better suited to the task - in that it is consistent with
;; reporting of "point" by Emacs - and faster than querying FILE-POSITION.
;; The slowness of FILE-POSITION on an FD-STREAM is due to making a system
;; call every time, to ensure that the case of an append-only stream works
;; correctly (where the OS forces all writes to the end), and other factors.

(defstruct (form-tracking-stream
            (:constructor %make-form-tracking-stream)
            (:include fd-stream
                      (misc #'tracking-stream-misc)
                      (input-char-pos 0))
            (:copier nil))
  ;; a function which is called for events on this stream.
  (observer (lambda (x y z) (declare (ignore x y z))) :type function)
  ;;  A vector of the character position of each #\Newline seen
  (newlines (make-array 10 :fill-pointer 0 :adjustable t))
  (last-newline -1 :type index-or-minus-1)
  ;; Better than reporting that a reader error occurred at a position
  ;; before any whitespace (or equivalently, a macro producing no value),
  ;; we can note the position at the first "good" character.
  (form-start-byte-pos)
  (form-start-char-pos))

(defun line/col-from-charpos
    (stream &optional (charpos (ansi-stream-input-char-pos stream)))
  (let ((newlines (form-tracking-stream-newlines stream)))
   (if charpos
       (let ((index (position charpos newlines :test #'>= :from-end t)))
         ;; Line numbers traditionally begin at 1, columns at 0.
         (if index
             ;; INDEX is 1 less than the number of newlines seen
             ;; up to and including this startpos.
             ;; e.g. index=0 => 1 newline seen => line=2
             (cons (+ index 2)
                   ;; 1 char after the newline = column 0
                   (- charpos (aref newlines index) 1))
             ;; zero newlines were seen
             (cons 1 charpos)))
       ;; No charpos means the error is before reading the first char
       ;; e.g. an encoding error. Take the last Newline.
       (cons (1+ (length newlines)) 0))))

;;;; CORE OUTPUT FUNCTIONS

;;; Buffer the section of THING delimited by START and END by copying
;;; to output buffer(s) of stream.
(defun buffer-output (stream thing start end)
  (declare (index start end))
  (when (< end start)
    (error ":END before :START!"))
  (when (> end start)
    ;; Copy bytes from THING to buffers.
    (flet ((copy-to-buffer (buffer tail count)
             (declare (buffer buffer) (index tail count))
             (aver (plusp count))
             (let ((sap (buffer-sap buffer)))
               (etypecase thing
                 (system-area-pointer
                  (system-area-ub8-copy thing start sap tail count))
                 ((simple-unboxed-array (*))
                  (copy-ub8-to-system-area thing start sap tail count))))
             ;; Not INCF! If another thread has moved tail from under
             ;; us, we don't want to accidentally increment tail
             ;; beyond buffer-length.
             (setf (buffer-tail buffer) (+ count tail))
             (incf start count)))
      (tagbody
         ;; First copy is special: the buffer may already contain
         ;; something, or be even full.
         (let* ((obuf (fd-stream-obuf stream))
                (tail (buffer-tail obuf))
                (buffer-length (buffer-length obuf))
                (space (- buffer-length tail))
                (length (- end start)))
           (cond ((and (not (fd-stream-serve-events stream))
                       (>= length buffer-length))
                  (flush-output-buffer stream)
                  (finish-writing-sequence thing stream start end)
                  (return-from buffer-output))
                 ((plusp space)
                  (copy-to-buffer obuf tail (min space length))
                  (go :more-output-p))))
       :flush-and-fill
         ;; Later copies should always have an empty buffer, since
         ;; they are freshly flushed, but if another thread is
         ;; stomping on the same buffer that might not be the case.
         (let* ((obuf (flush-output-buffer stream))
                (tail (buffer-tail obuf))
                (space (- (buffer-length obuf) tail)))
           (copy-to-buffer obuf tail (min space (- end start))))
       :more-output-p
         (when (> end start)
           (go :flush-and-fill))))))

(define-symbol-macro +write-failed+ "Couldn't write to ~S")

;;; Flush the current output buffer of the stream, ensuring that the
;;; new buffer is empty. Returns (for convenience) the new output
;;; buffer -- which may or may not be EQ to the old one. If the is no
;;; queued output we try to write the buffer immediately -- otherwise
;;; we queue it for later.
(defun flush-output-buffer (stream)
  (let ((obuf (fd-stream-obuf stream)))
    (when obuf
      (let ((head (buffer-head obuf))
            (tail (buffer-tail obuf)))
        (cond ((eql head tail)
               ;; Buffer is already empty -- just ensure that is is
               ;; set to zero as well.
               (reset-buffer obuf))
              ((fd-stream-output-queue stream)
               ;; There is already stuff on the queue -- go directly
               ;; there.
               (aver (< head tail))
               (%queue-and-replace-output-buffer stream))
              (t
               ;; Try a non-blocking write, if SERVE-EVENT is allowed, queue
               ;; whatever is left over. Otherwise wait until we can write.
               (aver (< head tail))
               (when (fd-stream-synchronize-output stream)
                 (synchronize-stream-output stream))
               (loop
                 (let ((length (- tail head)))
                   (multiple-value-bind (count errno)
                       (sb-unix:unix-write (fd-stream-fd stream) (buffer-sap obuf)
                                           head length)
                     (flet ((queue-or-wait ()
                              (if (fd-stream-serve-events stream)
                                  (return (%queue-and-replace-output-buffer stream))
                                  (or (wait-until-fd-usable (fd-stream-fd stream) :output
                                                            (fd-stream-timeout stream)
                                                            nil)
                                      (signal-timeout 'io-timeout
                                                      :stream stream
                                                      :direction :output
                                                      :seconds (fd-stream-timeout stream))))))
                        (cond ((eql count length)
                               ;; Complete write -- we can use the same buffer.
                               (return (reset-buffer obuf)))
                              (count
                               ;; Partial write -- update buffer status and
                               ;; queue or wait.
                               (incf head count)
                               (setf (buffer-head obuf) head)
                               (queue-or-wait))
                              #-win32
                              ((eql errno sb-unix:ewouldblock)
                               ;; Blocking, queue or wair.
                               (queue-or-wait))
                              ;; if interrupted on win32, just try again
                              #+win32 ((eql errno sb-unix:eintr))
                              (t
                               (simple-stream-perror +write-failed+
                                                     stream errno)))))))))))))

(defun finish-writing-sequence (sequence stream start end)
  (when (fd-stream-synchronize-output stream)
    (synchronize-stream-output stream))
  (loop
   (let ((length (- end start)))
     (multiple-value-bind (count errno)
         (sb-unix:unix-write (fd-stream-fd stream) sequence start length)
       (flet ((wait ()
                (or (wait-until-fd-usable (fd-stream-fd stream) :output
                                          (fd-stream-timeout stream)
                                          nil)
                    (signal-timeout 'io-timeout
                                    :stream stream
                                    :direction :output
                                    :seconds (fd-stream-timeout stream)))))
         (cond ((eql count length)
                (return t))
               (count
                (incf start count)
                (wait))
               #-win32
               ((eql errno sb-unix:ewouldblock)
                (wait))
               ;; if interrupted on win32, just try again
               #+win32 ((eql errno sb-unix:eintr))
               (t
                (simple-stream-perror +write-failed+
                                      stream errno))))))))

;;; Helper for FLUSH-OUTPUT-BUFFER -- returns the new buffer.
(defun %queue-and-replace-output-buffer (stream)
  (aver (fd-stream-serve-events stream))
  (let ((queue (fd-stream-output-queue stream))
        (later (list (or (fd-stream-obuf stream) (bug "Missing obuf."))))
        (new (get-buffer)))
    ;; Important: before putting the buffer on queue, give the stream
    ;; a new one. If we get an interrupt and unwind losing the buffer
    ;; is relatively OK, but having the same buffer in two places
    ;; would be bad.
    (setf (fd-stream-obuf stream) new)
    (cond (queue
           (nconc queue later))
          (t
           (setf (fd-stream-output-queue stream) later)))
    (unless (fd-stream-handler stream)
      (setf (fd-stream-handler stream)
            (add-fd-handler (fd-stream-fd stream)
                            :output
                            (lambda (fd)
                              (declare (ignore fd))
                              (write-output-from-queue stream)))))
    new))

;;; This is called by the FD-HANDLER for the stream when output is
;;; possible.
(defun write-output-from-queue (stream)
  (aver (fd-stream-serve-events stream))
  (when (fd-stream-synchronize-output stream)
    (synchronize-stream-output stream))
  (let (not-first-p)
    (tagbody
     :pop-buffer
       (let* ((buffer (pop (fd-stream-output-queue stream)))
              (head (buffer-head buffer))
              (length (- (buffer-tail buffer) head)))
         (declare (index head length))
         (aver (>= length 0))
         (multiple-value-bind (count errno)
             (sb-unix:unix-write (fd-stream-fd stream) (buffer-sap buffer)
                                 head length)
           (cond ((eql count length)
                  ;; Complete write, see if we can do another right
                  ;; away, or remove the handler if we're done.
                  (release-buffer buffer)
                  (cond ((fd-stream-output-queue stream)
                         (setf not-first-p t)
                         (go :pop-buffer))
                        (t
                         (let ((handler (fd-stream-handler stream)))
                           (aver handler)
                           (setf (fd-stream-handler stream) nil)
                           (remove-fd-handler handler)))))
                 (count
                  ;; Partial write. Update buffer status and requeue.
                  (aver (< count length))
                  ;; Do not use INCF! Another thread might have moved head.
                  (setf (buffer-head buffer) (+ head count))
                  (push buffer (fd-stream-output-queue stream)))
                 (not-first-p
                  ;; We tried to do multiple writes, and finally our
                  ;; luck ran out. Requeue.
                  (push buffer (fd-stream-output-queue stream)))
                 (t
                  ;; Could not write on the first try at all!
                  #+win32
                  (simple-stream-perror +write-failed+ stream errno)
                  #-win32
                  (if (= errno sb-unix:ewouldblock)
                      (bug "Unexpected blocking in WRITE-OUTPUT-FROM-QUEUE.")
                      (simple-stream-perror +write-failed+
                                            stream errno))))))))
  nil)

;;; Try to write THING directly to STREAM without buffering, if
;;; possible. If direct write doesn't happen, buffer.
(defun write-or-buffer-output (stream thing start end)
  (declare (index start end))
  (cond ((fd-stream-output-queue stream)
         (buffer-output stream thing start end))
        ((< end start)
         (error ":END before :START!"))
        ((> end start)
         (let ((length (- end start)))
           (when (fd-stream-synchronize-output stream)
             (synchronize-stream-output stream))
           (multiple-value-bind (count errno)
               (sb-unix:unix-write (fd-stream-fd stream) thing start length)
             (cond ((eql count length)
                    ;; Complete write -- done!
                    )
                   (count
                    (aver (< count length))
                    ;; Partial write -- buffer the rest.
                    (buffer-output stream thing (+ start count) end))
                   (t
                    ;; Could not write -- buffer or error.
                    #+win32
                    (simple-stream-perror +write-failed+ stream errno)
                    #-win32
                    (if (= errno sb-unix:ewouldblock)
                        (buffer-output stream thing start end)
                        (simple-stream-perror +write-failed+ stream errno)))))))))

;;;; output routines and related noise

(define-load-time-global *output-routines* ()
  "List of all available output routines. Each element is a list of the
  element-type output, the kind of buffering, the function name, and the number
  of bytes per element.")

(defun stream-errno-to-condition (errno)
  (case errno
    (#-win32 #.sb-unix:epipe
     #+win32 #.sb-win32::error-no-data
     'broken-pipe)
    (t 'simple-stream-error)))

;;; common idioms for reporting low-level stream and file problems
(define-error-wrapper simple-stream-perror (format-control stream &optional errno &rest format-arguments)
  (error (stream-errno-to-condition errno)
         :stream stream
         :format-control "~@<~?~@[: ~2I~_~A~]~:>"
         :format-arguments (list format-control
                                 (list* stream format-arguments)
                                 (when errno (strerror errno)))))

(define-error-wrapper file-perror (pathname errno &optional datum &rest arguments)
  (let ((message (when errno (strerror errno))))
    (multiple-value-bind (condition-type arguments)
        (typecase datum
          (format-control
           (values 'simple-file-error (list :format-control datum
                                            :format-arguments arguments)))
          (t
           (values datum arguments)))
      (apply #'error condition-type :pathname pathname :message message
             arguments))))

(define-error-wrapper c-string-encoding-error (external-format code)
  (error 'c-string-encoding-error
         :external-format external-format
         :code code))

(macrolet ((sap-ref-octets (sap offset count)
             `(let ((.buffer.
                     (make-array (the fixnum ,count) :element-type '(unsigned-byte 8))))
                (%byte-blt ,sap ,offset .buffer. 0 ,count)
                .buffer.)))

(define-error-wrapper c-string-decoding-error (external-format sap offset count)
  (error 'c-string-decoding-error
         :external-format external-format
         :octets (sap-ref-octets sap offset count)))

;;; Returning true goes into end of file handling, false will enter another
;;; round of input buffer filling followed by re-entering character decode.
(defun stream-decoding-error-and-handle (stream octet-count stream-unit-count)
  (let ((external-format (stream-external-format stream))
        (replacement (fd-stream-replacement stream)))
    (labels ((replacement (thing resyncp)
               (let* ((string (decoding-replacement-stringify thing external-format))
                      (reversed (reverse string))
                      (instead (fd-stream-instead stream)))
                 (dotimes (i (length reversed))
                   (vector-push-extend (char reversed i) instead))
                 (when (> (length reversed) 0)
                   (setf (fd-stream-listen stream) t))
                 (if resyncp
                     (resync)
                     (advance))))
             (resync ()
               (fd-stream-resync stream)
               nil)
             (advance ()
               (fd-stream-advance stream stream-unit-count)
               nil))
      (if replacement
          (replacement replacement nil)
          (restart-case
              (error 'stream-decoding-error
                     :external-format external-format
                     :stream stream
                     :octets (let ((buffer (fd-stream-ibuf stream)))
                               (sap-ref-octets (buffer-sap buffer)
                                               (buffer-head buffer)
                                               octet-count)))
            (attempt-resync ()
              :report (lambda (stream)
                        (format stream
                                "~@<Attempt to resync the stream at a ~
                        character boundary and continue.~@:>"))
              (resync))
            (force-end-of-file ()
              :report (lambda (stream)
                        (format stream "~@<Force an end of file.~@:>"))
              (setf (fd-stream-eof-forced-p stream) t))
            (use-value (replacement)
              :report (lambda (stream)
                        (format stream "~@<Use datum as replacement input ~
                                        and continue.~@:>"))
              :interactive (lambda ()
                             (read-evaluated-form
                              "Replacement byte, bytes, character, or string (evaluated): "))
              (replacement replacement nil))
            (input-replacement (thing)
              :report (lambda (stream)
                        (format stream "~@<Use string as replacement input, ~
                               attempt to resync at a character ~
                               boundary and continue.~@:>"))
              :interactive (lambda ()
                             (format *query-io* "~@<Enter a string: ~@:>")
                             (finish-output *query-io*)
                             (list (read *query-io*)))
              (replacement thing t)))))))
) ; end MACROLET

(defun encoding-replacement-adjust-charpos (replacement stream)
  (typecase replacement
    (character (if (char= replacement #\Newline)
                   (setf (fd-stream-output-column stream) 0)
                   (incf (fd-stream-output-column stream) 1)))
    (string (let ((newline-pos (position #\Newline replacement :from-end t)))
              (if newline-pos
                  (setf (fd-stream-output-column stream) (- (length replacement) newline-pos 1))
                  (incf (fd-stream-output-column stream) (length replacement)))))
    ((unsigned-byte 8))
    ((simple-array (unsigned-byte 8) 1))))

(defun stream-encoding-error-and-handle (stream code)
  (let ((external-format (stream-external-format stream))
        (replacement (fd-stream-replacement stream)))
    (labels ((replacement (thing)
               (let ((octets (encoding-replacement-octetify thing external-format)))
                 (ecase (fd-stream-buffering stream)
                   (:full (buffer-output stream octets 0 (length octets)))
                   (:line (buffer-output stream octets 0 (length octets)))
                   (:none (write-or-buffer-output stream octets 0 (length octets))))
                 (encoding-replacement-adjust-charpos thing stream))))
      (if replacement
          (replacement replacement)
          (restart-case
              (error 'stream-encoding-error
                     :external-format external-format
                     :stream stream
                     :code code)
            (output-nothing ()
              :report (lambda (stream)
                        (format stream "~@<Skip output of this character.~@:>")))
            (use-value (replacement)
              :report (lambda (stream)
                        (format stream "~@<Use datum as replacement output.~@:>"))
              :interactive (lambda ()
                             (read-evaluated-form
                              "Replacement byte, bytes, character, or string (evaluated): "))
              (replacement replacement))
            (output-replacement (string)
              :report (lambda (stream)
                        (format stream "~@<Output replacement string.~@:>"))
              :interactive (lambda ()
                             (format *query-io* "~@<Enter a string: ~@:>")
                             (finish-output *query-io*)
                             (list (read *query-io*)))
              (let ((string (string string)))
                (fd-sout stream (string string) 0 (length string)))))))))

(defun %external-format-encoding-error (stream code)
  (if (streamp stream)
      (stream-encoding-error-and-handle stream code)
      (c-string-encoding-error stream code)))

(defmacro external-format-encoding-error (stream code)
  `(return-from output-nothing (%external-format-encoding-error ,stream ,code)))

(defun synchronize-stream-output (stream)
  ;; If we're reading and writing on the same file, flush buffered
  ;; input and rewind file position accordingly.
  (when (fd-stream-synchronize-output stream)
    (let ((adjust (nth-value 1 (flush-input-buffer stream))))
      (unless (eql 0 adjust)
        (sb-unix:unix-lseek (fd-stream-fd stream) (- adjust) sb-unix:l_incr)))))

(defun fd-stream-output-finished-p (stream)
  (let ((obuf (fd-stream-obuf stream)))
    (or (not obuf)
        (and (zerop (buffer-tail obuf))
             (not (fd-stream-output-queue stream))))))

(defmacro output-wrapper/variable-width ((stream size buffering restart)
                                         &body body)
  (let ((stream-var '#:stream))
    `(let* ((,stream-var ,stream)
            (obuf (fd-stream-obuf ,stream-var))
            (tail (buffer-tail obuf))
            (size ,size))
      ,@(unless (eq (car buffering) :none)
         `((when (< (buffer-length obuf) (+ tail size))
            (setf obuf (flush-output-buffer ,stream-var)
                  tail (buffer-tail obuf)))))
      ,@(unless (eq (car buffering) :none)
         ;; FIXME: Why this here? Doesn't seem necessary.
         `((when (fd-stream-synchronize-output ,stream-var)
             (synchronize-stream-output ,stream-var))))
      ,(if restart
           `(block output-nothing
              ,@body
              (setf (buffer-tail obuf) (+ tail size)))
           `(progn
             ,@body
             (setf (buffer-tail obuf) (+ tail size))))
      ,@(ecase (car buffering)
         (:none `((flush-output-buffer ,stream-var)))
         (:line `((when (eql |ch| #\Newline)
                    (flush-output-buffer ,stream-var))))
         (:full)))))

(defmacro output-wrapper ((stream size buffering restart) &body body)
  (let ((stream-var '#:stream))
    `(let* ((,stream-var ,stream)
            (obuf (fd-stream-obuf ,stream-var))
            (tail (buffer-tail obuf)))
       ,@(unless (eq (car buffering) :none)
          `((when (< (buffer-length obuf) (+ tail ,size))
             (setf obuf (flush-output-buffer ,stream-var)
                   tail (buffer-tail obuf)))))
       ;; FIXME: Why this here? Doesn't seem necessary.
       ,@(unless (eq (car buffering) :none)
          `((synchronize-stream-output ,stream-var)))
       ,(if restart
            `(block output-nothing
               ,@body
               (setf (buffer-tail obuf) (+ tail ,size)))
            `(progn
               ,@body
               (setf (buffer-tail obuf) (+ tail ,size))))
       ,@(ecase (car buffering)
          (:none `((flush-output-buffer ,stream-var)))
          (:line `((when (eql |ch| #\Newline)
                     (flush-output-buffer ,stream-var))))
          (:full)))))

(defmacro def-output-routines/variable-width
    ((name-fmt size restart external-format &rest bufferings)
     &body body)
  (cons 'progn
        (mapcan
         (lambda (buffering)
           (let ((function
                  (intern (format nil name-fmt (string (car buffering))))))
             (list
                  `(defun ,function (stream |ch|)
                     (declare (optimize (sb-c:verify-arg-count 0)))
                     (output-wrapper/variable-width (stream ,size ,buffering ,restart)
                       ,@body)
                     ;; return char so WRITE-CHAR can tail-call the stream's output method
                     |ch|)
                  ;; FIXME: technically illegal use of quoted constant, as we keep NCONCing
                  ;; onto the tail of a literal
                  `(setf *output-routines*
                         (nconc *output-routines*
                                ',(mapcar
                                   (lambda (type)
                                     (list type
                                           (car buffering)
                                           function
                                           1
                                           external-format))
                                   (cdr buffering)))))))
         bufferings)))

;;; Define output routines that output numbers SIZE bytes long for the
;;; given bufferings. Use BODY to do the actual output.
(defmacro def-output-routines ((name-fmt size restart &rest bufferings)
                               &body body)
  (cons 'progn
        (mapcan
         (lambda (buffering)
           (let ((function
                     (intern (format nil name-fmt (string (car buffering))))))
             (list
                 `(defun ,function (stream byte)
                    (declare (optimize (sb-c:verify-arg-count 0)))
                     (output-wrapper (stream ,size ,buffering ,restart)
                       ,@body)
                     ;; return byte so WRITE-BYTE can tail-call the stream's output method
                     byte)
                 `(setf *output-routines*
                         (nconc *output-routines*
                                ',(mapcar
                                   (lambda (type)
                                     (list type
                                           (car buffering)
                                           function
                                           size
                                           nil))
                                   (cdr buffering)))))))
         bufferings)))

(def-output-routines ("OUTPUT-UNSIGNED-BYTE-~A-BUFFERED"
                      1
                      nil
                      (:none (unsigned-byte 8))
                      (:full (unsigned-byte 8)))
  (setf (sap-ref-8 (buffer-sap obuf) tail)
        byte))

(def-output-routines ("OUTPUT-SIGNED-BYTE-~A-BUFFERED"
                      1
                      nil
                      (:none (signed-byte 8))
                      (:full (signed-byte 8)))
  (setf (signed-sap-ref-8 (buffer-sap obuf) tail)
        byte))

(def-output-routines ("OUTPUT-UNSIGNED-SHORT-~A-BUFFERED"
                      2
                      nil
                      (:none (unsigned-byte 16))
                      (:full (unsigned-byte 16)))
  (setf (sap-ref-16 (buffer-sap obuf) tail)
        byte))

(def-output-routines ("OUTPUT-SIGNED-SHORT-~A-BUFFERED"
                      2
                      nil
                      (:none (signed-byte 16))
                      (:full (signed-byte 16)))
  (setf (signed-sap-ref-16 (buffer-sap obuf) tail)
        byte))

(def-output-routines ("OUTPUT-UNSIGNED-LONG-~A-BUFFERED"
                      4
                      nil
                      (:none (unsigned-byte 32))
                      (:full (unsigned-byte 32)))
  (setf (sap-ref-32 (buffer-sap obuf) tail)
        byte))

(def-output-routines ("OUTPUT-SIGNED-LONG-~A-BUFFERED"
                      4
                      nil
                      (:none (signed-byte 32))
                      (:full (signed-byte 32)))
  (setf (signed-sap-ref-32 (buffer-sap obuf) tail)
        byte))

#+64-bit
(progn
  (def-output-routines ("OUTPUT-UNSIGNED-LONG-LONG-~A-BUFFERED"
                        8
                        nil
                        (:none (unsigned-byte 64))
                        (:full (unsigned-byte 64)))
    (setf (sap-ref-64 (buffer-sap obuf) tail)
          byte))
  (def-output-routines ("OUTPUT-SIGNED-LONG-LONG-~A-BUFFERED"
                        8
                        nil
                        (:none (signed-byte 64))
                        (:full (signed-byte 64)))
    (setf (signed-sap-ref-64 (buffer-sap obuf) tail)
          byte)))

;;; the routine to use to output a string. If the stream is
;;; unbuffered, slam the string down the file descriptor, otherwise
;;; use BUFFER-OUTPUT to buffer the string. Update charpos by
;;; checking to see where the last newline was.
(defun fd-sout (stream thing start end)
  (declare (type fd-stream stream) (type string thing))
  (let ((start (or start 0))
        (end (or end (length (the vector thing)))))
    (declare (fixnum start end))
    (let ((last-newline
           (string-dispatch (simple-base-string
                             #+sb-unicode
                             (simple-array character (*))
                             string)
               thing
             (position #\newline thing :from-end t
                       :start start :end end))))
      (if (and (typep thing 'base-string)
               (let ((external-format (fd-stream-external-format stream)))
                 (and (memq (external-format-keyword external-format)
                            '(#+sb-unicode :utf-8 :latin-1))
                      (or (null last-newline)
                          (eq (external-format-newline-variant external-format) :lf)))))
          (ecase (fd-stream-buffering stream)
            (:full
             (buffer-output stream thing start end))
            (:line
             (buffer-output stream thing start end)
             (when last-newline
               (flush-output-buffer stream)))
            (:none
             (write-or-buffer-output stream thing start end)))
          (ecase (fd-stream-buffering stream)
            (:full (funcall (fd-stream-output-bytes stream)
                            stream thing nil start end))
            (:line (funcall (fd-stream-output-bytes stream)
                            stream thing last-newline start end))
            (:none (funcall (fd-stream-output-bytes stream)
                            stream thing t start end))))
      (if last-newline
          (setf (fd-stream-output-column stream) (- end last-newline 1))
          (incf (fd-stream-output-column stream) (- end start))))))

(defstruct (external-format
             (:constructor %make-external-format)
             (:conc-name ef-)
             (:predicate external-format-p)
             (:copier %copy-external-format))
  ;; All the names that can refer to this external format.  The first
  ;; one is the canonical name.
  (names (missing-arg) :type list :read-only t)
  (newline-variant (missing-arg) :type (member :crlf :lf :cr) :read-only t)
  (default-replacement-character (missing-arg) :type character)
  (replacement nil :type (or null character string (unsigned-byte 8) (simple-array (unsigned-byte 8) 1)))
  (read-n-chars-fun (missing-arg) :type function)
  (read-char-fun (missing-arg) :type function)
  (write-n-bytes-fun (missing-arg) :type function)
  (write-char-none-buffered-fun (missing-arg) :type function)
  (write-char-line-buffered-fun (missing-arg) :type function)
  (write-char-full-buffered-fun (missing-arg) :type function)
  ;; Can be nil for fixed-width formats.
  (resync-fun nil :type (or function null))
  (bytes-for-char-fun (missing-arg) :type function)
  (read-c-string-fun (missing-arg) :type function)
  (write-c-string-fun (missing-arg) :type function)
  (octets-to-string-fun (missing-arg) :type function)
  (string-to-octets-fun (missing-arg) :type function))
(declaim (freeze-type external-format))

(defun ef-char-size (ef-entry)
  (if (variable-width-external-format-p ef-entry)
      (bytes-for-char-fun ef-entry)
      (funcall (bytes-for-char-fun ef-entry) #\x)))

(defun sb-alien::string-to-c-string (string external-format)
  (declare (type simple-string string)
           (explicit-check :result))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (get-external-format-or-lose external-format)))
      (funcall (ef-write-c-string-fun external-format) string))))

(defun sb-alien::c-string-to-string (sap external-format element-type)
  (declare (type system-area-pointer sap)
           (explicit-check :result))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (get-external-format-or-lose external-format)))
      (funcall (ef-read-c-string-fun external-format) sap element-type))))

(defun get-external-format-or-lose (external-format)
  (or (get-external-format external-format)
      (error "Undefined external-format: ~S" external-format)))

(defun external-format-keyword (external-format)
  (typecase external-format
    (keyword external-format)
    ((cons keyword) (car external-format))))

(defun external-format-newline-variant (external-format)
  (typecase external-format
    (keyword :lf)
    ((cons keyword) (getf (cdr external-format) :newline :lf))))

(defun canonize-external-format (external-format entry)
  (typecase external-format
    (keyword (first (ef-names entry)))
    ((cons keyword) (cons (first (ef-names entry)) (rest external-format)))))

;;; Find an output routine to use given the type and buffering. Return
;;; as multiple values the routine, the real type transfered, and the
;;; number of bytes per element.
(defun pick-output-routine (type buffering &optional entry)
  (when (subtypep type 'character)
    (return-from pick-output-routine
      (values (ecase buffering
                (:none (ef-write-char-none-buffered-fun entry))
                (:line (ef-write-char-line-buffered-fun entry))
                (:full (ef-write-char-full-buffered-fun entry)))
              'character
              1
              (ef-write-n-bytes-fun entry)
              (ef-char-size entry)
              (ef-replacement entry))))
  (dolist (entry *output-routines*)
    (when (and (subtypep type (first entry))
               (eq buffering (second entry))
               (not (fifth entry)))
      (return-from pick-output-routine
        (values (symbol-function (third entry))
                (first entry)
                (fourth entry)))))
  ;; KLUDGE: dealing with the buffering here leads to excessive code
  ;; explosion.
  ;;
  ;; KLUDGE: also see comments in PICK-INPUT-ROUTINE
  (loop for i from 40 by 8 to 1024 ; ARB (KLUDGE)
        if (subtypep type `(unsigned-byte ,i))
        do (return-from pick-output-routine
             (values
              (ecase buffering
                (:none
                 (lambda (stream byte)
                   (output-wrapper (stream (/ i 8) (:none) nil)
                     (loop for j from 0 below (/ i 8)
                           do (setf (sap-ref-8 (buffer-sap obuf)
                                               (+ j tail))
                                    (ldb (byte 8 (- i 8 (* j 8))) byte))))))
                (:full
                 (lambda (stream byte)
                   (output-wrapper (stream (/ i 8) (:full) nil)
                     (loop for j from 0 below (/ i 8)
                           do (setf (sap-ref-8 (buffer-sap obuf)
                                               (+ j tail))
                                    (ldb (byte 8 (- i 8 (* j 8))) byte)))))))
              `(unsigned-byte ,i)
              (/ i 8))))
  (loop for i from 40 by 8 to 1024 ; ARB (KLUDGE)
        if (subtypep type `(signed-byte ,i))
        do (return-from pick-output-routine
             (values
              (ecase buffering
                (:none
                 (lambda (stream byte)
                   (output-wrapper (stream (/ i 8) (:none) nil)
                     (loop for j from 0 below (/ i 8)
                           do (setf (sap-ref-8 (buffer-sap obuf)
                                               (+ j tail))
                                    (ldb (byte 8 (- i 8 (* j 8))) byte))))))
                (:full
                 (lambda (stream byte)
                   (output-wrapper (stream (/ i 8) (:full) nil)
                     (loop for j from 0 below (/ i 8)
                           do (setf (sap-ref-8 (buffer-sap obuf)
                                               (+ j tail))
                                    (ldb (byte 8 (- i 8 (* j 8))) byte)))))))
              `(signed-byte ,i)
              (/ i 8)))))

;;;; input routines and related noise

;;; a list of all available input routines. Each element is a list of
;;; the element-type input, the function name, and the number of bytes
;;; per element.
(define-load-time-global *input-routines* ())

;;; Return whether a primitive partial read operation on STREAM's FD
;;; would (probably) block.  Signal a `simple-stream-error' if the
;;; system call implementing this operation fails.
;;;
;;; It is "may" instead of "would" because "would" is not quite
;;; correct on win32.  However, none of the places that use it require
;;; further assurance than "may" versus "will definitely not".
(defun sysread-may-block-p (stream)
  #+win32
  ;; This answers T at EOF on win32.
  (not (sb-win32:handle-listen (fd-stream-fd stream)))
  #-win32
  (not (sb-unix:unix-simple-poll (fd-stream-fd stream) :input 0)))

;;; If the read would block wait (using SERVE-EVENT) till input is available,
;;; then fill the input buffer, and return the number of bytes read. Throws
;;; to EOF-INPUT-CATCHER if the eof was reached.
(defun refill-input-buffer (stream)
  (let ((fd (fd-stream-fd stream))
        (errno 0)
        (count 0))
    (tagbody
       #+win32
       (go :main)

       ;; Check for blocking input before touching the stream if we are to
       ;; serve events: if the FD is blocking, we don't want to try an uninterruptible
       ;; read(). Regular files should never block, so we can elide the check.
       (if (and (neq :regular (fd-stream-fd-type stream))
                (sysread-may-block-p stream))
           (go :wait-for-input)
           (go :main))
       ;; These (:CLOSED-FLAME and :READ-ERROR) tags are here so what
       ;; we can signal errors outside the WITHOUT-INTERRUPTS.
     :closed-flame
       (closed-flame stream)
     :read-error
       (simple-stream-perror "couldn't read from ~S" stream errno)
     :wait-for-input
       ;; This tag is here so we can unwind outside the WITHOUT-INTERRUPTS
       ;; to wait for input if read tells us EWOULDBLOCK.
       (unless (wait-until-fd-usable fd :input (fd-stream-timeout stream)
                                     (fd-stream-serve-events stream))
         (signal-timeout 'io-timeout
                         :stream stream
                         :direction :input
                         :seconds (fd-stream-timeout stream)))
     :main
       ;; Since the read should not block, we'll disable the
       ;; interrupts here, so that we don't accidentally unwind and
       ;; leave the stream in an inconsistent state.

       ;; Execute the nlx outside without-interrupts to ensure the
       ;; resulting thunk is stack-allocatable.
       ((lambda (return-reason)
          (ecase return-reason
            ((nil))                     ; fast path normal cases
            ((:wait-for-input) (go #-win32 :wait-for-input #+win32 :main))
            ((:closed-flame)   (go :closed-flame))
            ((:read-error)     (go :read-error))))
        (without-interrupts
          ;; Check the buffer: if it is null, then someone has closed
          ;; the stream from underneath us. This is not ment to fix
          ;; multithreaded races, but to deal with interrupt handlers
          ;; closing the stream.
          (block nil
            (prog1 nil
              (let* ((ibuf (or (fd-stream-ibuf stream) (return :closed-flame)))
                     (sap (buffer-sap ibuf))
                     (length (buffer-length ibuf))
                     (head (buffer-head ibuf))
                     (tail (buffer-tail ibuf)))
                (declare (index length head tail)
                         (inline sb-unix:unix-read))
                (unless (zerop head)
                  (cond ((eql head tail)
                         ;; Buffer is empty, but not at yet reset -- make it so.
                         (setf head 0
                               tail 0)
                         (reset-buffer ibuf))
                        (t
                         ;; Buffer has things in it, but they are not at the
                         ;; head -- move them there.
                         (let ((n (- tail head)))
                           (system-area-ub8-copy sap head sap 0 n)
                           (setf head 0
                                 (buffer-head ibuf) head
                                 tail n
                                 (buffer-tail ibuf) tail)))))
                (setf (fd-stream-listen stream) nil)
                (setf (values count errno)
                      (sb-unix:unix-read fd (sap+ sap tail) (- length tail)))
                (cond ((null count)
                       (if (eql errno
                                #+win32 sb-unix:eintr
                                #-win32 sb-unix:ewouldblock)
                           (return :wait-for-input)
                           (return :read-error)))
                      ((zerop count)
                       (setf (fd-stream-listen stream) :eof)
                       (/show0 "THROWing EOF-INPUT-CATCHER")
                       (throw 'eof-input-catcher nil))
                      (t
                       ;; Success! (Do not use INCF, for sake of other threads.)
                       (setf (buffer-tail ibuf) (+ count tail))))))))))
    count))

;;; Make sure there are at least BYTES number of bytes in the input
;;; buffer. Keep calling REFILL-INPUT-BUFFER until that condition is met.
(defmacro input-at-least (stream bytes)
  (let ((stream-var (gensym "STREAM"))
        (bytes-var (gensym "BYTES"))
        (buffer-var (gensym "IBUF")))
    `(let* ((,stream-var ,stream)
            (,bytes-var ,bytes)
            (,buffer-var (fd-stream-ibuf ,stream-var)))
       (loop
         (when (>= (- (buffer-tail ,buffer-var)
                      (buffer-head ,buffer-var))
                   ,bytes-var)
           (return))
         (refill-input-buffer ,stream-var)))))

(defmacro input-wrapper/variable-width ((stream bytes eof-error eof-value)
                                        &body read-forms)
  (let ((stream-var (gensym "STREAM"))
        (retry-var (gensym "RETRY"))
        (element-var (gensym "ELT")))
    `(let* ((,stream-var ,stream)
            (ibuf (fd-stream-ibuf ,stream-var))
            (size nil)
            (unit ,(if (consp bytes)
                       (let ((size-info (car bytes)))
                         (if (consp size-info)
                             (cadr size-info)
                             size-info))
                       bytes)))
       (block use-instead
         (when (fd-stream-eof-forced-p ,stream-var)
           (setf (fd-stream-eof-forced-p ,stream-var) nil)
           (return-from use-instead
             (eof-or-lose ,stream-var ,eof-error ,eof-value)))
         (let ((,element-var nil)
               (decode-break-reason nil))
           (do ((,retry-var t))
               ((not ,retry-var))
             (if (> (length (fd-stream-instead ,stream-var)) 0)
                 (let* ((instead (fd-stream-instead ,stream-var))
                        (result (vector-pop instead))
                        (pointer (fill-pointer instead)))
                   (when (= pointer 0)
                     (setf (fd-stream-listen ,stream-var) nil))
                   (setf (buffer-prev-head ibuf) (buffer-head ibuf))
                   (return-from use-instead (values result 0)))
                 (unless
                     (catch 'eof-input-catcher
                       (setf decode-break-reason
                             (block decode-break-reason
                               ,(if (consp bytes)
                                    (let ((size-info (car bytes)))
                                      (if (consp size-info)
                                          `(progn
                                             (input-at-least ,stream-var ,(cadr size-info))
                                             (catch 'eof-input-catcher
                                               (input-at-least ,stream-var ,(car size-info))))
                                          `(input-at-least ,stream-var ,size-info)))
                                    `(input-at-least ,stream-var (setq size ,bytes)))
                               (let* ((byte (sap-ref-8 (buffer-sap ibuf) (buffer-head ibuf))))
                                 (declare (ignorable byte))
                                 ,@(when (consp bytes)
                                     `((let ((sap (buffer-sap ibuf))
                                             (head (buffer-head ibuf))
                                             (tail (buffer-tail ibuf)))
                                         (declare (ignorable sap head tail))
                                         (setq size ,(cadr bytes))
                                         (input-at-least ,stream-var size))))
                                 (setq ,element-var (locally ,@read-forms))
                                 (setq ,retry-var nil))
                               nil))
                       (when decode-break-reason
                         (when (stream-decoding-error-and-handle
                                stream decode-break-reason unit)
                           (setq ,retry-var nil)
                           (throw 'eof-input-catcher nil)))
                       t)
                   (let ((octet-count (- (buffer-tail ibuf)
                                         (buffer-head ibuf))))
                     (when (or (zerop octet-count)
                               (and (not ,element-var)
                                    (not decode-break-reason)
                                    (stream-decoding-error-and-handle
                                     stream octet-count unit)))
                       (setq ,retry-var nil))))))
           (cond (,element-var
                  (setf (buffer-prev-head ibuf) (buffer-head ibuf))
                  (incf (buffer-head ibuf) size)
                  (values ,element-var size))
                 (t
                  (eof-or-lose ,stream-var ,eof-error ,eof-value))))))))

;;; a macro to wrap around all input routines to handle EOF-ERROR noise
(defmacro input-wrapper ((stream bytes eof-error eof-value) &body read-forms)
  (let ((stream-var (gensym "STREAM"))
        (element-var (gensym "ELT")))
    `(let* ((,stream-var ,stream)
            (ibuf (fd-stream-ibuf ,stream-var)))
       (if (> (length (fd-stream-instead ,stream-var)) 0)
           (bug "INSTEAD not empty in INPUT-WRAPPER for ~S" ,stream-var)
           (let ((,element-var
                  (catch 'eof-input-catcher
                    (input-at-least ,stream-var ,bytes)
                    (locally ,@read-forms))))
             (cond (,element-var
                    (incf (buffer-head (fd-stream-ibuf ,stream-var)) ,bytes)
                    ,element-var)
                   (t
                    (eof-or-lose ,stream-var ,eof-error ,eof-value))))))))

(defmacro def-input-routine/variable-width (name
                                            (type external-format size sap head)
                                            &rest body)
  `(progn
     (defun ,name (stream eof-error eof-value)
       (declare (optimize (sb-c:verify-arg-count 0)))
       (input-wrapper/variable-width (stream ,size eof-error eof-value)
         (let ((,sap (buffer-sap ibuf))
               (,head (buffer-head ibuf)))
           ,@body)))
     (setf *input-routines*
           (nconc *input-routines*
                  (list (list ',type ',name 1 ',external-format))))))

(defmacro def-input-routine (name
                             (type size sap head)
                             &rest body)
  `(progn
     (defun ,name (stream eof-error eof-value)
       (declare (optimize (sb-c:verify-arg-count 0)))
       (input-wrapper (stream ,size eof-error eof-value)
         (let ((,sap (buffer-sap ibuf))
               (,head (buffer-head ibuf)))
           ,@body)))
     (setf *input-routines*
           (nconc *input-routines*
                  (list (list ',type ',name ',size nil))))))

;;; STREAM-IN routine for reading a string char
(def-input-routine input-character
                   (character 1 sap head)
  (code-char (sap-ref-8 sap head)))

;;; STREAM-IN routine for reading an unsigned 8 bit number
(def-input-routine input-unsigned-8bit-byte
                   ((unsigned-byte 8) 1 sap head)
  (sap-ref-8 sap head))

;;; STREAM-IN routine for reading a signed 8 bit number
(def-input-routine input-signed-8bit-number
                   ((signed-byte 8) 1 sap head)
  (signed-sap-ref-8 sap head))

;;; STREAM-IN routine for reading an unsigned 16 bit number
(def-input-routine input-unsigned-16bit-byte
                   ((unsigned-byte 16) 2 sap head)
  (sap-ref-16 sap head))

;;; STREAM-IN routine for reading a signed 16 bit number
(def-input-routine input-signed-16bit-byte
                   ((signed-byte 16) 2 sap head)
  (signed-sap-ref-16 sap head))

;;; STREAM-IN routine for reading a unsigned 32 bit number
(def-input-routine input-unsigned-32bit-byte
                   ((unsigned-byte 32) 4 sap head)
  (sap-ref-32 sap head))

;;; STREAM-IN routine for reading a signed 32 bit number
(def-input-routine input-signed-32bit-byte
                   ((signed-byte 32) 4 sap head)
  (signed-sap-ref-32 sap head))

#+64-bit
(progn
  (def-input-routine input-unsigned-64bit-byte
      ((unsigned-byte 64) 8 sap head)
    (sap-ref-64 sap head))
  (def-input-routine input-signed-64bit-byte
      ((signed-byte 64) 8 sap head)
    (signed-sap-ref-64 sap head)))

;;; Find an input routine to use given the type. Return as multiple
;;; values the routine, the real type transfered, and the number of
;;; bytes per element (and for character types string input routine).
(defun pick-input-routine (type &optional entry)
  (when (subtypep type 'character)
    (return-from pick-input-routine
      (values (ef-read-char-fun entry)
              'character
              1
              (ef-read-n-chars-fun entry)
              (ef-char-size entry)
              (ef-replacement entry))))
  (dolist (entry *input-routines*)
    (when (and (subtypep type (first entry))
               (not (fourth entry)))
      (return-from pick-input-routine
        (values (symbol-function (second entry))
                (first entry)
                (third entry)))))
  ;; FIXME: let's do it the hard way, then (but ignore things like
  ;; endianness, efficiency, and the necessary coupling between these
  ;; and the output routines).  -- CSR, 2004-02-09
  (loop for i from 40 by 8 to 1024 ; ARB (well, KLUDGE really)
        if (subtypep type `(unsigned-byte ,i))
        do (return-from pick-input-routine
             (values
              (lambda (stream eof-error eof-value)
                (input-wrapper (stream (/ i 8) eof-error eof-value)
                  (let ((sap (buffer-sap ibuf))
                        (head (buffer-head ibuf)))
                    (loop for j from 0 below (/ i 8)
                          with result = 0
                          do (setf result
                                   (+ (* 256 result)
                                      (sap-ref-8 sap (+ head j))))
                          finally (return result)))))
              `(unsigned-byte ,i)
              (/ i 8))))
  (loop for i from 40 by 8 to 1024 ; ARB (well, KLUDGE really)
        if (subtypep type `(signed-byte ,i))
        do (return-from pick-input-routine
             (values
              (lambda (stream eof-error eof-value)
                (input-wrapper (stream (/ i 8) eof-error eof-value)
                  (let ((sap (buffer-sap ibuf))
                        (head (buffer-head ibuf)))
                    (loop for j from 0 below (/ i 8)
                          with result = 0
                          do (setf result
                                   (+ (* 256 result)
                                      (sap-ref-8 sap (+ head j))))
                          finally (return (if (logbitp (1- i) result)
                                              (dpb result (byte i 0) -1)
                                              result))))))
              `(signed-byte ,i)
              (/ i 8)))))

;;; the N-BIN method for binary FD-STREAMs
;;;
;;; Note that this blocks in UNIX-READ. It is generally used where
;;; there is a definite amount of reading to be done, so blocking
;;; isn't too problematical.
(defun fd-stream-read-n-bytes (stream buffer sbuffer start requested eof-error-p
                               &aux (total-copied 0))
  (declare (type fd-stream stream))
  (declare (type index start requested total-copied))
  (declare (ignore sbuffer))
  (aver (= (length (fd-stream-instead stream)) 0))
  (let* ((ibuf (fd-stream-ibuf stream))
         (sap (buffer-sap ibuf)))
    (cond #+soft-card-marks ; read(2) doesn't like write-protected buffers
          ((and (typep buffer '(simple-array (unsigned-byte 8) (*)))
                (>= requested 256)
                (eq (fd-stream-fd-type stream) :regular)
                ;; TODO: handle non-empty initial buffers
                (= (buffer-head ibuf) (buffer-tail ibuf)))
           (prog ((fd (fd-stream-fd stream))
                  (offset start)
                  (errno 0)
                  (count 0))
              (declare ((or null index) count offset))
              (go :read)
            :read-error
              (simple-stream-perror "couldn't read from ~S" stream errno)
            :eof
              (if eof-error-p
                  (error 'end-of-file :stream stream)
                  (return total-copied))
            :read
              (without-interrupts
                (tagbody
                 :read
                   (with-pinned-objects (buffer)
                     (let ((sap (vector-sap buffer)))
                       (declare (inline sb-unix:unix-read))
                       (setf (fd-stream-listen stream) nil)
                       (setf (values count errno)
                             (sb-unix:unix-read fd (sap+ sap offset) (- requested total-copied)))
                       (cond ((null count)
                              (cond #-win32 ((eql errno sb-unix:eintr)
                                             (go :read))
                                    (t
                                     (go :read-error))))
                             ((zerop count)
                              (setf (fd-stream-listen stream) :eof)
                              (go :eof))
                             (t
                              (setf offset (truly-the index (+ offset count)))
                              (setf total-copied (truly-the index (+ total-copied count)))))
                       (when (= requested total-copied)
                         (return total-copied))
                       (go :read)))))))
          (t
           (do ()
               (nil)
             (let* ((remaining-request (- requested total-copied))
                    (head (buffer-head ibuf))
                    (tail (buffer-tail ibuf))
                    (available (- tail head))
                    (n-this-copy (min remaining-request available))
                    (this-start (+ start total-copied)))
               (declare (type index remaining-request head tail available))
               (declare (type index n-this-copy))
               ;; Copy data from stream buffer into user's buffer.
               (%byte-blt sap head buffer this-start n-this-copy)
               (incf (buffer-head ibuf) n-this-copy)
               (incf total-copied n-this-copy)
               ;; Maybe we need to refill the stream buffer.
               (cond (;; If there were enough data in the stream buffer, we're done.
                      (eql total-copied requested)
                      (return total-copied))
                     (;; If EOF, we're done in another way.
                      (null (catch 'eof-input-catcher (refill-input-buffer stream)))
                      (if eof-error-p
                          (error 'end-of-file :stream stream)
                          (return total-copied)))
                     ;; Otherwise we refilled the stream buffer, so fall
                     ;; through into another pass of the loop.
                     )))))))

(defun fd-stream-advance (stream unit)
  (let* ((buffer (fd-stream-ibuf stream))
         (head (buffer-head buffer)))
    (catch 'eof-input-catcher
      (input-at-least stream unit)
      ;; OK: we have another unit
      (setf (buffer-prev-head buffer) head)
      (setf (buffer-head buffer) (+ head unit))
      (return-from fd-stream-advance nil))
    ;; we do not have enough input: set the HEAD to TAIL and let the next caller
    ;; deal with EOF.
    (setf (buffer-head buffer) (buffer-tail buffer))
    nil))

(defun fd-stream-resync (stream)
  (let ((entry (get-external-format (fd-stream-external-format stream))))
    (when entry
      (funcall (ef-resync-fun entry) stream))))

(defun get-fd-stream-character-sizer (stream)
  (let ((entry (get-external-format (fd-stream-external-format stream))))
    (when entry
      (ef-bytes-for-char-fun entry))))

(defun fd-stream-character-size (stream char)
  (let ((sizer (get-fd-stream-character-sizer stream)))
    (when sizer (funcall sizer char))))

(defun fd-stream-string-size (stream string)
  (let ((sizer (get-fd-stream-character-sizer stream)))
    (when sizer
      (loop for char across string
            for size = (funcall sizer char)
            when (null size) do (return nil)
            summing size))))

(defun find-external-format (external-format)
  (when external-format
    (get-external-format external-format)))

(defun variable-width-external-format-p (ef-entry)
  ;; TODO: I'm pretty sure this is always true
  (and ef-entry (not (null (ef-resync-fun ef-entry)))))

(defun bytes-for-char-fun (ef-entry)
  (if ef-entry (ef-bytes-for-char-fun ef-entry) (constantly 1)))

(defmacro define-unibyte-mapping-external-format
    (canonical-name (&rest other-names) &body exceptions)
  (let ((->code-name (symbolicate canonical-name '->code-mapper))
        (code->-name (symbolicate 'code-> canonical-name '-mapper))
        (get-bytes-name (symbolicate 'get- canonical-name '-bytes))
        (string->-name (symbolicate 'string-> canonical-name))
        (string->/cr-name (symbolicate 'string-> canonical-name '/cr))
        (string->/crlf-name (symbolicate 'string-> canonical-name '/crlf))
        (string-name (symbolicate canonical-name '->string))
        (->string-aref-name (symbolicate canonical-name '->string-aref))
        (->string-aref/cr-name (symbolicate canonical-name '->string/cr-aref))
        (->string-aref/crlf-name (symbolicate canonical-name '->string/crlf-aref))
        (invalid-bytes-p (loop for (nil code) in exceptions thereis (null code))))
    `(progn
       (define-unibyte-mapper ,->code-name ,code->-name
         ,@exceptions)
       (define-unibyte-to-octets-functions ,canonical-name ,get-bytes-name ,string->-name ,code->-name)
       (define-unibyte-to-string-functions ,canonical-name ,string-name ,->code-name ,invalid-bytes-p)
       (define-unibyte-external-format-with-newline-variants ,canonical-name ,other-names
         (,->code-name ,code->-name)
         (,->string-aref-name ,string->-name)
         (,->string-aref/cr-name ,string->/cr-name)
         (,->string-aref/crlf-name ,string->/crlf-name)
         ,invalid-bytes-p))))

(defmacro define-unibyte-external-format
    (canonical-name (&rest other-names)
     char-encodable-p out-form in-form octets-to-string-symbol string-to-octets-symbol)
  `(define-external-format/variable-width (,canonical-name ,@other-names)
     t #\? 1
     ,out-form
     1
     ,in-form
     ,octets-to-string-symbol
     ,string-to-octets-symbol
     :char-encodable-p ,char-encodable-p))

(defmacro define-external-format/variable-width
    (external-format output-restart replacement-character
     out-size-expr out-expr in-size-expr in-expr
     octets-to-string-sym string-to-octets-sym
     &key base-string-direct-mapping
          fd-stream-read-n-characters
          (newline-variant :lf)
          (char-encodable-p t))
  (let* ((name (first external-format))
         (suffix (symbolicate name '/ newline-variant))
         (out-function (symbolicate "OUTPUT-BYTES/" suffix))
         (format (format nil "OUTPUT-CHAR-~A/~A-~~A-BUFFERED" (string name) newline-variant))
         (in-function (or fd-stream-read-n-characters
                          (symbolicate "FD-STREAM-READ-N-CHARACTERS/" suffix)))
         (in-char-function (symbolicate "INPUT-CHAR/" suffix))
         (resync-function (symbolicate "RESYNC/" suffix))
         (size-function (symbolicate "BYTES-FOR-CHAR/" suffix))
         (read-c-string-function (symbolicate "READ-FROM-C-STRING/" suffix))
         (output-c-string-function (symbolicate "OUTPUT-TO-C-STRING/" suffix))
         (n-buffer (gensym "BUFFER")))
    `(progn
       (defun ,size-function (|ch|)
         (declare (ignorable |ch|)
                  (optimize (sb-c:verify-arg-count 0)))
         (and ,char-encodable-p ,out-size-expr))
       (defun ,out-function (stream string flush-p start end)
         (declare (optimize (sb-c:verify-arg-count 0)))
         (let ((start (or start 0))
               (end (or end (length string))))
           (declare (type index start end))
           (when (fd-stream-synchronize-output stream)
             (synchronize-stream-output stream))
           (unless (<= 0 start end (length string))
             (sequence-bounding-indices-bad-error string start end))
           (do ()
               ((= end start))
             (let ((obuf (fd-stream-obuf stream)))
               (string-dispatch (simple-base-string
                                 #+sb-unicode (simple-array character (*))
                                 string)
                                string
                 (let ((len (buffer-length obuf))
                       (sap (buffer-sap obuf))
                       ;; FIXME: Rename
                       (tail (buffer-tail obuf)))
                   (declare (type index tail)
                            ;; STRING bounds have already been checked.
                            (optimize (safety 0)))
                   (,@(if output-restart
                          `(block output-nothing)
                          `(progn))
                    (do* ()
                         ((or (= start end) (< (- len tail) 4)))
                      (let* ((|ch| (aref string start))
                             (bits (char-code |ch|))
                             (size ,out-size-expr))
                        (declare (ignorable |ch| bits))
                        ,out-expr
                        (incf tail size)
                        (setf (buffer-tail obuf) tail)
                        (incf start)))
                    (go flush))
                   ;; Exited via RETURN-FROM OUTPUT-NOTHING: skip the current character.
                   (incf start))))
            flush
             (when (< start end)
               (flush-output-buffer stream)))
           (when flush-p
             (flush-output-buffer stream))))
       (def-output-routines/variable-width (,format
                                            ,out-size-expr
                                            ,output-restart
                                            ,external-format
                                            (:none character)
                                            (:line character)
                                            (:full character))
         (if (eql |ch| #\Newline)
             (setf (fd-stream-output-column stream) 0)
             (setf (fd-stream-output-column stream)
                   (+ (truly-the unsigned-byte (fd-stream-output-column stream)) 1)))
         (let ((bits (char-code |ch|))
               (sap (buffer-sap obuf))
               (tail (buffer-tail obuf)))
           ,out-expr))
       ,@(unless fd-stream-read-n-characters
           `((defun ,in-function (stream buffer sbuffer start requested &aux (total-copied 0))
               (declare (type fd-stream stream)
                        (type index start requested total-copied)
                        (type ansi-stream-cin-buffer buffer)
                        (type ansi-stream-csize-buffer sbuffer)
                        (optimize (sb-c:verify-arg-count 0)))
               (when (fd-stream-eof-forced-p stream)
                 (setf (fd-stream-eof-forced-p stream) nil)
                 (return-from ,in-function 0))
               (do ((instead (fd-stream-instead stream))
                    (index (+ start total-copied) (1+ index)))
                   ((= (fill-pointer instead) 0)
                    (setf (fd-stream-listen stream) nil))
                 (setf (aref buffer index) (vector-pop instead))
                 (setf (aref sbuffer index) 0)
                 (incf total-copied)
                 (when (= requested total-copied)
                   (when (= (fill-pointer instead) 0)
                     (setf (fd-stream-listen stream) nil))
                   (return-from ,in-function total-copied)))
               (do (;; external formats might wish for e.g. 2 octets
                    ;; to be available, but still be able to handle a
                    ;; single octet before end of file.  This flag
                    ;; lets the refilling be tracked so that we know
                    ;; if we've run out of octets and just have to
                    ;; make do with what we've got.
                    ;;
                    ;; but actually I don't think this works.
                    ;; Consider: a file which is exactly the size of
                    ;; the buffer, and whose last character is a CR.
                    ;; We will get to the last character and request a
                    ;; refill of the buffer; however, the refill will
                    ;; throw EOF.  We need something like the
                    ;; input-at-least logic here (where we catch EOF
                    ;; ourselves for refills between what the external
                    ;; format can handle if there is no more, and what
                    ;; it wants if there is more.)
                    ;;
                    ;; but actually actually: it might be that
                    ;; FAST-READ-CHAR-REFILL is doing this work for
                    ;; us.
                    (requested-refill nil))
                   (nil)
                 (let* ((ibuf (fd-stream-ibuf stream))
                        (head (buffer-head ibuf))
                        (tail (buffer-tail ibuf))
                        (sap (buffer-sap ibuf))
                        (decode-break-reason nil)
                        (unit ,(if (consp in-size-expr)
                                   (let ((size-info (car in-size-expr)))
                                     (if (consp size-info)
                                         (cadr size-info)
                                         size-info))
                                   in-size-expr)))
                   (declare (type index head tail))
                   ;; Copy data from stream buffer into user's buffer.
                   (do ((size nil nil))
                       ((or (= tail head) (= requested total-copied)))
                     (setf decode-break-reason
                           (block decode-break-reason
                             ,@(when (consp in-size-expr)
                                 (let* ((size-info (car in-size-expr))
                                        (want (if (consp size-info)
                                                  `(if requested-refill ,(cadr size-info) ,(car size-info))
                                                  size-info)))
                                   `((when (> ,want (- tail head))
                                       (setf requested-refill t)
                                       (return)))))
                             (let ((byte (sap-ref-8 sap head)))
                               (declare (ignorable byte))
                               (setf requested-refill nil)
                               (setq size ,(if (consp in-size-expr) (cadr in-size-expr) in-size-expr))
                               (when (> size (- tail head))
                                 (return))
                               (let ((index (+ start total-copied)))
                                 (setf (aref buffer index) ,in-expr)
                                 (setf (aref sbuffer index) size))
                               (incf total-copied)
                               (incf head size))
                             nil))
                     (setf (buffer-head ibuf) head)
                     (when decode-break-reason
                       ;; If we've already read some characters on when the invalid
                       ;; code sequence is detected, we return immediately. The
                       ;; handling of the error is deferred until the next call
                       ;; (where this check will be false). This allows establishing
                       ;; high-level handlers for decode errors (for example
                       ;; automatically resyncing in Lisp comments).
                       (unless (plusp total-copied)
                         (stream-decoding-error-and-handle stream decode-break-reason unit))
                       ;; we might have been given stuff to use instead, so
                       ;; we have to return (and trust our caller to know
                       ;; what to do about TOTAL-COPIED being 0).
                       (return-from ,in-function total-copied)))
                   (setf (buffer-head ibuf) head)
                   ;; Maybe we need to refill the stream buffer.
                   (when (or
                          ;; If there was data in the stream buffer, we're done.
                          (plusp total-copied)
                          ;; If EOF, we're also done
                          (null (catch 'eof-input-catcher
                                  (refill-input-buffer stream))))
                     (return total-copied))
                   ;; Otherwise we refilled the stream buffer, so fall
                   ;; through into another pass of the loop.
                   )))))
       (def-input-routine/variable-width ,in-char-function (character
                                                            ,external-format
                                                            ,in-size-expr
                                                            sap head)
                                         (let ((byte (sap-ref-8 sap head)))
                                           (declare (ignorable byte))
                                           ,in-expr))
       (defun ,resync-function (stream)
         (declare (optimize (sb-c:verify-arg-count 0)))
         (let ((ibuf (fd-stream-ibuf stream))
               size
               (unit ,(if (consp in-size-expr)
                          (let ((size-info (car in-size-expr)))
                            (if (consp size-info)
                                (cadr size-info)
                                size-info))
                          in-size-expr)))
           (catch 'eof-input-catcher
             (loop
              (incf (buffer-head ibuf) unit)
              ,(if (consp in-size-expr)
                   (let ((size-info (car in-size-expr)))
                     (if (consp size-info)
                         `(progn
                            (input-at-least stream ,(cadr size-info))
                            (catch 'eof-input-catcher
                              (input-at-least stream ,(car size-info))))
                         `(input-at-least stream ,size-info)))
                   `(input-at-least stream (setq size ,in-size-expr)))
              (unless (block decode-break-reason
                        (let* ((sap (buffer-sap ibuf))
                               (head (buffer-head ibuf))
                               (tail (buffer-tail ibuf))
                               (byte (sap-ref-8 sap head)))
                          (declare (ignorable byte tail))
                          ,@(when (consp in-size-expr)
                              `((setq size ,(cadr in-size-expr))
                                (input-at-least stream size)))
                          (setf head (buffer-head ibuf))
                          ,in-expr)
                        nil)
                (return))))))
       (defun ,read-c-string-function (sap element-type)
         (declare (type system-area-pointer sap)
                  (optimize (sb-c:verify-arg-count 0)))
         (locally
             (declare (optimize (speed 3) (safety 0)))
           (let* ((stream ,name)
                  (size 0) (head 0) (tail (1- array-dimension-limit)) (byte 0) (|ch| nil)
                  (decode-break-reason nil)
                  (length (dotimes (count (1- array-dimension-limit) count)
                            (setf decode-break-reason
                                  (block decode-break-reason
                                    (setf byte (sap-ref-8 sap head)
                                          size ,(if (consp in-size-expr)
                                                    (cadr in-size-expr)
                                                    in-size-expr)
                                          |ch| ,in-expr)
                                    (incf head size)
                                    nil))
                            (when decode-break-reason
                              (c-string-decoding-error
                               ,name sap head decode-break-reason))
                            (when (zerop (char-code |ch|))
                              (return count))))
                  (string (case element-type
                            (base-char
                             (make-string length :element-type 'base-char))
                            (character
                             (make-string length :element-type 'character))
                            (t
                             (make-string length :element-type element-type)))))
             (declare (ignorable stream byte tail)
                      (type index head length tail) ;; size
                      (type (unsigned-byte 8) byte)
                      (type (or null character) |ch|)
                      (type string string))
             (setf head 0)
             (dotimes (index length string)
               (setf decode-break-reason
                     (block decode-break-reason
                       (setf byte (sap-ref-8 sap head)
                             size ,(if (consp in-size-expr)
                                       (cadr in-size-expr)
                                       in-size-expr)
                             |ch| ,in-expr)
                       (incf head size)
                       nil))
               (when decode-break-reason
                 (c-string-decoding-error
                  ,name sap head decode-break-reason))
               (setf (aref string index) |ch|)))))

       (defun ,output-c-string-function (string)
         (declare (type simple-string string))
         (cond ,@(and base-string-direct-mapping
                      `(((simple-base-string-p string)
                         string)))
               (t
                (locally
                    (declare (optimize (speed 3) (safety 0)))
                  (block output-nothing
                    (let* ((length (length string))
                           ;; wtf? why not just "LET ((bits 0))" ?
                           (null-size (let* ((|ch| (code-char 0))
                                             (bits (char-code |ch|)))
                                        (declare (ignorable |ch| bits))
                                        (the index ,out-size-expr)))
                           (buffer-length
                             (+ (loop for i of-type index below length
                                      for |ch| of-type character = (aref string i)
                                      for bits = (char-code |ch|)
                                      sum (the index ,out-size-expr) of-type index)
                                null-size))
                           (tail 0)
                           (,n-buffer (make-array buffer-length
                                                  :element-type '(unsigned-byte 8)))
                           ;; For external-format-encoding-error
                           (stream ',name))
                      (declare (type index length buffer-length tail)
                               (ignorable stream))
                      (with-pinned-objects (,n-buffer)
                        (let ((sap (vector-sap ,n-buffer)))
                          (declare (system-area-pointer sap))
                          (loop for i of-type index below length
                                for |ch| of-type character = (aref string i)
                                for bits = (char-code |ch|)
                                for size of-type index = ,out-size-expr
                                do (prog1
                                       ,out-expr
                                     (incf tail size)))
                          (let* ((bits 0)
                                 (|ch| (code-char bits)) ; more wtf
                                 (size null-size))
                            (declare (ignorable bits |ch| size))
                            ,out-expr)))
                      ,n-buffer))))))

       (register-external-format
        ',external-format
        :newline-variant ,newline-variant
        :default-replacement-character ,replacement-character
        :read-n-chars-fun #',in-function
        :read-char-fun #',in-char-function
        :write-n-bytes-fun #',out-function
        ,@(mapcan #'(lambda (buffering)
                      (list (intern (format nil "WRITE-CHAR-~A-BUFFERED-FUN" buffering) :keyword)
                            `#',(intern (format nil format (string buffering)))))
                  '(:none :line :full))
        :resync-fun #',resync-function
        :bytes-for-char-fun #',size-function
        :read-c-string-fun #',read-c-string-function
        :write-c-string-fun #',output-c-string-function
        :octets-to-string-fun (lambda (&rest rest)
                                (declare (dynamic-extent rest))
                                (apply ',octets-to-string-sym rest))
        :string-to-octets-fun (lambda (&rest rest)
                                (declare (dynamic-extent rest))
                                (apply ',string-to-octets-sym rest))))))

;;;; utility functions (misc routines, etc)

;;; Fill in the various routine slots for the given type. INPUT-P and
;;; OUTPUT-P indicate what slots to fill. The buffering slot must be
;;; set prior to calling this routine.
(defun set-fd-stream-routines (fd-stream element-type canonized-external-format external-format-entry
                               input-p output-p buffer-p
                               dual-channel-p)
  (let* ((target-type (case element-type
                        (unsigned-byte '(unsigned-byte 8))
                        (signed-byte '(signed-byte 8))
                        (:default 'character)
                        (t element-type)))
         (character-stream-p (subtypep target-type 'character))
         (bivalent-stream-p (eq element-type :default))
         char-size
         replacement
         (bin-routine #'ill-bin)
         (bin-type nil)
         (bin-size nil)
         (cin-routine #'ill-in)
         (cin-type nil)
         (cin-size nil)
         (input-type nil)           ;calculated from bin-type/cin-type
         (input-size nil)           ;calculated from bin-size/cin-size
         (read-n-characters #'ill-in)
         (bout-routine #'ill-bout)
         (bout-type nil)
         (bout-size nil)
         (cout-routine #'ill-out)
         (cout-type nil)
         (cout-size nil)
         (output-type nil)
         (output-size nil)
         (output-bytes #'ill-bout))

    ;; Ensure that we have buffers in the desired direction(s) only,
    ;; getting new ones and dropping/resetting old ones as necessary.
    (let ((obuf (fd-stream-obuf fd-stream)))
      (if output-p
          (if obuf
              (reset-buffer obuf)
              (setf (fd-stream-obuf fd-stream) (get-buffer)))
          (when obuf
            (setf (fd-stream-obuf fd-stream) nil)
            (release-buffer obuf))))

    (let ((ibuf (fd-stream-ibuf fd-stream)))
      (if input-p
          (if ibuf
              (reset-buffer ibuf)
              (setf (fd-stream-ibuf fd-stream) (get-buffer)))
          (when ibuf
            (setf (fd-stream-ibuf fd-stream) nil)
            (release-buffer ibuf))))

    ;; FIXME: Why only for output? Why unconditionally?
    (when output-p
      (setf (fd-stream-output-column fd-stream) 0))

    (when input-p
      (flet ((no-input-routine ()
               (error "could not find any input routine for ~
                        ~/sb-impl:print-type-specifier/"
                      target-type)))
        (when (or (not character-stream-p) bivalent-stream-p)
          (setf (values bin-routine bin-type bin-size read-n-characters
                        char-size replacement)
                (pick-input-routine (if bivalent-stream-p '(unsigned-byte 8)
                                        target-type)
                                    external-format-entry))
          (unless bin-routine (no-input-routine)))
        (when character-stream-p
          (setf (values cin-routine cin-type cin-size read-n-characters
                        char-size replacement)
                (pick-input-routine target-type external-format-entry))
          (unless cin-routine (no-input-routine))))
      (setf (fd-stream-in fd-stream) cin-routine
            (fd-stream-bin fd-stream) bin-routine)
      ;; character type gets preferential treatment
      (setf input-size (or cin-size bin-size))
      (setf input-type (or cin-type bin-type))
      (when char-size
        (setf (fd-stream-external-format fd-stream) canonized-external-format
              (fd-stream-char-size fd-stream) char-size
              (fd-stream-replacement fd-stream) replacement))
      (when (= (or cin-size 1) (or bin-size 1) 1)
        (setf (fd-stream-n-bin fd-stream)
              (if (and character-stream-p (not bivalent-stream-p))
                  read-n-characters
                  #'fd-stream-read-n-bytes))
        ;; Sometimes turn on fast-read-char/fast-read-byte.  Switch on
        ;; for character and (unsigned-byte 8) streams.  In these
        ;; cases, fast-read-* will read from the
        ;; ansi-stream-(c)in-buffer, saving function calls.
        ;; Otherwise, the various data-reading functions in the stream
        ;; structure will be called.
        (when (and buffer-p
                   (not bivalent-stream-p)
                   ;; temporary disable on :io streams
                   (not output-p))
          (cond (character-stream-p
                 (setf (ansi-stream-cin-buffer fd-stream)
                       (make-array +ansi-stream-in-buffer-length+
                                   :element-type 'character))
                 (setf (ansi-stream-csize-buffer fd-stream)
                       (make-array +ansi-stream-in-buffer-length+
                                   :element-type '(unsigned-byte 8))))
                ((equal target-type '(unsigned-byte 8))
                 (setf (ansi-stream-in-buffer fd-stream)
                       (make-array +ansi-stream-in-buffer-length+
                                   :element-type '(unsigned-byte 8))))))))

    (when output-p
      (when (or (not character-stream-p) bivalent-stream-p)
        (setf (values bout-routine bout-type bout-size output-bytes
                      char-size replacement)
              (let ((buffering (fd-stream-buffering fd-stream)))
                (if bivalent-stream-p
                    (pick-output-routine '(unsigned-byte 8)
                                         (if (eq :line buffering)
                                             :full
                                             buffering))
                    (pick-output-routine target-type buffering external-format-entry))))
        (unless bout-routine
          (error "could not find any output routine for ~S buffered ~S"
                 (fd-stream-buffering fd-stream)
                 target-type)))
      (when character-stream-p
        (setf (values cout-routine cout-type cout-size output-bytes
                      char-size replacement)
              (pick-output-routine target-type
                                   (fd-stream-buffering fd-stream)
                                   external-format-entry))
        (unless cout-routine
          (error "could not find any output routine for ~S buffered ~S"
                 (fd-stream-buffering fd-stream)
                 target-type)))
      (when char-size
        (setf (fd-stream-external-format fd-stream) canonized-external-format
              (fd-stream-replacement fd-stream) replacement
              (fd-stream-char-size fd-stream) char-size))
      (when character-stream-p
        (setf (fd-stream-output-bytes fd-stream) output-bytes))
      (setf (fd-stream-cout fd-stream) cout-routine
            (fd-stream-bout fd-stream) bout-routine
            (fd-stream-sout fd-stream) (if (eql cout-size 1)
                                           #'fd-sout #'ill-out))
      (setf output-size (or cout-size bout-size))
      (setf output-type (or cout-type bout-type))
      (when (and input-p
                 (not dual-channel-p))
        (setf (fd-stream-synchronize-output fd-stream) t)))

    (when (and input-size output-size
               (not (eq input-size output-size)))
      (error "Element sizes for input (~S:~S) and output (~S:~S) differ?"
             input-type input-size
             output-type output-size))
    (setf (fd-stream-element-size fd-stream)
          (or input-size output-size))

    (setf (fd-stream-element-type fd-stream)
          (cond ((equal input-type output-type)
                 input-type)
                ((null output-type)
                 input-type)
                ((null input-type)
                 output-type)
                ((subtypep input-type output-type)
                 input-type)
                ((subtypep output-type input-type)
                 output-type)
                (t
                 (error "Input type (~/sb-impl:print-type-specifier/) and ~
                         output type (~/sb-impl:print-type-specifier/) ~
                         are unrelated?"
                        input-type output-type))))))

;;; Handles the resource-release aspects of stream closing, and marks
;;; it as closed.
(defun release-fd-stream-resources (fd-stream)
  (handler-case
      (without-interrupts
        ;; Drop handlers first.
        (when (fd-stream-handler fd-stream)
          (remove-fd-handler (fd-stream-handler fd-stream))
          (setf (fd-stream-handler fd-stream) nil))
        ;; Disable interrupts so that a asynch unwind will not leave
        ;; us with a dangling finalizer (that would close the same
        ;; --possibly reassigned-- FD again), or a stream with a closed
        ;; FD that appears open.
        (cancel-finalization fd-stream)
        (let ((fd (fd-stream-fd fd-stream)))
          (when (and (/= fd -1)
                     (eq (cas (fd-stream-fd fd-stream) fd -1) fd))
            (sb-unix:unix-close fd)))
        (set-closed-flame fd-stream))
    ;; On error unwind from WITHOUT-INTERRUPTS.
    (serious-condition (e)
      (error e)))
  ;; Release all buffers. If this is undone, or interrupted,
  ;; we're still safe: buffers have finalizers of their own.
  (release-fd-stream-buffers fd-stream))

;;; Flushes the current input buffer and any supplied replacements,
;;; and returns the input buffer, and the amount of flushed input in
;;; bytes.
(defun flush-input-buffer (stream)
  (let* ((instead (fd-stream-instead stream))
         (unread (length instead)))
    ;; (setf fill-pointer) performs some checks and is slower
    (setf (%array-fill-pointer instead) 0)
    (let ((ibuf (fd-stream-ibuf stream)))
      (if ibuf
          (let ((head (buffer-head ibuf))
                (tail (buffer-tail ibuf)))
            (values (reset-buffer ibuf) (- (+ unread tail) head)))
          (values nil unread)))))

(defun fd-stream-clear-input (stream)
  (flush-input-buffer stream)
  #+win32
  (progn
    (sb-win32:handle-clear-input (fd-stream-fd stream))
    (setf (fd-stream-listen stream) nil))
  #-win32
  (catch 'eof-input-catcher
    (loop until (sysread-may-block-p stream)
          do
          (refill-input-buffer stream)
          (reset-buffer (fd-stream-ibuf stream)))
    t))

;;; Handle miscellaneous operations on FD-STREAM.
(defun fd-stream-misc-routine (fd-stream operation arg1)
  (stream-misc-case (operation :default nil)
    (:listen
     (labels ((do-listen ()
                (let ((ibuf (fd-stream-ibuf fd-stream)))
                  (or (not (eql (buffer-head ibuf) (buffer-tail ibuf)))
                      (fd-stream-listen fd-stream)
                      ;; If the read can block, LISTEN will certainly return NIL.
                      (if (sysread-may-block-p fd-stream)
                          nil
                          ;; Otherwise select(2) and CL:LISTEN have slightly
                          ;; different semantics.  The former returns that an FD
                          ;; is readable when a read operation wouldn't block.
                          ;; That includes EOF.  However, LISTEN must return NIL
                          ;; at EOF.
                          (progn (catch 'eof-input-catcher
                                   ;; r-b/f too calls select, but it shouldn't
                                   ;; block as long as read can return once w/o
                                   ;; blocking
                                   (refill-input-buffer fd-stream))
                                 ;; At this point either IBUF-HEAD != IBUF-TAIL
                                 ;; and FD-STREAM-LISTEN is NIL, in which case
                                 ;; we should return T, or IBUF-HEAD ==
                                 ;; IBUF-TAIL and FD-STREAM-LISTEN is :EOF, in
                                 ;; which case we should return :EOF for this
                                 ;; call and all future LISTEN call on this stream.
                                 ;; Call ourselves again to determine which case
                                 ;; applies.
                                 (do-listen)))))))
       (do-listen)))
    (:unread
     (let* ((ibuf (fd-stream-ibuf fd-stream))
            (head (buffer-head ibuf))
            (prev (buffer-prev-head ibuf)))
       (if (= head prev)
           ;; we are unreading a character which we previously pulled
           ;; from INSTEAD; push it back there.
           (vector-push-extend arg1 (fd-stream-instead fd-stream))
           ;; reset the buffer position to where it was before we read
           ;; the previous character.
           (setf (buffer-head ibuf) (buffer-prev-head ibuf)))))
    (:close
     ;; Drop input buffers
     (setf (ansi-stream-in-index fd-stream) +ansi-stream-in-buffer-length+
           (ansi-stream-cin-buffer fd-stream) nil
           (ansi-stream-csize-buffer fd-stream) nil
           (ansi-stream-in-buffer fd-stream) nil)
     (cond (arg1
            ;; We got us an abort on our hands.
            (let ((outputp (fd-stream-obuf fd-stream))
                  (file (fd-stream-file fd-stream))
                  (orig (fd-stream-original fd-stream)))
              ;; This takes care of the important stuff -- everything
              ;; rest is cleaning up the file-system, which we cannot
              ;; do on some platforms as long as the file is open.
              (release-fd-stream-resources fd-stream)
              ;; We can't do anything unless we know what file were
              ;; dealing with, and we don't want to do anything
              ;; strange unless we were writing to the file.
              (when (and outputp file)
                (if orig
                    ;; If the original is EQ to file we are appending to
                    ;; and can just close the file without renaming.
                    (unless (eq orig file)
                      ;; We have a handle on the original, just revert.
                      (multiple-value-bind (okay err)
                          (sb-unix:unix-rename orig file)
                        ;; FIXME: Why is this a SIMPLE-STREAM-ERROR, and the
                        ;; others are SIMPLE-FILE-ERRORS? Surely they should
                        ;; all be the same?
                        (unless okay
                          (simple-stream-perror
                           "~@<Couldn't restore ~S to its original ~
                               contents from ~S while closing ~S~:>"
                           fd-stream err
                           file orig fd-stream))))
                    ;; We can't restore the original, and aren't
                    ;; appending, so nuke that puppy.
                    ;;
                    ;; FIXME: This is currently the fate of superseded
                    ;; files, and according to the CLOSE spec this is
                    ;; wrong. However, there seems to be no clean way to
                    ;; do that that doesn't involve either copying the
                    ;; data (bad if the :abort resulted from a full
                    ;; disk), or renaming the old file temporarily
                    ;; (probably bad because stream opening becomes more
                    ;; racy).
                    (multiple-value-bind (okay err)
                        (sb-unix:unix-unlink file)
                      (unless okay
                        (file-perror
                         file err
                         "~@<Couldn't remove ~S while closing ~S~:>" file fd-stream)))))))
           (t
            (finish-fd-stream-output fd-stream)
            (let ((orig (fd-stream-original fd-stream)))
              (when (and orig (fd-stream-delete-original fd-stream))
                (multiple-value-bind (okay err) (sb-unix:unix-unlink orig)
                  (unless okay
                    (file-perror
                     orig err
                     "~@<Couldn't delete ~S while closing ~S~:>" orig fd-stream)))))
            ;; In case of no-abort close, don't *really* close the
            ;; stream until the last moment -- the cleaning up of the
            ;; original can be done first.
            (release-fd-stream-resources fd-stream))))
    (:clear-input
     (fd-stream-clear-input fd-stream))
    (:force-output
     (flush-output-buffer fd-stream))
    (:finish-output
     (finish-fd-stream-output fd-stream))
    (:element-type
     (fd-stream-element-type fd-stream))
    (:element-mode
     (fd-stream-element-mode fd-stream))
    (:external-format
     (fd-stream-external-format fd-stream))
    (:interactive-p
     (plusp (the (integer 0)
              (sb-unix:unix-isatty (fd-stream-fd fd-stream)))))
    (:line-length
     80)
    (:charpos
     (fd-stream-output-column fd-stream))
    (:file-length
     (unless (fd-stream-file fd-stream)
       ;; This is a TYPE-ERROR because ANSI's species FILE-LENGTH
       ;; "should signal an error of type TYPE-ERROR if stream is not
       ;; a stream associated with a file". Too bad there's no very
       ;; appropriate value for the EXPECTED-TYPE slot..
       (error 'simple-type-error
              :datum fd-stream
              :expected-type 'fd-stream
              :format-control "~S is not a stream associated with a file."
              :format-arguments (list fd-stream)))
     #-win32
     (multiple-value-bind (okay dev ino mode nlink uid gid rdev size
                                atime mtime ctime blksize blocks)
         (sb-unix:unix-fstat (fd-stream-fd fd-stream))
       (declare (ignore ino nlink uid gid rdev
                        atime mtime ctime blksize blocks))
       (unless okay
         (simple-stream-perror "failed Unix fstat(2) on ~S" fd-stream dev))
       (if (zerop mode)
           nil
           (truncate size (fd-stream-element-size fd-stream))))
     #+win32
     (let* ((handle (fd-stream-fd fd-stream))
            (element-size (fd-stream-element-size fd-stream)))
       (multiple-value-bind (got native-size)
           (sb-win32:get-file-size-ex handle 0)
         (if (zerop got)
             ;; Might be a block device, in which case we fall back to
             ;; a non-atomic workaround:
             (let* ((here (sb-unix:unix-lseek handle 0 sb-unix:l_incr))
                    (there (sb-unix:unix-lseek handle 0 sb-unix:l_xtnd)))
               (when (and here there)
                 (sb-unix:unix-lseek handle here sb-unix:l_set)
                 (truncate there element-size)))
             (truncate native-size element-size)))))
    (:file-string-length
     (etypecase arg1
       (character (fd-stream-character-size fd-stream arg1))
       (string (fd-stream-string-size fd-stream arg1))))
    (:get-file-position (fd-stream-get-file-position fd-stream))
    (:set-file-position (fd-stream-set-file-position fd-stream arg1))))

;; FIXME: Think about this.
;;
;; (defun finish-fd-stream-output (fd-stream)
;;   (let ((timeout (fd-stream-timeout fd-stream)))
;;     (loop while (fd-stream-output-queue fd-stream)
;;        ;; FIXME: SIGINT while waiting for a timeout will
;;        ;; cause a timeout here.
;;        do (when (and (not (serve-event timeout)) timeout)
;;             (signal-timeout 'io-timeout
;;                             :stream fd-stream
;;                             :direction :write
;;                             :seconds timeout)))))

(defun finish-fd-stream-output (stream)
  (flush-output-buffer stream)
  (do ()
      ((null (fd-stream-output-queue stream)))
    (aver (fd-stream-serve-events stream))
    (serve-all-events)))

(defun fd-stream-get-file-position (stream)
  (declare (fd-stream stream))
  (without-interrupts
    (let ((posn (sb-unix:unix-lseek (fd-stream-fd stream) 0 sb-unix:l_incr)))
      (declare (type (or (alien sb-unix:unix-offset) null) posn))
      ;; We used to return NIL for errno==ESPIPE, and signal an error
      ;; in other failure cases. However, CLHS says to return NIL if
      ;; the position cannot be determined -- so that's what we do.
      (when (integerp posn)
        ;; Adjust for buffered output: If there is any output
        ;; buffered, the *real* file position will be larger
        ;; than reported by lseek() because lseek() obviously
        ;; cannot take into account output we have not sent
        ;; yet.
        (dolist (buffer (fd-stream-output-queue stream))
          (incf posn (- (buffer-tail buffer) (buffer-head buffer))))
        (let ((obuf (fd-stream-obuf stream)))
          (when obuf
            (incf posn (buffer-tail obuf))))
        ;; Adjust for unread input: If there is any input
        ;; read from UNIX but not supplied to the user of the
        ;; stream, the *real* file position will smaller than
        ;; reported, because we want to look like the unread
        ;; stuff is still available.
        (let ((ibuf (fd-stream-ibuf stream)))
          (when ibuf
            (decf posn (- (buffer-tail ibuf) (buffer-head ibuf)))))
        ;; Divide bytes by element size.
        (truncate posn (fd-stream-element-size stream))))))

(defun fd-stream-set-file-position (stream position-spec)
  (declare (fd-stream stream))
  (check-type position-spec
              (or (alien sb-unix:unix-offset) (member nil :start :end))
              "valid file position designator")
  (tagbody
   :again
     ;; Make sure we don't have any output pending, because if we
     ;; move the file pointer before writing this stuff, it will be
     ;; written in the wrong location.
     (finish-fd-stream-output stream)
     ;; Disable interrupts so that interrupt handlers doing output
     ;; won't screw us.
     (without-interrupts
       (unless (fd-stream-output-finished-p stream)
         ;; We got interrupted and more output came our way during
         ;; the interrupt. Wrapping the FINISH-FD-STREAM-OUTPUT in
         ;; WITHOUT-INTERRUPTS gets nasty as it can signal errors,
         ;; so we prefer to do things like this...
         (go :again))
       ;; Clear out any pending input to force the next read to go to
       ;; the disk.
       (flush-input-buffer stream)
       ;; Trash cached value for listen, so that we check next time.
       (setf (fd-stream-listen stream) nil)
         ;; Now move it.
         (multiple-value-bind (offset origin)
             (case position-spec
               (:start
                (values 0 sb-unix:l_set))
               (:end
                (values 0 sb-unix:l_xtnd))
               (t
                (values (* position-spec (fd-stream-element-size stream))
                        sb-unix:l_set)))
           (declare (type (alien sb-unix:unix-offset) offset))
           (let ((posn (sb-unix:unix-lseek (fd-stream-fd stream)
                                           offset origin)))
             ;; CLHS says to return true if the file-position was set
             ;; successfully, and NIL otherwise. We are to signal an error
             ;; only if the given position was out of bounds, and that is
             ;; dealt with above. In times past we used to return NIL for
             ;; errno==ESPIPE, and signal an error in other cases.
             ;;
             ;; FIXME: We are still liable to signal an error if flushing
             ;; output fails.
             (return-from fd-stream-set-file-position
               (typep posn '(alien sb-unix:unix-offset))))))))


;;;; creation routines (MAKE-FD-STREAM and OPEN)

;;; Create a stream for the given Unix file descriptor.
;;;
;;; If INPUT is non-NIL, allow input operations. If OUTPUT is non-nil,
;;; allow output operations. If neither INPUT nor OUTPUT is specified,
;;; default to allowing input.
;;;
;;; ELEMENT-TYPE indicates the element type to use (as for OPEN).
;;;
;;; BUFFERING indicates the kind of buffering to use.
;;;
;;; TIMEOUT (if true) is the number of seconds to wait for input. If
;;; NIL (the default), then wait forever. When we time out, we signal
;;; IO-TIMEOUT.
;;;
;;; FILE is the name of the file (will be returned by PATHNAME).
;;;
;;; NAME is used to identify the stream when printed.
;;;
;;; If SERVE-EVENTS is true, SERVE-EVENT machinery is used to
;;; handle blocking IO on the stream.
(defun make-fd-stream (fd
                       &key
                       (class 'fd-stream)
                       (input nil input-p)
                       (output nil output-p)
                       (element-type 'base-char)
                       (buffering :full)
                       (external-format :default)
                       serve-events
                       timeout
                       file
                       original
                       delete-original
                       pathname
                       input-buffer-p
                       dual-channel-p
                       (name (if file
                                 (format nil "file ~A" file)
                                 (format nil "descriptor ~W" fd)))
                         auto-close)
  (declare (type index fd) (type (or real null) timeout)
           (type (member :none :line :full) buffering))
  ;; OPEN ensures that the external-format argument is OK before
  ;; creating an FD and calling here.  MAKE-FD-STREAM isn't really
  ;; part of a public interface but has numerous callers with an
  ;; :EXTERNAL-FORMAT argument, so we need to repeat the check and
  ;; canonization here, but if we detect a problem we must make sure
  ;; to close the FD.
  (let* ((defaulted-external-format (if (eql external-format :default)
                                        (default-external-format)
                                        external-format))
         (external-format-entry (get-external-format defaulted-external-format))
         (canonized-external-format
          (and external-format-entry (canonize-external-format external-format external-format-entry))))
    (unless external-format-entry
      (unwind-protect
           (error "Undefined external-format: ~S" external-format)
        (sb-unix:unix-close fd)))

    (cond ((not (or input-p output-p))
           (setf input t))
          ((not (or input output))
           (unwind-protect
                (error "File descriptor must be opened either for input or output.")
             (sb-unix:unix-close fd))))
    (let* ((constructor (ecase class
                          (fd-stream '%make-fd-stream)
                          (form-tracking-stream '%make-form-tracking-stream)))
           (element-mode (stream-element-type-stream-element-mode element-type))
           (stream (funcall constructor
                            :fd fd
                            :fd-type
                            #-win32 (sb-unix:fd-type fd)
                            ;; KLUDGE.
                            #+win32 (if serve-events
                                        :unknown
                                        :regular)
                            :name name
                            :file file
                            :original original
                            :delete-original delete-original
                            :pathname pathname
                            :buffering buffering
                            :element-mode element-mode
                            :serve-events serve-events
                            :timeout
                            (if timeout
                                (coerce timeout 'single-float)
                                nil))))
      (set-fd-stream-routines stream element-type canonized-external-format external-format-entry
                              input output input-buffer-p dual-channel-p)
      (when auto-close
        (finalize stream
                  (lambda ()
                    (sb-unix:unix-close fd)
                    #+sb-show
                    (format *terminal-io* "** closed file descriptor ~W **~%"
                            fd))
                  :dont-save t))
      stream)))

;;; Pick a name to use for the backup file for the :IF-EXISTS
;;; :RENAME-AND-DELETE and :RENAME options.
(defun pick-backup-name (name)
  (declare (type simple-string name))
  (concatenate 'simple-string name ".bak"))

;;; Rename NAMESTRING to ORIGINAL. First, check whether we have write
;;; access, since we don't want to trash unwritable files even if we
;;; technically can. We return true if we succeed in renaming.
(defun rename-the-old-one (namestring original)
  (unless (sb-unix:unix-access namestring sb-unix:w_ok)
    (error "~@<The file ~2I~_~S ~I~_is not writable.~:>" namestring))
  (multiple-value-bind (okay err) (sb-unix:unix-rename namestring original)
    (if okay
        t
        (file-perror
         namestring err
         "~@<couldn't rename ~2I~_~S ~I~_to ~2I~_~S~:>" namestring original))))

#+unix (defun file-exists-p (path) (sb-unix:unix-access path sb-unix:f_ok))

(defun %open-error (pathname errno if-exists if-does-not-exist)
  (flet ((signal-it (&rest arguments)
           (apply #'file-perror pathname errno arguments)))
    (restart-case
        (case errno
          (#-win32 #.sb-unix:enoent
           #+win32 #.sb-win32::error_file_not_found
           (case if-does-not-exist
             (:error
              (restart-case
                  (signal-it 'file-does-not-exist)
                (create ()
                  :report "Reopen with :if-does-not-exist :create"
                  '(:new-if-does-not-exist :create))))
             (:create
              (sb-kernel::%file-error
               pathname
               "~@<The path ~2I~_~S ~I~_does not exist.~:>" pathname))
             (t '(:return t))))
          (#-win32 #.sb-unix:eexist
           #+win32 #.sb-win32::error_file_exists
           (if (null if-exists)
               '(:return t)
               (restart-case
                   (signal-it 'file-exists)
                 (supersede ()
                   :report "Reopen with :if-exists :supersede"
                   '(:new-if-exists :supersede))
                 (overwrite ()
                   :report "Reopen with :if-exists :overwrite"
                   '(:new-if-exists :overwrite))
                 (rename ()
                   :report "Reopen with :if-exists :rename"
                   '(:new-if-exists :rename))
                 (append ()
                   :report "Reopen with :if-exists :append"
                   '(:new-if-exists :append)))))
          (t
           (signal-it "Error opening ~S" pathname)))
      (continue ()
        :report "Retry opening."
        '())
      (use-value (value)
        :report "Try opening a different file."
        :interactive read-evaluated-form
        (list :new-filename (the pathname-designator value))))))

(defun open (filename
             &key
               (direction :input)
               (element-type 'character)
               (if-exists nil if-exists-given)
               (if-does-not-exist nil if-does-not-exist-given)
               (external-format :default)
               ;; private options - use at your own risk
               (class 'fd-stream)
               #+win32
               (overlapped t)
             &aux
               ;; Squelch assignment warning.
               (filename filename)
               (direction direction)
               (if-does-not-exist if-does-not-exist)
               (if-exists if-exists))
  "Return a stream which reads from or writes to FILENAME.
  Defined keywords:
   :DIRECTION - one of :INPUT, :OUTPUT, :IO, or :PROBE
   :ELEMENT-TYPE - the type of object to read or write, default BASE-CHAR
   :IF-EXISTS - one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE,
                       :OVERWRITE, :APPEND, :SUPERSEDE or NIL
   :IF-DOES-NOT-EXIST - one of :ERROR, :CREATE or NIL
  See the manual for details."
  (let* ((defaulted-external-format (if (eql external-format :default)
                                        (default-external-format)
                                        external-format))
         (external-format-entry (get-external-format defaulted-external-format))
         (canonized-external-format
          (and external-format-entry (canonize-external-format external-format external-format-entry))))
    (unless external-format-entry
      (error "Undefined external-format: ~S" external-format))
    ;; Calculate useful stuff.
    (loop
     (multiple-value-bind (input output mask)
         (ecase direction
           (:input  (values   t nil sb-unix:o_rdonly))
           (:output (values nil   t sb-unix:o_wronly))
           (:io     (values   t   t sb-unix:o_rdwr))
           (:probe  (values   t nil sb-unix:o_rdonly)))
       (declare (type index mask))
       (let* (;; PATHNAME is the pathname we associate with the stream.
              (pathname (merge-pathnames filename))
              (physical (native-namestring (physicalize-pathname pathname) :as-file t))
              ;; One call to access() is reasonable. 40 calls to lstat() is not.
              ;; So DO NOT CALL TRUENAME HERE.
              (existsp (file-exists-p physical))
              ;; Leave NAMESTRING as NIL if nonexistent and not creating a file.
              (namestring (when (or existsp
                                    (or (not input)
                                        (and input (eq if-does-not-exist :create))
                                        (and (eq direction :io)
                                             (not if-does-not-exist-given))))
                            physical)))
         ;; Process if-exists argument if we are doing any output.
         (cond (output
                (unless if-exists-given
                  (setf if-exists
                        (if (eq (pathname-version pathname) :newest)
                            :new-version
                            :error)))
                (case if-exists
                  ((:new-version :error nil)
                   (setf mask (logior mask sb-unix:o_excl)))
                  ((:rename :rename-and-delete)
                   (setf mask (logior mask sb-unix:o_creat)))
                  ((:supersede)
                   (setf mask (logior mask sb-unix:o_trunc)))
                  (:append
                   (setf mask (logior mask sb-unix:o_append)))))
               (t
                (setf if-exists :ignore-this-arg)))

         (unless if-does-not-exist-given
           (setf if-does-not-exist
                 (cond ((eq direction :input) :error)
                       ((and output
                             (member if-exists '(:overwrite :append)))
                        :error)
                       ((eq direction :probe)
                        nil)
                       (t
                        :create))))
         (cond ((and existsp if-exists-given (eq if-exists :new-version))
                (sb-kernel::%file-error
                 pathname "OPEN :IF-EXISTS :NEW-VERSION is not supported ~
                           when a new version must be created."))
               ((eq if-does-not-exist :create)
                (setf mask (logior mask sb-unix:o_creat)))
               ((not (member if-exists '(:error nil))))
               ;; Both if-does-not-exist and if-exists now imply
               ;; that there will be no opening of files, and either
               ;; an error would be signalled, or NIL returned
               ((and (not if-exists) (not if-does-not-exist))
                (return-from open))
               ((and if-exists if-does-not-exist)
                (sb-kernel::%file-error
                 pathname "OPEN :IF-DOES-NOT-EXIST ~s ~
                          :IF-EXISTS ~s will always signal an error."
                 if-does-not-exist if-exists))
               (existsp
                (if if-exists
                    (sb-kernel::%file-error pathname 'file-exists)
                    (return)))
               (if-does-not-exist
                (sb-kernel::%file-error pathname 'file-does-not-exist))
               (t
                (return)))
         (let ((original (case if-exists
                           ((:rename :rename-and-delete)
                            (pick-backup-name namestring))
                           ((:append :overwrite)
                            ;; KLUDGE: Prevent CLOSE from deleting
                            ;; appending streams when called with :ABORT T
                            namestring)))
               (delete-original (eq if-exists :rename-and-delete))
               (mode #o666))
           (when (and original (not (eq original namestring)))
             ;; We are doing a :RENAME or :RENAME-AND-DELETE. Determine
             ;; whether the file already exists, make sure the original
             ;; file is not a directory, and keep the mode.
             (let ((exists
                    (and namestring
                         (multiple-value-bind (okay err/dev inode orig-mode)
                             (sb-unix:unix-stat namestring)
                           (declare (ignore inode)
                                    (type (or index null) orig-mode))
                           (cond
                             (okay
                              (when (and output (= (logand orig-mode #o170000)
                                                   #o40000))
                                (file-perror
                                 pathname nil
                                 "Can't open ~S for output: is a directory"
                                 pathname))
                              (setf mode (logand orig-mode #o777))
                              t)
                             ((eql err/dev sb-unix:enoent)
                              nil)
                             (t
                              (file-perror namestring err/dev
                                           "Can't find ~S" namestring)))))))
               (unless (and exists
                            (rename-the-old-one namestring original))
                 (setf original nil)
                 (setf delete-original nil)
                 ;; In order to use :SUPERSEDE instead, we have to make
                 ;; sure SB-UNIX:O_CREAT corresponds to
                 ;; IF-DOES-NOT-EXIST. SB-UNIX:O_CREAT was set before
                 ;; because of IF-EXISTS being :RENAME.
                 (unless (eq if-does-not-exist :create)
                   (setf mask
                         (logior (logandc2 mask sb-unix:o_creat)
                                 sb-unix:o_trunc)))
                 (setf if-exists :supersede))))

           ;; Now we can try the actual Unix open(2).
           (multiple-value-bind (fd errno)
               (if namestring
                   (sb-unix:unix-open namestring mask mode
                                      #+win32 :overlapped #+win32 overlapped)
                   (values nil #-win32 sb-unix:enoent
                           #+win32 sb-win32::error_file_not_found))
             (when (numberp fd)
               (return (case direction
                         ((:input :output :io)
                          ;; For O_APPEND opened files, lseek returns 0 until first write.
                          ;; So we jump ahead here.
                          (when (eq if-exists :append)
                            (sb-unix:unix-lseek fd 0 sb-unix:l_xtnd))
                          (make-fd-stream fd
                                          :class class
                                          :input input
                                          :output output
                                          :element-type element-type
                                          :external-format canonized-external-format
                                          :file namestring
                                          :original original
                                          :delete-original delete-original
                                          :pathname pathname
                                          :dual-channel-p nil
                                          :serve-events nil
                                          :input-buffer-p t
                                          :auto-close t))
                         (:probe
                          (let ((stream
                                 (%make-fd-stream :name namestring
                                                  :fd fd
                                                  :pathname pathname
                                                  :element-type element-type)))
                            (close stream)
                            stream)))))
             (destructuring-bind (&key return
                                       new-filename
                                       new-if-exists
                                       new-if-does-not-exist)
                 (%open-error pathname errno if-exists if-does-not-exist)
               (when return
                 (return))
               (when new-filename
                 (setf filename new-filename))
               (when new-if-exists
                 (setf if-exists new-if-exists if-exists-given t))
               (when new-if-does-not-exist
                 (setf if-does-not-exist new-if-does-not-exist
                       if-does-not-exist-given t))))))))))
;;;; miscellany

;;; the Unix way to beep
(defun beep (stream)
  (write-char (code-char bell-char-code) stream)
  (finish-output stream))

;;; This is kind of like FILE-POSITION, but is an internal hack used
;;; by the filesys stuff to get and set the file name.
;;;
;;; FIXME: misleading name, screwy interface
(defun file-name (stream &optional new-name)
  (stream-api-dispatch (stream)
    :gray nil
    :native
    (when (typep stream 'fd-stream)
      (cond (new-name
             (setf (fd-stream-pathname stream) new-name)
             (setf (fd-stream-file stream)
                   (native-namestring (physicalize-pathname new-name)
                                      :as-file t))
             t)
            (t
             (fd-stream-pathname stream))))
    :simple (s-%file-name stream new-name)))

;; Fix the INPUT-CHAR-POS slot of STREAM after having consumed characters
;; from the CIN-BUFFER. This operation is done upon exit from a FAST-READ-CHAR
;; loop, and for each buffer refill inside the loop.
(defun update-input-char-pos (stream &optional (end +ansi-stream-in-buffer-length+))
  (do ((chars (ansi-stream-cin-buffer stream))
       (pos (form-tracking-stream-input-char-pos stream))
       (i (ansi-stream-in-index stream) (1+ i)))
      ((>= i end)
       (setf (form-tracking-stream-input-char-pos stream) pos))
    (let ((char (aref chars i)))
      (when (and (eql char #\Newline)
                 ;; record it only if it wasn't unread and re-read
                 (> pos (form-tracking-stream-last-newline stream)))
        (vector-push-extend pos (form-tracking-stream-newlines stream))
        (setf (form-tracking-stream-last-newline stream) pos))
      (incf pos))))

(defun tracking-stream-misc (stream operation arg1)
  ;; The :UNREAD operation will never be invoked because STREAM has a buffer,
  ;; so unreading is implemented entirely within ANSI-STREAM-UNREAD-CHAR.
  ;; But we do need to prevent attempts to change the absolute position.
  (stream-misc-case (operation)
    (:set-file-position (simple-stream-perror "~S is not positionable" stream))
    (t ; call next method
     (fd-stream-misc-routine stream operation arg1))))
