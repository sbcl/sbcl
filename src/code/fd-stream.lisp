;;;; streams for UNIX file descriptors

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; BUFFER
;;;;
;;;; Streams hold BUFFER objects, which contain a SAP, size of the
;;;; memory area the SAP stands for (LENGTH bytes), and HEAD and TAIL
;;;; indexes which delimit the "valid", or "active" area of the
;;;; memory. HEAD is inclusive, TAIL is exclusive.
;;;;
;;;; Buffers get allocated lazily, and are recycled by returning them
;;;; to the *AVAILABLE-BUFFERS* list. Every buffer has it's own
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

(defstruct (buffer (:constructor %make-buffer (sap length)))
  (sap (missing-arg) :type system-area-pointer :read-only t)
  (length (missing-arg) :type index :read-only t)
  (head 0 :type index)
  (tail 0 :type index))
(declaim (freeze-type buffer))

(defvar *available-buffers* ()
  #!+sb-doc
  "List of available buffers.")

(defvar *available-buffers-lock* (sb!thread:make-mutex
                                  :name "lock for *AVAILABLE-BUFFERS*")
  #!+sb-doc
  "Mutex for access to *AVAILABLE-BUFFERS*.")

(defmacro with-available-buffers-lock ((&optional) &body body)
  ;; CALL-WITH-SYSTEM-MUTEX because streams are low-level enough to be
  ;; async signal safe, and in particular a C-c that brings up the
  ;; debugger while holding the mutex would lose badly.
  `(sb!thread::with-system-mutex (*available-buffers-lock*)
     ,@body))

(defconstant +bytes-per-buffer+ (* 4 1024)
  #!+sb-doc
  "Default number of bytes per buffer.")

(defun alloc-buffer (&optional (size +bytes-per-buffer+))
  ;; Don't want to allocate & unwind before the finalizer is in place.
  (without-interrupts
    (let* ((sap (allocate-system-memory size))
           (buffer (%make-buffer sap size)))
      (when (zerop (sap-int sap))
        (error "Could not allocate ~D bytes for buffer." size))
      (finalize buffer (lambda ()
                         (deallocate-system-memory sap size))
                :dont-save t)
      buffer)))

(defun get-buffer ()
  ;; Don't go for the lock if there is nothing to be had -- sure,
  ;; another thread might just release one before we get it, but that
  ;; is not worth the cost of locking. Also release the lock before
  ;; allocation, since it's going to take a while.
  (if *available-buffers*
      (or (with-available-buffers-lock ()
            (pop *available-buffers*))
          (alloc-buffer))
      (alloc-buffer)))

(declaim (inline reset-buffer))
(defun reset-buffer (buffer)
  (setf (buffer-head buffer) 0
        (buffer-tail buffer) 0)
  buffer)

(defun release-buffer (buffer)
  (reset-buffer buffer)
  (with-available-buffers-lock ()
    (push buffer *available-buffers*)))


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
  (fd -1 :type #!-win32 fixnum #!+win32 sb!vm:signed-word)
  ;; What do we know about the FD?
  (fd-type :unknown :type keyword)
  ;; controls when the output buffer is flushed
  (buffering :full :type (member :full :line :none))
  ;; controls whether the input buffer must be cleared before output
  ;; (must be done for files, not for sockets, pipes and other data
  ;; sources where input and output aren't related).  non-NIL means
  ;; don't clear input buffer.
  (dual-channel-p nil)
  ;; character position if known -- this may run into bignums, but
  ;; we probably should flip it into null then for efficiency's sake...
  (output-column nil :type (or unsigned-byte null))
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
  (output-bytes #'ill-out :type function))

(defun fd-stream-bivalent-p (stream)
  (eq (fd-stream-element-mode stream) :bivalent))

(def!method print-object ((fd-stream fd-stream) stream)
  (declare (type stream stream))
  (print-unreadable-object (fd-stream stream :type t :identity t)
    (format stream "for ~S" (fd-stream-name fd-stream))))

;;; This is a separate buffer management function, as it wants to be
;;; clever about locking -- grabbing the lock just once.
(defun release-fd-stream-buffers (fd-stream)
  (let ((ibuf (fd-stream-ibuf fd-stream))
        (obuf (fd-stream-obuf fd-stream))
        (queue (loop for item in (fd-stream-output-queue fd-stream)
                       when (buffer-p item)
                       collect (reset-buffer item))))
    (when ibuf
      (push (reset-buffer ibuf) queue))
    (when obuf
      (push (reset-buffer obuf) queue))
    ;; ...so, anything found?
    (when queue
      ;; detach from stream
      (setf (fd-stream-ibuf fd-stream) nil
            (fd-stream-obuf fd-stream) nil
            (fd-stream-output-queue fd-stream) nil)
      ;; splice to *available-buffers*
      (with-available-buffers-lock ()
        (setf *available-buffers* (nconc queue *available-buffers*))))))

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
  (let* ((newlines (form-tracking-stream-newlines stream))
         (index (position charpos newlines :test #'>= :from-end t)))
    ;; Line numbers traditionally begin at 1, columns at 0.
    (if index
        ;; INDEX is 1 less than the number of newlines seen
        ;; up to and including this startpos.
        ;; e.g. index=0 => 1 newline seen => line=2
        (cons (+ index 2)
              ;; 1 char after the newline = column 0
              (- charpos (aref newlines index) 1))
        ;; zero newlines were seen
        (cons 1 charpos))))

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
                (space (- (buffer-length obuf) tail)))
           (when (plusp space)
             (copy-to-buffer obuf tail (min space (- end start)))
             (go :more-output-p)))
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
               (synchronize-stream-output stream)
               (loop
                 (let ((length (- tail head)))
                   (multiple-value-bind (count errno)
                       (sb!unix:unix-write (fd-stream-fd stream) (buffer-sap obuf)
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
                              #!-win32
                              ((eql errno sb!unix:ewouldblock)
                               ;; Blocking, queue or wair.
                               (queue-or-wait))
                              ;; if interrupted on win32, just try again
                              #!+win32 ((eql errno sb!unix:eintr))
                              (t
                               (simple-stream-perror "Couldn't write to ~s"
                                                     stream errno)))))))))))))

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
  (synchronize-stream-output stream)
  (let (not-first-p)
    (tagbody
     :pop-buffer
       (let* ((buffer (pop (fd-stream-output-queue stream)))
              (head (buffer-head buffer))
              (length (- (buffer-tail buffer) head)))
         (declare (index head length))
         (aver (>= length 0))
         (multiple-value-bind (count errno)
             (sb!unix:unix-write (fd-stream-fd stream) (buffer-sap buffer)
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
                  #!+win32
                  (simple-stream-perror "Couldn't write to ~S." stream errno)
                  #!-win32
                  (if (= errno sb!unix:ewouldblock)
                      (bug "Unexpected blocking in WRITE-OUTPUT-FROM-QUEUE.")
                      (simple-stream-perror "Couldn't write to ~S"
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
           (synchronize-stream-output stream)
           (multiple-value-bind (count errno)
               (sb!unix:unix-write (fd-stream-fd stream) thing start length)
             (cond ((eql count length)
                    ;; Complete write -- done!
                    )
                   (count
                    (aver (< count length))
                    ;; Partial write -- buffer the rest.
                    (buffer-output stream thing (+ start count) end))
                   (t
                    ;; Could not write -- buffer or error.
                    #!+win32
                    (simple-stream-perror "couldn't write to ~s" stream errno)
                    #!-win32
                    (if (= errno sb!unix:ewouldblock)
                        (buffer-output stream thing start end)
                        (simple-stream-perror "couldn't write to ~s" stream errno)))))))))

;;; Deprecated -- can go away after 1.1 or so. Deprecated because
;;; this is not something we want to export. Nikodemus thinks the
;;; right thing is to support a low-level non-stream like IO layer,
;;; akin to java.nio.
(declaim (inline output-raw-bytes))
(define-deprecated-function :late "1.0.8.16" output-raw-bytes write-sequence
    (stream thing &optional start end)
  (write-or-buffer-output stream thing (or start 0) (or end (length thing))))

;;;; output routines and related noise

(defvar *output-routines* ()
  #!+sb-doc
  "List of all available output routines. Each element is a list of the
  element-type output, the kind of buffering, the function name, and the number
  of bytes per element.")

;;; common idioms for reporting low-level stream and file problems
(defun simple-stream-perror (note-format stream errno)
  (error 'simple-stream-error
         :stream stream
         :format-control "~@<~?: ~2I~_~A~:>"
         :format-arguments (list note-format (list stream) (strerror errno))))
(defun simple-file-perror (note-format pathname errno)
  (error 'simple-file-error
         :pathname pathname
         :format-control "~@<~?: ~2I~_~A~:>"
         :format-arguments
         (list note-format (list pathname) (strerror errno))))

(defun c-string-encoding-error (external-format code)
  (error 'c-string-encoding-error
         :external-format external-format
         :code code))
(defun c-string-decoding-error (external-format sap offset count)
  (error 'c-string-decoding-error
         :external-format external-format
         :octets (sap-ref-octets sap offset count)))

;;; Returning true goes into end of file handling, false will enter another
;;; round of input buffer filling followed by re-entering character decode.
(defun stream-decoding-error-and-handle (stream octet-count)
  (restart-case
      (error 'stream-decoding-error
             :external-format (stream-external-format stream)
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
      (fd-stream-resync stream)
      nil)
    (force-end-of-file ()
      :report (lambda (stream)
                (format stream "~@<Force an end of file.~@:>"))
      (setf (fd-stream-eof-forced-p stream) t))
    (input-replacement (string)
      :report (lambda (stream)
                (format stream "~@<Use string as replacement input, ~
                               attempt to resync at a character ~
                               boundary and continue.~@:>"))
      :interactive (lambda ()
                     (format *query-io* "~@<Enter a string: ~@:>")
                     (finish-output *query-io*)
                     (list (read *query-io*)))
      (let ((string (reverse (string string)))
            (instead (fd-stream-instead stream)))
        (dotimes (i (length string))
          (vector-push-extend (char string i) instead))
        (fd-stream-resync stream)
        (when (> (length string) 0)
          (setf (fd-stream-listen stream) t)))
      nil)))

(defun stream-encoding-error-and-handle (stream code)
  (restart-case
      (error 'stream-encoding-error
             :external-format (stream-external-format stream)
             :stream stream
             :code code)
    (output-nothing ()
      :report (lambda (stream)
                (format stream "~@<Skip output of this character.~@:>"))
      (throw 'output-nothing nil))
    (output-replacement (string)
      :report (lambda (stream)
                (format stream "~@<Output replacement string.~@:>"))
      :interactive (lambda ()
                     (format *query-io* "~@<Enter a string: ~@:>")
                     (finish-output *query-io*)
                     (list (read *query-io*)))
      (let ((string (string string)))
        (fd-sout stream (string string) 0 (length string)))
      (throw 'output-nothing nil))))

(defun external-format-encoding-error (stream code)
  (if (streamp stream)
      (stream-encoding-error-and-handle stream code)
      (c-string-encoding-error stream code)))

(defun synchronize-stream-output (stream)
  ;; If we're reading and writing on the same file, flush buffered
  ;; input and rewind file position accordingly.
  (unless (fd-stream-dual-channel-p stream)
    (let ((adjust (nth-value 1 (flush-input-buffer stream))))
      (unless (eql 0 adjust)
        (sb!unix:unix-lseek (fd-stream-fd stream) (- adjust) sb!unix:l_incr)))))

(defun fd-stream-output-finished-p (stream)
  (let ((obuf (fd-stream-obuf stream)))
    (or (not obuf)
        (and (zerop (buffer-tail obuf))
             (not (fd-stream-output-queue stream))))))

(defmacro output-wrapper/variable-width ((stream size buffering restart)
                                         &body body)
  (let ((stream-var (gensym "STREAM")))
    `(let* ((,stream-var ,stream)
            (obuf (fd-stream-obuf ,stream-var))
            (tail (buffer-tail obuf))
            (size ,size))
      ,(unless (eq (car buffering) :none)
         `(when (< (buffer-length obuf) (+ tail size))
            (setf obuf (flush-output-buffer ,stream-var)
                  tail (buffer-tail obuf))))
      ,(unless (eq (car buffering) :none)
         ;; FIXME: Why this here? Doesn't seem necessary.
         `(synchronize-stream-output ,stream-var))
      ,(if restart
           `(catch 'output-nothing
              ,@body
              (setf (buffer-tail obuf) (+ tail size)))
           `(progn
             ,@body
             (setf (buffer-tail obuf) (+ tail size))))
      ,(ecase (car buffering)
         (:none
          `(flush-output-buffer ,stream-var))
         (:line
          `(when (eql byte #\Newline)
             (flush-output-buffer ,stream-var)))
         (:full))
    (values))))

(defmacro output-wrapper ((stream size buffering restart) &body body)
  (let ((stream-var (gensym "STREAM")))
    `(let* ((,stream-var ,stream)
            (obuf (fd-stream-obuf ,stream-var))
            (tail (buffer-tail obuf)))
       ,(unless (eq (car buffering) :none)
          `(when (< (buffer-length obuf) (+ tail ,size))
             (setf obuf (flush-output-buffer ,stream-var)
                   tail (buffer-tail obuf))))
       ;; FIXME: Why this here? Doesn't seem necessary.
       ,(unless (eq (car buffering) :none)
          `(synchronize-stream-output ,stream-var))
       ,(if restart
            `(catch 'output-nothing
               ,@body
               (setf (buffer-tail obuf) (+ tail ,size)))
            `(progn
               ,@body
               (setf (buffer-tail obuf) (+ tail ,size))))
       ,(ecase (car buffering)
          (:none
           `(flush-output-buffer ,stream-var))
          (:line
           `(when (eql byte #\Newline)
              (flush-output-buffer ,stream-var)))
          (:full))
       (values))))

(defmacro def-output-routines/variable-width
    ((name-fmt size restart external-format &rest bufferings)
     &body body)
  (declare (optimize (speed 1)))
  (cons 'progn
        (mapcar
            (lambda (buffering)
              (let ((function
                     (intern (format nil name-fmt (string (car buffering))))))
                `(progn
                   (defun ,function (stream byte)
                     (declare (ignorable byte))
                     (output-wrapper/variable-width (stream ,size ,buffering ,restart)
                       ,@body))
                   (setf *output-routines*
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
  (declare (optimize (speed 1)))
  (cons 'progn
        (mapcar
            (lambda (buffering)
              (let ((function
                     (intern (format nil name-fmt (string (car buffering))))))
                `(progn
                   (defun ,function (stream byte)
                     (output-wrapper (stream ,size ,buffering ,restart)
                       ,@body))
                   (setf *output-routines*
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

;;; FIXME: is this used anywhere any more?
(def-output-routines ("OUTPUT-CHAR-~A-BUFFERED"
                      1
                      t
                      (:none character)
                      (:line character)
                      (:full character))
  (if (eql byte #\Newline)
      (setf (fd-stream-output-column stream) 0)
      (incf (fd-stream-output-column stream)))
  (setf (sap-ref-8 (buffer-sap obuf) tail)
        (char-code byte)))

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

#!+64-bit
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
;;; use OUTPUT-RAW-BYTES to buffer the string. Update charpos by
;;; checking to see where the last newline was.
(defun fd-sout (stream thing start end)
  (declare (type fd-stream stream) (type string thing))
  (let ((start (or start 0))
        (end (or end (length (the vector thing)))))
    (declare (fixnum start end))
    (let ((last-newline
           (string-dispatch (simple-base-string
                             #!+sb-unicode
                             (simple-array character (*))
                             string)
               thing
             (position #\newline thing :from-end t
                       :start start :end end))))
      (if (and (typep thing 'base-string)
               (eq (fd-stream-external-format-keyword stream) :latin-1))
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
  (default-replacement-character (missing-arg) :type character)
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
  ;; We indirect through symbols in these functions so that a
  ;; developer working on the octets code can easily redefine things
  ;; and use the new function definition without redefining the
  ;; external format as well.  The slots above don't do any
  ;; indirection because a developer working with those slots would be
  ;; redefining the external format anyway.
  (octets-to-string-fun (missing-arg) :type function)
  (string-to-octets-fun (missing-arg) :type function))

(defun ef-char-size (ef-entry)
  (if (variable-width-external-format-p ef-entry)
      (bytes-for-char-fun ef-entry)
      (funcall (bytes-for-char-fun ef-entry) #\x)))

(defun sb!alien::string-to-c-string (string external-format)
  (declare (type simple-string string))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (get-external-format-or-lose external-format)))
      (funcall (ef-write-c-string-fun external-format) string))))

(defun sb!alien::c-string-to-string (sap external-format element-type)
  (declare (type system-area-pointer sap))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (get-external-format-or-lose external-format)))
      (funcall (ef-read-c-string-fun external-format) sap element-type))))

(defun wrap-external-format-functions (external-format fun)
  (let ((result (%copy-external-format external-format)))
    (macrolet ((frob (accessor)
                 `(setf (,accessor result) (funcall fun (,accessor result)))))
      (frob ef-read-n-chars-fun)
      (frob ef-read-char-fun)
      (frob ef-write-n-bytes-fun)
      (frob ef-write-char-none-buffered-fun)
      (frob ef-write-char-line-buffered-fun)
      (frob ef-write-char-full-buffered-fun)
      (frob ef-resync-fun)
      (frob ef-bytes-for-char-fun)
      (frob ef-read-c-string-fun)
      (frob ef-write-c-string-fun)
      (frob ef-octets-to-string-fun)
      (frob ef-string-to-octets-fun))
    result))

(defvar *external-formats* (make-hash-table)
  #!+sb-doc
  "Hashtable of all available external formats. The table maps from
  external-format names to EXTERNAL-FORMAT structures.")

(defun get-external-format-or-lose (external-format)
  (or (get-external-format external-format)
      (error "Undefined external-format: ~S" external-format)))

(defun external-format-keyword (external-format)
  (typecase external-format
    (keyword external-format)
    ((cons keyword) (car external-format))))

(defun fd-stream-external-format-keyword (stream)
  (external-format-keyword (fd-stream-external-format stream)))

(defun canonize-external-format (external-format entry)
  (typecase external-format
    (keyword (first (ef-names entry)))
    ((cons keyword) (cons (first (ef-names entry)) (rest external-format)))))

;;; Find an output routine to use given the type and buffering. Return
;;; as multiple values the routine, the real type transfered, and the
;;; number of bytes per element.
(defun pick-output-routine (type buffering &optional external-format)
  (when (subtypep type 'character)
    (let ((entry (get-external-format-or-lose external-format)))
      (return-from pick-output-routine
        (values (ecase buffering
                  (:none (ef-write-char-none-buffered-fun entry))
                  (:line (ef-write-char-line-buffered-fun entry))
                  (:full (ef-write-char-full-buffered-fun entry)))
                'character
                1
                (ef-write-n-bytes-fun entry)
                (ef-char-size entry)
                (canonize-external-format external-format entry)))))
  (dolist (entry *output-routines*)
    (when (and (subtypep type (first entry))
               (eq buffering (second entry))
               (or (not (fifth entry))
                   (eq external-format (fifth entry))))
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
(defvar *input-routines* ())

;;; Return whether a primitive partial read operation on STREAM's FD
;;; would (probably) block.  Signal a `simple-stream-error' if the
;;; system call implementing this operation fails.
;;;
;;; It is "may" instead of "would" because "would" is not quite
;;; correct on win32.  However, none of the places that use it require
;;; further assurance than "may" versus "will definitely not".
(defun sysread-may-block-p (stream)
  #!+win32
  ;; This answers T at EOF on win32, I think.
  (not (sb!win32:fd-listen (fd-stream-fd stream)))
  #!-win32
  (not (sb!unix:unix-simple-poll (fd-stream-fd stream) :input 0)))

;;; If the read would block wait (using SERVE-EVENT) till input is available,
;;; then fill the input buffer, and return the number of bytes read. Throws
;;; to EOF-INPUT-CATCHER if the eof was reached.
(defun refill-input-buffer (stream)
  (let ((fd (fd-stream-fd stream))
        (errno 0)
        (count 0))
    (tagbody
       #!+win32
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
            ((:wait-for-input) (go #!-win32 :wait-for-input #!+win32 :main))
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
                         (inline sb!unix:unix-read))
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
                      (sb!unix:unix-read fd (sap+ sap tail) (- length tail)))
                (cond ((null count)
                       (if (eql errno
                                #!+win32 sb!unix:eintr
                                #!-win32 sb!unix:ewouldblock)
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
            (size nil))
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
                   (return-from use-instead result))
                 (unless
                     (catch 'eof-input-catcher
                       (setf decode-break-reason
                             (block decode-break-reason
                               (input-at-least ,stream-var ,(if (consp bytes)
                                                                (car bytes)
                                                                `(setq size ,bytes)))
                               (let* ((byte (sap-ref-8 (buffer-sap ibuf) (buffer-head ibuf))))
                                 (declare (ignorable byte))
                                 ,@(when (consp bytes)
                                     `((let ((sap (buffer-sap ibuf))
                                             (head (buffer-head ibuf)))
                                         (declare (ignorable sap head))
                                         (setq size ,(cadr bytes))
                                         (input-at-least ,stream-var size))))
                                 (setq ,element-var (locally ,@read-forms))
                                 (setq ,retry-var nil))
                               nil))
                       (when decode-break-reason
                         (when (stream-decoding-error-and-handle
                                stream decode-break-reason)
                           (setq ,retry-var nil)
                           (throw 'eof-input-catcher nil)))
                       t)
                   (let ((octet-count (- (buffer-tail ibuf)
                                         (buffer-head ibuf))))
                     (when (or (zerop octet-count)
                               (and (not ,element-var)
                                    (not decode-break-reason)
                                    (stream-decoding-error-and-handle
                                     stream octet-count)))
                       (setq ,retry-var nil))))))
           (cond (,element-var
                  (incf (buffer-head ibuf) size)
                  ,element-var)
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

#!+64-bit
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
(defun pick-input-routine (type &optional external-format)
  (when (subtypep type 'character)
    (let ((entry (get-external-format-or-lose external-format)))
      (return-from pick-input-routine
        (values (ef-read-char-fun entry)
                'character
                1
                (ef-read-n-chars-fun entry)
                (ef-char-size entry)
                (canonize-external-format external-format entry)))))
  (dolist (entry *input-routines*)
    (when (and (subtypep type (first entry))
               (or (not (fourth entry))
                   (eq external-format (fourth entry))))
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

;;; the N-BIN method for FD-STREAMs
;;;
;;; Note that this blocks in UNIX-READ. It is generally used where
;;; there is a definite amount of reading to be done, so blocking
;;; isn't too problematical.
(defun fd-stream-read-n-bytes (stream buffer start requested eof-error-p
                               &aux (total-copied 0))
  (declare (type fd-stream stream))
  (declare (type index start requested total-copied))
  (aver (= (length (fd-stream-instead stream)) 0))
  (do ()
      (nil)
    (let* ((remaining-request (- requested total-copied))
           (ibuf (fd-stream-ibuf stream))
           (head (buffer-head ibuf))
           (tail (buffer-tail ibuf))
           (available (- tail head))
           (n-this-copy (min remaining-request available))
           (this-start (+ start total-copied))
           (this-end (+ this-start n-this-copy))
           (sap (buffer-sap ibuf)))
      (declare (type index remaining-request head tail available))
      (declare (type index n-this-copy))
      ;; Copy data from stream buffer into user's buffer.
      (%byte-blt sap head buffer this-start this-end)
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
            ))))

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
      (loop for char across string summing (funcall sizer char)))))

(defun find-external-format (external-format)
  (when external-format
    (get-external-format external-format)))

(defun variable-width-external-format-p (ef-entry)
  (and ef-entry (not (null (ef-resync-fun ef-entry)))))

(defun bytes-for-char-fun (ef-entry)
  (if ef-entry (ef-bytes-for-char-fun ef-entry) (constantly 1)))

(defmacro define-unibyte-mapping-external-format
    (canonical-name (&rest other-names) &body exceptions)
  (let ((->code-name (symbolicate canonical-name '->code-mapper))
        (code->-name (symbolicate 'code-> canonical-name '-mapper))
        (get-bytes-name (symbolicate 'get- canonical-name '-bytes))
        (string->-name (symbolicate 'string-> canonical-name))
        (define-string*-name (symbolicate 'define- canonical-name '->string*))
        (string*-name (symbolicate canonical-name '->string*))
        (define-string-name (symbolicate 'define- canonical-name '->string))
        (string-name (symbolicate canonical-name '->string))
        (->string-aref-name (symbolicate canonical-name '->string-aref)))
    `(progn
       (define-unibyte-mapper ,->code-name ,code->-name
         ,@exceptions)
       (declaim (inline ,get-bytes-name))
       (defun ,get-bytes-name (string pos)
         (declare (optimize speed (safety 0))
                  (type simple-string string)
                  (type array-range pos))
         (get-latin-bytes #',code->-name ,canonical-name string pos))
       (defun ,string->-name (string sstart send null-padding)
         (declare (optimize speed (safety 0))
                  (type simple-string string)
                  (type array-range sstart send))
         (values (string->latin% string sstart send #',get-bytes-name null-padding)))
       (defmacro ,define-string*-name (accessor type)
         (declare (ignore type))
         (let ((name (make-od-name ',string*-name accessor)))
           `(progn
              (defun ,name (string sstart send array astart aend)
                (,(make-od-name 'latin->string* accessor)
                  string sstart send array astart aend #',',->code-name)))))
       (instantiate-octets-definition ,define-string*-name)
       (defmacro ,define-string-name (accessor type)
         (declare (ignore type))
         (let ((name (make-od-name ',string-name accessor)))
           `(progn
              (defun ,name (array astart aend)
                (,(make-od-name 'latin->string accessor)
                  array astart aend #',',->code-name)))))
       (instantiate-octets-definition ,define-string-name)
       (define-unibyte-external-format ,canonical-name ,other-names
         (let ((octet (,code->-name bits)))
           (if octet
               (setf (sap-ref-8 sap tail) octet)
               (external-format-encoding-error stream bits)))
         (let ((code (,->code-name byte)))
           (if code
               (code-char code)
               (return-from decode-break-reason 1)))
         ,->string-aref-name
         ,string->-name))))

(defmacro define-unibyte-external-format
    (canonical-name (&rest other-names)
     out-form in-form octets-to-string-symbol string-to-octets-symbol)
  `(define-external-format/variable-width (,canonical-name ,@other-names)
     t #\? 1
     ,out-form
     1
     ,in-form
     ,octets-to-string-symbol
     ,string-to-octets-symbol))

(defmacro define-external-format/variable-width
    (external-format output-restart replacement-character
     out-size-expr out-expr in-size-expr in-expr
     octets-to-string-sym string-to-octets-sym)
  (let* ((name (first external-format))
         (out-function (symbolicate "OUTPUT-BYTES/" name))
         (format (format nil "OUTPUT-CHAR-~A-~~A-BUFFERED" (string name)))
         (in-function (symbolicate "FD-STREAM-READ-N-CHARACTERS/" name))
         (in-char-function (symbolicate "INPUT-CHAR/" name))
         (resync-function (symbolicate "RESYNC/" name))
         (size-function (symbolicate "BYTES-FOR-CHAR/" name))
         (read-c-string-function (symbolicate "READ-FROM-C-STRING/" name))
         (output-c-string-function (symbolicate "OUTPUT-TO-C-STRING/" name))
         (n-buffer (gensym "BUFFER")))
    `(progn
      (defun ,size-function (byte)
        (declare (ignorable byte))
        ,out-size-expr)
      (defun ,out-function (stream string flush-p start end)
        (let ((start (or start 0))
              (end (or end (length string))))
          (declare (type index start end))
          (synchronize-stream-output stream)
          (unless (<= 0 start end (length string))
            (sequence-bounding-indices-bad-error string start end))
          (do ()
              ((= end start))
            (let ((obuf (fd-stream-obuf stream)))
              (string-dispatch (simple-base-string
                                #!+sb-unicode (simple-array character (*))
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
                         `(catch 'output-nothing)
                         `(progn))
                     (do* ()
                          ((or (= start end) (< (- len tail) 4)))
                       (let* ((byte (aref string start))
                              (bits (char-code byte))
                              (size ,out-size-expr))
                         ,out-expr
                         (incf tail size)
                         (setf (buffer-tail obuf) tail)
                         (incf start)))
                     (go flush))
                  ;; Exited via CATCH: skip the current character.
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
          (if (eql byte #\Newline)
              (setf (fd-stream-output-column stream) 0)
              (incf (fd-stream-output-column stream)))
        (let ((bits (char-code byte))
              (sap (buffer-sap obuf))
              (tail (buffer-tail obuf)))
          ,out-expr))
      (defun ,in-function (stream buffer start requested eof-error-p
                           &aux (total-copied 0))
        (declare (type fd-stream stream)
                 (type index start requested total-copied)
                 (type
                  (simple-array character (#.+ansi-stream-in-buffer-length+))
                  buffer))
        (when (fd-stream-eof-forced-p stream)
          (setf (fd-stream-eof-forced-p stream) nil)
          (return-from ,in-function 0))
        (do ((instead (fd-stream-instead stream)))
            ((= (fill-pointer instead) 0)
             (setf (fd-stream-listen stream) nil))
          (setf (aref buffer (+ start total-copied)) (vector-pop instead))
          (incf total-copied)
          (when (= requested total-copied)
            (when (= (fill-pointer instead) 0)
              (setf (fd-stream-listen stream) nil))
            (return-from ,in-function total-copied)))
        (do ()
            (nil)
          (let* ((ibuf (fd-stream-ibuf stream))
                 (head (buffer-head ibuf))
                 (tail (buffer-tail ibuf))
                 (sap (buffer-sap ibuf))
                 (decode-break-reason nil))
            (declare (type index head tail))
            ;; Copy data from stream buffer into user's buffer.
            (do ((size nil nil))
                ((or (= tail head) (= requested total-copied)))
              (setf decode-break-reason
                    (block decode-break-reason
                      ,@(when (consp in-size-expr)
                          `((when (> ,(car in-size-expr) (- tail head))
                              (return))))
                      (let ((byte (sap-ref-8 sap head)))
                        (declare (ignorable byte))
                        (setq size ,(if (consp in-size-expr) (cadr in-size-expr) in-size-expr))
                        (when (> size (- tail head))
                          (return))
                        (setf (aref buffer (+ start total-copied)) ,in-expr)
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
                (when (plusp total-copied)
                  (return-from ,in-function total-copied))
                (when (stream-decoding-error-and-handle
                       stream decode-break-reason)
                  (if eof-error-p
                      (error 'end-of-file :stream stream)
                      (return-from ,in-function total-copied)))
                ;; we might have been given stuff to use instead, so
                ;; we have to return (and trust our caller to know
                ;; what to do about TOTAL-COPIED being 0).
                (return-from ,in-function total-copied)))
            (setf (buffer-head ibuf) head)
            ;; Maybe we need to refill the stream buffer.
            (cond ( ;; If was data in the stream buffer, we're done.
                   (plusp total-copied)
                   (return total-copied))
                  ( ;; If EOF, we're done in another way.
                   (or (eq decode-break-reason 'eof)
                       (null (catch 'eof-input-catcher
                               (refill-input-buffer stream))))
                   (if eof-error-p
                       (error 'end-of-file :stream stream)
                       (return total-copied)))
                  ;; Otherwise we refilled the stream buffer, so fall
                  ;; through into another pass of the loop.
                  ))))
      (def-input-routine/variable-width ,in-char-function (character
                                                           ,external-format
                                                           ,in-size-expr
                                                           sap head)
        (let ((byte (sap-ref-8 sap head)))
          (declare (ignorable byte))
          ,in-expr))
      (defun ,resync-function (stream)
        (let ((ibuf (fd-stream-ibuf stream))
              size)
          (catch 'eof-input-catcher
            (loop
               (incf (buffer-head ibuf))
               (input-at-least stream ,(if (consp in-size-expr) (car in-size-expr) `(setq size ,in-size-expr)))
               (unless (block decode-break-reason
                         (let* ((sap (buffer-sap ibuf))
                                (head (buffer-head ibuf))
                                (byte (sap-ref-8 sap head)))
                           (declare (ignorable byte))
                           ,@(when (consp in-size-expr)
                               `((setq size ,(cadr in-size-expr))
                                 (input-at-least stream size)))
                           (setf head (buffer-head ibuf))
                           ,in-expr)
                         nil)
                 (return))))))
      (defun ,read-c-string-function (sap element-type)
        (declare (type system-area-pointer sap))
        (locally
            (declare (optimize (speed 3) (safety 0)))
          (let* ((stream ,name)
                 (size 0) (head 0) (byte 0) (char nil)
                 (decode-break-reason nil)
                 (length (dotimes (count (1- ARRAY-DIMENSION-LIMIT) count)
                           (setf decode-break-reason
                                 (block decode-break-reason
                                   (setf byte (sap-ref-8 sap head)
                                         size ,(if (consp in-size-expr)
                                                   (cadr in-size-expr)
                                                   in-size-expr)
                                         char ,in-expr)
                                   (incf head size)
                                   nil))
                           (when decode-break-reason
                             (c-string-decoding-error
                              ,name sap head decode-break-reason))
                           (when (zerop (char-code char))
                             (return count))))
                 (string (make-string length :element-type element-type)))
            (declare (ignorable stream)
                     (type index head length) ;; size
                     (type (unsigned-byte 8) byte)
                     (type (or null character) char)
                     (type string string))
            (setf head 0)
            (dotimes (index length string)
              (setf decode-break-reason
                    (block decode-break-reason
                      (setf byte (sap-ref-8 sap head)
                            size ,(if (consp in-size-expr)
                                      (cadr in-size-expr)
                                      in-size-expr)
                            char ,in-expr)
                      (incf head size)
                      nil))
              (when decode-break-reason
                (c-string-decoding-error
                 ,name sap head decode-break-reason))
              (setf (aref string index) char)))))

      (defun ,output-c-string-function (string)
        (declare (type simple-string string))
        (locally
            (declare (optimize (speed 3) (safety 0)))
          (let* ((length (length string))
                 (char-length (make-array (1+ length) :element-type 'index))
                 (buffer-length
                  (+ (loop for i of-type index below length
                        for byte of-type character = (aref string i)
                        for bits = (char-code byte)
                        sum (setf (aref char-length i)
                                  (the index ,out-size-expr)) of-type index)
                     (let* ((byte (code-char 0))
                            (bits (char-code byte)))
                       (declare (ignorable byte bits))
                       (setf (aref char-length length)
                             (the index ,out-size-expr)))))
                 (tail 0)
                 (,n-buffer (make-array buffer-length
                                        :element-type '(unsigned-byte 8)))
                 stream)
            (declare (type index length buffer-length tail)
                     (type null stream)
                     (ignorable stream))
            (with-pinned-objects (,n-buffer)
              (let ((sap (vector-sap ,n-buffer)))
                (declare (system-area-pointer sap))
                (loop for i of-type index below length
                      for byte of-type character = (aref string i)
                      for bits = (char-code byte)
                      for size of-type index = (aref char-length i)
                      do (prog1
                             ,out-expr
                           (incf tail size)))
                (let* ((bits 0)
                       (byte (code-char bits))
                       (size (aref char-length length)))
                  (declare (ignorable bits byte size))
                  ,out-expr)))
            ,n-buffer)))

      (let ((entry (%make-external-format
                    :names ',external-format
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
                                            (apply ',string-to-octets-sym rest)))))
        (dolist (ef ',external-format)
          (setf (gethash ef *external-formats*) entry))))))

;;;; utility functions (misc routines, etc)

;;; Fill in the various routine slots for the given type. INPUT-P and
;;; OUTPUT-P indicate what slots to fill. The buffering slot must be
;;; set prior to calling this routine.
(defun set-fd-stream-routines (fd-stream element-type external-format
                               input-p output-p buffer-p)
  (let* ((target-type (case element-type
                        (unsigned-byte '(unsigned-byte 8))
                        (signed-byte '(signed-byte 8))
                        (:default 'character)
                        (t element-type)))
         (character-stream-p (subtypep target-type 'character))
         (bivalent-stream-p (eq element-type :default))
         normalized-external-format
         char-size
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

    (when (and character-stream-p (eq external-format :default))
      (/show0 "/getting default external format")
      (setf external-format (default-external-format)))

    (when input-p
      (when (or (not character-stream-p) bivalent-stream-p)
        (setf (values bin-routine bin-type bin-size read-n-characters
                      char-size normalized-external-format)
              (pick-input-routine (if bivalent-stream-p '(unsigned-byte 8)
                                      target-type)
                                  external-format))
        (unless bin-routine
          (error "could not find any input routine for ~S" target-type)))
      (when character-stream-p
        (setf (values cin-routine cin-type cin-size read-n-characters
                      char-size normalized-external-format)
              (pick-input-routine target-type external-format))
        (unless cin-routine
          (error "could not find any input routine for ~S" target-type)))
      (setf (fd-stream-in fd-stream) cin-routine
            (fd-stream-bin fd-stream) bin-routine)
      ;; character type gets preferential treatment
      (setf input-size (or cin-size bin-size))
      (setf input-type (or cin-type bin-type))
      (when normalized-external-format
        (setf (fd-stream-external-format fd-stream) normalized-external-format
              (fd-stream-char-size fd-stream) char-size))
      (when (= (or cin-size 1) (or bin-size 1) 1)
        (setf (fd-stream-n-bin fd-stream) ;XXX
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
                                   :element-type 'character)))
                ((equal target-type '(unsigned-byte 8))
                 (setf (ansi-stream-in-buffer fd-stream)
                       (make-array +ansi-stream-in-buffer-length+
                                   :element-type '(unsigned-byte 8))))))))

    (when output-p
      (when (or (not character-stream-p) bivalent-stream-p)
        (setf (values bout-routine bout-type bout-size output-bytes
                      char-size normalized-external-format)
              (let ((buffering (fd-stream-buffering fd-stream)))
                (if bivalent-stream-p
                    (pick-output-routine '(unsigned-byte 8)
                                         (if (eq :line buffering)
                                             :full
                                             buffering)
                                         external-format)
                    (pick-output-routine target-type buffering external-format))))
        (unless bout-routine
          (error "could not find any output routine for ~S buffered ~S"
                 (fd-stream-buffering fd-stream)
                 target-type)))
      (when character-stream-p
        (setf (values cout-routine cout-type cout-size output-bytes
                      char-size normalized-external-format)
              (pick-output-routine target-type
                                   (fd-stream-buffering fd-stream)
                                   external-format))
        (unless cout-routine
          (error "could not find any output routine for ~S buffered ~S"
                 (fd-stream-buffering fd-stream)
                 target-type)))
      (when normalized-external-format
        (setf (fd-stream-external-format fd-stream) normalized-external-format
              (fd-stream-char-size fd-stream) char-size))
      (when character-stream-p
        (setf (fd-stream-output-bytes fd-stream) output-bytes))
      (setf (fd-stream-out fd-stream) cout-routine
            (fd-stream-bout fd-stream) bout-routine
            (fd-stream-sout fd-stream) (if (eql cout-size 1)
                                           #'fd-sout #'ill-out))
      (setf output-size (or cout-size bout-size))
      (setf output-type (or cout-type bout-type)))

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
                 (error "Input type (~S) and output type (~S) are unrelated?"
                        input-type
                        output-type))))))

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
        (sb!unix:unix-close (fd-stream-fd fd-stream))
        (set-closed-flame fd-stream)
        (cancel-finalization fd-stream))
    ;; On error unwind from WITHOUT-INTERRUPTS.
    (serious-condition (e)
      (error e)))
  ;; Release all buffers. If this is undone, or interrupted,
  ;; we're still safe: buffers have finalizers of their own.
  (release-fd-stream-buffers fd-stream))

;;; Flushes the current input buffer and any supplied replacements,
;;; and returns the input buffer, and the amount of of flushed input
;;; in bytes.
(defun flush-input-buffer (stream)
  (let ((unread (length (fd-stream-instead stream))))
    (setf (fill-pointer (fd-stream-instead stream)) 0)
    (let ((ibuf (fd-stream-ibuf stream)))
      (if ibuf
          (let ((head (buffer-head ibuf))
                (tail (buffer-tail ibuf)))
            (values (reset-buffer ibuf) (- (+ unread tail) head)))
          (values nil unread)))))

(defun fd-stream-clear-input (stream)
  (flush-input-buffer stream)
  #!+win32
  (progn
    (sb!win32:fd-clear-input (fd-stream-fd stream))
    (setf (fd-stream-listen stream) nil))
  #!-win32
  (catch 'eof-input-catcher
    (loop until (sysread-may-block-p stream)
          do
          (refill-input-buffer stream)
          (reset-buffer (fd-stream-ibuf stream)))
    t))

;;; Handle miscellaneous operations on FD-STREAM.
(defun fd-stream-misc-routine (fd-stream operation &optional arg1 arg2)
  (declare (ignore arg2))
  (case operation
    (:listen
     (labels ((do-listen ()
                (let ((ibuf (fd-stream-ibuf fd-stream)))
                  (or (not (eql (buffer-head ibuf) (buffer-tail ibuf)))
                      (fd-stream-listen fd-stream)
                      #!+win32
                      (sb!win32:fd-listen (fd-stream-fd fd-stream))
                      #!-win32
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
     (decf (buffer-head (fd-stream-ibuf fd-stream))
           (fd-stream-character-size fd-stream arg1)))
    (:close
     ;; Drop input buffers
     (setf (ansi-stream-in-index fd-stream) +ansi-stream-in-buffer-length+
           (ansi-stream-cin-buffer fd-stream) nil
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
                          (sb!unix:unix-rename orig file)
                        ;; FIXME: Why is this a SIMPLE-STREAM-ERROR, and the
                        ;; others are SIMPLE-FILE-ERRORS? Surely they should
                        ;; all be the same?
                        (unless okay
                          (error 'simple-stream-error
                                 :format-control
                                 "~@<Couldn't restore ~S to its original contents ~
                                  from ~S while closing ~S: ~2I~_~A~:>"
                                 :format-arguments
                                 (list file orig fd-stream (strerror err))
                                 :stream fd-stream))))
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
                        (sb!unix:unix-unlink file)
                      (unless okay
                        (error 'simple-file-error
                               :pathname file
                               :format-control
                               "~@<Couldn't remove ~S while closing ~S: ~2I~_~A~:>"
                               :format-arguments
                               (list file fd-stream (strerror err)))))))))
           (t
            (finish-fd-stream-output fd-stream)
            (let ((orig (fd-stream-original fd-stream)))
              (when (and orig (fd-stream-delete-original fd-stream))
                (multiple-value-bind (okay err) (sb!unix:unix-unlink orig)
                  (unless okay
                    (error 'simple-file-error
                           :pathname orig
                           :format-control
                           "~@<couldn't delete ~S while closing ~S: ~2I~_~A~:>"
                           :format-arguments
                           (list orig fd-stream (strerror err)))))))
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
    (:external-format
     (fd-stream-external-format fd-stream))
    (:interactive-p
     (plusp (the (integer 0)
              (sb!unix:unix-isatty (fd-stream-fd fd-stream)))))
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
     #!-win32
     (multiple-value-bind (okay dev ino mode nlink uid gid rdev size
                                atime mtime ctime blksize blocks)
         (sb!unix:unix-fstat (fd-stream-fd fd-stream))
       (declare (ignore ino nlink uid gid rdev
                        atime mtime ctime blksize blocks))
       (unless okay
         (simple-stream-perror "failed Unix fstat(2) on ~S" fd-stream dev))
       (if (zerop mode)
           nil
           (truncate size (fd-stream-element-size fd-stream))))
     #!+win32
     (let* ((handle (fd-stream-fd fd-stream))
            (element-size (fd-stream-element-size fd-stream)))
       (multiple-value-bind (got native-size)
           (sb!win32:get-file-size-ex handle 0)
         (if (zerop got)
             ;; Might be a block device, in which case we fall back to
             ;; a non-atomic workaround:
             (let* ((here (sb!unix:unix-lseek handle 0 sb!unix:l_incr))
                    (there (sb!unix:unix-lseek handle 0 sb!unix:l_xtnd)))
               (when (and here there)
                 (sb!unix:unix-lseek handle here sb!unix:l_set)
                 (truncate there element-size)))
             (truncate native-size element-size)))))
    (:file-string-length
     (etypecase arg1
       (character (fd-stream-character-size fd-stream arg1))
       (string (fd-stream-string-size fd-stream arg1))))
    (:file-position
     (if arg1
         (fd-stream-set-file-position fd-stream arg1)
         (fd-stream-get-file-position fd-stream)))))

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
    (let ((posn (sb!unix:unix-lseek (fd-stream-fd stream) 0 sb!unix:l_incr)))
      (declare (type (or (alien sb!unix:unix-offset) null) posn))
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
              (or (alien sb!unix:unix-offset) (member nil :start :end))
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
                (values 0 sb!unix:l_set))
               (:end
                (values 0 sb!unix:l_xtnd))
               (t
                (values (* position-spec (fd-stream-element-size stream))
                        sb!unix:l_set)))
           (declare (type (alien sb!unix:unix-offset) offset))
           (let ((posn (sb!unix:unix-lseek (fd-stream-fd stream)
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
               (typep posn '(alien sb!unix:unix-offset))))))))


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
  (cond ((not (or input-p output-p))
         (setf input t))
        ((not (or input output))
         (error "File descriptor must be opened either for input or output.")))
  (let* ((constructor (ecase class
                        (fd-stream '%make-fd-stream)
                        (form-tracking-stream '%make-form-tracking-stream)))
         (element-mode (stream-element-type-stream-element-mode element-type))
         (stream (funcall constructor
                          :fd fd
                          :fd-type
                          #!-win32 (sb!unix:fd-type fd)
                          ;; KLUDGE.
                          #!+win32 (if serve-events
                                       :unknown
                                       :regular)
                          :name name
                          :file file
                          :original original
                          :delete-original delete-original
                          :pathname pathname
                          :buffering buffering
                          :dual-channel-p dual-channel-p
                          :element-mode element-mode
                          :serve-events serve-events
                          :timeout
                          (if timeout
                              (coerce timeout 'single-float)
                              nil))))
    (set-fd-stream-routines stream element-type external-format
                            input output input-buffer-p)
    (when auto-close
      (finalize stream
                (lambda ()
                  (sb!unix:unix-close fd)
                  #!+sb-show
                  (format *terminal-io* "** closed file descriptor ~W **~%"
                          fd))
                :dont-save t))
    stream))

;;; Pick a name to use for the backup file for the :IF-EXISTS
;;; :RENAME-AND-DELETE and :RENAME options.
(defun pick-backup-name (name)
  (declare (type simple-string name))
  (concatenate 'simple-string name ".bak"))

;;; Rename NAMESTRING to ORIGINAL. First, check whether we have write
;;; access, since we don't want to trash unwritable files even if we
;;; technically can. We return true if we succeed in renaming.
(defun rename-the-old-one (namestring original)
  (unless (sb!unix:unix-access namestring sb!unix:w_ok)
    (error "~@<The file ~2I~_~S ~I~_is not writable.~:>" namestring))
  (multiple-value-bind (okay err) (sb!unix:unix-rename namestring original)
    (if okay
        t
        (error 'simple-file-error
               :pathname namestring
               :format-control
               "~@<couldn't rename ~2I~_~S ~I~_to ~2I~_~S: ~4I~_~A~:>"
               :format-arguments (list namestring original (strerror err))))))

(defun open (filename
             &key
               (direction :input)
               (element-type 'base-char)
               (if-exists nil if-exists-given)
               (if-does-not-exist nil if-does-not-exist-given)
               (external-format :default)
               ;; :class is a private option - use it at your own risk
               (class 'fd-stream)
             &aux                       ; Squelch assignment warning.
             (direction direction)
             (if-does-not-exist if-does-not-exist)
             (if-exists if-exists))
  #!+sb-doc
  "Return a stream which reads from or writes to FILENAME.
  Defined keywords:
   :DIRECTION - one of :INPUT, :OUTPUT, :IO, or :PROBE
   :ELEMENT-TYPE - the type of object to read or write, default BASE-CHAR
   :IF-EXISTS - one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE,
                       :OVERWRITE, :APPEND, :SUPERSEDE or NIL
   :IF-DOES-NOT-EXIST - one of :ERROR, :CREATE or NIL
  See the manual for details."

  ;; Calculate useful stuff.
  (multiple-value-bind (input output mask)
      (ecase direction
        (:input  (values   t nil sb!unix:o_rdonly))
        (:output (values nil   t sb!unix:o_wronly))
        (:io     (values   t   t sb!unix:o_rdwr))
        (:probe  (values   t nil sb!unix:o_rdonly)))
    (declare (type index mask))
    (let* ( ;; PATHNAME is the pathname we associate with the stream.
           (pathname (merge-pathnames filename))
           (physical (physicalize-pathname pathname))
           (truename (probe-file physical))
           ;; NAMESTRING is the native namestring we open the file with.
           (namestring (cond (truename
                              (native-namestring truename :as-file t))
                             ((or (not input)
                                  (and input (eq if-does-not-exist :create))
                                  (and (eq direction :io)
                                       (not if-does-not-exist-given)))
                              (native-namestring physical :as-file t)))))
      (flet ((open-error (format-control &rest format-arguments)
               (error 'simple-file-error
                      :pathname pathname
                      :format-control format-control
                      :format-arguments format-arguments)))
        ;; Process if-exists argument if we are doing any output.
        (cond (output
               (unless if-exists-given
                 (setf if-exists
                       (if (eq (pathname-version pathname) :newest)
                           :new-version
                           :error)))
               (case if-exists
                 ((:new-version :error nil)
                  (setf mask (logior mask sb!unix:o_excl)))
                 ((:rename :rename-and-delete)
                  (setf mask (logior mask sb!unix:o_creat)))
                 ((:supersede)
                  (setf mask (logior mask sb!unix:o_trunc)))
                 (:append
                  (setf mask (logior mask sb!unix:o_append)))))
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
        (cond ((and if-exists-given
                    truename
                    (eq if-exists :new-version))
               (open-error "OPEN :IF-EXISTS :NEW-VERSION is not supported ~
                            when a new version must be created."))
              ((eq if-does-not-exist :create)
               (setf mask (logior mask sb!unix:o_creat)))
              ((not (member if-exists '(:error nil))))
              ;; Both if-does-not-exist and if-exists now imply
              ;; that there will be no opening of files, and either
              ;; an error would be signalled, or NIL returned
              ((and (not if-exists) (not if-does-not-exist))
               (return-from open))
              ((and if-exists if-does-not-exist)
               (open-error "OPEN :IF-DOES-NOT-EXIST ~s ~
                                 :IF-EXISTS ~s will always signal an error."
                           if-does-not-exist if-exists))
              (truename
               (if if-exists
                   (open-error "File exists ~s." pathname)
                   (return-from open)))
              (if-does-not-exist
               (open-error "File does not exist ~s." pathname))
              (t
               (return-from open)))
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
                             (sb!unix:unix-stat namestring)
                           (declare (ignore inode)
                                    (type (or index null) orig-mode))
                           (cond
                             (okay
                              (when (and output (= (logand orig-mode #o170000)
                                                   #o40000))
                                (error 'simple-file-error
                                       :pathname pathname
                                       :format-control
                                       "can't open ~S for output: is a directory"
                                       :format-arguments (list namestring)))
                              (setf mode (logand orig-mode #o777))
                              t)
                             ((eql err/dev sb!unix:enoent)
                              nil)
                             (t
                              (simple-file-perror "can't find ~S"
                                                  namestring
                                                  err/dev)))))))
              (unless (and exists
                           (rename-the-old-one namestring original))
                (setf original nil)
                (setf delete-original nil)
                ;; In order to use :SUPERSEDE instead, we have to make
                ;; sure SB!UNIX:O_CREAT corresponds to
                ;; IF-DOES-NOT-EXIST. SB!UNIX:O_CREAT was set before
                ;; because of IF-EXISTS being :RENAME.
                (unless (eq if-does-not-exist :create)
                  (setf mask
                        (logior (logandc2 mask sb!unix:o_creat)
                                sb!unix:o_trunc)))
                (setf if-exists :supersede))))

          ;; Now we can try the actual Unix open(2).
          (multiple-value-bind (fd errno)
              (if namestring
                  (sb!unix:unix-open namestring mask mode)
                  (values nil sb!unix:enoent))
            (flet ((vanilla-open-error ()
                     (simple-file-perror "error opening ~S" pathname errno)))
              (cond ((numberp fd)
                     (case direction
                       ((:input :output :io)
                        ;; For O_APPEND opened files, lseek returns 0 until first write.
                        ;; So we jump ahead here.
                        (when (eq if-exists :append)
                          (sb!unix:unix-lseek fd 0 sb!unix:l_xtnd))
                        (make-fd-stream fd
                                        :class class
                                        :input input
                                        :output output
                                        :element-type element-type
                                        :external-format external-format
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
                          stream))))
                    ((eql errno sb!unix:enoent)
                     (case if-does-not-exist
                       (:error (vanilla-open-error))
                       (:create
                        (open-error "~@<The path ~2I~_~S ~I~_does not exist.~:>"
                                    pathname))
                       (t nil)))
                    ((and (eql errno sb!unix:eexist) (null if-exists))
                     nil)
                    (t
                     (vanilla-open-error))))))))))

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
  ;; FIXME: on gencgc the 4 standard fd-streams {stdin,stdout,stderr,tty}
  ;; and these synonym streams are baked into +pseudo-static-generation+.
  ;; Is that inadvertent?
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
  ;; Unbind to make sure we're not accidently dealing with it
  ;; before we're ready (or after we think it's been deinitialized).
  (with-available-buffers-lock ()
    (without-package-locks
        (makunbound '*available-buffers*))))

(defun stdstream-external-format (fd outputp)
  #!-win32 (declare (ignore fd outputp))
  (let* ((keyword #!+win32 (if (and (/= fd -1)
                                    (logbitp 0 fd)
                                    (logbitp 1 fd))
                               :ucs-2
                               (if outputp
                                   (sb!win32::console-output-codepage)
                                   (sb!win32::console-input-codepage)))
                  #!-win32 (default-external-format))
         (ef (get-external-format keyword))
         (replacement (ef-default-replacement-character ef)))
    `(,keyword :replacement ,replacement)))

;;; This is called whenever a saved core is restarted.
(defun stream-reinit (&optional init-buffers-p)
  (when init-buffers-p
    (with-available-buffers-lock ()
      (aver (not (boundp '*available-buffers*)))
      (setf *available-buffers* nil)))
  (with-simple-output-to-string (*error-output*)
    (multiple-value-bind (in out err)
        #!-win32 (values 0 1 2)
        #!+win32 (sb!win32::get-std-handles)
      (labels (#!+win32
               (nul-stream (name inputp outputp)
                 (let* ((nul-name #.(coerce "NUL" 'simple-base-string))
                        (nul-handle
                          (cond
                            ((and inputp outputp)
                             (sb!win32:unixlike-open nul-name sb!unix:o_rdwr 0))
                            (inputp
                             (sb!win32:unixlike-open nul-name sb!unix:o_rdonly 0))
                            (outputp
                             (sb!win32:unixlike-open nul-name sb!unix:o_wronly 0))
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
                    :external-format (stdstream-external-format nul-handle outputp))))
               (stdio-stream (handle name inputp outputp)
                 (cond
                   #!+win32
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
                     :external-format (stdstream-external-format handle outputp))))))
        (setf *stdin*  (stdio-stream in  "standard input"  t   nil)
              *stdout* (stdio-stream out "standard output" nil t)
              *stderr* (stdio-stream err "standard error"  nil t))))
    #!+win32
    (setf *tty* (make-two-way-stream *stdin* *stdout*))
    #!-win32
    ;; FIXME: what is this call to COERCE doing? XC can't dump non-base-strings.
    (let* ((ttyname #.(coerce "/dev/tty" 'simple-base-string))
           (tty (sb!unix:unix-open ttyname sb!unix:o_rdwr #o666)))
      (setf *tty*
            (if tty
                (make-fd-stream tty :name "the terminal"
                                    :input t :output t :buffering :line
                                    :external-format (stdstream-external-format
                                                      tty t)
                                    :serve-events t
                                    :auto-close t)
                (make-two-way-stream *stdin* *stdout*))))
    (princ (get-output-stream-string *error-output*) *stderr*))
  (values))

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
  (when (typep stream 'fd-stream)
      (cond (new-name
             (setf (fd-stream-pathname stream) new-name)
             (setf (fd-stream-file stream)
                   (native-namestring (physicalize-pathname new-name)
                                      :as-file t))
             t)
            (t
             (fd-stream-pathname stream)))))

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

(defun tracking-stream-misc (stream operation &optional arg1 arg2)
  ;; The :UNREAD operation will never be invoked because STREAM has a buffer,
  ;; so unreading is implemented entirely within ANSI-STREAM-UNREAD-CHAR.
  ;; But we do need to prevent attempts to change the absolute position.
  (case operation
    (:file-position
     (if arg1
         (error 'simple-stream-error
                :format-control "~S is not positionable"
                :format-arguments (list stream))
         (fd-stream-get-file-position stream)))
    (t ; call next method
     (fd-stream-misc-routine stream operation arg1 arg2))))

;; Using (get-std-handle-or-null +std-error-handle+) instead of the file
;; descriptor might make this work on win32, but I don't know.
#!-win32
(macrolet ((stderr () 2))
 (!defglobal *!cold-stderr-buf* " ")
 (defun !make-cold-stderr-stream ()
   (%make-fd-stream
    :out (lambda (stream ch)
           (declare (ignore stream))
           (let ((b (truly-the (simple-base-string 1) *!cold-stderr-buf*)))
             (setf (char b 0) ch)
             (sb!unix:unix-write (stderr) b 0 1)))
    :sout (lambda (stream string start end)
            (declare (ignore stream))
            (flet ((out (s start len)
                     (sb!unix:unix-write (stderr) s start len)))
              (if (typep string 'simple-base-string)
                  (out string start (- end start))
                  (let ((n (- end start)))
                    ;; will croak if there is any non-BASE-CHAR in the string
                    (out (replace (make-array n :element-type 'base-char)
                                  string :start2 start) 0 n)))))
    :misc (constantly nil))))
