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
;;; Base class and generic function definitions for simple-streams

;;; See chapter
;;; 12.2 Strategy descriptions necessary for encapsulation
;;; in the Franz documentation for a description of the j-xxx-fn slots.

;;;; Types for buffer and strategy functions

(deftype simple-stream-buffer ()
  '(or sb-sys:system-area-pointer (sb-kernel:simple-unboxed-array (*))))

(deftype blocking ()
  '(member t nil :bnb))

(deftype j-listen-fn ()
  '(function (simple-stream) boolean))

(deftype j-read-char-fn ()
  '(function (simple-stream boolean t boolean) t)) ; may return EOF-VALUE

(deftype j-read-chars-fn ()
  '(function (simple-stream string (or character null) fixnum fixnum blocking)
             (values fixnum &optional (member nil t :eof))))

(deftype j-write-char-fn ()
  '(function ((or character null) simple-stream) (or character null)))

(deftype j-write-chars-fn ()
  '(function (string simple-stream fixnum fixnum) t)) ; return chars-written?

(deftype j-unread-char-fn ()
  '(function (simple-stream t) t)) ; "relaxed" arg is boolean?  what return?

;;;; Base simple-stream classes

(def-stream-class simple-stream (standard-object stream)
  (;; instance flags (not a normal slot in Allegro CL)
   (%flags :initform 0 :type fixnum)
   (plist :initform nil :type list :accessor stream-plist)

   ;; Strategy slots.  See section 12.2 of streams.htm for function
   ;; signatures and possible side-effects.

   ;; A function that determines if one character can be successfully
   ;; read from stream.
   (j-listen :initform #'sb-kernel:ill-in :type j-listen-fn)
   ;; A function that reads one character.
   (j-read-char :initform #'sb-kernel:ill-in :type j-read-char-fn)
   ;; A function that reads characters into a string.
   (j-read-chars :initform #'sb-kernel:ill-in :type j-read-chars-fn)
   ;; A function that writes one character.
   (j-write-char :initform #'sb-kernel:ill-out :type j-write-char-fn)
   ;; A function that writes characters from a string into the stream.
   (j-write-chars :initform #'sb-kernel:ill-out :type j-write-chars-fn)
   ;; A function that unreads the last character read.
   (j-unread-char :initform #'sb-kernel:ill-in :type j-unread-char-fn)

   ;; Other slots

   ;; TODO: find out what this one does
   (oc-state :initform nil)
   ;; TODO: find out what this one does
   (co-state :initform nil)
   (external-format :initform (find-external-format :default))

   ;; A fixnum (denoting a valid file descriptor), a stream, or nil if
   ;; the stream is not open for input.
   (input-handle :initform nil :initarg :input-handle
                 :type (or null fixnum stream)
                 :accessor stream-input-handle)
   ;; A fixnum (denoting a valid file descriptor), a stream, or nil if
   ;; the stream is not open for output.
   (output-handle :initform nil :initarg :output-handle
                  :type (or null fixnum stream)
                  :accessor stream-output-handle)
   (control-in :initform nil :type (or null simple-vector))
   (control-out :initform nil :type (or null simple-vector))

   ;; a stream, allowing for composing external formats (see
   ;; streams.htm, section 12.5) TODO: document this better
   (melded-stream :type (or null simple-stream))
   ;; a stream, allowing for composing external formats (see
   ;; streams.htm, section 12.5) TODO: document this better
   (melding-base :type (or null simple-stream))

   ;; Number of octets the last read-char operation consumed TODO:
   ;; document this better; what is the difference to
   ;; last-char-read-size ?
   (encapsulated-char-read-size :initform 0 :type fixnum)
   ;; Number of octets the last read-char operation consumed
   (last-char-read-size :initform 0 :type fixnum)
   (charpos :initform 0 :type (or null integer)
            :accessor stream-line-column)
   (record-end :initform nil :type (or null fixnum))

  ;; Input/output buffer.
   (buffer :initform nil :type (or simple-stream-buffer null))
   ;; Current position in buffer.
   (buffpos :initform 0 :type fixnum)
   ;; Maximum valid position in buffer, or -1 on eof.
   (buffer-ptr :initform 0 :type fixnum)
   (buf-len :initform 0 :type fixnum)

   (pending :initform nil :type list)
   (handler :initform nil :type (or null sb-impl::handler))))

(setq sb-pcl::*simple-stream-root-classoid* (sb-kernel:find-classoid 'simple-stream))

(def-stream-class single-channel-simple-stream (simple-stream)
  (;; the "dirty" flag -- if this is > 0, write out buffer contents
   ;; before changing position; see flush-buffer
   (mode :initform 0 :type fixnum)))

(def-stream-class dual-channel-simple-stream (simple-stream)
  (;; Output buffer.
   (out-buffer :initform nil :type (or simple-stream-buffer null))
   ;; Current position in output buffer.
   (outpos :initform 0 :type fixnum)
   ;; Buffer length (one greater than maximum output buffer index)
   (max-out-pos :initform 0 :type fixnum)))

;;; A stream with a string as buffer.
(def-stream-class string-simple-stream (simple-stream string-stream)
  ())


;;; ======================================================


;;;
;;; DEVICE-LEVEL FUNCTIONS
;;;

(defgeneric device-open (stream options))

(defgeneric device-close (stream abort))

(defgeneric device-buffer-length (stream))

(defgeneric device-file-position (stream))

(defgeneric (setf device-file-position) (value stream))

(defgeneric device-file-length (stream))

(defgeneric device-read (stream buffer start end blocking))

(defgeneric device-clear-input (stream buffer-only))

(defgeneric device-write (stream buffer start end blocking))

(defgeneric device-clear-output (stream))

(defgeneric device-finish-record (stream blocking action))


(defmethod shared-initialize :after ((instance simple-stream) slot-names
                                     &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names))
  (unless (slot-boundp instance 'melded-stream)
    (setf (slot-value instance 'melded-stream) instance)
    (setf (slot-value instance 'melding-base) instance))
  (unless (device-open instance initargs)
    (device-close instance t)))


(defmethod print-object ((object simple-stream) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (cond ((not (any-stream-instance-flags object :simple))
           (princ "Invalid " stream))
          ((not (any-stream-instance-flags object :input :output))
           (princ "Closed " stream)))
    (format stream "~:(~A~)" (type-of object))))

;;; This takes care of the things all device-close methods have to do,
;;; regardless of the type of simple-stream
(defmethod device-close :around ((stream simple-stream) abort)
  (with-stream-class (simple-stream stream)
    (when (any-stream-instance-flags stream :input :output)
      (when (any-stream-instance-flags stream :output)
        (ignore-errors (if abort
                           (clear-output stream)
                           (finish-output stream))))
      (call-next-method)
      (setf (sm input-handle stream) nil
            (sm output-handle stream) nil)
      (remove-stream-instance-flags stream :input :output)
      (sb-ext:cancel-finalization stream)
      ;; This sets all readers and writers to error-raising functions
      (setf (stream-external-format stream) :void))))

(defmethod device-close ((stream simple-stream) abort)
  (declare (ignore abort))
  t)

(defmethod device-buffer-length ((stream simple-stream))
  4096)

(defmethod device-file-position ((stream simple-stream))
  (with-stream-class (simple-stream stream)
    (sm buffpos stream)))

(defmethod (setf device-file-position) (value (stream simple-stream))
  (with-stream-class (simple-stream stream)
    (setf (sm buffpos stream) value)))

(defmethod device-file-length ((stream simple-stream))
  nil)

(defgeneric (setf stream-external-format) (value stream))

(defmethod (setf stream-external-format) :before (value (stream simple-stream))
  ;; (unless (eq value (sm external-format stream))
  ;;   flush out the existing external-format
  )

(defmethod (setf stream-external-format) :after
    (ef (stream single-channel-simple-stream))
  (compose-encapsulating-streams stream ef)
  (install-single-channel-character-strategy (melding-stream stream) ef nil))

(defmethod (setf stream-external-format) :after
    (ef (stream dual-channel-simple-stream))
  (compose-encapsulating-streams stream ef)
  (install-dual-channel-character-strategy (melding-stream stream) ef))


(defmethod device-read ((stream single-channel-simple-stream) buffer
                        start end blocking)
  (read-octets stream buffer start end blocking))

(defmethod device-read ((stream dual-channel-simple-stream) buffer
                        start end blocking)
  (read-octets stream buffer start end blocking))

(defmethod device-clear-input ((stream simple-stream) buffer-only)
  (declare (ignore buffer-only))
  nil)

(defmethod device-write ((stream single-channel-simple-stream) buffer
                         start end blocking)
  ;; buffer may be :flush to force/finish-output
  (when (or (and (null buffer) (not (eql start end)))
            (eq buffer :flush))
    (with-stream-class (single-channel-simple-stream stream)
      (setf buffer (sm buffer stream))
      (setf end (sm buffpos stream))))
  (write-octets stream buffer start end blocking))

(defmethod device-write ((stream dual-channel-simple-stream) buffer
                         start end blocking)
  ;; buffer may be :flush to force/finish-output
  (when (or (and (null buffer) (not (eql start end)))
            (eq buffer :flush))
    (with-stream-class (dual-channel-simple-stream stream)
      (setf buffer (sm out-buffer stream))
      (setf end (sm outpos stream))))
  (write-octets stream buffer start end blocking))

(defmethod device-clear-output ((stream simple-stream))
  nil)
