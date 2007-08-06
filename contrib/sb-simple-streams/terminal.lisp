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
;;; Terminal-Simple-Stream

(defvar *terminal-control-in-table*
  (make-control-table #\Newline #'std-dc-newline-in-handler))

(def-stream-class terminal-simple-stream (dual-channel-simple-stream)
  ())

(defmethod device-open ((stream terminal-simple-stream) options)
  (with-stream-class (terminal-simple-stream stream)
    (when (getf options :input-handle)
      (setf (sm input-handle stream) (getf options :input-handle))
      (add-stream-instance-flags stream :simple :dual :input)
      (when (sb-unix:unix-isatty (sm input-handle stream))
        (add-stream-instance-flags stream :interactive))
      (unless (sm buffer stream)
        (let ((length (device-buffer-length stream)))
          (setf (sm buffer stream) (allocate-buffer length)
                (sm buf-len stream) length)))
      (setf (sm control-in stream) *terminal-control-in-table*))
    (when (getf options :output-handle)
      (setf (sm output-handle stream) (getf options :output-handle))
      (add-stream-instance-flags stream :simple :dual :output)
      (unless (sm out-buffer stream)
        (let ((length (device-buffer-length stream)))
          (setf (sm out-buffer stream) (make-string length)
                (sm max-out-pos stream) length)))
      (setf (sm control-out stream) *std-control-out-table*))
    (let ((efmt (getf options :external-format :default)))
      (compose-encapsulating-streams stream efmt)
      (install-dual-channel-character-strategy
       (melding-stream stream) efmt)))
  stream)

(defmethod device-read ((stream terminal-simple-stream) buffer
                        start end blocking)
  (let ((result (call-next-method)))
    (if (= result -1) -2 result)))

(defmethod device-clear-input ((stream terminal-simple-stream) buffer-only)
  (unless buffer-only
    (let ((buffer (allocate-buffer sb-impl::+bytes-per-buffer+)))
      (unwind-protect
           (loop until (<= (read-octets stream buffer
                                        0 sb-impl::+bytes-per-buffer+ nil)
                           0))
        (free-buffer buffer)))))
