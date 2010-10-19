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
;;; Socket-simple-stream and socket-base-simple-stream

(def-stream-class socket-simple-stream (dual-channel-simple-stream)
  (;; keep the socket around; it could be handy e.g. for querying peer
   ;; host/port
   (socket :initform nil :type (or sb-bsd-sockets:socket null)
           :initarg :socket)))

(defmethod print-object ((object socket-simple-stream) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (with-stream-class (socket-simple-stream object)
      (cond ((not (any-stream-instance-flags object :simple))
             (princ "Invalid " stream))
            ((not (any-stream-instance-flags object :input :output))
             (princ "Closed " stream)))
      (format stream "~:(~A~)"
              (type-of object))
      (when (any-stream-instance-flags object :input :output)
        (multiple-value-bind (host port)
            (sb-bsd-sockets:socket-peername (sm socket object))
          (format stream " connected to host ~S, port ~S" host port))))))

(def-stream-class socket-base-simple-stream (dual-channel-simple-stream)
  ())

(defmethod device-open ((stream socket-simple-stream) options)
  (let* ((remote-host (getf options :remote-host))
         (remote-port (getf options :remote-port))
         (socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
    (unless (and remote-host remote-port)
      (error "device-open on ~S requires :remote-host and :remote-port arguments"
             'socket-simple-stream))
    (with-stream-class (socket-simple-stream stream)
      (ecase (getf options :direction :input)
        (:input (add-stream-instance-flags stream :input))
        (:output (add-stream-instance-flags stream :output))
        (:io (add-stream-instance-flags stream :input :output)))
      (setf (sm socket stream) socket)
      (sb-bsd-sockets:socket-connect socket remote-host remote-port)
      (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
        (when fd
          (add-stream-instance-flags stream :dual :simple)
          (when (any-stream-instance-flags stream :input)
            (setf (sm input-handle stream) fd)
            (unless (sm buffer stream)
              (let ((length (device-buffer-length stream)))
                (setf (sm buffer stream) (allocate-buffer length)
                      (sm buffpos stream) 0
                      (sm buffer-ptr stream) 0
                      (sm buf-len stream) length))))
          (when (any-stream-instance-flags stream :output)
            (setf (sm output-handle stream) fd)
            (unless (sm out-buffer stream)
              (let ((length (device-buffer-length stream)))
                (setf (sm out-buffer stream) (allocate-buffer length)
                      (sm outpos stream) 0
                      (sm max-out-pos stream) length)))
            (setf (sm control-out stream) *std-control-out-table*))
          (sb-ext:cancel-finalization socket)
          (sb-ext:finalize stream
                           (lambda ()
                             (sb-unix:unix-close fd)
                             (format *debug-io*
                                     "~&;;; ** closed socket (fd ~D)~%" fd))
                           :dont-save t)
          ;; this should be done with (setf stream-external-format)
          (let ((efmt (getf options :external-format :default)))
            (compose-encapsulating-streams stream efmt)
            (install-dual-channel-character-strategy (melding-stream stream)
                                                     efmt))
          stream)))))

(defmethod device-close ((stream socket-simple-stream) abort)
  (with-stream-class (socket-simple-stream stream)
    (sb-unix:unix-close (or (sm input-handle stream)
                            (sm output-handle stream)))
    (when (sm buffer stream)
      (free-buffer (sm buffer stream))
      (setf (sm buffer stream) nil))
    (when (sm out-buffer stream)
      (free-buffer (sm out-buffer stream))
      (setf (sm out-buffer stream) nil))
    (sb-ext:cancel-finalization stream)
    t))

(defmethod device-open ((stream socket-base-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-write ((stream socket-base-simple-stream) buffer
                         start end blocking)
  ;; @@2
  (call-next-method))

