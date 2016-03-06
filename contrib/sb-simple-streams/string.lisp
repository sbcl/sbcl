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
;;; String-Simple-Stream and relatives

(def-stream-class string-input-simple-stream (string-simple-stream)
  ())

(def-stream-class string-output-simple-stream (string-simple-stream)
  ((out-buffer :initform nil :type (or simple-stream-buffer null))
   (outpos :initform 0 :type fixnum)
   (max-out-pos :initform 0 :type fixnum)))

(def-stream-class composing-stream (string-simple-stream)
  ())

(def-stream-class fill-pointer-output-simple-stream
    (string-output-simple-stream)
  ())

(def-stream-class xp-simple-stream (string-output-simple-stream)
  ())

(def-stream-class annotation-output-simple-stream (string-output-simple-stream)
  ())

(defmethod device-open :before ((stream string-input-simple-stream) options)
  ;; Taken with permission from ftp://ftp.franz.com/pub/duane/Simp-stms.ppt
  (with-stream-class (string-input-simple-stream stream)
    (let ((string (getf options :string)))
      (when (and string (null (sm buffer stream)))
        (let ((start (getf options :start))
              (end (or (getf options :end) (length string))))
          (setf (sm buffer stream) string
                (sm buffpos stream) start
                (sm buffer-ptr stream) end))))
    (install-string-input-character-strategy stream)
    (add-stream-instance-flags stream :string :input :simple)))

(defmethod device-open :before ((stream string-output-simple-stream) options)
  ;; Taken with permission from ftp://ftp.franz.com/pub/duane/Simp-stms.ppt
  (with-stream-class (string-output-simple-stream stream)
    (unless (sm out-buffer stream)
      (let ((string (getf options :string)))
        (if string
            (setf (sm out-buffer stream) string
                  (sm max-out-pos stream) (length string))
            (let ((buflen (max (device-buffer-length stream) 16)))
              (setf (sm out-buffer stream) (make-string buflen)
                    (sm max-out-pos stream) buflen)))))
    (unless (sm control-out stream)
      (setf (sm control-out stream) *std-control-out-table*))
    (install-string-output-character-strategy stream)
    (add-stream-instance-flags stream :string :output :simple)))

(defmethod device-open ((stream string-simple-stream) options)
  (declare (ignore options))
  (with-stream-class (string-simple-stream stream)
    (if (and (any-stream-instance-flags stream :simple)
             (any-stream-instance-flags stream :input :output))
        t
        nil)))

(defmethod device-file-position ((stream string-simple-stream))
  (with-stream-class (simple-stream stream)
    (sm buffpos stream)))

(defmethod (setf device-file-position) (value (stream string-simple-stream))
  (with-stream-class (simple-stream stream)
    (cond ((or (> value (sm buffer-ptr stream))
               (< value (- -1 (sm buffer-ptr stream))))
           nil)
          ((>= value 0)
           (setf (sm buffpos stream) value)
           t)
          (t
           (setf (sm buffpos stream) (+ (sm buffer-ptr stream) value 1))
           t))))

(defmethod device-file-length ((stream string-simple-stream))
  (with-stream-class (simple-stream stream)
    (sm buffer-ptr stream)))

(defmethod device-open ((stream fill-pointer-output-simple-stream) options)
  #| do something |#
  stream)

(defmethod device-file-position ((stream fill-pointer-output-simple-stream))
  (with-stream-class (fill-pointer-output-simple-stream stream)
    (fill-pointer (sm out-buffer stream))))

(defmethod (setf device-file-position)
    (value (stream fill-pointer-output-simple-stream))
  (with-stream-class (fill-pointer-output-simple-stream stream)
    (let ((buffer (sm out-buffer stream)))
      (cond ((or (> value (array-total-size buffer))
                 (< value (- -1 (array-total-size buffer))))
             nil)
            ((>= value 0)
             (setf (fill-pointer buffer) value))
            (t
             (setf (fill-pointer buffer)
                   (+ (array-total-size buffer) value 1)))))))

(defmethod device-open ((stream xp-simple-stream) options)
  #| do something |#
  stream)
