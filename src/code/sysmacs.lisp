;;;; miscellaneous system hacking macros

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defmacro atomic-incf/symbol (symbol-name &optional (delta 1))
  #!-sb-thread
  `(incf ,symbol-name ,delta)
  #!+sb-thread
  `(locally
    (declare (optimize (safety 0) (speed 3)))
    (sb!vm::locked-symbol-global-value-add ',symbol-name ,delta)))

;;; When >0, inhibits garbage collection.
(declaim (type index *gc-inhibit*))
(defvar *gc-inhibit*) ; initialized in cold init

(defmacro without-gcing (&body body)
  #!+sb-doc
  "Executes the forms in the body without doing a garbage collection."
  `(unwind-protect
    (progn
      (atomic-incf/symbol *gc-inhibit*)
      ,@body)
    (atomic-incf/symbol *gc-inhibit* -1)
    (when (and *need-to-collect-garbage* (zerop *gc-inhibit*))
      (sub-gc))))


;;; EOF-OR-LOSE is a useful macro that handles EOF.
(defmacro eof-or-lose (stream eof-error-p eof-value)
  `(if ,eof-error-p
       (error 'end-of-file :stream ,stream)
       ,eof-value))

;;; These macros handle the special cases of T and NIL for input and
;;; output streams.
;;;
;;; FIXME: Shouldn't these be functions instead of macros?
(defmacro in-synonym-of (stream &optional check-type)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-input*)
             ((eq ,svar t) *terminal-io*)
             (t ,@(when check-type `((enforce-type ,svar ,check-type))) ;
                #!+high-security
                (unless (input-stream-p ,svar)
                  (error 'simple-type-error
                         :datum ,svar
                         :expected-type '(satisfies input-stream-p)
                         :format-control "~S isn't an input stream"
                         :format-arguments (list ,svar)))
                ,svar)))))
(defmacro out-synonym-of (stream &optional check-type)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-output*)
             ((eq ,svar t) *terminal-io*)
             (t ,@(when check-type `((check-type ,svar ,check-type)))
                #!+high-security
                (unless (output-stream-p ,svar)
                  (error 'simple-type-error
                         :datum ,svar
                         :expected-type '(satisfies output-stream-p)
                         :format-control "~S isn't an output stream."
                         :format-arguments (list ,svar)))
                ,svar)))))

;;; WITH-mumble-STREAM calls the function in the given SLOT of the
;;; STREAM with the ARGS for ANSI-STREAMs, or the FUNCTION with the
;;; ARGS for FUNDAMENTAL-STREAMs.
(defmacro with-in-stream (stream (slot &rest args) &optional stream-dispatch)
  `(let ((stream (in-synonym-of ,stream)))
    ,(if stream-dispatch
         `(if (ansi-stream-p stream)
              (funcall (,slot stream) stream ,@args)
              ,@(when stream-dispatch
                  `(,(destructuring-bind (function &rest args) stream-dispatch
                       `(,function stream ,@args)))))
         `(funcall (,slot stream) stream ,@args))))

(defmacro with-out-stream (stream (slot &rest args) &optional stream-dispatch)
  `(let ((stream (out-synonym-of ,stream)))
    ,(if stream-dispatch
         `(if (ansi-stream-p stream)
              (funcall (,slot stream) stream ,@args)
              ,@(when stream-dispatch
                  `(,(destructuring-bind (function &rest args) stream-dispatch
                                         `(,function stream ,@args)))))
         `(funcall (,slot stream) stream ,@args))))

;;;; These are hacks to make the reader win.

;;; This macro sets up some local vars for use by the
;;; FAST-READ-CHAR macro within the enclosed lexical scope. The stream
;;; is assumed to be a ANSI-STREAM.
(defmacro prepare-for-fast-read-char (stream &body forms)
  `(let* ((%frc-stream% ,stream)
          (%frc-method% (ansi-stream-in %frc-stream%))
          (%frc-buffer% (ansi-stream-cin-buffer %frc-stream%))
          (%frc-index% (ansi-stream-in-index %frc-stream%)))
     (declare (type index %frc-index%)
              (type ansi-stream %frc-stream%))
     ,@forms))

;;; This macro must be called after one is done with FAST-READ-CHAR
;;; inside its scope to decache the ANSI-STREAM-IN-INDEX.
(defmacro done-with-fast-read-char ()
  `(setf (ansi-stream-in-index %frc-stream%) %frc-index%))

;;; a macro with the same calling convention as READ-CHAR, to be used
;;; within the scope of a PREPARE-FOR-FAST-READ-CHAR
(defmacro fast-read-char (&optional (eof-error-p t) (eof-value ()))
  `(cond
    ((not %frc-buffer%)
     (funcall %frc-method% %frc-stream% ,eof-error-p ,eof-value))
    ((= %frc-index% +ansi-stream-in-buffer-length+)
     (prog1 (fast-read-char-refill %frc-stream% ,eof-error-p ,eof-value)
            (setq %frc-index% (ansi-stream-in-index %frc-stream%))))
    (t
     (prog1 (aref %frc-buffer% %frc-index%)
            (incf %frc-index%)))))

;;;; And these for the fasloader...

;;; Just like PREPARE-FOR-FAST-READ-CHAR except that we get the BIN
;;; method. The stream is assumed to be a ANSI-STREAM.
;;;
;;; KLUDGE: It seems weird to have to remember to explicitly call
;;; DONE-WITH-FAST-READ-BYTE at the end of this, given that we're
;;; already wrapping the stuff inside in a block. Why not rename this
;;; macro to WITH-FAST-READ-BYTE, do the DONE-WITH-FAST-READ-BYTE stuff
;;; automatically at the end of the block, and eliminate
;;; DONE-WITH-FAST-READ-BYTE as a separate entity? (and similarly
;;; for the FAST-READ-CHAR stuff) -- WHN 19990825
(defmacro prepare-for-fast-read-byte (stream &body forms)
  `(let* ((%frc-stream% ,stream)
          (%frc-method% (ansi-stream-bin %frc-stream%))
          (%frc-buffer% (ansi-stream-in-buffer %frc-stream%))
          (%frc-index% (ansi-stream-in-index %frc-stream%)))
     (declare (type index %frc-index%)
              (type ansi-stream %frc-stream%))
     ,@forms))

;;; Similar to fast-read-char, but we use a different refill routine & don't
;;; convert to characters. If ANY-TYPE is true, then this can be used on any
;;; integer streams, and we don't assert the result type.
(defmacro fast-read-byte (&optional (eof-error-p t) (eof-value ()) any-type)
  ;; KLUDGE: should use ONCE-ONLY on EOF-ERROR-P and EOF-VALUE -- WHN 19990825
  `(truly-the
    ,(if (and (eq eof-error-p t) (not any-type)) '(unsigned-byte 8) t)
    (cond
     ((not %frc-buffer%)
      (funcall %frc-method% %frc-stream% ,eof-error-p ,eof-value))
     ((= %frc-index% +ansi-stream-in-buffer-length+)
      (prog1 (fast-read-byte-refill %frc-stream% ,eof-error-p ,eof-value)
        (setq %frc-index% (ansi-stream-in-index %frc-stream%))))
     (t
      (prog1 (aref %frc-buffer% %frc-index%)
        (incf %frc-index%))))))
(defmacro done-with-fast-read-byte ()
  `(done-with-fast-read-char))
