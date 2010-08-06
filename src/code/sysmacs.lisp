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

;;;; these are initialized in cold init

(defvar *in-without-gcing*)
(defvar *gc-inhibit*)

;;; When the dynamic usage increases beyond this amount, the system
;;; notes that a garbage collection needs to occur by setting
;;; *GC-PENDING* to T. It starts out as NIL meaning nobody has figured
;;; out what it should be yet.
(defvar *gc-pending*)

#!+sb-thread
(defvar *stop-for-gc-pending*)

;;; This one is initialized by the runtime, at thread creation.  On
;;; non-x86oid gencgc targets, this is a per-thread list of objects
;;; which must not be moved during GC.  It is frobbed by the code for
;;; with-pinned-objects in src/compiler/target/macros.lisp.
#!+(and gencgc (not (or x86 x86-64)))
(defvar sb!vm::*pinned-objects*)

(defmacro without-gcing (&body body)
  #!+sb-doc
  "Executes the forms in the body without doing a garbage collection. It
inhibits both automatically and explicitly triggered collections. Finally,
upon leaving the BODY if gc is not inhibited it runs the pending gc.
Similarly, if gc is triggered in another thread then it waits until gc is
enabled in this thread.

Implies SB-SYS:WITHOUT-INTERRUPTS for BODY, and causes any nested
SB-SYS:WITH-INTERRUPTS to signal a warning during execution of the BODY.

Should be used with great care, and not at all in multithreaded application
code: Any locks that are ever acquired while GC is inhibited need to be always
held with GC inhibited to prevent deadlocks: if T1 holds the lock and is
stopped for GC while T2 is waiting for the lock inside WITHOUT-GCING the
system will be deadlocked. Since SBCL does not currently document its internal
locks, application code can never be certain that this invariant is
maintained."
  (with-unique-names (without-gcing-body)
    `(flet ((,without-gcing-body ()
              ,@body))
       (if *gc-inhibit*
           (,without-gcing-body)
           ;; We need to disable interrupts before disabling GC, so
           ;; that signal handlers using locks don't accidentally try
           ;; to grab them with GC inhibited.
           (let ((*in-without-gcing* t))
             (unwind-protect
                  (let* ((*allow-with-interrupts* nil)
                         (*interrupts-enabled* nil)
                         (*gc-inhibit* t))
                    (,without-gcing-body))
               ;; This is not racy becuase maybe_defer_handler
               ;; defers signals if *GC-INHIBIT* is NIL but there
               ;; is a pending gc or stop-for-gc.
               (when (or *interrupt-pending*
                         *gc-pending*
                         #!+sb-thread *stop-for-gc-pending*)
                 (sb!unix::receive-pending-interrupt))))))))

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

(defmacro with-out-stream/no-synonym (stream (slot &rest args) &optional stream-dispatch)
  `(let ((stream ,stream))
    ,(if stream-dispatch
         `(if (ansi-stream-p stream)
              (funcall (,slot stream) stream ,@args)
              ,@(when stream-dispatch
                  `(,(destructuring-bind (function &rest args) stream-dispatch
                                         `(,function stream ,@args)))))
         `(funcall (,slot stream) stream ,@args))))

(defmacro with-out-stream (stream (slot &rest args) &optional stream-dispatch)
  `(with-out-stream/no-synonym (out-synonym-of ,stream)
    (,slot ,@args) ,stream-dispatch))


;;;; These are hacks to make the reader win.

;;; This macro sets up some local vars for use by the
;;; FAST-READ-CHAR macro within the enclosed lexical scope. The stream
;;; is assumed to be a ANSI-STREAM.
;;;
;;; KLUDGE: Some functions (e.g. ANSI-STREAM-READ-LINE) use these variables
;;; directly, instead of indirecting through FAST-READ-CHAR.
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
;;; within the scope of a PREPARE-FOR-FAST-READ-CHAR.
(defmacro fast-read-char (&optional (eof-error-p t) (eof-value ()))
  `(cond
     ((not %frc-buffer%)
      (funcall %frc-method% %frc-stream% ,eof-error-p ,eof-value))
     ((= %frc-index% +ansi-stream-in-buffer-length+)
      (multiple-value-bind (eof-p index-or-value)
          (fast-read-char-refill %frc-stream% ,eof-error-p ,eof-value)
        (if eof-p
            index-or-value
            (progn
              (setq %frc-index% (1+ index-or-value))
              (aref %frc-buffer% index-or-value)))))
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
