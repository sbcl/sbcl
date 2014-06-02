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
    `(dx-flet ((,without-gcing-body ()
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
;;; If EOF-ERROR-P is statically T (not any random expression evaluating
;;; to T) then wrap the whole thing in (TRULY-THE CHARACTER ...)
;;; because it's either going to yield a character or signal EOF.
(defmacro fast-read-char (&optional (eof-error-p t) (eof-value ()))
  (let ((result
         `(cond
            ((not %frc-buffer%)
             (funcall %frc-method% %frc-stream% ,eof-error-p ,eof-value))
            ((= %frc-index% +ansi-stream-in-buffer-length+)
             (multiple-value-bind (eof-p index-or-value)
                 (fast-read-char-refill %frc-stream% ,eof-error-p ,eof-value)
               (if eof-p
                   index-or-value
                   (progn
                     (setq %frc-index% (1+ (truly-the index index-or-value)))
                     (aref %frc-buffer% index-or-value)))))
            (t
             (prog1 (aref %frc-buffer% %frc-index%)
               (incf %frc-index%))))))
    (if (eq eof-error-p 't)
        `(truly-the character ,result)
        result)))

;;;; And these for the fasloader...

;;; Just like PREPARE-FOR-FAST-READ-CHAR except that we get the BIN
;;; method. The stream is assumed to be a ANSI-STREAM.
;;;
;;; FIXME: Refactor PREPARE-FOR-FAST-READ-CHAR into similar shape.
(defmacro with-fast-read-byte ((type stream &optional (eof-error-p t) eof-value)
                               &body body)
  (aver (or (eq t eof-error-p) (eq t type)))
  (with-unique-names (f-stream f-method f-buffer f-index eof-p eof-val)
    `(let* ((,f-stream ,stream)
            (,eof-p ,eof-error-p)
            (,eof-val ,eof-value)
            (,f-method (ansi-stream-bin ,f-stream))
            (,f-buffer (ansi-stream-in-buffer ,f-stream))
            (,f-index (ansi-stream-in-index ,f-stream)))
       (declare (type ansi-stream ,f-stream)
                (type index ,f-index))
       (declare (disable-package-locks fast-read-byte))
       (flet ((fast-read-byte ()
                  (,@(cond ((equal '(unsigned-byte 8) type)
                            ;; KLUDGE: For some reason I haven't tracked down
                            ;; this makes a difference even in given the TRULY-THE.
                            `(logand #xff))
                           (t
                            `(identity)))
                   (truly-the ,type
                              (cond
                                ((not ,f-buffer)
                                 (funcall ,f-method ,f-stream ,eof-p ,eof-val))
                                ((= ,f-index +ansi-stream-in-buffer-length+)
                                 (prog1 (fast-read-byte-refill ,f-stream ,eof-p ,eof-val)
                                   (setq ,f-index (ansi-stream-in-index ,f-stream))))
                                (t
                                 (prog1 (aref ,f-buffer ,f-index)
                                   (incf ,f-index))))))))
         (declare (inline fast-read-byte))
         (declare (enable-package-locks read-byte))
         (unwind-protect
              (locally ,@body)
           (setf (ansi-stream-in-index ,f-stream) ,f-index))))))
