;;;; miscellaneous system hacking macros

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; these are initialized by create_thread_struct()

(defvar *in-without-gcing*)
(defvar *gc-inhibit*)
(defvar *gc-pin-code-pages*)

;;; When the dynamic usage increases beyond this amount, the system
;;; notes that a garbage collection needs to occur by setting
;;; *GC-PENDING* to T. It starts out as NIL meaning nobody has figured
;;; out what it should be yet.
(defvar *gc-pending*)

#+sb-thread
(defvar *stop-for-gc-pending*)

;;; This one is initialized by the runtime, at thread creation.
;;; It is a per-thread list of objects which must not be moved during GC,
;;; and manipulated by WITH-PINNED-OBJECTS.
;;; If doesn't really do anything if #+cheneygc
(defvar sb-vm::*pinned-objects*)

(defmacro without-gcing (&body body)
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
               ;; This is not racy because can_hande_now()
               ;; defers signals if *GC-INHIBIT* is NIL but there
               ;; is a pending gc or stop-for-gc.
               (when (or *interrupt-pending*
                         *gc-pending*
                         #+sb-thread *stop-for-gc-pending*)
                 (sb-unix::receive-pending-interrupt))))))))

;;; EOF-OR-LOSE is a useful macro that handles EOF.
(defmacro eof-or-lose (stream eof-error-p eof-value)
  `(if ,eof-error-p
       (error 'end-of-file :stream ,stream)
       ,eof-value))

;;; These macros handle the special cases of T and NIL for input and
;;; output streams.
;;; FIXME: should we kill the high-security feature? Or, if enabled,
;;; ensure that the designated stream has the right directionality?
;;; (Nothing prevents *TERMINAL-IO* from being bound to an output-only stream, e.g.)
;;;
;;; FIXME: Shouldn't these be functions instead of macros?
(defmacro in-stream-from-designator (stream)
  (let ((svar (sb-xc:gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-input*)
             ((eq ,svar t) *terminal-io*)
             (t
                #+high-security
                (unless (input-stream-p ,svar)
                  (error 'simple-type-error
                         :datum ,svar
                         :expected-type '(satisfies input-stream-p)
                         :format-control "~S isn't an input stream"
                         :format-arguments (list ,svar)))
                ,svar)))))
;; As noted above, this code is a tad wasteful for probably not a huge
;; performance gain. On the other hand, where STREAM is known to be of type
;; STREAM, it produces shorter code. But we could shorten the general case:
#|
   (lambda (x)
    (block nil
     (symbol-value
      (case x ((nil) '*standard-output*)
              ((t) '*terminal-io*)
              (t (return x))))))
|#
(defmacro out-stream-from-designator (stream)
  (let ((svar (sb-xc:gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-output*)
             ((eq ,svar t) *terminal-io*)
             (t
                #+high-security
                (unless (output-stream-p ,svar)
                  (error 'simple-type-error
                         :datum ,svar
                         :expected-type '(satisfies output-stream-p)
                         :format-control "~S isn't an output stream."
                         :format-arguments (list ,svar)))
                ,svar)))))

;;;; These are hacks to make the reader win.

;;; This macro sets up some local vars for use by the
;;; FAST-READ-CHAR macro within the enclosed lexical scope. The stream
;;; is assumed to be a ANSI-STREAM.
;;;
;;; KLUDGE: Some functions (e.g. ANSI-STREAM-READ-LINE) use these variables
;;; directly, instead of indirecting through FAST-READ-CHAR.
;;; When ANSI-STREAM-INPUT-CHAR-POS is non-null, we take care to update it,
;;; but not for each character of input.
(defmacro prepare-for-fast-read-char (stream &body forms)
  `(let* ((%frc-stream% ,stream)
          (%frc-method% (ansi-stream-in %frc-stream%))
          (%frc-buffer% (ansi-stream-cin-buffer %frc-stream%))
          (%frc-index% (ansi-stream-in-index %frc-stream%)))
     (declare (type (mod ,(1+ +ansi-stream-in-buffer-length+)) %frc-index%)
              (type ansi-stream %frc-stream%))
     ,@forms))

;;; This macro must be called after one is done with FAST-READ-CHAR
;;; inside its scope to decache the ANSI-STREAM-IN-INDEX.
;;; To keep the amount of code injected by FAST-READ-CHAR as small as possible,
;;; we avoid bumping the absolute stream position counter at each character.
;;; When finished looping, one extra function call takes care of that.
;;; If buffer refills occurred within FAST-READ-CHAR, the refill logic
;;; similarly scans the cin-buffer before placing anything new into it.
(defmacro done-with-fast-read-char ()
  `(progn
     (when (ansi-stream-input-char-pos %frc-stream%)
       (update-input-char-pos %frc-stream% %frc-index%))
     (setf (ansi-stream-in-index %frc-stream%) %frc-index%)))

;;; a macro with the same calling convention as READ-CHAR, to be used
;;; within the scope of a PREPARE-FOR-FAST-READ-CHAR.
;;; If EOF-ERROR-P is statically T (not any random expression evaluating
;;; to T) then wrap the whole thing in (TRULY-THE CHARACTER ...)
;;; because it's either going to yield a character or signal EOF.
(defmacro fast-read-char (&optional (eof-error-p t) (eof-value ()))
  (let ((result
         `(if (not %frc-buffer%)
              (funcall %frc-method% %frc-stream% ,eof-error-p ,eof-value)
              (block nil
                (when (= %frc-index% +ansi-stream-in-buffer-length+)
                  (let ((index-or-nil
                         (fast-read-char-refill %frc-stream% ,eof-error-p)))
                    ,@(unless (eq eof-error-p 't)
                        `((when (null index-or-nil)
                            (return ,eof-value))))
                    (setq %frc-index%
                          (truly-the (mod ,+ansi-stream-in-buffer-length+)
                                     index-or-nil))))
                (prog1 (aref %frc-buffer%
                             (truly-the (mod ,+ansi-stream-in-buffer-length+)
                                        %frc-index%))
                  (incf %frc-index%))))))
    (cond ((eq eof-error-p 't)
           `(truly-the character ,result))
          ((and (symbolp eof-value) (constantp eof-value)
                ;; use an EQL specifier only if the const is EQL-comparable
                (typep (symbol-value eof-value) '(or symbol fixnum)))
           `(truly-the (or (eql ,(symbol-value eof-value)) character) ,result))
          (t
           result))))

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
                              ((< ,f-index (length ,f-buffer))
                               (prog1 (aref ,f-buffer ,f-index)
                                 (setf (ansi-stream-in-index ,f-stream) (incf ,f-index))))
                              (t
                               (prog1 (fast-read-byte-refill ,f-stream ,eof-p ,eof-val)
                                 (setq ,f-index (ansi-stream-in-index ,f-stream)))))))))
         (declare (inline fast-read-byte))
         (declare (enable-package-locks fast-read-byte))
         (locally ,@body)))))

;; This is an internal-use-only macro.
(defmacro do-rest-arg (((var &optional index-var) rest-var
                        &optional (start 0) result)
                       &body body)
  ;; If the &REST arg never needs to be reified, this is slightly quicker
  ;; than using a DX list.
  (let ((index (sb-xc:gensym "INDEX")))
    `(let ((,index ,start))
       (loop
        (cond ((< (truly-the index ,index) (length ,rest-var))
               (let ((,var (fast-&rest-nth ,index ,rest-var))
                     ,@(if index-var `((,index-var ,index))))
                 ,@body)
               (incf ,index))
              (t
               (return ,result)))))))

(in-package "SB-THREAD")

(defmacro with-system-mutex ((mutex &key without-gcing allow-with-interrupts)
                                    &body body)
  `(dx-flet ((with-system-mutex-thunk () ,@body))
     (,(cond (without-gcing
               'call-with-system-mutex/without-gcing)
             (allow-with-interrupts
              'call-with-system-mutex/allow-with-interrupts)
             (t
              'call-with-system-mutex))
       #'with-system-mutex-thunk
       ,mutex)))

(defmacro with-recursive-system-lock ((lock) &body body)
  `(dx-flet ((recursive-system-lock-thunk () ,@body))
     (call-with-recursive-system-lock #'recursive-system-lock-thunk ,lock)))
