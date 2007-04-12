;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; file descriptor I/O noise

(defstruct (handler
            (:constructor make-handler (direction descriptor function))
            (:copier nil))
  ;; Reading or writing...
  (direction nil :type (member :input :output))
  ;; File descriptor this handler is tied to.
  (descriptor 0 :type (mod #.sb!unix:fd-setsize))
  ;; T iff this handler is running.
  ;;
  ;; FIXME: unused. At some point this used to be set to T
  ;; around the call to the handler-function, but that was commented
  ;; out with the verbose explantion "Doesn't work -- ACK".
  active
  ;; Function to call.
  (function nil :type function)
  ;; T if this descriptor is bogus.
  bogus)

(def!method print-object ((handler handler) stream)
  (print-unreadable-object (handler stream :type t)
    (format stream
            "~A on ~:[~;BOGUS ~]descriptor ~W: ~S"
            (handler-direction handler)
            (handler-bogus handler)
            (handler-descriptor handler)
            (handler-function handler))))

(defvar *descriptor-handlers* nil
  #!+sb-doc
  "List of all the currently active handlers for file descriptors")

(defvar *descriptor-handler-lock*
  (sb!thread::make-spinlock :name "descriptor handle lock"))

(sb!xc:defmacro with-descriptor-handlers (&body forms)
  ;; FD-STREAM functionality can add and remove descriptors on it's
  ;; own, and two threads adding add the same time could lose one.
  ;;
  ;; This is never held for long, so a spinlock is fine.
  `(without-interrupts
     (sb!thread::with-spinlock (*descriptor-handler-lock*)
       ,@forms)))

(defun list-all-descriptor-handlers ()
  (with-descriptor-handlers
    (copy-list *descriptor-handlers*)))

(defun select-descriptor-handlers (function)
  (declare (function function))
  (with-descriptor-handlers
    (remove-if-not function *descriptor-handlers*)))

(defun map-descriptor-handlers (function)
  (declare (function function))
  (with-descriptor-handlers
    (dolist (handler *descriptor-handlers*)
      (funcall function handler))))

;;; Add a new handler to *descriptor-handlers*.
(defun add-fd-handler (fd direction function)
  #!+sb-doc
  "Arange to call FUNCTION whenever FD is usable. DIRECTION should be
  either :INPUT or :OUTPUT. The value returned should be passed to
  SYSTEM:REMOVE-FD-HANDLER when it is no longer needed."
  (unless (member direction '(:input :output))
    ;; FIXME: should be TYPE-ERROR?
    (error "Invalid direction ~S, must be either :INPUT or :OUTPUT" direction))
  (let ((handler (make-handler direction fd function)))
    (with-descriptor-handlers
      (push handler *descriptor-handlers*))
    handler))

;;; Remove an old handler from *descriptor-handlers*.
(defun remove-fd-handler (handler)
  #!+sb-doc
  "Removes HANDLER from the list of active handlers."
  (with-descriptor-handlers
    (setf *descriptor-handlers*
          (delete handler *descriptor-handlers*))))

;;; Search *descriptor-handlers* for any reference to fd, and nuke 'em.
(defun invalidate-descriptor (fd)
  #!+sb-doc
  "Remove any handers refering to fd. This should only be used when attempting
  to recover from a detected inconsistancy."
  (with-descriptor-handlers
    (setf *descriptor-handlers*
          (delete fd *descriptor-handlers*
                  :key #'handler-descriptor))))

;;; Add the handler to *descriptor-handlers* for the duration of BODY.
(defmacro with-fd-handler ((fd direction function) &rest body)
  #!+sb-doc
  "Establish a handler with SYSTEM:ADD-FD-HANDLER for the duration of BODY.
   DIRECTION should be either :INPUT or :OUTPUT, FD is the file descriptor to
   use, and FUNCTION is the function to call whenever FD is usable."
  (let ((handler (gensym)))
    `(let (,handler)
       (unwind-protect
           (progn
             (setf ,handler (add-fd-handler ,fd ,direction ,function))
             ,@body)
         (when ,handler
           (remove-fd-handler ,handler))))))

;;; First, get a list and mark bad file descriptors. Then signal an error
;;; offering a few restarts.
(defun handler-descriptors-error ()
  (let ((bogus-handlers nil))
    (dolist (handler (list-all-descriptor-handlers))
      (unless (or (handler-bogus handler)
                  (sb!unix:unix-fstat (handler-descriptor handler)))
        (setf (handler-bogus handler) t)
        (push handler bogus-handlers)))
    (restart-case (error "~S ~[have~;has a~:;have~] bad file descriptor~:P."
                         bogus-handlers (length bogus-handlers))
      (remove-them ()
        :report "Remove bogus handlers."
        (with-descriptor-handlers
          (setf *descriptor-handlers*
                (delete-if #'handler-bogus *descriptor-handlers*))))
      (retry-them ()
        :report "Retry bogus handlers."
       (dolist (handler bogus-handlers)
         (setf (handler-bogus handler) nil)))
      (continue ()
        :report "Go on, leaving handlers marked as bogus."))))

;;;; SERVE-ALL-EVENTS, SERVE-EVENT, and friends

;;; Break a real timeout into seconds and microseconds.
(defun decode-timeout (timeout)
  (declare (values (or index null) index))
  (typecase timeout
    (integer (values timeout 0))
    (null (values nil 0))
    (real
     (multiple-value-bind (q r) (truncate (coerce timeout 'single-float))
       (declare (type index q) (single-float r))
       (values q (the (values index t) (truncate (* r 1f6))))))
    (t
     (error "Timeout is not a real number or NIL: ~S" timeout))))

;;; Wait until FD is usable for DIRECTION. The timeout given to serve-event is
;;; recalculated each time through the loop so that WAIT-UNTIL-FD-USABLE will
;;; timeout at the correct time irrespective of how many events are handled in
;;; the meantime.
(defun wait-until-fd-usable (fd direction &optional timeout)
  #!+sb-doc
  "Wait until FD is usable for DIRECTION. DIRECTION should be either :INPUT or
  :OUTPUT. TIMEOUT, if supplied, is the number of seconds to wait before giving
  up."
  (declare (type (or real null) timeout))
  (let (usable)
    (multiple-value-bind (to-sec to-usec) (decode-timeout timeout)
      (declare (type (or index null) to-sec to-usec))
      (multiple-value-bind (stop-sec stop-usec)
          (if to-sec
              (multiple-value-bind (okay start-sec start-usec)
                  (sb!unix:unix-gettimeofday)
                (declare (ignore okay))
                (let ((usec (+ to-usec start-usec))
                      (sec (+ to-sec start-sec)))
                  (declare (type (unsigned-byte 31) usec sec))
                  (if (>= usec 1000000)
                      (values (1+ sec) (- usec 1000000))
                      (values sec usec))))
              (values 0 0))
        (declare (type (unsigned-byte 31) stop-sec stop-usec))
        (with-fd-handler (fd direction (lambda (fd)
                                         (declare (ignore fd))
                                         (setf usable t)))
          (loop
            (sub-serve-event to-sec to-usec)

            (when usable
              (return t))

            (when timeout
              (multiple-value-bind (okay sec usec) (sb!unix:unix-gettimeofday)
                (declare (ignore okay))
                (when (or (> sec stop-sec)
                          (and (= sec stop-sec) (>= usec stop-usec)))
                  (return nil))
                (setq to-sec (- stop-sec sec))
                (cond ((> usec stop-usec)
                       (decf to-sec)
                       (setq to-usec (- (+ stop-usec 1000000) usec)))
                      (t
                       (setq to-usec (- stop-usec usec))))))))))))

;;; Wait for up to timeout seconds for an event to happen. Make sure all
;;; pending events are processed before returning.
(defun serve-all-events (&optional timeout)
  #!+sb-doc
  "SERVE-ALL-EVENTS calls SERVE-EVENT with the specified timeout. If
  SERVE-EVENT does something (returns T) it loops over SERVE-EVENT with timeout
  0 until all events have been served. SERVE-ALL-EVENTS returns T if
  SERVE-EVENT did something and NIL if not."
  (do ((res nil)
       (sval (serve-event timeout) (serve-event 0)))
      ((null sval) res)
    (setq res t)))

;;; Serve a single event.
(defun serve-event (&optional timeout)
  #!+sb-doc
  "Receive on all ports and Xevents and dispatch to the appropriate handler
  function. If timeout is specified, server will wait the specified time (in
  seconds) and then return, otherwise it will wait until something happens.
  Server returns T if something happened and NIL otherwise."
  (multiple-value-bind (to-sec to-usec) (decode-timeout timeout)
    (sub-serve-event to-sec to-usec)))

;;; When a *periodic-polling-function* is defined the server will not
;;; block for more than the maximum event timeout and will call the
;;; polling function if it does time out.
(declaim (type (or null function) *periodic-polling-function*))
(defvar *periodic-polling-function* nil)
(declaim (type (unsigned-byte 29) *max-event-to-sec* *max-event-to-usec*))
(defvar *max-event-to-sec* 1)
(defvar *max-event-to-usec* 0)

;;; Takes timeout broken into seconds and microseconds.
(defun sub-serve-event (to-sec to-usec)
  (declare (type (or null (unsigned-byte 29)) to-sec to-usec))

  (let ((call-polling-fn nil))
    (when (and *periodic-polling-function*
               ;; Enforce a maximum timeout.
               (or (null to-sec)
                   (> to-sec *max-event-to-sec*)
                   (and (= to-sec *max-event-to-sec*)
                        (> to-usec *max-event-to-usec*))))
      (setf to-sec *max-event-to-sec*)
      (setf to-usec *max-event-to-usec*)
      (setf call-polling-fn t))

    ;; Next, wait for something to happen.
    (sb!alien:with-alien ((read-fds (sb!alien:struct sb!unix:fd-set))
                          (write-fds (sb!alien:struct sb!unix:fd-set)))
      (sb!unix:fd-zero read-fds)
      (sb!unix:fd-zero write-fds)
      (let ((count 0))
        (declare (type index count))

        ;; Initialize the fd-sets for UNIX-SELECT and return the active
        ;; descriptor count.
        (map-descriptor-handlers
         (lambda (handler)
           ;; FIXME: If HANDLER-ACTIVE ever is reinstanted, it needs
           ;; to be checked here in addition to HANDLER-BOGUS
           (unless (handler-bogus handler)
             (let ((fd (handler-descriptor handler)))
               (ecase (handler-direction handler)
                 (:input (sb!unix:fd-set fd read-fds))
                 (:output (sb!unix:fd-set fd write-fds)))
               (when (> fd count)
                 (setf count fd))))))
        (incf count)

        (multiple-value-bind (value err)
            (sb!unix:unix-fast-select count
                                      (sb!alien:addr read-fds)
                                      (sb!alien:addr write-fds)
                                      nil to-sec to-usec)
          #!+win32 (declare (ignorable err))
          (cond ((eql 0 value)
                 ;; Timed out.
                 (when call-polling-fn
                   (funcall *periodic-polling-function*)))
                (value
                 ;; Call file descriptor handlers according to the
                 ;; readable and writable masks returned by select.
                 (dolist (handler
                           (select-descriptor-handlers
                            (lambda (handler)
                              (let ((fd (handler-descriptor handler)))
                                (ecase (handler-direction handler)
                                  (:input (sb!unix:fd-isset fd read-fds))
                                  (:output (sb!unix:fd-isset fd write-fds)))))))
                   (funcall (handler-function handler)
                            (handler-descriptor handler)))
                 t)
                #!-win32
                ((eql err sb!unix:eintr)
                 ;; We did an interrupt.
                 ;;
                 ;; FIXME: Why T here?
                 t)
                (t
                 ;; One of the file descriptors is bad.
                 (handler-descriptors-error)
                 nil)))))))

