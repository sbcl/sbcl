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

(sb!xc:defmacro with-descriptor-handlers (&body forms)
  ;; FD-STREAM functionality can add and remove descriptors on it's
  ;; own, so getting an interrupt while modifying this and the
  ;; starting to recursively modify it could lose...
  `(without-interrupts ,@forms))

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
  (unless (<= 0 fd (1- sb!unix:fd-setsize))
    (error "Cannot add an FD handler for ~D: not under FD_SETSIZE limit." fd))
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
    (when bogus-handlers
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
  nil)


;;;; SERVE-ALL-EVENTS, SERVE-EVENT, and friends

;;; When a *periodic-polling-function* is defined the server will not
;;; block for more than the maximum event timeout and will call the
;;; polling function if it does time out.
(declaim (type (or null symbol function) *periodic-polling-function*))
(defvar *periodic-polling-function* nil
  "Either NIL, or a designator for a function callable without any
arguments. Called when the system has been waiting for input for
longer then *PERIODIC-POLLING-PERIOD* seconds. Shared between all
threads, unless locally bound. EXPERIMENTAL.")
(declaim (real *periodic-polling-period*))
(defvar *periodic-polling-period* 0
  "A real number designating the number of seconds to wait for input
at maximum, before calling the *PERIODIC-POLLING-FUNCTION* \(if any.)
Shared between all threads, unless locally bound. EXPERIMENTAL.")

;;; Wait until FD is usable for DIRECTION. The timeout given to serve-event is
;;; recalculated each time through the loop so that WAIT-UNTIL-FD-USABLE will
;;; timeout at the correct time irrespective of how many events are handled in
;;; the meantime.
(defun wait-until-fd-usable (fd direction &optional timeout (serve-events t))
  #!+sb-doc
  "Wait until FD is usable for DIRECTION. DIRECTION should be either :INPUT or
:OUTPUT. TIMEOUT, if supplied, is the number of seconds to wait before giving
up. Returns true once the FD is usable, NIL return indicates timeout.

If SERVE-EVENTS is true (the default), events on other FDs are served while
waiting."
  (tagbody
   :restart
     (multiple-value-bind (to-sec to-usec stop-sec stop-usec signalp)
         (decode-timeout timeout)
       (declare (type (or integer null) to-sec to-usec))
       (flet ((maybe-update-timeout ()
                ;; If we return early, recompute the timeouts, possibly
                ;; signaling the deadline or returning with NIL to caller.
                (multiple-value-bind (sec usec)
                    (decode-internal-time (get-internal-real-time))
                  (setf to-sec (- stop-sec sec))
                  (cond ((> usec stop-usec)
                         (decf to-sec)
                         (setf to-usec (- (+ stop-usec 1000000) usec)))
                        (t
                         (setf to-usec (- stop-usec usec)))))
                (when (or (minusp to-sec) (and (zerop to-sec) (not (plusp to-usec))))
                  (cond (signalp
                         (signal-deadline)
                         (go :restart))
                        (t
                         (return-from wait-until-fd-usable nil))))))
         (if (and serve-events
                  ;; No timeout or non-zero timeout
                  (or (not to-sec)
                      (not (= 0 to-sec to-usec)))
                  ;; Something to do while we wait
                  (or *descriptor-handlers* *periodic-polling-function*))
             ;; Loop around SUB-SERVE-EVENT till done.
             (dx-let ((usable (list nil)))
               (dx-flet ((usable! (fd)
                                  (declare (ignore fd))
                                  (setf (car usable) t)))
                 (with-fd-handler (fd direction #'usable!)
                   (loop
                     (sub-serve-event to-sec to-usec signalp)
                     (when (car usable)
                       (return-from wait-until-fd-usable t))
                     (when to-sec
                       (maybe-update-timeout))))))
             ;; If we don't have to serve events, just poll on the single FD instead.
             (loop for to-msec = (if (and to-sec to-usec)
                                     (+ (* 1000 to-sec) (truncate to-usec 1000))
                                     -1)
                   when (sb!unix:unix-simple-poll fd direction to-msec)
                   do (return-from wait-until-fd-usable t)
                   else
                   do (when to-sec (maybe-update-timeout))))))))

;;; Wait for up to timeout seconds for an event to happen. Make sure all
;;; pending events are processed before returning.
(defun serve-all-events (&optional timeout)
  #!+sb-doc
  "SERVE-ALL-EVENTS calls SERVE-EVENT with the specified timeout. If
SERVE-EVENT does something (returns T) it loops over SERVE-EVENT with a
timeout of 0 until there are no more events to serve. SERVE-ALL-EVENTS returns
T if SERVE-EVENT did something and NIL if not."
  (do ((res nil)
       (sval (serve-event timeout) (serve-event 0)))
      ((null sval) res)
    (setq res t)))

;;; Serve a single set of events.
(defun serve-event (&optional timeout)
  #!+sb-doc
  "Receive pending events on all FD-STREAMS and dispatch to the appropriate
handler functions. If timeout is specified, server will wait the specified
time (in seconds) and then return, otherwise it will wait until something
happens. Server returns T if something happened and NIL otherwise. Timeout
0 means polling without waiting."
  (multiple-value-bind (to-sec to-usec stop-sec stop-usec signalp)
      (decode-timeout timeout)
    (declare (ignore stop-sec stop-usec))
    (sub-serve-event to-sec to-usec signalp)))

;;; Takes timeout broken into seconds and microseconds, NIL timeout means
;;; to wait as long as needed.
(defun sub-serve-event (to-sec to-usec deadlinep)
  (or
   (if *periodic-polling-function*
       (multiple-value-bind (p-sec p-usec)
           (decode-internal-time
            (seconds-to-internal-time *periodic-polling-period*))
         (if to-sec
             (loop repeat (/ (+ to-sec (/ to-usec 1e6))
                             *periodic-polling-period*)
                   thereis (sub-sub-serve-event p-sec p-usec)
                   do (funcall *periodic-polling-function*))
             (loop thereis (sub-sub-serve-event p-sec p-usec)
                   do (funcall *periodic-polling-function*))))
       (sub-sub-serve-event to-sec to-usec))
   (when deadlinep
     (signal-deadline))))

;;; Handles the work of the above, except for periodic polling. Returns
;;; true if something of interest happened.
(defun sub-sub-serve-event (to-sec to-usec)
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

      ;; Next, wait for something to happen.
      (multiple-value-bind (value err)
          (sb!unix:unix-fast-select count
                                    (sb!alien:addr read-fds)
                                    (sb!alien:addr write-fds)
                                    nil to-sec to-usec)
        #!+win32
        (declare (ignore err))
        ;; Now see what it was (if anything)
        (cond ((not value)
               ;; Interrupted or one of the file descriptors is bad.
               ;; FIXME: Check for other errnos. Why do we return true
               ;; when interrupted?
               #!-win32
               (case err
                 (#.sb!unix:ebadf
                  (handler-descriptors-error))
                 ((#.sb!unix:eintr #.sb!unix:eagain)
                  t)
                 (otherwise
                  (with-simple-restart (continue "Ignore failure and continue.")
                    (simple-perror "Unix system call select() failed"
                                   :errno err))))
               #!+win32
               (handler-descriptors-error))
              ((plusp value)
               ;; Got something. Call file descriptor handlers
               ;; according to the readable and writable masks
               ;; returned by select.
               (dolist (handler
                        (select-descriptor-handlers
                         (lambda (handler)
                           (let ((fd (handler-descriptor handler)))
                             (ecase (handler-direction handler)
                               (:input (sb!unix:fd-isset fd read-fds))
                               (:output (sb!unix:fd-isset fd write-fds)))))))
                 (funcall (handler-function handler)
                          (handler-descriptor handler)))
               t))))))

