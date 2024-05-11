;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; file descriptor I/O noise

(defstruct (handler
            (:constructor make-handler (direction descriptor function))
            (:copier nil))
  ;; Reading or writing...
  (direction nil :type (member :input :output))
  ;; File descriptor this handler is tied to.
  (descriptor 0 :type #-os-provides-poll (mod #.sb-unix:fd-setsize)
                      #+os-provides-poll (and fixnum unsigned-byte))
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
(declaim (freeze-type handler))

(defstruct (pollfds (:constructor make-pollfds (list))
                    (:copier nil))
  (list)
  ;; If using poll() we maintain, in addition to a list of HANDLER,
  ;; an (ALIEN (* STRUCT POLLFD)) to avoid creating it repeatedly.
  ;; The C array is created only when SUB-SUB-SERVE-EVENT needs it,
  ;; not on each call to ADD/REMOVE-FD-HANDLER.
  ;; If using select(), the C array is not used.
  #+os-provides-poll (fds) ;
  ;; N-FDS is less than or equal to the length of the C array,
  ;; which is created potentially oversized.
  #+os-provides-poll (n-fds)
  ;; map from index in LIST to index into alien FDS
  #+os-provides-poll (map))
(declaim (freeze-type pollfds))

(defmethod print-object ((handler handler) stream)
  (print-unreadable-object (handler stream :type t)
    (format stream
            "~A on ~:[~;BOGUS ~]descriptor ~W: ~S"
            (handler-direction handler)
            (handler-bogus handler)
            (handler-descriptor handler)
            (handler-function handler))))

(define-thread-local *descriptor-handlers* nil
  "List of all the currently active handlers for file descriptors")

(defmacro with-descriptor-handlers (&body forms)
  ;; FD-STREAM functionality can add and remove descriptors on it's
  ;; own, so getting an interrupt while modifying this and the
  ;; starting to recursively modify it could lose...
  `(without-interrupts ,@forms))

;; Deallocating the pollfds is a no-op if not using poll()
#-os-provides-poll (declaim (inline deallocate-pollfds))

;; Free the cached C structures, if allocated.
;; Must be called within the extent of WITH-DESCRIPTOR-HANDLERS.
(defun deallocate-pollfds ()
  #+os-provides-poll
  (awhen *descriptor-handlers*
    (when (pollfds-fds it)
      (free-alien (pollfds-fds it)))
    (setf (pollfds-fds it) nil
          (pollfds-n-fds it) nil
          (pollfds-map it) nil)))

(defun list-all-descriptor-handlers ()
  (with-descriptor-handlers
    (awhen *descriptor-handlers*
      (copy-list (pollfds-list it)))))

(defun map-descriptor-handlers (function)
  (declare (function function))
  (with-descriptor-handlers
    (awhen *descriptor-handlers*
      (dolist (handler (pollfds-list it))
        (funcall function handler)))))

;;; Add a new handler to *descriptor-handlers*.
(defun add-fd-handler (fd direction function)
  "Arrange to call FUNCTION whenever FD is usable. DIRECTION should be
  either :INPUT or :OUTPUT. The value returned should be passed to
  SYSTEM:REMOVE-FD-HANDLER when it is no longer needed."
  (unless (member direction '(:input :output))
    ;; FIXME: should be TYPE-ERROR?
    (error "Invalid direction ~S, must be either :INPUT or :OUTPUT" direction))
  ;; lp#316068 - generate a more specific error than "X is not (MOD n)"
  #-os-provides-poll
  (unless (<= 0 fd (1- sb-unix:fd-setsize))
    (error "Cannot add an FD handler for ~D: not under FD_SETSIZE limit." fd))
  (let ((handler (make-handler direction fd function)))
    (with-descriptor-handlers
      (deallocate-pollfds)
      (let ((handlers *descriptor-handlers*))
        (if (not handlers)
            (setf *descriptor-handlers* (make-pollfds (list handler)))
            (push handler (pollfds-list handlers)))))
    handler))

(macrolet ((filter-handlers (newval-form)
             `(with-descriptor-handlers
                (deallocate-pollfds)
                (let* ((holder *descriptor-handlers*)
                       (handlers (if holder (pollfds-list holder)))
                       (list ,newval-form))
                  ;; The case of "no handlers" is *DESCRIPTOR-HANDLERS* = NIL,
                  ;; like it starts as. So we set it back to NIL rather than
                  ;; an empty struct if no handlers remain.
                  (if list
                      ;; Since this macro is only for deletion of handlers,
                      ;; if LIST is not nil then HOLDER was too.
                      (setf (pollfds-list holder) list)
                      (setf *descriptor-handlers* nil))))))

;;; Remove an old handler from *descriptor-handlers*.
(defun remove-fd-handler (handler)
  "Removes HANDLER from the list of active handlers."
  (filter-handlers (delete handler handlers)))

;;; Search *descriptor-handlers* for any reference to fd, and nuke 'em.
(defun invalidate-descriptor (fd)
  "Remove any handlers referring to FD. This should only be used when attempting
  to recover from a detected inconsistency."
  (filter-handlers (delete fd handlers :key #'handler-descriptor)))

;;; Add the handler to *descriptor-handlers* for the duration of BODY.
;;; Note: this makes the poll() interface not super efficient because
;;; it discards the cached C array of (struct pollfd), as it must do
;;; each time the list of Lisp HANDLER structs is touched.
(defmacro with-fd-handler ((fd direction function) &rest body)
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

  (defun invoke-handler (handler)
    (with-simple-restart (remove-fd-handler "Remove ~S" handler)
      (funcall (handler-function handler)
               (handler-descriptor handler))
      (return-from invoke-handler))
    (remove-fd-handler handler))
;;; First, get a list and mark bad file descriptors. Then signal an error
;;; offering a few restarts.
(defun handler-descriptors-error
  #+os-provides-poll (&optional (bogus-handlers nil handlers-supplied-p))
  #-os-provides-poll (&aux bogus-handlers handlers-supplied-p)
  (if handlers-supplied-p
      (dolist (handler bogus-handlers)
        ;; no fstat() - the kernel deemed them bogus already
        (setf (handler-bogus handler) t))
      (dolist (handler (list-all-descriptor-handlers))
        (unless (or (handler-bogus handler)
                    (sb-unix:unix-fstat (handler-descriptor handler)))
          (setf (handler-bogus handler) t)
          (push handler bogus-handlers))))
  (when bogus-handlers
      (restart-case (error "~S ~[have~;has a~:;have~] bad file descriptor~:P."
                           bogus-handlers (length bogus-handlers))
        (remove-them ()
          :report "Remove bogus handlers."
          (filter-handlers (delete-if #'handler-bogus handlers)))
        (retry-them ()
          :report "Retry bogus handlers."
          (dolist (handler bogus-handlers)
            (setf (handler-bogus handler) nil)))
        (continue ()
          :report "Go on, leaving handlers marked as bogus.")))
  nil))


;;;; SERVE-ALL-EVENTS, SERVE-EVENT, and friends

(declaim (start-block wait-until-fd-usable serve-event serve-all-events compute-pollfds))

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
                (setf (values to-sec to-usec)
                      (relative-decoded-times stop-sec stop-usec))
                (when (and (zerop to-sec) (not (plusp to-usec)))
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
                   when (or #+win32 (eq direction :output)
                            #+win32 (sb-win32:handle-listen fd (or to-sec 0) (or to-usec 0))
                            #-win32
                            (sb-unix:unix-simple-poll fd direction to-msec))
                   do (return-from wait-until-fd-usable t)
                   else
                   do (when to-sec (maybe-update-timeout))
                   #+win32 (sb-thread:thread-yield)))))))

;;; Wait for up to timeout seconds for an event to happen. Make sure all
;;; pending events are processed before returning.
(defun serve-all-events (&optional timeout)
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
#-os-provides-poll
(defun sub-sub-serve-event (to-sec to-usec)
  (with-alien ((read-fds (struct sb-unix:fd-set))
               (write-fds (struct sb-unix:fd-set)))
    (sb-unix:fd-zero read-fds)
    (sb-unix:fd-zero write-fds)
    (let ((count 0))
      (declare (type index count))

      ;; Initialize the fd-sets for UNIX-FAST-SELECT and return the active
      ;; descriptor count.
      (map-descriptor-handlers
       (lambda (handler)
         ;; FIXME: If HANDLER-ACTIVE ever is reinstanted, it needs
         ;; to be checked here in addition to HANDLER-BOGUS
         (unless (handler-bogus handler)
           (let ((fd (handler-descriptor handler)))
             (ecase (handler-direction handler)
               (:input (sb-unix:fd-set fd read-fds))
               (:output (sb-unix:fd-set fd write-fds)))
             (when (> fd count)
               (setf count fd))))))
      (incf count)

      ;; Next, wait for something to happen.
      (multiple-value-bind (value err)
          (sb-unix:unix-fast-select count
                                    (addr read-fds)
                                    (addr write-fds)
                                    nil to-sec to-usec)
        (declare (ignorable err)) ; unused if win32
        ;; Now see what it was (if anything)
        (cond ((not value)
               ;; Interrupted or one of the file descriptors is bad.
               ;; FIXME: Check for other errnos. Why do we return true
               ;; when interrupted?
               #-win32
               (case err
                 (#.sb-unix:ebadf
                  (handler-descriptors-error))
                 ((#.sb-unix:eintr #.sb-unix:eagain)
                  t)
                 (otherwise
                  (with-simple-restart (continue "Ignore failure and continue.")
                    (simple-perror "Unix system call select() failed"
                                   :errno err))))
               #+win32
               (handler-descriptors-error))
              ((plusp value)
               ;; Got something. Call file descriptor handlers
               ;; according to the readable and writable masks
               ;; returned by select.
               (map-descriptor-handlers
                (lambda (handler)
                  (let ((fd (handler-descriptor handler)))
                    (when (ecase (handler-direction handler)
                            (:input (sb-unix:fd-isset fd read-fds))
                            (:output (sb-unix:fd-isset fd write-fds)))
                      (invoke-handler handler)))))
               t))))))

;; Return an pointer to an array of (struct pollfd).
;; This isn't done via WITH-ALIEN for 2 reasons:
;; 1. WITH-ALIEN can't make variable-length arrays
;; 2. it's slightly nontrivial to condense the :input and :output masks
;; Making USE-SCRATCHPAD-P an optional argument is a KLUDGE, but it allows
;; picking which method of file descriptor de-duplication is used,
;; so that both can be exercised by tests.
#+os-provides-poll
(defun compute-pollfds (handlers &optional
                                 (n-handlers (length handlers))
                                 (use-scratchpad-p (>= n-handlers 15)))
  ;; Assuming that all fds in HANDLERS are unique and none are bogus,
  ;; either of which could be untrue,
  ;; allocate the maximum length C array we'd need.
  ;; Since interrupts are disabled inside WITH-DESCRIPTOR-HANDLERS,
  ;; and *DESCRIPTOR-HANDLERS* is per-thread, double traversal is ok.
  (let* ((fds (make-alien (struct sb-unix:pollfd) n-handlers))
         (map (make-array n-handlers :initial-element nil))
         (n-fds 0)
         (handler-index -1))
    (labels ((flag-bit (handler)
               (ecase (handler-direction handler)
                 (:input sb-unix:pollin)
                 (:output sb-unix:pollout)))
             (set-flag (handler)
               (let ((fd-index n-fds))
                 (incf n-fds)
                 (setf (slot (deref fds fd-index) 'sb-unix:fd)
                       (handler-descriptor handler)
                       (slot (deref fds fd-index) 'sb-unix:events)
                       (flag-bit handler))
                 fd-index))
             (or-flag (index handler)
               (setf (slot (deref fds index) 'sb-unix:events)
                     (logior (slot (deref fds index) 'sb-unix:events)
                             (flag-bit handler)))))
      ;; Now compute unique non-bogus file descriptors.
      (if use-scratchpad-p
          ;; This is O(N), but wastes space if there are, say, 16 descriptors
          ;; numbered 1 through 15 and then 1025.
          (let ((scratchpad ; direct map from file descriptor to position in FDS
                 (make-array (1+ (loop for handler in handlers
                                       maximize (handler-descriptor handler)))
                             :initial-element nil)))
            (dolist (handler handlers)
              (incf handler-index)
              (unless (handler-bogus handler)
                (let* ((fd (handler-descriptor handler))
                       (fd-index (svref scratchpad fd)))
                  (if fd-index
                      (or-flag fd-index handler)
                      (setf fd-index (set-flag handler)
                            (svref scratchpad fd) fd-index))
                  (setf (svref map handler-index) fd-index)))))
          ;; This is O(N^2) but fast for small inputs.
          (dolist (handler handlers)
            (incf handler-index)
            (unless (handler-bogus handler)
              (let ((dup-of (position (handler-descriptor handler)
                                      handlers :key (lambda (x)
                                                      (unless (handler-bogus x)
                                                        (handler-descriptor x)))
                                               :end handler-index))
                    (fd-index nil))
                (if dup-of ; fd already got an index into pollfds
                    (or-flag (setq fd-index (svref map dup-of)) handler)
                    (setq fd-index (set-flag handler)))
                (setf (svref map handler-index) fd-index))))))
    (values fds n-fds map)))

;;; Handles the work of the above, except for periodic polling. Returns
;;; true if something of interest happened.
#+os-provides-poll
(defun sub-sub-serve-event (to-sec to-usec)
  (let (list fds count map)
    (with-descriptor-handlers
      (let ((handlers *descriptor-handlers*))
        (when handlers
          (setq list  (pollfds-list handlers)
                fds   (pollfds-fds handlers)
                count (pollfds-n-fds handlers)
                map   (pollfds-map handlers))
          (when (and list (not fds)) ; make the C array
            (multiple-value-setq (fds count map) (compute-pollfds list))
            (setf (pollfds-fds handlers)   fds
                  (pollfds-n-fds handlers) count
                  (pollfds-map handlers)   map)))))
    ;; poll() wants the timeout in milliseconds.
    (let ((to-millisec
           (if (or (null to-sec) (null to-usec))
               -1
               (ceiling (+ (* to-sec 1000000) to-usec) 1000))))
      ;; Next, wait for something to happen.
      (multiple-value-bind (value err)
          (if list
              (sb-unix:unix-poll fds count to-millisec)
              ;; If invoked with no descriptors only for the effect of waiting
              ;; until the timeout, make a valid pointer to a (struct pollfd).
              (with-alien ((a (struct sb-unix:pollfd)))
                (sb-unix:unix-poll a 0 to-millisec)))
        ;; From here down is mostly the same as the code
        ;; for #-os-provides-poll.
        ;;
        ;; Now see what it was (if anything)
        (cond ((not value)
               ;; Interrupted or one of the file descriptors is bad.
               ;; FIXME: Check for other errnos. Why do we return true
               ;; when interrupted?
               (case err
                 (#.sb-unix:ebadf
                  ;; poll() should never return EBADF, but I'm afraid that by
                  ;; removing this, someone will find a broken OS which does.
                  (handler-descriptors-error))
                 ((#.sb-unix:eintr #.sb-unix:eagain)
                  t)
                 (otherwise
                  (with-simple-restart (continue "Ignore failure and continue.")
                    (simple-perror "Unix system call poll() failed"
                                   :errno err)))))
              ((plusp value)
               ;; Got something. Scan the 'revents' fields of the pollfds
               ;; to decide what to call.
               ;; The #-os-provides-poll code looks at *DESCRIPTOR-HANDLERS*
               ;; again at this point, which seems wrong, but not terribly wrong
               ;; because at worst there will be a a zero bit for a handler's
               ;; descriptor. But I can't see how it would make sense here to
               ;; look again because if anything changed, then the map of
               ;; handler to index into the C array was necessarily clobbered.
               (loop for handler in list
                     for fd-index across map
                     for revents = (slot (deref fds fd-index) 'sb-unix:revents)
                     when (logtest revents sb-unix:pollnval)
                     collect handler into bad
                     ;; James Knight says that if POLLERR is set, user code
                     ;; _should_ attempt to perform I/O to observe the error.
                     ;; So we trigger either handler direction.
                     else when (logtest revents
                                        (ecase (handler-direction handler)
                                          ;; POLLHUP implies read will not block
                                          (:input (logior sb-unix:pollin
                                                          sb-unix:pollhup
                                                          sb-unix:pollerr))
                                          (:output (logior sb-unix:pollout
                                                           sb-unix:pollerr))))
                     collect handler into good
                     finally
                  (return
                    (if bad
                        (handler-descriptors-error bad)
                        (dolist (handler good t)
                          (invoke-handler handler)))))))))))
