;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

#|
;;;; object set stuff

;;; a hashtable from ports to objects. Each entry is a cons (object . set).
;(defvar *port-table* (make-hash-table :test 'eql))

(defstruct (object-set
	    (:constructor make-object-set
			  (name &optional
				(default-handler #'default-default-handler)))
	    (:print-object
	     (lambda (s stream)
	       (format stream "#<Object Set ~S>" (object-set-name s))))
	    (:copier nil))
  name					; Name, for descriptive purposes.
  (table (make-hash-table :test 'eq))   ; Message-ID or
					;   xevent-type --> handler fun.
  default-handler)

#!+sb-doc
(setf (fdocumentation 'make-object-set 'function)
      "Make an object set for use by a RPC/xevent server. Name is for
      descriptive purposes only.")

;;; If no such operation defined, signal an error.
(defun default-default-handler (object)
  (error "You lose, object: ~S" object))

;;; Look up the handler function for a given message ID.
(defun object-set-operation (object-set message-id)
  #!+sb-doc
  "Return the handler function in Object-Set for the operation specified by
   Message-ID, if none, NIL is returned."
  (enforce-type object-set object-set)
  (enforce-type message-id fixnum)
  (values (gethash message-id (object-set-table object-set))))

;;; The setf inverse for Object-Set-Operation.
(defun %set-object-set-operation (object-set message-id new-value)
  (enforce-type object-set object-set)
  (enforce-type message-id fixnum)
  (setf (gethash message-id (object-set-table object-set)) new-value))

|#

;;;; file descriptor I/O noise

(defstruct (handler
	    (:constructor make-handler (direction descriptor function))
	    (:copier nil))
  ;; Reading or writing...
  (direction nil :type (member :input :output))
  ;; File descriptor this handler is tied to.
  (descriptor 0 :type (mod #.sb!unix:fd-setsize))

  active		      ; T iff this handler is running.
  (function nil :type function) ; Function to call.
  bogus)		      ; T if this descriptor is bogus.
(def!method print-object ((handler handler) stream)
  (print-unreadable-object (handler stream :type t)
    (format stream
	    "~A on ~:[~;BOGUS ~]descriptor ~D: ~S"
	    (handler-direction handler)
	    (handler-bogus handler)
	    (handler-descriptor handler)
	    (handler-function handler))))

(defvar *descriptor-handlers* nil
  #!+sb-doc
  "List of all the currently active handlers for file descriptors")

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
    (push handler *descriptor-handlers*)
    handler))

;;; Remove an old handler from *descriptor-handlers*.
(defun remove-fd-handler (handler)
  #!+sb-doc
  "Removes HANDLER from the list of active handlers."
  (setf *descriptor-handlers*
	(delete handler *descriptor-handlers*
		:test #'eq)))

;;; Search *descriptor-handlers* for any reference to fd, and nuke 'em.
(defun invalidate-descriptor (fd)
  #!+sb-doc
  "Remove any handers refering to fd. This should only be used when attempting
  to recover from a detected inconsistancy."
  (setf *descriptor-handlers*
	(delete fd *descriptor-handlers*
		:key #'handler-descriptor)))

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
    (dolist (handler *descriptor-handlers*)
      (unless (or (handler-bogus handler)
		  (sb!unix:unix-fstat (handler-descriptor handler)))
	(setf (handler-bogus handler) t)
	(push handler bogus-handlers)))
    (restart-case (error "~S ~[have~;has a~:;have~] bad file descriptor~:P."
			 bogus-handlers (length bogus-handlers))
      (remove-them () :report "Remove bogus handlers."
       (setf *descriptor-handlers*
	     (delete-if #'handler-bogus *descriptor-handlers*)))
      (retry-them () :report "Retry bogus handlers."
       (dolist (handler bogus-handlers)
	 (setf (handler-bogus handler) nil)))
      (continue () :report "Go on, leaving handlers marked as bogus."))))

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
	(with-fd-handler (fd direction #'(lambda (fd)
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

;;; These macros are chunks of code from SUB-SERVE-EVENT. They randomly
;;; reference the READ-FDS and WRITE-FDS Alien variables (which wold be consed
;;; if passed as function arguments.)
(eval-when (:compile-toplevel :execute)

;;; Initialize the fd-sets for UNIX-SELECT and return the active descriptor
;;; count.
(sb!xc:defmacro calc-masks ()
  '(progn
     (sb!unix:fd-zero read-fds)
     (sb!unix:fd-zero write-fds)
     (let ((count 0))
       (declare (type index count))
       (dolist (handler *descriptor-handlers*)
	 (unless (or (handler-active handler)
		     (handler-bogus handler))
	   (let ((fd (handler-descriptor handler)))
	     (ecase (handler-direction handler)
	       (:input (sb!unix:fd-set fd read-fds))
	       (:output (sb!unix:fd-set fd write-fds)))
	     (when (> fd count)
	       (setf count fd)))))
       (1+ count))))

;;; Call file descriptor handlers according to the readable and writable masks
;;; returned by select.
(sb!xc:defmacro call-fd-handler ()
  '(let ((result nil))
     (dolist (handler *descriptor-handlers*)
       (let ((desc (handler-descriptor handler)))
	 (when (ecase (handler-direction handler)
		 (:input (sb!unix:fd-isset desc read-fds))
		 (:output (sb!unix:fd-isset desc write-fds)))
	   (unwind-protect
	       (progn
		 ;; Doesn't work -- ACK
		 ;(setf (handler-active handler) t)
		 (funcall (handler-function handler) desc))
	     (setf (handler-active handler) nil))
	   (ecase (handler-direction handler)
	     (:input (sb!unix:fd-clr desc read-fds))
	     (:output (sb!unix:fd-clr desc write-fds)))
	   (setf result t)))
       result)))

) ; EVAL-WHEN

;;; When a *periodic-polling-function* is defined the server will not
;;; block for more than the maximum event timeout and will call the
;;; polling function if it does time out. One important use of this
;;; is to periodically call process-yield.
(declaim (type (or null function) *periodic-polling-function*))
(defvar *periodic-polling-function*
  #!-mp nil #!+mp #'sb!mp:process-yield)
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
      (let ((count (calc-masks)))
	(multiple-value-bind (value err)
	    (sb!unix:unix-fast-select count
				      (sb!alien:addr read-fds)
				      (sb!alien:addr write-fds)
				      nil to-sec to-usec)
	
	  ;; Now see what it was (if anything)
	  (cond (value
		 (cond ((zerop value)
			;; Timed out.
			(when call-polling-fn
			  (funcall *periodic-polling-function*)))
		       (t
			(call-fd-handler))))
		((eql err sb!unix:eintr)
		 ;; We did an interrupt.
		 t)
		(t
		 ;; One of the file descriptors is bad.
		 (handler-descriptors-error)
		 nil)))))))
