;;;; Environment query functions, DOCUMENTATION and DRIBBLE.
;;;;
;;;; FIXME: If there are exactly three things in here, it could be
;;;; exactly three files named e.g. equery.lisp, doc.lisp, and dribble.lisp.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; cobbled from stuff in describe.lisp.
(defun function-doc (x)
  (let ((name
	 (case (widetag-of x)
	   (#.sb!vm:closure-header-widetag
	    (%simple-fun-name (%closure-fun x)))
	   ((#.sb!vm:simple-fun-header-widetag
	     #.sb!vm:closure-fun-header-widetag)
	    (%simple-fun-name x))
	   (#.sb!vm:funcallable-instance-header-widetag
	    (%simple-fun-name
	     (funcallable-instance-fun x))))))
    (when (and name (typep name '(or symbol cons)))
      (values (info :function :documentation name)))))

(defvar *features* '#.sb-cold:*shebang-features*
  #!+sb-doc
  "a list of symbols that describe features provided by the
   implementation")

;;; various environment inquiries

(defun machine-instance ()
  #!+sb-doc
  "Return a string giving the name of the local machine."
  (sb!unix:unix-gethostname))

;;; FIXME: Don't forget to set these in a sample site-init file.
;;; FIXME: Perhaps the functions could be SETFable instead of having the
;;; interface be through special variables? As far as I can tell
;;; from ANSI 11.1.2.1.1 "Constraints on the COMMON-LISP Package
;;; for Conforming Implementations" it is kosher to add a SETF function for
;;; a symbol in COMMON-LISP..
(defvar *short-site-name* nil
  #!+sb-doc
  "The value of SHORT-SITE-NAME.")
(defvar *long-site-name* nil
  #!+sb-doc "the value of LONG-SITE-NAME")
(defun short-site-name ()
  #!+sb-doc
  "Return a string with the abbreviated site name, or NIL if not known."
  *short-site-name*)
(defun long-site-name ()
  #!+sb-doc
  "Return a string with the long form of the site name, or NIL if not known."
  *long-site-name*)

;;;; dribble stuff

;;; Each time we start dribbling to a new stream, we put it in
;;; *DRIBBLE-STREAM*, and push a list of *DRIBBLE-STREAM*, *STANDARD-INPUT*,
;;; *STANDARD-OUTPUT* and *ERROR-OUTPUT* in *PREVIOUS-DRIBBLE-STREAMS*.
;;; *STANDARD-OUTPUT* and *ERROR-OUTPUT* is changed to a broadcast stream that
;;; broadcasts to *DRIBBLE-STREAM* and to the old values of the variables.
;;; *STANDARD-INPUT* is changed to an echo stream that echos input from the old
;;; value of standard input to *DRIBBLE-STREAM*.
;;;
;;; When dribble is called with no arguments, *DRIBBLE-STREAM* is closed,
;;; and the values of *DRIBBLE-STREAM*, *STANDARD-INPUT*, and
;;; *STANDARD-OUTPUT* are popped from *PREVIOUS-DRIBBLE-STREAMS*.

(defvar *previous-dribble-streams* nil)
(defvar *dribble-stream* nil)

(defun dribble (&optional pathname &key (if-exists :append))
  #!+sb-doc
  "With a file name as an argument, dribble opens the file and sends a
  record of further I/O to that file. Without an argument, it closes
  the dribble file, and quits logging."
  (cond (pathname
	 (let* ((new-dribble-stream
		 (open pathname
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create))
		(new-standard-output
		 (make-broadcast-stream *standard-output* new-dribble-stream))
		(new-error-output
		 (make-broadcast-stream *error-output* new-dribble-stream))
		(new-standard-input
		 (make-echo-stream *standard-input* new-dribble-stream)))
	   (push (list *dribble-stream* *standard-input* *standard-output*
		       *error-output*)
		 *previous-dribble-streams*)
	   (setf *dribble-stream* new-dribble-stream)
	   (setf *standard-input* new-standard-input)
	   (setf *standard-output* new-standard-output)
	   (setf *error-output* new-error-output)))
	((null *dribble-stream*)
	 (error "not currently dribbling"))
	(t
	 (let ((old-streams (pop *previous-dribble-streams*)))
	   (close *dribble-stream*)
	   (setf *dribble-stream* (first old-streams))
	   (setf *standard-input* (second old-streams))
	   (setf *standard-output* (third old-streams))
	   (setf *error-output* (fourth old-streams)))))
  (values))

(defun %byte-blt (src src-start dst dst-start dst-end)
  (%byte-blt src src-start dst dst-start dst-end))
