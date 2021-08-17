;;;; Environment query functions, and DRIBBLE.
;;;;

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; various environment inquiries

;;; This is a tentative list of target features; many are removed later.
;;; :SB-XC is removed now, because it is plain wrong unless cross-compiling.
(defvar *features* '#.(remove :sb-xc sb-xc:*features*)
  "a list of symbols that describe features provided by the
   implementation")
(defconstant !sbcl-architecture #.(sb-cold::target-platform-keyword))

(defun machine-instance ()
  "Return a string giving the name of the local machine."
  #+win32 (sb-win32::get-computer-name)
  #-win32 (truly-the simple-string (sb-unix:unix-gethostname)))

(declaim (type (or null simple-string) *machine-version*))
(declaim (global *machine-version*))

(defun machine-version ()
  "Return a string describing the version of the computer hardware we
are running on, or NIL if we can't find any useful information."
  (if (boundp '*machine-version*)
      *machine-version*
      (setf *machine-version*
            (awhen (get-machine-version) (possibly-base-stringize it)))))

;;; FIXME: Don't forget to set these in a sample site-init file.
;;; FIXME: Perhaps the functions could be SETFable instead of having the
;;; interface be through special variables? As far as I can tell
;;; from ANSI 11.1.2.1.1 "Constraints on the COMMON-LISP Package
;;; for Conforming Implementations" it is kosher to add a SETF function for
;;; a symbol in COMMON-LISP..
(declaim (type (or null simple-string) *short-site-name* *long-site-name*))
(define-load-time-global *short-site-name* nil
  "The value of SHORT-SITE-NAME.")
(define-load-time-global *long-site-name* nil
  "The value of LONG-SITE-NAME.")
(defun short-site-name ()
  "Return a string with the abbreviated site name, or NIL if not known."
  *short-site-name*)
(defun long-site-name ()
  "Return a string with the long form of the site name, or NIL if not known."
  *long-site-name*)

;;;; ED
(declaim (type list *ed-functions*))
(defvar *ed-functions* '()
  "See function documentation for ED.")

(defun ed (&optional x)
  "Starts the editor (on a file or a function if named).  Functions
from the list *ED-FUNCTIONS* are called in order with X as an argument
until one of them returns non-NIL; these functions are responsible for
signalling a FILE-ERROR to indicate failure to perform an operation on
the file system."
  (dolist (fun *ed-functions*
           (error 'extension-failure
                  :format-control "Don't know how to ~S ~A"
                  :format-arguments (list 'ed x)
                  :references '((:sbcl :variable *ed-functions*))))
    (when (funcall fun x)
      (return t))))

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

(defvar *previous-dribble-streams* '())
(defvar *dribble-stream* nil)

(defun dribble (&optional pathname &key (if-exists :append))
  "With a file name as an argument, dribble opens the file and sends a
  record of further I/O to that file. Without an argument, it closes
  the dribble file, and quits logging."
  (flet ((install-streams (dribble input output error)
           (setf *dribble-stream* dribble
                 *standard-input* input
                 *standard-output* output
                 *error-output* error)))
    (cond (pathname
           (push (list *dribble-stream* *standard-input* *standard-output*
                       *error-output*)
                 *previous-dribble-streams*)
           (let ((new-dribble (open pathname
                                    :direction :output
                                    :if-exists if-exists
                                    :if-does-not-exist :create)))
             (install-streams
              new-dribble
              (make-echo-stream *standard-input* new-dribble)
              (make-broadcast-stream *standard-output* new-dribble)
              (make-broadcast-stream *error-output* new-dribble))))
          ((null *dribble-stream*)
           (error "not currently dribbling"))
          (t
           (close *dribble-stream*)
           (apply #'install-streams (pop *previous-dribble-streams*)))))
  (values))

;;;; some *LOAD-FOO* variables

(defvar *load-print* nil
  "the default for the :PRINT argument to LOAD")

(defvar *load-verbose* nil
  ;; Note that CMU CL's default for this was T, and ANSI says it's
  ;; implementation-dependent. We choose NIL on the theory that it's
  ;; a nicer default behavior for Unix programs.
  "the default for the :VERBOSE argument to LOAD")

;;; DEFmumble helpers

(defun %defglobal (name value source-location &optional (doc nil docp))
  (%compiler-defglobal name :always-bound
                       (not (unbound-marker-p value)) value)
  (when docp
    (setf (documentation name 'variable) doc))
  (when source-location
    (setf (info :source-location :variable name) source-location))
  name)

(defun %defparameter (var val source-location &optional (doc nil docp))
  (%compiler-defvar var)
  (set var val)
  (when docp
    (setf (documentation var 'variable) doc))
  (when source-location
    (setf (info :source-location :variable var) source-location))
  var)

(defun %defvar (var source-location &optional (val nil valp) (doc nil docp))
  (%compiler-defvar var)
  (when (and valp
             (not (boundp var)))
    (set var val))
  (when docp
    (setf (documentation var 'variable) doc))
  (when source-location
    (setf (info :source-location :variable var) source-location))
  var)

(defun %defun (name def &optional inline-lambda extra-info)
  (declare (type function def))
  ;; should've been checked by DEFMACRO DEFUN
  (aver (legal-fun-name-p name))
  ;; If a warning handler decides to disallow this redefinition
  ;; by nonlocally exiting, then we'll skip the rest of this stuff.
  (when (and (fboundp name)
             *type-system-initialized*)
    (handler-bind (((satisfies sb-c::handle-condition-p)
                     #'sb-c::handle-condition-handler))
      (warn 'redefinition-with-defun :name name :new-function def)))
  (sb-c:%compiler-defun name nil inline-lambda extra-info)
  (setf (fdefinition name) def)
  ;; %COMPILER-DEFUN doesn't do this except at compile-time, when it
  ;; also checks package locks. By doing this here we let (SETF
  ;; FDEFINITION) do the load-time package lock checking before
  ;; we frob any existing inline expansions.
  (sb-c::%set-inline-expansion name nil inline-lambda extra-info)
  (sb-c::note-name-defined name :function)
  name)

(in-package "SB-C")

(defun real-function-name (name)
  ;; Resolve the actual name of the function named by NAME
  ;; e.g. (setf (name-function 'x) #'car)
  ;; (real-function-name 'x) => CAR
  (cond ((not (fboundp name))
         nil)
        ((and (symbolp name)
              (macro-function name))
         (let ((name (%fun-name (macro-function name))))
           (and (consp name)
                (eq (car name) 'macro-function)
                (cadr name))))
        (t
         (%fun-name (fdefinition name)))))

(defun random-documentation (name type)
  (cdr (assoc type (info :random-documentation :stuff name))))

(defun (setf random-documentation) (new-value name type)
  (let ((pair (assoc type (info :random-documentation :stuff name))))
    (if pair
        (setf (cdr pair) new-value)
        (push (cons type new-value)
              (info :random-documentation :stuff name))))
  new-value)

(defun split-version-string (string)
  (loop with subversion and start = 0
        with end = (length string)
        when (setf (values subversion start)
                   (parse-integer string :start start :junk-allowed t))
        collect it
        while (and subversion
                   (< start end)
                   (char= (char string start) #\.))
        do (incf start)))

(defun version>= (x y)
  (unless (or x y)
    (return-from version>= t))
  (let ((head-x (or (first x) 0))
        (head-y (or (first y) 0)))
    (or (> head-x head-y)
        (and (= head-x head-y)
             (version>= (rest x) (rest y))))))

(defun assert-version->= (&rest subversions)
  "Asserts that the current SBCL is of version equal to or greater than
the version specified in the arguments.  A continuable error is signaled
otherwise.

The arguments specify a sequence of subversion numbers in big endian order.
They are compared lexicographically with the runtime version, and versions
are treated as though trailed by an unbounded number of 0s.

For example, (assert-version->= 1 1 4) asserts that the current SBCL is
version 1.1.4[.0.0...] or greater, and (assert-version->= 1) that it is
version 1[.0.0...] or greater."
  (let ((version (split-version-string (lisp-implementation-version))))
    (unless (version>= version subversions)
      (cerror "Disregard this version requirement."
              "SBCL ~A is too old for this program (version ~{~A~^.~} ~
               or later is required)."
              (lisp-implementation-version)
              subversions))))

(defvar sb-pcl::*!docstrings* nil)
(defun (setf documentation) (string name doc-type)
  (declare (type (or null string) string))
  (push (list string name doc-type) sb-pcl::*!docstrings*)
  string)

(in-package "SB-LOCKLESS")
(defstruct (list-node
            (:conc-name nil)
            (:constructor %make-sentinel-node ())
            (:copier nil))
  (%node-next nil))

;;; Specialized list variants will be created for
;;;  fixnum, integer, real, string, generic "comparable"
;;; but the node type and list type is the same regardless of key type.
(defstruct (linked-list
            (:constructor %make-lfl
                          (head inserter deleter finder inequality equality))
            (:conc-name list-))
  (head       nil :type list-node :read-only t)
  (inserter   nil :type function :read-only t)
  (deleter    nil :type function :read-only t)
  (finder     nil :type function :read-only t)
  (inequality nil :type function :read-only t)
  (equality   nil :type function :read-only t))
