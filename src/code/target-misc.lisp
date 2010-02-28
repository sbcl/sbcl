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

;;;; function names and documentation

;;;; the ANSI interface to function names (and to other stuff too)
;;; Note: this function gets called by the compiler (as of 1.0.17.x,
;;; in MAYBE-INLINE-SYNTACTIC-CLOSURE), and so although ANSI says
;;; we're allowed to return NIL here freely, it seems plausible that
;;; small changes to the circumstances under which this function
;;; returns non-NIL might have subtle consequences on the compiler.
;;; So it might be desirable to have the compiler not rely on this
;;; function, eventually.
(defun function-lambda-expression (fun)
  "Return (VALUES DEFINING-LAMBDA-EXPRESSION CLOSURE-P NAME), where
  DEFINING-LAMBDA-EXPRESSION is NIL if unknown, or a suitable argument
  to COMPILE otherwise, CLOSURE-P is non-NIL if the function's definition
  might have been enclosed in some non-null lexical environment, and
  NAME is some name (for debugging only) or NIL if there is no name."
  (declare (type function fun))
  (etypecase fun
    #!+sb-eval
    (sb!eval:interpreted-function
     (let ((name (sb!eval:interpreted-function-name fun))
           (lambda-list (sb!eval:interpreted-function-lambda-list fun))
           (declarations (sb!eval:interpreted-function-declarations fun))
           (body (sb!eval:interpreted-function-body fun)))
       (values `(lambda ,lambda-list
                  ,@(when declarations `((declare ,@declarations)))
                  ,@body)
               t name)))
    (function
     (let* ((fun (%simple-fun-self (%fun-fun fun)))
            (name (%fun-name fun))
            (code (sb!di::fun-code-header fun))
            (info (sb!kernel:%code-debug-info code)))
       (if info
           (let ((source (sb!c::debug-info-source info)))
             (cond ((and (sb!c::debug-source-form source)
                         (eq (sb!c::debug-source-function source) fun))
                    (values (sb!c::debug-source-form source)
                            nil
                            name))
                   ((legal-fun-name-p name)
                    (let ((exp (fun-name-inline-expansion name)))
                      (values exp (not exp) name)))
                   (t
                    (values nil t name))))
           (values nil t name))))))

;;;; Generalizing over SIMPLE-FUN, CLOSURE, and FUNCALLABLE-INSTANCEs

;;; Underlying SIMPLE-FUN
(defun %fun-fun (function)
  (declare (function function))
  (typecase function
    (simple-fun
     function)
    (closure
     (%closure-fun function))
    (funcallable-instance
     (%fun-fun (funcallable-instance-fun function)))))

(defun %fun-lambda-list (function)
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (sb!eval:interpreted-function-debug-lambda-list function))
    (t
     (%simple-fun-arglist (%fun-fun function)))))

(defun (setf %fun-lambda-list) (new-value function)
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (setf (sb!eval:interpreted-function-debug-lambda-list function) new-value))
    ;; FIXME: Eliding general funcallable-instances for now.
    ((or simple-fun closure)
     (setf (%simple-fun-arglist (%fun-fun function)) new-value)))
  new-value)

(defun %fun-type (function)
  (%simple-fun-type (%fun-fun function)))

;;; a SETFable function to return the associated debug name for FUN
;;; (i.e., the third value returned from CL:FUNCTION-LAMBDA-EXPRESSION),
;;; or NIL if there's none
(defun %fun-name (function)
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (sb!eval:interpreted-function-debug-name function))
    (t
     (%simple-fun-name (%fun-fun function)))))

(defun (setf %fun-name) (new-value function)
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (setf (sb!eval:interpreted-function-debug-name function) new-value))
    ;; FIXME: Eliding general funcallable-instances for now.
    ((or simple-fun closure)
     (setf (%simple-fun-name (%fun-fun function)) new-value)))
  new-value)

(defun %fun-doc (function)
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (sb!eval:interpreted-function-documentation function))
    (t
     (%simple-fun-doc (%fun-fun function)))))

(defun (setf %fun-doc) (new-value function)
  (declare (type (or null string) new-value))
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (setf (sb!eval:interpreted-function-documentation function) new-value))
    ((or simple-fun closure)
     (setf (%simple-fun-doc (%fun-fun function)) new-value)))
  new-value)

;;; various environment inquiries

(defvar *features* '#.sb-cold:*shebang-features*
  #!+sb-doc
  "a list of symbols that describe features provided by the
   implementation")

(defun machine-instance ()
  #!+sb-doc
  "Return a string giving the name of the local machine."
  #!+win32 (sb!win32::get-computer-name)
  #!-win32 (sb!unix:unix-gethostname))

(defvar *machine-version*)

(defun machine-version ()
  #!+sb-doc
  "Return a string describing the version of the computer hardware we
are running on, or NIL if we can't find any useful information."
  (unless (boundp '*machine-version*)
    (setf *machine-version* (get-machine-version)))
  *machine-version*)

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

;;;; ED
(defvar *ed-functions* nil
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
                  :references (list '(:sbcl :variable *ed-functions*))))
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

;;;; some *LOAD-FOO* variables

(defvar *load-print* nil
  #!+sb-doc
  "the default for the :PRINT argument to LOAD")

(defvar *load-verbose* nil
  ;; Note that CMU CL's default for this was T, and ANSI says it's
  ;; implementation-dependent. We choose NIL on the theory that it's
  ;; a nicer default behavior for Unix programs.
  #!+sb-doc
  "the default for the :VERBOSE argument to LOAD")
