;;;; the bare essentials of compiler error handling
;;;;
;;;; (Logically, this might belong in early-c.lisp, since it's stuff
;;;; which might as well be visible to all compiler code. However,
;;;; physically its DEFINE-CONDITION forms depend on the condition
;;;; system being set up before it can be cold loaded, so we keep it
;;;; in this separate, loaded-later file instead of in early-c.lisp.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; error-handling definitions which are easy to define early and
;;;; which are nice to have visible everywhere

;;; a function that is called to unwind out of COMPILER-ERROR
(declaim (type (function (&optional condition) nil) *compiler-error-bailout*))
(defvar *compiler-error-bailout*)

;;; an application programmer's error caught by the compiler
;;;
;;; We want a separate condition for application programmer errors so
;;; that we can distinguish them from system programming errors (bugs
;;; in SBCL itself). Application programmer errors should be caught
;;; and turned into diagnostic output and a FAILURE-P return value
;;; from COMPILE or COMPILE-FILE. Bugs in SBCL itself throw us into
;;; the debugger.
;;;
;;; A further word or two of explanation might be warranted here,
;;; since I (CSR) have spent the last day or so wandering in a
;;; confused daze trying to get this to behave nicely before finally
;;; hitting on the right solution.
;;;
;;; These objects obey a slightly involved protocol in order to
;;; achieve the right dynamic behaviour.  If we signal a
;;; COMPILER-ERROR from within the compiler, we want that the
;;; outermost call to COMPILE/COMPILE-FILE cease attempting to compile
;;; the code in question and instead compile a call to signal a
;;; PROGRAM-ERROR.  This is achieved by resignalling the condition
;;; from within the handler, so that the condition travels up the
;;; handler stack until it finds the outermost handler.  Why the
;;; outermost?  Well, COMPILE-FILE could call EVAL from an EVAL-WHEN,
;;; which could recursively call COMPILE, which could then signal an
;;; error; we want the inner EVAL not to fail so that we can go on
;;; compiling, so it's the outer COMPILE-FILE that needs to replace
;;; the erroneous call with a call to ERROR.
;;;
;;; This resignalling up the stack means that COMPILER-ERROR should
;;; not be a generalized instance of ERROR, as otherwise code such as
;;; (IGNORE-ERRORS (DEFGENERIC IF (X))) will catch and claim to handle
;;; the COMPILER-ERROR.  So we make COMPILER-ERROR inherit from
;;; CONDITION instead, as of sbcl-0.8alpha.0.2x, so that unless
;;; the user claims to be able to handle general CONDITIONs (and if he
;;; does, he deserves what's going to happen :-) [ Note: we don't make
;;; COMPILER-ERROR inherit from SERIOUS-CONDITION, because
;;; conventionally SERIOUS-CONDITIONs, if unhandled, end up in the
;;; debugger; although the COMPILER-ERROR might well trigger an entry
;;; into the debugger, it won't be the COMPILER-ERROR itself that is
;;; the direct cause. ]
;;;
;;; So, what if we're not inside the compiler, then?  Well, in that
;;; case we're in the evaluator, so we want to convert the
;;; COMPILER-ERROR into a PROGRAM-ERROR and signal it immediately.  We
;;; have to signal the PROGRAM-ERROR from the dynamic environment of
;;; attempting to evaluate the erroneous code, and not from any
;;; exterior handler, so that user handlers for PROGRAM-ERROR and
;;; ERROR stand a chance of running, in e.g. (IGNORE-ERRORS
;;; (DEFGENERIC IF (X))).  So this is where the SIGNAL-PROGRAM-ERROR
;;; restart comes in; the handler in EVAL-IN-LEXENV chooses this
;;; restart if it believes that the compiler is not present (which it
;;; tests using the BOUNDPness of *COMPILER-ERROR-BAILOUT*).  The
;;; restart executes in the dynamic environment of the original
;;; COMPILER-ERROR call, and all is well.
;;;
;;; CSR, 2003-05-13
(define-condition compiler-error (encapsulated-condition) ()
  (:report (lambda (condition stream)
             (print-object (encapsulated-condition condition) stream))))

;;; Signal the appropriate condition. COMPILER-ERROR calls the bailout
;;; function so that it never returns (but compilation continues).
(defun compiler-error (datum &rest arguments)
  (let ((condition (apply #'coerce-to-condition datum
                          'simple-program-error 'compiler-error arguments)))
    (restart-case
        (cerror "Replace form with call to ERROR."
                'compiler-error
                :condition condition)
      (signal-error ()
        (error condition)))
    (funcall *compiler-error-bailout* condition)
    (bug "Control returned from *COMPILER-ERROR-BAILOUT*.")))

(defmacro with-compiler-error-resignalling (&body body)
  `(handler-bind
       ((compiler-error
          (lambda (c)
            (if (boundp '*compiler-error-bailout*)
                ;; if we're in the compiler, delegate either to a higher
                ;; authority or, if that's us, back down to the
                ;; outermost compiler handler...
                (signal c)
                ;; ... if we're not in the compiler, better signal the
                ;; error straight away.
                (invoke-restart 'signal-error)))))
     ,@body))

(defun compiler-warn (datum &rest arguments)
  ;; WARN isn't actually inlined, however the NOTINLINE avoids use of #'WARN
  ;; directly as a code constant, without indirection through its #<fdefn>.
  ;; This matters because:
  ;;  (1) a layer of indirection is needed for encapulation to work properly
  ;;      via APPLY, and the minor optimization is hardly worth doing for WARN
  ;;  (2) WARN is redefined in warm load
  (declare (notinline warn))
  (apply #'warn datum arguments)
  (values))

(defun compiler-style-warn (datum &rest arguments)
  (declare (notinline style-warn)) ; same reasoning as above
  (apply #'style-warn datum arguments)
  (values))

(defun source-to-string (source)
  (with-sane-io-syntax
    (write-to-string source
                     :escape t :pretty t
                     :circle t :array nil)))

(defun make-compiler-error-form (condition source)
  `(error 'compiled-program-error
          :message ,(princ-to-string condition)
          :source ,(source-to-string source)))

;;; Fatal compiler errors. We export FATAL-COMPILER-ERROR as an
;;; interface for errors that kill the compiler dead
;;;
;;; These are not a COMPILER-ERRORs, since we don't try to recover
;;; from them and keep chugging along, but instead immediately bail
;;; out of the entire COMPILE-FILE.

(define-condition fatal-compiler-error (encapsulated-condition)
  ())

;;; the condition of COMPILE-FILE being unable to READ from the
;;; source file
;;;
;;; (The old CMU CL code did try to recover from this condition, but
;;; the code for doing that was messy and didn't always work right.
;;; Since in Common Lisp the simple act of reading and compiling code
;;; (even without ever loading the compiled result) can have side
;;; effects, it's a little scary to go on reading code when you're
;;; deeply confused, so we violate what'd otherwise be good compiler
;;; practice by not trying to recover from this error and bailing out
;;; instead.)
;;; This name is inaccurate. Perhaps COMPILE/LOAD-INPUT-ERROR would be better.
(define-condition input-error-in-compile-file (reader-error encapsulated-condition)
  (;; the position where the bad READ began, or NIL if unavailable,
   ;; redundant, or irrelevant
   (position :reader input-error-in-compile-file-position
             :initarg :position
             :initform nil)
   (line/col :reader input-error-in-compile-file-line/col
             :initarg :line/col
             :initform nil)
   (invoker :reader input-error-in-compile-file-invoker
            :initarg :invoker :initform 'compile-file))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<~S error during ~S:~
                ~@:_ ~2I~_~A~
                  ~@[~@:_~@:_(in form starting at ~:{~(~A~): ~S~:^, ~:_~})~]~
              ~:>"
             'read
             (input-error-in-compile-file-invoker condition)
             (encapsulated-condition condition)
             (let ((pos (input-error-in-compile-file-position condition)))
               (acond ((input-error-in-compile-file-line/col condition)
                       `((:line ,(car it))
                         (:column ,(cdr it))
                         (:position
                          ,(or pos (1- (file-position
                                        (stream-error-stream condition)))))))
                      (pos
                       (stream-error-position-info
                        (stream-error-stream condition) pos))))))))

(define-condition input-error-in-load (input-error-in-compile-file) ()
  (:default-initargs :invoker 'load))
