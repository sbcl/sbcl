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
;;;; Generalizing over SIMPLE-FUN, CLOSURE, and FUNCALLABLE-INSTANCEs

;;; Underlying SIMPLE-FUN
(defun %fun-fun (function)
  (declare (function function))
  ;; It's too bad that TYPECASE isn't able to generate equivalent code.
  (case (fun-subtype function)
    (#.sb!vm:closure-widetag
     (%closure-fun function))
    (#.sb!vm:funcallable-instance-widetag
     ;; %FUNCALLABLE-INSTANCE-FUNCTION is not known to return a FUNCTION.
     ;; Is that right? Shouldn't we always initialize to something
     ;; that is a function, such as an error-signaling trampoline?
     (%fun-fun (%funcallable-instance-function function)))
    (t function)))

(defun %fun-lambda-list (function)
  (typecase function
    #!+sb-fasteval
    (sb!interpreter:interpreted-function
     (sb!interpreter:proto-fn-pretty-arglist
      (sb!interpreter:fun-proto-fn function)))
    #!+sb-eval
    (sb!eval:interpreted-function
     (sb!eval:interpreted-function-debug-lambda-list function))
    (t
     (%simple-fun-arglist (%fun-fun function)))))

(defun (setf %fun-lambda-list) (new-value function)
  (typecase function
    #!+sb-fasteval
    (sb!interpreter:interpreted-function
     (setf (sb!interpreter:proto-fn-pretty-arglist
            (sb!interpreter:fun-proto-fn function)) new-value))
    #!+sb-eval
    (sb!eval:interpreted-function
     (setf (sb!eval:interpreted-function-debug-lambda-list function) new-value))
    ;; FIXME: Eliding general funcallable-instances for now.
    ((or simple-fun closure)
     (setf (%simple-fun-arglist (%fun-fun function)) new-value)))
  new-value)

(defun %fun-type (function)
  (typecase function
    #!+sb-fasteval
    ;; Obtain a list of the right shape, usually with T for each
    ;; arg type, but respecting local declarations if any.
    (sb!interpreter:interpreted-function (sb!interpreter:%fun-type function))
    (t (%simple-fun-type (%fun-fun function)))))

(defconstant +closure-header-namedp+ #x800000)
(macrolet ((closure-header-word (closure)
             `(sap-ref-word (int-sap (get-lisp-obj-address ,closure))
                            (- sb!vm:fun-pointer-lowtag))))
  (defun closure-name (closure)
    (declare (closure closure))
    (if (logtest (with-pinned-objects (closure) (closure-header-word closure))
                 +closure-header-namedp+)
        ;; GET-CLOSURE-LENGTH counts the 'fun' slot
        (values (%closure-index-ref closure (- (get-closure-length closure) 2)) t)
        (values nil nil)))

  ;; Return a new object that has 1 more slot than CLOSURE,
  ;; and frob its header bit signifying that it is named.
  (declaim (ftype (sfunction (closure) closure) nameify-closure))
  (defun nameify-closure (closure)
    (declare (closure closure))
    (let* ((n-words (get-closure-length closure)) ; excluding header
           ;; N-WORDS includes the trampoline, so the number of slots we would
           ;; pass to %COPY-CLOSURE is 1 less than that, were it not for
           ;; the fact that we actually want to create 1 additional slot.
           ;; So in effect, asking for N-WORDS does exactly the right thing.
           (copy #!-(or x86 x86-64)
                 (sb!vm::%copy-closure n-words (%closure-fun closure))
                 #!+(or x86 x86-64)
                 ;; CLOSURE was tested on entry as (SATISFIES CLOSUREP) which,
                 ;; sadly, does not imply FUNCTIONP of the object
                 ;; because the type system is not smart enough.
                 (with-pinned-objects ((%closure-fun (truly-the function closure)))
                   ;; %CLOSURE-CALLEE manifests as a fixnum which remains
                   ;; valid across GC due to %CLOSURE-FUN being pinned
                   ;; until after the new closure is made.
                   (sb!vm::%copy-closure n-words (sb!vm::%closure-callee closure)))))
      (with-pinned-objects (copy)
        (loop with sap = (int-sap (get-lisp-obj-address copy))
              for i from 0 below (1- n-words)
              for ofs from (- (ash 2 sb!vm:word-shift) sb!vm:fun-pointer-lowtag)
                        by sb!vm:n-word-bytes
              do (setf (sap-ref-lispobj sap ofs) (%closure-index-ref closure i)))
        (setf (closure-header-word copy) ; Update the header
              ;; Closure copy lost its high header bits, so OR them in again.
              (logior #!+(and immobile-space 64-bit)
                      (get-lisp-obj-address sb!vm:function-layout)
                      +closure-header-namedp+
                      (closure-header-word copy))))
      (truly-the closure copy)))

  ;; Rename a closure. Doing so changes its identity unless it was already named.
  (defun set-closure-name (closure new-name)
    (declare (closure closure))
    (unless (logtest (with-pinned-objects (closure) (closure-header-word closure))
                     +closure-header-namedp+)
      (setq closure (nameify-closure closure)))
    ;; There are no closure slot setters, and in fact SLOT-SET
    ;; does not exist in a variant that takes a non-constant index.
    (with-pinned-objects (closure)
      (setf (sap-ref-lispobj (int-sap (get-lisp-obj-address closure))
                             (- (ash (get-closure-length closure) sb!vm:word-shift)
                                sb!vm:fun-pointer-lowtag)) new-name))
    closure))

;;; a SETFable function to return the associated debug name for FUN
;;; (i.e., the third value returned from CL:FUNCTION-LAMBDA-EXPRESSION),
;;; or NIL if there's none
(defun %fun-name (function)
  (case (fun-subtype function)
    (#.sb!vm:funcallable-instance-widetag
     (let (#!+(or sb-eval sb-fasteval)
           (layout (%funcallable-instance-layout function)))
       ;; We know that funcallable-instance-p is true,
       ;; and so testing via TYPEP would be wasteful.
       (cond #!+sb-eval
             ((eq layout #.(find-layout 'sb!eval:interpreted-function))
              (return-from %fun-name
                (sb!eval:interpreted-function-debug-name function)))
             #!+sb-fasteval
             ((eq layout #.(find-layout 'sb!interpreter:interpreted-function))
               (return-from %fun-name
                 (sb!interpreter:proto-fn-name
                  (sb!interpreter:fun-proto-fn
                   (truly-the sb!interpreter:interpreted-function function)))))
             ((classoid-cell-typep #.(find-classoid-cell 'standard-generic-function)
                                   function)
              (return-from %fun-name
                (sb!mop:generic-function-name function))))))
    (#.sb!vm:closure-widetag
     (multiple-value-bind (name namedp) (closure-name function)
       (when namedp
         (return-from %fun-name name)))))
  (%simple-fun-name (%fun-fun function)))

(defun (setf %fun-name) (new-value function)
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (setf (sb!eval:interpreted-function-debug-name function) new-value))
    #!+sb-fasteval
    (sb!interpreter:interpreted-function
     (setf (sb!interpreter:proto-fn-name (sb!interpreter:fun-proto-fn function))
           new-value))
    (generic-function
     ;; STANDARD-GENERIC-FUNCTION definitely has a NAME,
     ;; but other subtypes of GENERIC-FUNCTION could as well.
     (when (slot-exists-p function 'sb!pcl::name)
       (setf (slot-value function 'sb!pcl::name) new-value)))
    ;; This does not set the name of an un-named closure because doing so
    ;; is not a side-effecting operation that it ought to be.
    ;; In contrast, SB-PCL::SET-FUN-NAME specifically says that only if the
    ;; argument fun is a funcallable instance must it retain its identity.
    ;; That function *is* allowed to cons a new closure to name it.
    ((or simple-fun closure)
     (if (and (closurep function) (nth-value 1 (closure-name function)))
         (set-closure-name function new-value)
         (setf (%simple-fun-name (%fun-fun function)) new-value))))
  new-value)

(defun %fun-doc (function)
  (typecase function
    #!+sb-fasteval
    (sb!interpreter:interpreted-function
     (sb!interpreter:proto-fn-docstring (sb!interpreter:fun-proto-fn function)))
    #!+sb-eval
    (sb!eval:interpreted-function
     (sb!eval:interpreted-function-documentation function))
    (t
     (when (closurep function)
       (multiple-value-bind (name namedp) (closure-name function)
         (when namedp
           (return-from %fun-doc (random-documentation name 'function)))))
     (%simple-fun-doc (%fun-fun function)))))

(defun (setf %fun-doc) (new-value function)
  (declare (type (or null string) new-value))
  (typecase function
    #!+sb-fasteval
    (sb!interpreter:interpreted-function
     (setf (sb!interpreter:proto-fn-docstring
            (sb!interpreter:fun-proto-fn function)) new-value))
    #!+sb-eval
    (sb!eval:interpreted-function
     (setf (sb!eval:interpreted-function-documentation function) new-value))
    ((or simple-fun closure)
     (when (closurep function)
       (multiple-value-bind (name namedp) (closure-name function)
         (when namedp
           (return-from %fun-doc
             (setf (random-documentation name 'function) new-value)))))
     (setf (%simple-fun-doc (%fun-fun function)) new-value)))
  new-value)

(defun code-n-entries (code-obj)
  ;; The internal %n-entries slot is a fixnum storing the number
  ;; of simple-funs in the low 14 bits (16 bits of the machine word),
  ;; and the first function's offset in the high 16 bits.
  #!-64-bit (ldb (byte 14 0) (sb!vm::%code-n-entries code-obj))
  ;; The header stores the count.
  #!+64-bit (ldb (byte 16 24) (get-header-data code-obj)))

(defun %code-entry-point (code-obj fun-index)
  (declare (type (unsigned-byte 16) fun-index))
  (if (>= fun-index (code-n-entries code-obj))
      nil
      (%primitive sb!c:compute-fun
                  code-obj
                  (cond ((zerop fun-index) ; special case for the first simple-fun
                         #!-64-bit (ldb (byte 16 14) (sb!vm::%code-n-entries code-obj))
                         #!+64-bit (ldb (byte 16 40) (get-header-data code-obj)))
                        (t
                         (let ((i (+ (- sb!vm:other-pointer-lowtag)
                                     (ash (code-header-words code-obj)
                                          sb!vm:word-shift)
                                     (ash (1- fun-index) 2))))
                           (with-pinned-objects (code-obj)
                            (sap-ref-32 (int-sap (get-lisp-obj-address code-obj))
                                        i))))))))

(defun code-entry-points (code-obj)
  (let ((a (make-array (code-n-entries code-obj))))
    (dotimes (i (length a) a)
      (setf (aref a i) (%code-entry-point code-obj i)))))

(defun code-n-unboxed-data-words (code-obj)
  ;; If the number of boxed words (from the header) is not the same as
  ;; the displacement backwards from the first simple-fun to the header,
  ;; then there are unboxed constants between the end of the boxed constants
  ;; and the first simple-fun.
  (let ((f (%code-entry-point code-obj 0)))
    (or (and f
             (let ((from (code-header-words code-obj))
                   ;; Ignore the layout pointer (if present) in the upper bits
                   ;; of the function header.
                   (to (ldb (byte 24 sb!vm:n-widetag-bits)
                            (with-pinned-objects (f)
                              (sap-ref-word (int-sap (get-lisp-obj-address f))
                                            (- sb!vm:fun-pointer-lowtag))))))
               (and (< from to) (- to from))))
        0)))

;;; various environment inquiries

(defvar *features*
  '#.(sort (copy-list sb-cold:*shebang-features*) #'string<)
  "a list of symbols that describe features provided by the
   implementation")

(defun machine-instance ()
  "Return a string giving the name of the local machine."
  #!+win32 (sb!win32::get-computer-name)
  #!-win32 (truly-the simple-string (sb!unix:unix-gethostname)))

(declaim (type (or null string) *machine-version*))
(defvar *machine-version*)

(defun machine-version ()
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
(declaim (type (or null string) *short-site-name* *long-site-name*))
(defvar *short-site-name* nil
  "The value of SHORT-SITE-NAME.")
(defvar *long-site-name* nil
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

(defun %byte-blt (src src-start dst dst-start dst-end)
  (%byte-blt src src-start dst dst-start dst-end))

;;;; some *LOAD-FOO* variables

(defvar *load-print* nil
  "the default for the :PRINT argument to LOAD")

(defvar *load-verbose* nil
  ;; Note that CMU CL's default for this was T, and ANSI says it's
  ;; implementation-dependent. We choose NIL on the theory that it's
  ;; a nicer default behavior for Unix programs.
  "the default for the :VERBOSE argument to LOAD")
