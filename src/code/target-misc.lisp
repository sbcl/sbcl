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
    (#.sb!vm:simple-fun-header-widetag
     function)
    (#.sb!vm:closure-header-widetag
     (%closure-fun function))
    (#.sb!vm:funcallable-instance-header-widetag
     ;; %FUNCALLABLE-INSTANCE-FUNCTION is not known to return a FUNCTION.
     ;; Is that right? Shouldn't we always initialize to something
     ;; that is a function, such as an error-signaling trampoline?
     (%fun-fun (%funcallable-instance-function function)))))

(defun %fun-lambda-list (function)
  (typecase function
    #!+sb-fasteval
    (sb!interpreter:interpreted-function
     (sb!interpreter:fun-pretty-arglist function))
    #!+sb-eval
    (sb!eval:interpreted-function
     (sb!eval:interpreted-function-debug-lambda-list function))
    (t
     (%simple-fun-arglist (%fun-fun function)))))

(defun (setf %fun-lambda-list) (new-value function)
  (typecase function
    #!+sb-fasteval
    (sb!interpreter:interpreted-function
     (sb!interpreter:set-fun-pretty-arglist function new-value))
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

(!defglobal *closure-name-marker* (make-symbol ".CLOSURE-NAME."))
(defun closure-name (closure)
  (declare (closure closure))
  (let ((len (get-closure-length closure)))
    (if (and (>= len 4)
             ;; The number of closure-values is 1- the len.
             ;; The index of the last value is 1- that.
             ;; The index of the name-marker is 1- that.
             ;; (closure index 0 is the first closed-over value)
             (eq (%closure-index-ref closure (- len 3))
                 (load-time-value *closure-name-marker* t)))
        (values (%closure-index-ref closure (- len 2)) t)
        (values nil nil))))

;; Add 2 "slots" to the payload of a closure, one for the magic symbol
;; signifying that there is a name, and one for the name itself.
(defun nameify-closure (closure)
  (declare (closure closure))
  (let* ((physical-len (get-closure-length closure)) ; excluding header
         ;; subtract 1 because physical-len includes the trampoline word.
         (new-n-closure-vals (+ 2 (1- physical-len)))
         ;; Closures and funcallable-instances are pretty much the same to GC.
         ;; They're both varying-length boxed-payload objects.
         ;; But funcallable-instance has <tramp, function, info>
         ;; where closure has <tramp, info> so subtract 1 more word.
         (copy (%make-funcallable-instance (1- new-n-closure-vals))))
    (with-pinned-objects (closure copy)
      ;; change the widetag from funcallable-instance to closure.
      (setf (sap-ref-word (int-sap (get-lisp-obj-address copy))
                          (- sb!vm:fun-pointer-lowtag))
            (logior (ash (+ physical-len 2) 8) sb!vm:closure-header-widetag))
      (macrolet ((word (obj index)
                   `(sap-ref-lispobj (int-sap (get-lisp-obj-address ,obj))
                                     (+ (- sb!vm:fun-pointer-lowtag)
                                        (ash ,index sb!vm:word-shift)))))
        (loop for i from 1 to physical-len
              do (setf (word copy i) (word closure i)))
        (setf (word copy (1+ physical-len)) *closure-name-marker*)))
    copy))

;; Rename a closure. Doing so changes its identity unless it was already named.
;; To do this without allocating a new closure, we'd need an interface that
;; requests a placeholder from the outset. One possibility is that
;; (NAMED-LAMBDA NIL (x) ...) would allocate the name, initially stored as nil.
;; In that case, the simple-fun's debug-info could also contain a bit that
;; indicates that all closures over it are named, eliminating the storage
;; and check for *closure-name-marker* in the closure values.
(defun set-closure-name (closure new-name)
  (declare (closure closure))
  (unless (nth-value 1 (closure-name closure))
    (setq closure (nameify-closure closure)))
  ;; There are no closure slot setters, and in fact SLOT-SET
  ;; does not exist in a variant that takes a non-constant index.
  (with-pinned-objects (closure)
    (setf (sap-ref-lispobj (int-sap (get-lisp-obj-address closure))
                           (+ (- sb!vm:fun-pointer-lowtag)
                              (ash (get-closure-length closure)
                                   sb!vm:word-shift)))
          new-name))
  closure)

;;; FIXME: there is no reason to expose two FUN-NAME readers,
;;; one that works for everything except generic, and one that always works.
(defun fun-name (x)
  (if (typep x 'standard-generic-function)
      (sb!mop:generic-function-name x)
      (%fun-name x)))

;;; a SETFable function to return the associated debug name for FUN
;;; (i.e., the third value returned from CL:FUNCTION-LAMBDA-EXPRESSION),
;;; or NIL if there's none
(defun %fun-name (function)
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (sb!eval:interpreted-function-debug-name function))
    #!+sb-fasteval
    (sb!interpreter:interpreted-function (sb!interpreter:fun-name function))
    (t
     (let (name namedp)
       (if (and (closurep function)
                (progn
                  (multiple-value-setq (name namedp) (closure-name function))
                  namedp))
           name
           (%simple-fun-name (%fun-fun function)))))))

(defun (setf %fun-name) (new-value function)
  (typecase function
    #!+sb-eval
    (sb!eval:interpreted-function
     (setf (sb!eval:interpreted-function-debug-name function) new-value))
    #!+sb-fasteval
    (sb!interpreter:interpreted-function
     (sb!interpreter:set-fun-name function new-value))
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
     (sb!interpreter:fun-docstring function))
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
     (sb!interpreter:set-fun-docstring function new-value))
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

(defun code-n-unboxed-data-words (code-obj)
  ;; If the number of boxed words (from the header) is not the same as
  ;; the displacement backwards from the first simple-fun to the header,
  ;; then there are unboxed constants between the end of the boxed constants
  ;; and the first simple-fun.
  (let ((f (%code-entry-points code-obj)))
    (or (and f
             (let ((from (get-header-data code-obj))
                   (to (ash (with-pinned-objects (f)
                              (sap-ref-word (int-sap (get-lisp-obj-address f))
                                            (- sb!vm:fun-pointer-lowtag)))
                            (- sb!vm:n-widetag-bits))))
               (and (< from to) (- to from))))
        0)))

;;; various environment inquiries

(defvar *features*
  '#.(sort (copy-list sb-cold:*shebang-features*) #'string<)
  #!+sb-doc
  "a list of symbols that describe features provided by the
   implementation")

(defun machine-instance ()
  #!+sb-doc
  "Return a string giving the name of the local machine."
  #!+win32 (sb!win32::get-computer-name)
  #!-win32 (truly-the simple-string (sb!unix:unix-gethostname)))

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
  #!+sb-doc
  "See function documentation for ED.")

(defun ed (&optional x)
  #!+sb-doc
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
