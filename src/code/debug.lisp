;;;; the debugger

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-DEBUG")

;;;; variables and constants

;;; things to consider when tweaking these values:
;;;   * We're afraid to just default them to NIL and NIL, in case the
;;;     user inadvertently causes a hairy data structure to be printed
;;;     when he inadvertently enters the debugger.
;;;   * We don't want to truncate output too much. These days anyone
;;;     can easily run their Lisp in a windowing system or under Emacs,
;;;     so it's not the end of the world even if the worst case is a
;;;     few thousand lines of output.
;;;   * As condition :REPORT methods are converted to use the pretty
;;;     printer, they acquire *PRINT-LEVEL* constraints, so e.g. under
;;;     sbcl-0.7.1.28's old value of *DEBUG-PRINT-LEVEL*=3, an
;;;     ARG-COUNT-ERROR printed as
;;;       error while parsing arguments to DESTRUCTURING-BIND:
;;;         invalid number of elements in
;;;           #
;;;         to satisfy lambda list
;;;           #:
;;;         exactly 2 expected, but 5 found
(defvar *debug-print-variable-alist* nil
  "an association list describing new bindings for special variables
to be used within the debugger. Eg.

 ((*PRINT-LENGTH* . 10) (*PRINT-LEVEL* . 6) (*PRINT-PRETTY* . NIL))

The variables in the CAR positions are bound to the values in the CDR
during the execution of some debug commands. When evaluating arbitrary
expressions in the debugger, the normal values of the printer control
variables are in effect.

Initially empty, *DEBUG-PRINT-VARIABLE-ALIST* is typically used to
provide bindings for printer control variables.")

(defvar *debug-readtable*
  ;; KLUDGE: This can't be initialized in a cold toplevel form,
  ;; because the *STANDARD-READTABLE* isn't initialized until after
  ;; cold toplevel forms have run. So instead we initialize it
  ;; immediately after *STANDARD-READTABLE*. -- WHN 20000205
  nil
  "*READTABLE* for the debugger")

(defvar *in-the-debugger* nil
  "This is T while in the debugger.")

;;; nestedness inside debugger command loops
(defvar *debug-command-level* 0)

;;; If this is bound before the debugger is invoked, it is used as the stack
;;; top by the debugger. It can either be the first interesting frame, or the
;;; name of the last uninteresting frame.
(defvar *stack-top-hint* nil)
(defvar *current-frame* nil)
(declaim (always-bound *stack-top-hint* *current-frame*))

;;; Beginner-oriented help messages are important because you end up
;;; in the debugger whenever something bad happens, or if you try to
;;; get out of the system with Ctrl-C or (EXIT) or EXIT or whatever.
;;; But after memorizing them the wasted screen space gets annoying..
(defvar *debug-beginner-help-p* t
  "Should the debugger display beginner-oriented help messages?")

(defun debug-prompt (stream)
  (sb-thread:get-foreground)
  (format stream
          "~%~W~:[~;[~W~]] "
          (sb-di:frame-number *current-frame*)
          (> *debug-command-level* 1)
          *debug-command-level*))

(define-load-time-global *debug-help-string*
"The debug prompt is square brackets, with number(s) indicating the current
  control stack level and, if you've entered the debugger recursively, how
  deeply recursed you are.
Any command -- including the name of a restart -- may be uniquely abbreviated.
The debugger rebinds various special variables for controlling i/o, sometimes
  to defaults (much like WITH-STANDARD-IO-SYNTAX does) and sometimes to
  its own special values, based on SB-EXT:*DEBUG-PRINT-VARIABLE-ALIST*.
Debug commands do not affect *, //, and similar variables, but evaluation in
  the debug loop does affect these variables.
SB-DEBUG:*FLUSH-DEBUG-ERRORS* controls whether errors at the debug prompt
  drop you deeper into the debugger. The default NIL allows recursive entry
  to debugger.

Getting in and out of the debugger:
  TOPLEVEL, TOP  exits debugger and returns to top level REPL
  RESTART        invokes restart numbered as shown (prompt if not given).
  ERROR          prints the error condition and restart cases.

  The number of any restart, or its name, or a unique abbreviation for its
   name, is a valid command, and is the same as using RESTART to invoke
   that restart.

Changing frames:
  UP     up frame         DOWN     down frame
  BOTTOM bottom frame     FRAME n  frame n (n=0 for top frame)

Inspecting frames:
  BACKTRACE [n]  shows n frames going down the stack.
  LIST-LOCALS, L lists locals in current frame.
  PRINT, P       displays function call for current frame.
  SOURCE [n]     displays frame's source form with n levels of enclosing forms.

Stepping:
  START Selects the CONTINUE restart if one exists and starts
        single-stepping. Single stepping affects only code compiled with
        under high DEBUG optimization quality. See User Manual for details.
  STEP  Steps into the current form.
  NEXT  Steps over the current form.
  OUT   Stops stepping temporarily, but resumes it when the topmost frame that
        was stepped into returns.
  STOP  Stops single-stepping.

Function and macro commands:
 (SB-DEBUG:ARG n)
    Return the n'th argument in the current frame.
 (SB-DEBUG:VAR string-or-symbol [id])
    Returns the value of the specified variable in the current frame.

Other commands:
  RETURN expr
    Return the values resulting from evaluation of expr from the
    current frame, if this frame was compiled with a sufficiently high
    DEBUG optimization quality.

  RESTART-FRAME
    Restart execution of the current frame, if this frame is for a
    global function which was compiled with a sufficiently high
    DEBUG optimization quality.

  SLURP
    Discard all pending input on *STANDARD-INPUT*. (This can be
    useful when the debugger was invoked to handle an error in
    deeply nested input syntax, and now the reader is confused.)")

(defmacro with-debug-io-syntax (() &body body)
  (let ((thunk (gensym "THUNK")))
    `(dx-flet ((,thunk ()
                       ,@body))
       (funcall-with-debug-io-syntax #',thunk))))

;;; If LOC is an unknown location, then try to find the block start
;;; location. Used by source printing to some information instead of
;;; none for the user.
(defun maybe-block-start-location (loc)
  (if (sb-di:code-location-unknown-p loc)
      (let* ((block (sb-di:code-location-debug-block loc))
             (start (sb-di:do-debug-block-locations (loc block)
                      (return loc))))
        (cond ((and (not (sb-di:debug-block-elsewhere-p block))
                    start)
               start)
              (t
               loc)))
      loc))

;;;; BACKTRACE

(declaim (unsigned-byte *backtrace-frame-count*))
(defvar *backtrace-frame-count* 1000
  "Default number of frames to backtrace. Defaults to 1000.")

(declaim (boolean *backtrace-print-pc*))
(defvar *backtrace-print-pc* nil)
(declaim (unsigned-byte *default-argument-limit*))
(defvar *default-argument-limit* call-arguments-limit)

(declaim (type (member :minimal :normal :full) *method-frame-style*))
(defvar *method-frame-style* :normal
  "Determines how frames corresponding to method functions are represented in
backtraces. Possible values are :MINIMAL, :NORMAL, and :FULL.

  :MINIMAL represents them as

    (<gf-name> ...args...)

    if all arguments are available, and only a single method is applicable to
    the arguments -- otherwise behaves as :NORMAL.

  :NORMAL represents them as

    ((:method <gf-name> [<qualifier>*] (<specializer>*)) ...args...)

    The frame is then followed by either [fast-method] or [slow-method],
    designating the kind of method function. (See below.)

  :FULL represents them using the actual funcallable method function name:

    ((sb-pcl:fast-method <gf-name> [<qualifier>*] (<specializer>*)) ...args...)

   or

    ((sb-pcl:slow-method <gf-name> [<qualifier>*] (<specializer>*)) ...args...)

   In the this case arguments may include values internal to SBCL's method
   dispatch machinery.")

(define-deprecated-function :early "1.2.15" backtrace (print-backtrace)
    (&optional (count *backtrace-frame-count*) (stream *debug-io*))
  (print-backtrace :count count :stream stream))

(define-deprecated-function :early "1.2.15" backtrace-as-list (list-backtrace)
    (&optional (count *backtrace-frame-count*))
  (list-backtrace :count count))

(defun backtrace-start-frame (frame-designator)
  (let ((here (sb-di:top-frame)))
    (labels ((current-frame ()
               (let ((frame here))
                 ;; Our caller's caller.
                 (loop repeat 2
                       do (setf frame (or (sb-di:frame-down frame) frame)))
                 frame))
             (interrupted-frame ()
               (or (find-interrupted-frame)
                   (current-frame))))
     (cond ((eq :current-frame frame-designator)
            (current-frame))
           ((eq :interrupted-frame frame-designator)
            (interrupted-frame))
           ((eq :debugger-frame frame-designator)
            (if (and *in-the-debugger* *current-frame*)
                *current-frame*
                (interrupted-frame)))
           ((sb-di:frame-p frame-designator)
            frame-designator)
           (t
            (error "Invalid designator for initial backtrace frame: ~S"
                   frame-designator))))))

(defun map-backtrace (function &key
                      (start 0)
                      (from :debugger-frame)
                      (count *backtrace-frame-count*))
  "Calls the designated FUNCTION with each frame on the call stack.
Returns the last value returned by FUNCTION.

COUNT is the number of frames to backtrace, defaulting to
*BACKTRACE-FRAME-COUNT*.

START is the number of the frame the backtrace should start from.

FROM specifies the frame relative to which the frames are numbered. Possible
values are an explicit SB-DI:FRAME object, and the
keywords :CURRENT-FRAME, :INTERRUPTED-FRAME, and :DEBUGGER-FRAME. Default
is :DEBUGGER-FRAME.

  :CURRENT-FRAME
    specifies the caller of MAP-BACKTRACE.

  :INTERRUPTED-FRAME
    specifies the first interrupted frame on the stack \(typically the frame
    where the error occurred, as opposed to error handling frames) if any,
    otherwise behaving as :CURRENT-FRAME.

  :DEBUGGER-FRAME
    specifies the currently debugged frame when inside the debugger, and
    behaves as :INTERRUPTED-FRAME outside the debugger.
"
  (declare (dynamic-extent function))
  (loop with result = nil
        for index upfrom 0
        for frame = (backtrace-start-frame from)
        then (sb-di:frame-down frame)
        until (null frame)
        when (<= start index) do
        (if (minusp (decf count))
            (return result)
            (setf result (funcall function frame)))
        finally (return result)))

(defun print-backtrace (&key
                        (stream *debug-io*)
                        (start 0)
                        (from :debugger-frame)
                        (count *backtrace-frame-count*)
                        (print-thread t)
                        (print-pc *backtrace-print-pc*)
                        (argument-limit *default-argument-limit*)
                        (print-frame-source nil)
                        (method-frame-style *method-frame-style*)
                        (emergency-best-effort (> *debug-command-level* 1)))
  "Print a listing of the call stack to STREAM, defaulting to *DEBUG-IO*.

COUNT is the number of frames to backtrace, defaulting to
*BACKTRACE-FRAME-COUNT*.

START is the number of the frame the backtrace should start from.

FROM specifies the frame relative to which the frames are numbered. Possible
values are an explicit SB-DI:FRAME object, and the
keywords :CURRENT-FRAME, :INTERRUPTED-FRAME, and :DEBUGGER-FRAME. Default
is :DEBUGGER-FRAME.

  :CURRENT-FRAME
    specifies the caller of PRINT-BACKTRACE.

  :INTERRUPTED-FRAME
    specifies the first interrupted frame on the stack \(typically the frame
    where the error occured, as opposed to error handling frames) if any,
    otherwise behaving as :CURRENT-FRAME.

  :DEBUGGER-FRAME
    specifies the currently debugged frame when inside the debugger, and
    behaves as :INTERRUPTED-FRAME outside the debugger.

If PRINT-THREAD is true (default), backtrace is preceded by printing the
thread object the backtrace is from.

If PRINT-FRAME-SOURCE is true (default is false), each frame is followed by
printing the currently executing source form in the function responsible for
that frame, when available. Requires the function to have been compiled at
DEBUG 2 or higher. If PRINT-FRAME-SOURCE is :ALWAYS, it also reports \"no
source available\" for frames for which were compiled at lower debug settings.

METHOD-FRAME-STYLE (defaulting to *METHOD-FRAME-STYLE*), determines how frames
corresponding to method functions are printed. Possible values
are :MINIMAL, :NORMAL, and :FULL. See *METHOD-FRAME-STYLE* for more
information.

If EMERGENCY-BEST-EFFORT is true then try to print as much information as
possible while navigating and ignoring possible errors."
  (let ((start-frame (backtrace-start-frame from)))
    (with-debug-io-syntax ()
      (let ((*suppress-print-errors* (if (and emergency-best-effort
                                              (not (subtypep 'serious-condition *suppress-print-errors*)))
                                         'serious-condition
                                         *suppress-print-errors*))
            (frame-index start))
        (labels
            ((print-frame (frame stream)
               (print-frame-call frame stream
                                 :number frame-index
                                 :print-pc print-pc :argument-limit argument-limit
                                 :method-frame-style method-frame-style
                                 :print-frame-source print-frame-source
                                 :emergency-best-effort emergency-best-effort))
             (print-frame/normal (frame)
               (print-frame frame stream))
             (print-frame/emergency-best-effort (frame)
               (with-open-stream (buffer (make-string-output-stream))
                 (handler-case
                     (progn
                       (fresh-line stream)
                       (print-frame frame buffer)
                       (write-string (get-output-stream-string buffer) stream))
                   (serious-condition (error)
                     (print-unreadable-object (error stream :type t)
                       (format stream "while printing frame ~S. The partial output is: ~S"
                               frame-index (get-output-stream-string buffer))))))))
          (handler-bind
              ((print-not-readable #'print-unreadably))
            (fresh-line stream)
            (when print-thread
              (format stream "Backtrace for: ~S~%" sb-thread:*current-thread*))
            (map-backtrace (lambda (frame)
                             (restart-case
                                 (if emergency-best-effort
                                     (print-frame/emergency-best-effort frame)
                                     (print-frame/normal frame))
                               (skip-printing-frame ()
                                 :report (lambda (stream)
                                           (format stream "Skip printing frame ~S" frame-index))
                                 (print-unreadable-object (frame stream :type t :identity t))))
                             (incf frame-index))
                           :from start-frame
                           :start start
                           :count count))))
      (fresh-line stream)
      (values))))

(defun list-backtrace (&key
                       (count *backtrace-frame-count*)
                       (argument-limit *default-argument-limit*)
                       (start 0)
                       (from :debugger-frame)
                       (method-frame-style *method-frame-style*))
    "Returns a list describing the call stack. Each frame is represented
by a sublist:

  \(<name> ...args...)

where the name describes the function responsible for the frame. The name
might not be bound to the actual function object. Unavailable arguments are
represented by dummy objects that print as #<unavailable argument>. Objects
with dynamic-extent allocation by the current thread are represented by
substitutes to avoid references to them from leaking outside their legal
extent.

COUNT is the number of frames to backtrace, defaulting to
*BACKTRACE-FRAME-COUNT*.

START is the number of the frame the backtrace should start from.

FROM specifies the frame relative to which the frames are numbered. Possible
values are an explicit SB-DI:FRAME object, and the
keywords :CURRENT-FRAME, :INTERRUPTED-FRAME, and :DEBUGGER-FRAME. Default
is :DEBUGGER-FRAME.

  :CURRENT-FRAME
    specifies the caller of LIST-BACKTRACE.

  :INTERRUPTED-FRAME
    specifies the first interrupted frame on the stack \(typically the frame
    where the error occured, as opposed to error handling frames) if any,
    otherwise behaving as :CURRENT-FRAME.

  :DEBUGGER-FRAME
    specifies the currently debugged frame when inside the debugger, and
    behaves as :INTERRUPTED-FRAME outside the debugger.

METHOD-FRAME-STYLE (defaulting to *METHOD-FRAME-STYLE*), determines how frames
corresponding to method functions are printed. Possible values
are :MINIMAL, :NORMAL, and :FULL. See *METHOD-FRAME-STYLE* for more
information."
  (let (rbacktrace)
     (map-backtrace
      (lambda (frame)
        (push (frame-call-as-list frame argument-limit :method-frame-style method-frame-style)
              rbacktrace))
      :count count
      :start start
      :from (backtrace-start-frame from))
     (nreverse rbacktrace)))

(defun frame-call-as-list (frame argument-limit &key (method-frame-style *method-frame-style*))
  (multiple-value-bind (name args info)
      (frame-call frame :method-frame-style method-frame-style
                        :argument-limit argument-limit
                        :replace-dynamic-extent-objects t)
    (values (cons name args) info)))

;;; This is used in constructing arg lists for debugger printing,
;;; and when needing to print unbound slots in PCL.
(defstruct (unprintable-object
            (:constructor make-unprintable-object (string))
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s)
                               (write-string (unprintable-object-string x) s))))
            (:copier nil))
  (string nil :read-only t))
(declaim (freeze-type unprintable-object))

(defun replace-dynamic-extent-object (obj)
  (if (stack-allocated-p obj)
      (make-unprintable-object
       (handler-case
           (format nil "dynamic-extent: ~S" obj)
         (error ()
           "error printing dynamic-extent object")))
      obj))

;;; If X is stack-allocated and on the current thread's stack, then return
;;; the value of *current-thread*. Otherwise, if ALL-THREADS is T, then
;;; look for X on any stack, returning the thread that contains it.
;;; If X is not stack-allocated, or allocated on a different thread's stack
;;; when ALL-THREADS is NIL, then return NIL.
(defun stack-allocated-p (x &optional all-threads)
  (let ((a (get-lisp-obj-address x)))
    (and (sb-vm:is-lisp-pointer a)
         (cond ((and (<= (get-lisp-obj-address sb-vm:*control-stack-start*) a)
                     (< a (get-lisp-obj-address sb-vm:*control-stack-end*)))
                sb-thread:*current-thread*)
               (all-threads
                (macrolet ((in-stack-range-p ()
                             `(and (>= a (sb-thread::thread-control-stack-start thread))
                                   (< a (sb-thread::thread-control-stack-end thread)))))
                  #+win32 ; exhaustive search
                  (dolist (thread (sb-thread:list-all-threads)) ; conses, but I don't care
                    (when (in-stack-range-p)
                      (return thread)))
                  #-win32
                  ;; find a stack whose primitive-thread is nearest and above A.
                  (awhen (sb-thread::avl-find>= a sb-thread::*all-threads*)
                    (let ((thread (sb-thread::avlnode-data it)))
                      (when (in-stack-range-p)
                        thread)))))))))

;;;; frame printing

(eval-when (:compile-toplevel :execute)

;;; This is a convenient way to express what to do for each type of
;;; lambda-list element.
(sb-xc:defmacro lambda-list-element-dispatch (element
                                              &key
                                              required
                                              optional
                                              rest
                                              keyword
                                              more
                                              deleted)
  `(etypecase ,element
     (sb-di:debug-var
      ,@required)
     (cons
      (ecase (car ,element)
        (:optional ,@optional)
        (:rest ,@rest)
        (:keyword ,@keyword)
        (:more ,@more)))
     (symbol
      (aver (eq ,element :deleted))
      ,@deleted)))

(sb-xc:defmacro lambda-var-dispatch (variable location deleted valid other)
  (let ((var (gensym)))
    `(let ((,var ,variable))
       (cond ((eq ,var :deleted) ,deleted)
             ((eq (sb-di:debug-var-validity ,var ,location) :valid)
              ,valid)
             (t ,other)))))

) ; EVAL-WHEN

;;; Extract the function argument values for a debug frame.
(defun map-frame-args (thunk frame limit)
  (unless (zerop limit)
    (let ((debug-fun (sb-di:frame-debug-fun frame)))
      (dolist (element (sb-di:debug-fun-lambda-list debug-fun))
        (funcall thunk element)
        (when (zerop (decf limit))
          (return))))))

;;; When the frame is interrupted before any of the function code is called
;;; we can recover all the arguments, include the extra ones.
;;; This includes the ARG-COUNT-ERROR and UNDEFINED-FUNCTION coming from
;;; undefined-tramp.
(defun early-frame-nth-arg (n frame)
  (let* ((escaped (sb-di::compiled-frame-escaped frame))
         (pointer (sb-di::frame-pointer frame))
         (arg-count (sb-di::sub-access-debug-var-slot
                     pointer sb-c:arg-count-sc escaped)))
    (if (and (>= n 0)
             (< n arg-count))
        (sb-di::sub-access-debug-var-slot
         pointer
         (sb-c:standard-arg-location-sc n)
         escaped)
        (error "Index ~a out of bounds for ~a supplied argument~:p." n arg-count))))

;;; Return no more than LIMIT args. Aside from the default value of "no limit",
;;; the other most useful possibility is 0, if you're just going to discard the args.
;;; Any smaller value is OK too, but "be careful what you wish for" if you're going
;;; to use the argument list to restart the frame.
(defun early-frame-args (frame limit)
  (unless (zerop limit)
    (let* ((escaped (sb-di::compiled-frame-escaped frame))
           (pointer (sb-di::frame-pointer frame))
           (arg-count (sb-di::sub-access-debug-var-slot
                       pointer sb-c:arg-count-sc escaped)))
      (loop for i below (min arg-count limit)
            collect (sb-di::sub-access-debug-var-slot
                     pointer
                     (sb-c:standard-arg-location-sc i)
                     escaped)))))

(defun frame-args-as-list (frame limit)
  (declare (type sb-di:frame frame)
           (type (and unsigned-byte fixnum) limit))
  ;;; All args are available if the function has not proceeded beyond its external
  ;;; entry point, so every imcoming value is in its argument-passing location.
  (when (sb-di::all-args-available-p frame)
    (return-from frame-args-as-list (early-frame-args frame limit)))
  (handler-case
      (let ((location (sb-di:frame-code-location frame))
            (reversed-result nil))
        (block enumerating
          (map-frame-args
           (lambda (element)
             (lambda-list-element-dispatch element
              :required ((push (frame-call-arg element location frame) reversed-result))
              :optional ((push (frame-call-arg (second element) location frame)
                               reversed-result))
              :keyword ((push (second element) reversed-result)
                        (push (frame-call-arg (third element) location frame)
                              reversed-result))
              :deleted ((push (frame-call-arg element location frame) reversed-result))
              :rest ((lambda-var-dispatch (second element) location
                      nil
                      (let ((rest (sb-di:debug-var-value (second element) frame)))
                        (if (listp rest)
                            (setf reversed-result (append (reverse rest) reversed-result))
                            (push (make-unprintable-object "unavailable &REST argument")
                                  reversed-result))
                        (return-from enumerating))
                      (push (make-unprintable-object
                             "unavailable &REST argument")
                            reversed-result)))
              :more ((lambda-var-dispatch (second element) location
                      nil
                      (let ((context (sb-di:debug-var-value (second element) frame))
                            (count (sb-di:debug-var-value (third element) frame)))
                        (setf reversed-result
                              (append (reverse
                                       (multiple-value-list
                                        (sb-c:%more-arg-values context 0 count)))
                                      reversed-result))
                        (return-from enumerating))
                      (push (make-unprintable-object "unavailable &MORE argument")
                            reversed-result)))))
           frame limit))
        (nreverse reversed-result))
    (sb-di:lambda-list-unavailable ()
      (make-unprintable-object "unavailable lambda list"))))

(defun clean-xep (frame name args info)
  (values name
          (if (and (consp args)
                   ;; EARLY-FRAME-ARGS doesn't include arg-count
                   (not (sb-di::all-args-available-p frame)))
              (rest args)
              args)
          info))

(defun clean-&more-processor (name args info)
  (values name
          (if (consp args)
              (let* ((more (last args 2))
                     (context (first more))
                     (count (second more)))
                (append
                 (butlast args 2)
                 (if (fixnump count)
                     (multiple-value-list
                      (sb-c:%more-arg-values context 0 count))
                     (list
                      (make-unprintable-object "more unavailable arguments")))))
              args)
          info))

(defun clean-fast-method (name args style info)
  (declare (type (member :minimal :normal :full) style))
  (multiple-value-bind (cname cargs)
      ;; Make no attempt to simplify the display if ARGS could not be found
      ;; due to low (OPTIMIZE (DEBUG)) quality in the method.
      (if (or (eq style :full) (not (listp args)))
          (values name args)
          (let ((gf-name (second name))
                (real-args (the list (cddr args)))) ; strip .PV. and .N-M-CALL.
            (if (and (eq style :minimal)
                     (fboundp gf-name)
                     (notany #'unprintable-object-p real-args)
                     (singleton-p (compute-applicable-methods
                                   (fdefinition gf-name) real-args)))
                (values gf-name real-args)
                (values (cons :method (cdr name)) real-args))))
    (values cname cargs (cons :fast-method info))))

(defun clean-frame-call (frame argument-limit name method-frame-style info)
  (let ((args (frame-args-as-list frame argument-limit)))
    (when (typep name '(cons (eql sb-pcl::gf-dispatch)))
      (setf name (cadr name)))
    (cond ((typep name '(cons (eql sb-pcl::fast-method)))
           (clean-fast-method name args method-frame-style info))
          ((memq :external info)
           (clean-xep frame name args info))
          ((memq :more info)
           (clean-&more-processor name args info))
          (t
           (values name args info)))))

;;; This is an *internal* symbol of SB-DI. Tell me people don't use it directly???
;;; Otherwise why have such a verbose docstring. And why take &KEY args?
;;; We should pass in the parameters positionally. But I fear people must be using it.
(defun frame-call (frame &key (method-frame-style *method-frame-style*)
                              (argument-limit call-arguments-limit)
                              replace-dynamic-extent-objects)
  "Returns as multiple values a descriptive name for the function responsible
for FRAME, arguments that that function, and a list providing additional
information about the frame.

Unavailable arguments are represented using dummy-objects printing as
#<unavailable argument>.

METHOD-FRAME-STYLE (defaulting to *METHOD-FRAME-STYLE*), determines how frames
corresponding to method functions are printed. Possible values
are :MINIMAL, :NORMAL, and :FULL. See *METHOD-FRAME-STYLE* for more
information.

If REPLACE-DYNAMIC-EXTENT-OBJECTS is true, objects allocated on the stack of
the current thread are replaced with dummy objects which can safely escape."
  (let* ((debug-fun (sb-di:frame-debug-fun frame))
         (kind (sb-di:debug-fun-kind debug-fun)))
    (multiple-value-bind (name args info)
        (clean-frame-call frame
                          argument-limit
                          (or (sb-di:debug-fun-closure-name debug-fun frame)
                              (sb-di:debug-fun-name debug-fun))
                          method-frame-style
                          (when kind (list kind)))
      (let ((args (if (and (consp args) replace-dynamic-extent-objects)
                      (mapcar #'replace-dynamic-extent-object args)
                      args)))
        (values name args info)))))

(defun ensure-printable-object (object)
  (handler-case
      (with-open-stream (out sb-impl::*null-broadcast-stream*)
        (prin1 object out)
        object)
    (error (cond)
      (declare (ignore cond))
      (multiple-value-bind (type address)
          (ignore-errors (values (type-of object)
                                 (get-lisp-obj-address object)))
        (make-unprintable-object
         (if type
             (format nil "error printing ~a {~x}" type address)
             "error printing object"))))))

(defun frame-call-arg (var location frame)
  (lambda-var-dispatch var location
    (make-unprintable-object "unused argument")
    (sb-di:debug-var-value var frame)
    (make-unprintable-object "unavailable argument")))

;;; Prints a representation of the function call causing FRAME to
;;; exist. VERBOSITY indicates the level of information to output;
;;; zero indicates just printing the DEBUG-FUN's name, and one
;;; indicates displaying call-like, one-liner format with argument
;;; values.
(defun print-frame-call (frame stream
                         &key print-frame-source
                              number
                              (print-pc *backtrace-print-pc*)
                              (argument-limit *default-argument-limit*)
                              (method-frame-style *method-frame-style*)
                              (emergency-best-effort (> *debug-command-level* 1)))
  (when number
    (format stream "~&~S: " (if (integerp number)
                                number
                                (sb-di:frame-number frame))))
  (when print-pc
    (let ((debug-fun (sb-di:frame-debug-fun frame)))
      (when (typep debug-fun 'sb-di::compiled-debug-fun)
        (format stream "#x~x "
                (sap-int (sap+ (code-instructions
                                (sb-di::compiled-debug-fun-component debug-fun))
                               (sb-di::compiled-code-location-pc
                                (sb-di:frame-code-location frame))))))))
  (multiple-value-bind (name args info)
      (frame-call frame :argument-limit argument-limit
                        :method-frame-style method-frame-style)
    (pprint-logical-block (stream nil :prefix "(" :suffix ")")
      (let ((*print-pretty* nil)
            (*print-circle* t))
        ;; Since we go to some trouble to make nice informative
        ;; function names like (PRINT-OBJECT :AROUND (CLOWN T)), let's
        ;; make sure that they aren't truncated by *PRINT-LENGTH* and
        ;; *PRINT-LEVEL*.
        (let ((*print-length* nil)
              (*print-level* nil)
              (name (if emergency-best-effort
                        (ensure-printable-object name)
                        name)))
          (write name :stream stream :escape t :pretty (equal '(lambda ()) name)))

        ;; For the function arguments, we can just print normally.  If
        ;; we hit a &REST arg, then print as many of the values as
        ;; possible, punting the loop over lambda-list variables since
        ;; any other arguments will be in the &REST arg's list of
        ;; values.
        (let ((args (cond ((not emergency-best-effort)
                           args)
                          ((consp args)
                           (mapcar #'ensure-printable-object args))
                          (t
                           (ensure-printable-object args)))))
          (cond ((not (listp args))
                 (format stream " ~S" args))
                (t
                 (dolist (arg args)
                   (write-char #\space stream)
                   (pprint-newline :linear stream)
                   (write arg :stream stream :escape t)))))))
    (when info
      (format stream " [~{~(~A~)~^,~}]" info)))
  (when print-frame-source
    (let* ((loc (sb-di:frame-code-location frame))
           (path (and (sb-di::compiled-debug-fun-p
                       (sb-di:code-location-debug-fun loc))
                      (handler-case (sb-di:code-location-debug-source loc)
                        (sb-di:no-debug-blocks ())
                        (:no-error (source)
                          (sb-di:debug-source-namestring source))))))
      (when (or (eq print-frame-source :always)
                ;; Avoid showing sources for internals,
                ;; it will either fail anyway due to the
                ;; reader conditionals or show something nobody has
                ;; any iterest in.
                (not (eql (search "SYS:SRC;" path) 0)))
        (handler-case
            (let ((source (handler-case
                              (code-location-source-form loc 0)
                            (error (c)
                              (format stream "~&   error finding frame source: ~A" c)))))
              (format stream "~%   source: ~S" source))
          (sb-di:debug-condition ()
            ;; This is mostly noise.
            (when (eq :always print-frame-source)
              (format stream "~&   no source available for frame")))
          (error (c)
            (format stream "~&   error printing frame source: ~A" c)))))))

;;;; INVOKE-DEBUGGER

(defvar *debugger-hook* nil
  "This is either NIL or a function of two arguments, a condition and the value
   of *DEBUGGER-HOOK*. This function can either handle the condition or return
   which causes the standard debugger to execute. The system passes the value
   of this variable to the function because it binds *DEBUGGER-HOOK* to NIL
   around the invocation.")

;;; These are bound on each invocation of INVOKE-DEBUGGER.
(defvar *debug-restarts*)
(defvar *debug-condition*)
(defvar *nested-debug-condition*)

;;; Oh, what a tangled web we weave when we preserve backwards
;;; compatibility with 1968-style use of global variables to control
;;; per-stream i/o properties; there's really no way to get this
;;; quite right, but we do what we can.
(defun funcall-with-debug-io-syntax (fun &rest rest)
  (declare (type function fun))
  ;; Try to force the other special variables into a useful state.
  (let (;; Protect from WITH-STANDARD-IO-SYNTAX some variables where
        ;; any default we might use is less useful than just reusing
        ;; the global values.
        (original-package *package*)
        (original-print-pretty *print-pretty*))
    (with-standard-io-syntax
      (with-sane-io-syntax
          (let (;; We want the printer and reader to be in a useful
                ;; state, regardless of where the debugger was invoked
                ;; in the program. WITH-STANDARD-IO-SYNTAX and
                ;; WITH-SANE-IO-SYNTAX do much of what we want, but
                ;;   * It doesn't affect our internal special variables
                ;;     like *CURRENT-LEVEL-IN-PRINT*.
                ;;   * It isn't customizable.
                ;;   * It sets *PACKAGE* to COMMON-LISP-USER, which is not
                ;;     helpful behavior for a debugger.
                ;;   * There's no particularly good debugger default for
                ;;     *PRINT-PRETTY*, since T is usually what you want
                ;;     -- except absolutely not what you want when you're
                ;;     debugging failures in PRINT-OBJECT logic.
                ;; We try to address all these issues with explicit
                ;; rebindings here.
                (*current-level-in-print* 0)
                (*package* original-package)
                (*print-pretty* original-print-pretty)
                ;; Clear the circularity machinery to try to to reduce the
                ;; pain from sharing the circularity table across all
                ;; streams; if these are not rebound here, then setting
                ;; *PRINT-CIRCLE* within the debugger when debugging in a
                ;; state where something circular was being printed (e.g.,
                ;; because the debugger was entered on an error in a
                ;; PRINT-OBJECT method) makes a hopeless mess. Binding them
                ;; here does seem somewhat ugly because it makes it more
                ;; difficult to debug the printing-of-circularities code
                ;; itself; however, as far as I (WHN, 2004-05-29) can see,
                ;; that's almost entirely academic as long as there's one
                ;; shared *C-H-T* for all streams (i.e., it's already
                ;; unreasonably difficult to debug print-circle machinery
                ;; given the buggy crosstalk between the debugger streams
                ;; and the stream you're trying to watch), and any fix for
                ;; that buggy arrangement will likely let this hack go away
                ;; naturally.
                (sb-impl::*circularity-hash-table* . nil)
                (sb-impl::*circularity-counter* . nil)
                (*readtable* *debug-readtable*))
            (progv
                ;; (Why NREVERSE? PROGV makes the later entries have
                ;; precedence over the earlier entries.
                ;; *DEBUG-PRINT-VARIABLE-ALIST* is called an alist, so it's
                ;; expected that its earlier entries have precedence. And
                ;; the earlier-has-precedence behavior is mostly more
                ;; convenient, so that programmers can use PUSH or LIST* to
                ;; customize *DEBUG-PRINT-VARIABLE-ALIST*.)
                (nreverse (mapcar #'car *debug-print-variable-alist*))
                (nreverse (mapcar #'cdr *debug-print-variable-alist*))
              (apply fun rest)))))))

;;; This function is not inlined so it shows up in the backtrace; that
;;; can be rather handy when one has to debug the interplay between
;;; *INVOKE-DEBUGGER-HOOK* and *DEBUGGER-HOOK*.
(declaim (notinline run-hook))
(defun run-hook (variable condition)
  (let ((old-hook (symbol-value variable)))
    (when old-hook
      (progv (list variable) (list nil)
        (funcall old-hook condition old-hook)))))

;;; We can bind *stack-top-hint* to a symbol, in which case this function will
;;; resolve that hint lazily before we enter the debugger.
(defun resolve-stack-top-hint ()
  (let ((hint *stack-top-hint*)
        (*stack-top-hint* nil))
    (cond
      ;; No hint, just keep the debugger guts out.
      ((not hint)
       (find-caller-frame))
      ;; Interrupted. Look for the interrupted frame -- if we don't find one
      ;; this falls back to the next case.
      ((and (eq hint 'invoke-interruption)
            (find-interrupted-frame)))
      ;; Name of the first uninteresting frame.
      ((symbolp hint)
       (find-caller-of-named-frame hint))
      ;; We already have a resolved hint.
      (t
       hint))))

(defun invoke-debugger (condition)
  "Enter the debugger."
  (let ((*stack-top-hint* (resolve-stack-top-hint))
        (sb-impl::*deadline* nil))
    ;; call *INVOKE-DEBUGGER-HOOK* first, so that *DEBUGGER-HOOK* is not
    ;; called when the debugger is disabled
    (run-hook '*invoke-debugger-hook* condition)
    (run-hook '*debugger-hook* condition)
    ;; We definitely want *PACKAGE* to be of valid type.
    ;;
    ;; Elsewhere in the system, we use the SANE-PACKAGE function for
    ;; this, but here causing an exception just as we're trying to handle
    ;; an exception would be confusing, so instead we use a special hack.
    (unless (package-name *package*)
      (setf *package* (find-package :cl-user))
      (format *error-output*
              "The value of ~S was not an undeleted PACKAGE. It has been ~
               reset to ~S."
              '*package* *package*))
    ;; Before we start our own output, finish any pending output.
    ;; Otherwise, if the user tried to track the progress of his program
    ;; using PRINT statements, he'd tend to lose the last line of output
    ;; or so, which'd be confusing.
    (flush-standard-output-streams)
    (funcall-with-debug-io-syntax #'%invoke-debugger condition)))

(defun %print-debugger-invocation-reason (condition stream)
  (format stream "~2&")
  ;; Note: Ordinarily it's only a matter of taste whether to use
  ;; FORMAT "~<...~:>" or to use PPRINT-LOGICAL-BLOCK directly, but
  ;; until bug 403 is fixed, PPRINT-LOGICAL-BLOCK (STREAM NIL) is
  ;; definitely preferred, because the FORMAT alternative was acting odd.
  (pprint-logical-block (stream nil)
    #-sb-thread
    (format stream "debugger invoked on a ~S: ~2I~_~A" (type-of condition) condition)
    #+sb-thread
    (format stream
            "debugger invoked on a ~S~@[ @~x~] in thread ~_~A: ~2I~_~A"
            (type-of condition)
            (when (boundp '*current-internal-error-context*)
              (if (system-area-pointer-p *current-internal-error-context*)
                  (sb-alien:with-alien ((context (* os-context-t)
                                                 sb-kernel:*current-internal-error-context*))
                    (sap-int (sb-vm:context-pc context)))
                  (sap-int (sb-vm:context-pc *current-internal-error-context*))))
            sb-thread:*current-thread*
            condition))
  (terpri stream))

(defun %invoke-debugger (condition)
  (let ((*debug-condition* condition)
        (*debug-restarts* (compute-restarts condition))
        (*nested-debug-condition* nil))
    (handler-case
        ;; (The initial output here goes to *ERROR-OUTPUT*, because the
        ;; initial output is not interactive, just an error message, and
        ;; when people redirect *ERROR-OUTPUT*, they could reasonably
        ;; expect to see error messages logged there, regardless of what
        ;; the debugger does afterwards.)
        (unless (typep condition 'step-condition)
          (%print-debugger-invocation-reason condition *error-output*))
      (error (condition)
        (setf *nested-debug-condition* condition)
        (let ((ndc-type (type-of *nested-debug-condition*)))
          (format *error-output*
                  "~&~@<(A ~S was caught when trying to print ~S when ~
                      entering the debugger. Printing was aborted and the ~
                      ~S was stored in ~S.)~@:>~%"
                  ndc-type
                  '*debug-condition*
                  ndc-type
                  '*nested-debug-condition*))
        (when (typep *nested-debug-condition* 'cell-error)
          ;; what we really want to know when it's e.g. an UNBOUND-VARIABLE:
          (format *error-output*
                  "~&(CELL-ERROR-NAME ~S) = ~S~%"
                  '*nested-debug-condition*
                  (cell-error-name *nested-debug-condition*)))))

    (let ((background-p (sb-thread::debugger-wait-until-foreground-thread
                         *debug-io*)))

      ;; After the initial error/condition/whatever announcement to
      ;; *ERROR-OUTPUT*, we become interactive, and should talk on
      ;; *DEBUG-IO* from now on. (KLUDGE: This is a normative
      ;; statement, not a description of reality.:-| There's a lot of
      ;; older debugger code which was written to do i/o on whatever
      ;; stream was in fashion at the time, and not all of it has
      ;; been converted to behave this way. -- WHN 2000-11-16)
      (flet ((debug ()
               (unwind-protect
                    (let ( ;; We used to bind *STANDARD-OUTPUT* to *DEBUG-IO*
                          ;; here as well, but that is probably bogus since it
                          ;; removes the users ability to do output to a redirected
                          ;; *S-O*. Now we just rebind it so that users can temporarily
                          ;; frob it. FIXME: This and other "what gets bound when"
                          ;; behaviour should be documented in the manual.
                          (*standard-output* *standard-output*)
                          ;; This seems reasonable: e.g. if the user has redirected
                          ;; *ERROR-OUTPUT* to some log file, it's probably wrong
                          ;; to send errors which occur in interactive debugging to
                          ;; that file, and right to send them to *DEBUG-IO*.
                          (*error-output* *debug-io*))
                      (unless (typep condition 'step-condition)
                        (when *debug-beginner-help-p*
                          (format *debug-io*
                                  "~%~@<Type HELP for debugger help, or ~
                               (SB-EXT:EXIT) to exit from SBCL.~:@>~2%"))
                        (show-restarts *debug-restarts* *debug-io*))
                      (internal-debug))
                 (when background-p
                   (sb-thread:release-foreground)))))
        (if (find 'abort *debug-restarts* :key #'restart-name)
            (debug)
            (restart-case (let* ((restarts (compute-restarts condition))
                                 ;; Put the ABORT restart last,
                                 ;; as if it were provided by a toplevel function.
                                 (*debug-restarts* (nconc (cdr restarts)
                                                          (list (car restarts)))))
                            (debug))
              (abort ()
                :report (lambda (stream)
                          (format stream "~@<Exit from the current thread.~@:>"))
                (sb-thread:abort-thread :allow-exit t))))))))

;;; this function is for use in *INVOKE-DEBUGGER-HOOK* when ordinary
;;; ANSI behavior has been suppressed by the "--disable-debugger"
;;; command-line option
(defun debugger-disabled-hook (condition previous-hook &key (quit t))
  (declare (ignore previous-hook))
  ;; There is no one there to interact with, so report the
  ;; condition and terminate the program.
  (let ((*suppress-print-errors* t)
        (condition-error-message
         #.(format nil "A nested error within --disable-debugger error ~
            handling prevents displaying the original error. Attempting ~
            to print a backtrace."))
        (backtrace-error-message
         #.(format nil "A nested error within --disable-debugger error ~
            handling prevents printing the backtrace. Sorry, exiting.")))
    (labels
        ((failure-quit (&key abort)
           (/show0 "in FAILURE-QUIT (in --disable-debugger debugger hook)")
           (exit :code 1 :abort abort))
         (display-condition ()
           (handler-case
               (handler-case
                   (print-condition)
                 (condition ()
                   ;; printing failed, try to describe it
                   (describe-condition)))
             (condition ()
               ;; ok, give up trying to display the error and inform the user about it
               (finish-output *error-output*)
               (%primitive print condition-error-message))))
         (print-condition ()
           (format *error-output*
                   "~&~@<Unhandled ~S~@[ in thread ~S~]: ~2I~_~A~:>~2%"
                   (type-of condition)
                   #+sb-thread sb-thread:*current-thread*
                   #-sb-thread nil
                   condition)
           (finish-output *error-output*))
         (describe-condition ()
           (format *error-output*
                   "~&Unhandled ~S~@[ in thread ~S~]:~%"
                   (type-of condition)
                   #+sb-thread sb-thread:*current-thread*
                   #-sb-thread nil)
           (describe condition *error-output*)
           (finish-output *error-output*))
         (display-backtrace ()
           (handler-case
               (print-backtrace :stream *error-output*
                                :from :interrupted-frame
                                :print-thread t
                                :emergency-best-effort t)
             (condition ()
               (values)))
           (finish-output *error-output*)))
      ;; This HANDLER-CASE is here mostly to stop output immediately
      ;; (and fall through to QUIT) when there's an I/O error. Thus,
      ;; when we're run under a shell script or something, we can die
      ;; cleanly when the script dies (and our pipes are cut), instead
      ;; of falling into ldb or something messy like that. Similarly, we
      ;; can terminate cleanly even if BACKTRACE dies because of bugs in
      ;; user PRINT-OBJECT methods. Separate the error handling of the
      ;; two phases to maximize the chance of emitting some useful
      ;; information.
      (handler-case
          (progn
            (display-condition)
            (display-backtrace)
            (format *error-output*
                    "~%unhandled condition in --disable-debugger mode, quitting~%")
            (finish-output *error-output*)
            (when quit
              (failure-quit)))
        (condition ()
          ;; We IGNORE-ERRORS here because even %PRIMITIVE PRINT can
          ;; fail when our output streams are blown away, as e.g. when
          ;; we're running under a Unix shell script and it dies somehow
          ;; (e.g. because of a SIGINT). In that case, we might as well
          ;; just give it up for a bad job, and stop trying to notify
          ;; the user of anything.
          ;;
          ;; Actually, the only way I've run across to exercise the
          ;; problem is to have more than one layer of shell script.
          ;; I have a shell script which does
          ;;   time nice -10 sh make.sh "$1" 2>&1 | tee make.tmp
          ;; and the problem occurs when I interrupt this with Ctrl-C
          ;; under Linux 2.2.14-5.0 and GNU bash, version 1.14.7(1).
          ;; I haven't figured out whether it's bash, time, tee, Linux, or
          ;; what that is responsible, but that it's possible at all
          ;; means that we should IGNORE-ERRORS here. -- WHN 2001-04-24
          (ignore-errors
            (%primitive print backtrace-error-message))
          (failure-quit :abort t))))))

(defvar *old-debugger-hook* nil)

;;; halt-on-failures and prompt-on-failures modes, suitable for
;;; noninteractive and interactive use respectively
(defun disable-debugger ()
  "When invoked, this function will turn off both the SBCL debugger
and LDB (the low-level debugger).  See also ENABLE-DEBUGGER."
  ;; *DEBUG-IO* used to be set here to *ERROR-OUTPUT* which is sort
  ;; of unexpected but mostly harmless, but then ENABLE-DEBUGGER had
  ;; to set it to a suitable value again and be very careful,
  ;; especially if the user has also set it. -- MG 2005-07-15
  (unless (eq *invoke-debugger-hook* 'debugger-disabled-hook)
    (setf *old-debugger-hook* *invoke-debugger-hook*
          *invoke-debugger-hook* 'debugger-disabled-hook))
  ;; This is not inside the UNLESS to ensure that LDB is disabled
  ;; regardless of what the old value of *INVOKE-DEBUGGER-HOOK* was.
  ;; This might matter for example when restoring a core.
  (sb-alien:alien-funcall (sb-alien:extern-alien "disable_lossage_handler"
                                                 (function sb-alien:void))))

(defun enable-debugger ()
  "Restore the debugger if it has been turned off by DISABLE-DEBUGGER."
  (when (eql *invoke-debugger-hook* 'debugger-disabled-hook)
    (setf *invoke-debugger-hook* *old-debugger-hook*
          *old-debugger-hook* nil))
  (sb-alien:alien-funcall (sb-alien:extern-alien "enable_lossage_handler"
                                                 (function sb-alien:void))))

(defun show-restarts (restarts s)
  (cond ((null restarts)
         (format s
                 "~&(no restarts: If you didn't do this on purpose, ~
                  please report it as a bug.)~%"))
        (t
         (format s "~&restarts (invokable by number or by ~
                    possibly-abbreviated name):~%")
         (let ((count 0)
               (names-used '(nil))
               (max-name-len 0))
           (dolist (restart restarts)
             (let ((name (restart-name restart)))
               (when name
                 (let ((len (length (princ-to-string name))))
                   (when (> len max-name-len)
                     (setf max-name-len len))))))
           (unless (zerop max-name-len)
             (incf max-name-len 3))
           (dolist (restart restarts)
             (let ((name (restart-name restart)))
               ;; FIXME: maybe it would be better to display later names
               ;; in parens instead of brakets, not just omit them fully.
               ;; Call BREAK, call BREAK in the debugger, and tell me
               ;; it's not confusing looking. --NS 20050310
               (cond ((member name names-used)
                      (format s "~& ~2D: ~V@T~A~%" count max-name-len restart))
                     (t
                      (format s "~& ~2D: [~VA] ~A~%"
                              count (- max-name-len 3) name restart)
                      (push name names-used))))
             (incf count))))))

(defvar *debug-loop-fun* #'debug-loop-fun
  "A function taking no parameters that starts the low-level debug loop.")

;;; When the debugger is invoked due to a stepper condition, we don't
;;; want to print the current frame before the first prompt for aesthetic
;;; reasons.
(defvar *suppress-frame-print* nil)

;;; This calls DEBUG-LOOP, performing some simple initializations
;;; before doing so. INVOKE-DEBUGGER calls this to actually get into
;;; the debugger. SB-KERNEL::ERROR-ERROR calls this in emergencies
;;; to get into a debug prompt as quickly as possible with as little
;;; risk as possible for stepping on whatever is causing recursive
;;; errors.
(defun internal-debug ()
  (let ((*in-the-debugger* t)
        (*read-suppress* nil))
    (unless (typep *debug-condition* 'step-condition)
      (clear-input *debug-io*))
    (let ((*suppress-frame-print* (typep *debug-condition* 'step-condition)))
      (funcall *debug-loop-fun*))))

;;;; DEBUG-LOOP

;;; Note: This defaulted to T in CMU CL. The changed default in SBCL
;;; was motivated by desire to play nicely with ILISP.
(defvar *flush-debug-errors* nil
  "When set, avoid calling INVOKE-DEBUGGER recursively when errors occur while
   executing in the debugger.")

(defun debug-read (stream eof-restart)
  (declare (type stream stream))
  (let* ((eof-marker (cons nil nil))
         (form (read stream nil eof-marker)))
    (cond ((eq form eof-marker)
           ;; So that the REPL starts on a new line, because the
           ;; debugger is using *DEBUG-IO* and the REPL STDIN/STDOUT.
           (fresh-line stream)
           (invoke-restart eof-restart))
          (t
           form))))

(defun debug-loop-fun ()
  (let* ((*debug-command-level* (1+ *debug-command-level*))
         (*current-frame* (or *stack-top-hint* (sb-di:top-frame)))
         (*stack-top-hint* nil))
    (handler-bind ((sb-di:debug-condition
                    (lambda (condition)
                      (princ condition *debug-io*)
                      (/show0 "handling d-c by THROWing DEBUG-LOOP-CATCHER")
                      (throw 'debug-loop-catcher nil))))
      (cond (*suppress-frame-print*
             (setf *suppress-frame-print* nil))
            (t
             (terpri *debug-io*)
             (print-frame-call *current-frame* *debug-io* :print-frame-source t)))
      (loop
       (catch 'debug-loop-catcher
         (handler-bind ((error (lambda (condition)
                                 (when *flush-debug-errors*
                                   (clear-input *debug-io*)
                                   (princ condition *debug-io*)
                                   (format *debug-io*
                                           "~&error flushed (because ~
                                             ~S is set)"
                                           '*flush-debug-errors*)
                                   (/show0 "throwing DEBUG-LOOP-CATCHER")
                                   (throw 'debug-loop-catcher nil)))))
           ;; We have to bind LEVEL for the restart function created
           ;; by WITH-SIMPLE-RESTART, and we need the explicit ABORT
           ;; restart that exists now so that EOF from read can drop
           ;; one debugger level.
           (let ((level *debug-command-level*)
                 (restart-commands (make-restart-commands))
                 (abort-restart-for-eof (find-restart 'abort)))
             (flush-standard-output-streams)
             (debug-prompt *debug-io*)
             (force-output *debug-io*)
             (with-simple-restart (abort
                                   "~@<Reduce debugger level (to debug level ~W).~@:>"
                                   level)
               (let* ((exp (debug-read *debug-io* abort-restart-for-eof))
                      (cmd-fun (debug-command-p exp restart-commands)))
                 (cond ((not cmd-fun)
                        (debug-eval-print exp))
                       ((consp cmd-fun)
                        (format *debug-io*
                                "~&Your command, ~S, is ambiguous:~%"
                                exp)
                        (dolist (ele cmd-fun)
                          (format *debug-io* "   ~A~%" ele)))
                       (t
                        (funcall cmd-fun))))))))))))

(defvar *auto-eval-in-frame* t
  "When set (the default), evaluations in the debugger's command loop occur
relative to the current frame's environment without the need of debugger
forms that explicitly control this kind of evaluation.")

(defun debug-eval (expr)
  (cond ((not (and (fboundp 'compile) *auto-eval-in-frame*))
         (eval expr))
        ((frame-has-debug-vars-p *current-frame*)
         (sb-di:eval-in-frame *current-frame* expr))
        (t
         (format *debug-io* "; No debug variables for current frame: ~
                               using EVAL instead of EVAL-IN-FRAME.~%")
         (eval expr))))

(defun debug-eval-print (expr)
  (/noshow "entering DEBUG-EVAL-PRINT" expr)
  (let ((values (multiple-value-list
                 (interactive-eval expr :eval #'debug-eval))))
    (/noshow "done with EVAL in DEBUG-EVAL-PRINT")
    (dolist (value values)
      (fresh-line *debug-io*)
      (prin1 value *debug-io*)))
  (force-output *debug-io*))

;;;; debug loop functions

;;; These commands are functions, not really commands, so that users
;;; can get their hands on the values returned.

(defun var-valid-in-frame-p (var location &optional (frame *current-frame*))
  ;; arg count errors are checked before anything is set up but they
  ;; are reported in the elsewhere segment, which is after start-pc saved in the
  ;; debug function, defeating the checks.
  (and (not (sb-di::all-args-available-p frame))
       (eq (sb-di:debug-var-validity var location) :valid)))

(eval-when (:execute :compile-toplevel)

(sb-xc:defmacro define-var-operation (ref-or-set &optional value-var)
  `(let* ((temp (etypecase name
                  (symbol (sb-di:debug-fun-symbol-vars
                           (sb-di:frame-debug-fun *current-frame*)
                           name))
                  (simple-string (sb-di:ambiguous-debug-vars
                                  (sb-di:frame-debug-fun *current-frame*)
                                  name))))
          (location (sb-di:frame-code-location *current-frame*))
          ;; Let's only deal with valid variables.
          (vars (remove-if-not (lambda (v)
                                 (var-valid-in-frame-p v location))
                               temp)))
     (declare (list vars))
     (cond ((null vars)
            (error "No known valid variables match ~S." name))
           ((= (length vars) 1)
            ,(ecase ref-or-set
               (:ref
                '(sb-di:debug-var-value (car vars) *current-frame*))
               (:set
                `(setf (sb-di:debug-var-value (car vars) *current-frame*)
                       ,value-var))))
           (t
            ;; Since we have more than one, first see whether we have
            ;; any variables that exactly match the specification.
            (let* ((name (etypecase name
                           (symbol (symbol-name name))
                           (simple-string name)))
                   (exact (remove-if-not (lambda (v)
                                           (string= (sb-di:debug-var-name v)
                                                    name))
                                         vars))
                   (vars (or exact vars)))
              (declare (simple-string name)
                       (list exact vars))
              (cond
               ;; Check now for only having one variable.
               ((= (length vars) 1)
                ,(ecase ref-or-set
                   (:ref
                    '(sb-di:debug-var-value (car vars) *current-frame*))
                   (:set
                    `(setf (sb-di:debug-var-value (car vars) *current-frame*)
                           ,value-var))))
               ;; If there weren't any exact matches, flame about
               ;; ambiguity unless all the variables have the same
               ;; name.
               ((and (not exact)
                     (find-if-not
                      (lambda (v)
                        (string= (sb-di:debug-var-name v)
                                 (sb-di:debug-var-name (car vars))))
                      (cdr vars)))
                (error "specification ambiguous:~%~{   ~A~%~}"
                       (mapcar #'sb-di:debug-var-name
                               (delete-duplicates
                                vars :test #'string=
                                :key #'sb-di:debug-var-name))))
               ;; All names are the same, so see whether the user
               ;; ID'ed one of them.
               (id-supplied
                (let ((v (find id vars :key #'sb-di:debug-var-id)))
                  (unless v
                    (error
                     "invalid variable ID, ~W: should have been one of ~S"
                     id
                     (mapcar #'sb-di:debug-var-id vars)))
                  ,(ecase ref-or-set
                     (:ref
                      '(sb-di:debug-var-value v *current-frame*))
                     (:set
                      `(setf (sb-di:debug-var-value v *current-frame*)
                             ,value-var)))))
               (t
                (error "Specify variable ID to disambiguate ~S. Use one of ~S."
                       name
                       (mapcar #'sb-di:debug-var-id vars)))))))))

) ; EVAL-WHEN

;;; FIXME: This doesn't work. It would be real nice we could make it
;;; work! Alas, it doesn't seem to work in CMU CL X86 either..
(defun var (name &optional (id 0 id-supplied))
  "Return a variable's value if possible. NAME is a simple-string or symbol.
   If it is a simple-string, it is an initial substring of the variable's name.
   If name is a symbol, it has the same name and package as the variable whose
   value this function returns. If the symbol is uninterned, then the variable
   has the same name as the symbol, but it has no package.

   If name is the initial substring of variables with different names, then
   this return no values after displaying the ambiguous names. If name
   determines multiple variables with the same name, then you must use the
   optional id argument to specify which one you want. If you left id
   unspecified, then this returns no values after displaying the distinguishing
   id values.

   The result of this function is limited to the availability of variable
   information. This is SETF'able."
  (define-var-operation :ref))
(defun (setf var) (value name &optional (id 0 id-supplied))
  (define-var-operation :set value))

;;; This returns the COUNT'th arg as the user sees it from args, the
;;; result of SB-DI:DEBUG-FUN-LAMBDA-LIST. If this returns a
;;; potential DEBUG-VAR from the lambda-list, then the second value is
;;; T. If this returns a keyword symbol or a value from a rest arg,
;;; then the second value is NIL.
;;;
;;; FIXME: There's probably some way to merge the code here with
;;; FRAME-ARGS-AS-LIST. (A fair amount of logic is already shared
;;; through LAMBDA-LIST-ELEMENT-DISPATCH, but I suspect more could be.)
(declaim (ftype (function (index list)) nth-arg))
(defun nth-arg (count args)
  (let ((n count))
    (dolist (ele args (error "The argument specification ~S is out of range."
                             n))
      (lambda-list-element-dispatch ele
        :required ((if (zerop n) (return (values ele t))))
        :optional ((if (zerop n) (return (values (second ele) t))))
        :keyword ((cond ((zerop n)
                         (return (values (second ele) nil)))
                        ((zerop (decf n))
                         (return (values (third ele) t)))))
        :deleted ((if (zerop n) (return (values ele t))))
        :rest ((let ((var (second ele)))
                 (lambda-var-dispatch var (sb-di:frame-code-location
                                           *current-frame*)
                   (error "unused &REST argument before n'th argument")
                   (dolist (value
                            (sb-di:debug-var-value var *current-frame*)
                            (error
                             "The argument specification ~S is out of range."
                             n))
                     (if (zerop n)
                         (return-from nth-arg (values value nil))
                         (decf n)))
                   (error "invalid &REST argument before n'th argument")))))
      (decf n))))

(defun arg (n)
  "Return the N'th argument's value if possible. Argument zero is the first
   argument in a frame's default printed representation. Count keyword/value
   pairs as separate arguments."
  (when (sb-di::all-args-available-p *current-frame*)
    (return-from arg
      (early-frame-nth-arg n *current-frame*)))
  (multiple-value-bind (var lambda-var-p)
      (nth-arg n (handler-case (sb-di:debug-fun-lambda-list
                                (sb-di:frame-debug-fun *current-frame*))
                   (sb-di:lambda-list-unavailable ()
                     (error "No argument values are available."))))
    (if lambda-var-p
        (lambda-var-dispatch var (sb-di:frame-code-location *current-frame*)
          (error "Unused arguments have no values.")
          (sb-di:debug-var-value var *current-frame*)
          (error "invalid argument value"))
        var)))

;;;; machinery for definition of debug loop commands

(define-load-time-global *debug-commands* nil)

;;; Interface to *DEBUG-COMMANDS*. No required arguments in args are
;;; permitted.
(defmacro !def-debug-command (name args &rest body)
  (let ((fun-name (symbolicate name "-DEBUG-COMMAND")))
    `(progn
       (setf *debug-commands*
             (remove ,name *debug-commands* :key #'car :test #'string=))
       (defun ,fun-name ,args
         (unless *in-the-debugger*
           (error "invoking debugger command while outside the debugger"))
         ,@body)
       (push (cons ,name #',fun-name) *debug-commands*)
       ',fun-name)))

(defun !def-debug-command-alias (new-name existing-name)
  (let ((pair (assoc existing-name *debug-commands* :test #'string=)))
    (unless pair (error "unknown debug command name: ~S" existing-name))
    (push (cons new-name (cdr pair)) *debug-commands*))
  new-name)

;;; This takes a symbol and uses its name to find a debugger command,
;;; using initial substring matching. It returns the command function
;;; if form identifies only one command, but if form is ambiguous,
;;; this returns a list of the command names. If there are no matches,
;;; this returns nil. Whenever the loop that looks for a set of
;;; possibilities encounters an exact name match, we return that
;;; command function immediately.
(defun debug-command-p (form &optional other-commands)
  (if (or (symbolp form) (integerp form))
      (let* ((name
              (if (symbolp form)
                  (symbol-name form)
                  (format nil "~W" form)))
             (len (length name))
             (res nil))
        (declare (simple-string name)
                 (fixnum len)
                 (list res))

        ;; Find matching commands, punting if exact match.
        (flet ((match-command (ele)
                 (let* ((str (car ele))
                        (str-len (length str)))
                   (declare (simple-string str)
                            (fixnum str-len))
                   (cond ((< str-len len))
                         ((= str-len len)
                          (when (string= name str :end1 len :end2 len)
                            (return-from debug-command-p (cdr ele))))
                         ((string= name str :end1 len :end2 len)
                          (push ele res))))))
          (mapc #'match-command *debug-commands*)
          (mapc #'match-command other-commands))

        ;; Return the right value.
        (cond ((not res) nil)
              ((= (length res) 1)
               (cdar res))
              (t ; Just return the names.
               (do ((cmds res (cdr cmds)))
                   ((not cmds) res)
                 (setf (car cmds) (caar cmds))))))))

;;; Return a list of debug commands (in the same format as
;;; *DEBUG-COMMANDS*) that invoke each active restart.
;;;
;;; Two commands are made for each restart: one for the number, and
;;; one for the restart name (unless it's been shadowed by an earlier
;;; restart of the same name, or it is NIL).
(defun make-restart-commands (&optional (restarts *debug-restarts*))
  (let ((commands)
        (num 0))                        ; better be the same as show-restarts!
    (dolist (restart restarts)
      (let ((name (string (restart-name restart))))
        (let ((restart-fun
                (lambda ()
                  (/show0 "in restart-command closure, about to i-r-i")
                  (invoke-restart-interactively restart))))
          (push (cons (prin1-to-string num) restart-fun) commands)
          (unless (or (null (restart-name restart))
                      (find name commands :key #'car :test #'string=))
            (push (cons name restart-fun) commands))))
    (incf num))
  commands))

;;;; frame-changing commands

(!def-debug-command "UP" ()
  (let ((next (sb-di:frame-up *current-frame*)))
    (cond (next
           (setf *current-frame* next)
           (print-frame-call next *debug-io*))
          (t
           (format *debug-io* "~&Top of stack.")))))

(!def-debug-command "DOWN" ()
  (let ((next (sb-di:frame-down *current-frame*)))
    (cond (next
           (setf *current-frame* next)
           (print-frame-call next *debug-io*))
          (t
           (format *debug-io* "~&Bottom of stack.")))))

(!def-debug-command-alias "D" "DOWN")

(!def-debug-command "BOTTOM" ()
  (do ((prev *current-frame* lead)
       (lead (sb-di:frame-down *current-frame*) (sb-di:frame-down lead)))
      ((null lead)
       (setf *current-frame* prev)
       (print-frame-call prev *debug-io*))))

(!def-debug-command-alias "B" "BOTTOM")

(!def-debug-command "FRAME" (&optional
                             (n (read-prompting-maybe "frame number: ")))
  (setf *current-frame*
        (multiple-value-bind (next-frame-fun limit-string)
            (if (< n (sb-di:frame-number *current-frame*))
                (values #'sb-di:frame-up "top")
              (values #'sb-di:frame-down "bottom"))
          (do ((frame *current-frame*))
              ((= n (sb-di:frame-number frame))
               frame)
            (let ((next-frame (funcall next-frame-fun frame)))
              (cond (next-frame
                     (setf frame next-frame))
                    (t
                     (format *debug-io*
                             "The ~A of the stack was encountered.~%"
                             limit-string)
                     (return frame)))))))
  (print-frame-call *current-frame* *debug-io*))

(!def-debug-command-alias "F" "FRAME")

;;;; commands for entering and leaving the debugger

(!def-debug-command "TOPLEVEL" ()
  (throw 'toplevel-catcher nil))

;;; make T safe
(!def-debug-command-alias "TOP" "TOPLEVEL")

(!def-debug-command "RESTART" ()
  (/show0 "doing RESTART debug-command")
  (let ((num (read-if-available :prompt)))
    (when (eq num :prompt)
      (show-restarts *debug-restarts* *debug-io*)
      (write-string "restart: " *debug-io*)
      (force-output *debug-io*)
      (setf num (read *debug-io*)))
    (let ((restart (typecase num
                     (unsigned-byte
                      (nth num *debug-restarts*))
                     (symbol
                      (find num *debug-restarts* :key #'restart-name
                            :test (lambda (sym1 sym2)
                                    (string= (symbol-name sym1)
                                             (symbol-name sym2)))))
                     (t
                      (format *debug-io* "~S is invalid as a restart name.~%"
                              num)
                      (return-from restart-debug-command nil)))))
      (/show0 "got RESTART")
      (if restart
          (invoke-restart-interactively restart)
          (princ "There is no such restart." *debug-io*)))))

;;;; information commands

(!def-debug-command "HELP" ()
  ;; CMU CL had a little toy pager here, but "if you aren't running
  ;; ILISP (or a smart windowing system, or something) you deserve to
  ;; lose", so we've dropped it in SBCL. However, in case some
  ;; desperate holdout is running this on a dumb terminal somewhere,
  ;; we tell him where to find the message stored as a string.
  (format *debug-io*
          "~&~A~2%(The HELP string is stored in ~S.)~%"
          *debug-help-string*
          '*debug-help-string*))

(!def-debug-command-alias "?" "HELP")

(!def-debug-command "ERROR" ()
  (format *debug-io* "~A~%" *debug-condition*)
  (show-restarts *debug-restarts* *debug-io*))

(!def-debug-command "BACKTRACE" ()
 (print-backtrace :count (read-if-available most-positive-fixnum)))

(!def-debug-command "PRINT" ()
  (print-frame-call *current-frame* *debug-io*))

(!def-debug-command-alias "P" "PRINT")

(!def-debug-command "LIST-LOCALS" ()
  (let ((d-fun (sb-di:frame-debug-fun *current-frame*)))
    #+sb-fasteval
    (when (typep (sb-di:debug-fun-name d-fun nil)
                 '(cons (eql sb-interpreter::.eval.)))
      (let ((env (arg 1)))
        (when (typep env 'sb-interpreter:basic-env)
          (return-from list-locals-debug-command
            (sb-interpreter:list-locals env)))))
    (if (sb-di:debug-var-info-available d-fun)
        (let ((*standard-output* *debug-io*)
              (location (sb-di:frame-code-location *current-frame*))
              (prefix (read-if-available nil))
              (any-p nil)
              (any-valid-p nil))
          (multiple-value-bind (more-context more-count)
              (sb-di:debug-fun-more-args d-fun)
            (dolist (v (sb-di:ambiguous-debug-vars
                        d-fun
                        (if prefix (string prefix) "")))
              (setf any-p t)
              (when (var-valid-in-frame-p v location)
                (setf any-valid-p t)
                (unless (or (eq v more-context)
                            (eq v more-count))
                  (format *debug-io* "~S~:[#~W~;~*~]  =  ~S~%"
                          (sb-di:debug-var-symbol v)
                          (zerop (sb-di:debug-var-id v))
                          (sb-di:debug-var-id v)
                          (sb-di:debug-var-value v *current-frame*)))))
            (when (and more-context more-count)
              (format *debug-io* "~S  =  ~S~%"
                      'more
                      (multiple-value-list
                       (sb-c:%more-arg-values
                        (sb-di:debug-var-value more-context *current-frame*)
                        0
                        (sb-di:debug-var-value more-count *current-frame*))))))
          (cond
           ((not any-p)
            (format *debug-io*
                    "There are no local variables ~@[starting with ~A ~]~
                    in the function."
                    prefix))
           ((not any-valid-p)
            (format *debug-io*
                    "All variables ~@[starting with ~A ~]currently ~
                    have invalid values."
                    prefix))))
        (write-line "There is no variable information available."
                    *debug-io*))))

(!def-debug-command-alias "L" "LIST-LOCALS")

(!def-debug-command "SOURCE" ()
  (print (code-location-source-form (sb-di:frame-code-location *current-frame*)
                                    (read-if-available 0))
         *debug-io*))

;;;; source location printing

(defun code-location-source-form (location context &optional (errorp t))
  (let* ((start-location (maybe-block-start-location location))
         (form-num (sb-di:code-location-form-number start-location)))
    (multiple-value-bind (translations form)
        (sb-di:get-toplevel-form start-location)
      (declare (notinline warn))
      (cond ((< form-num (length translations))
             (sb-di:source-path-context form
                                        (svref translations form-num)
                                        context))
            (t
             (funcall (if errorp #'error #'warn)
                      "~@<Bogus form-number: the source file has ~
                          probably changed too much to cope with.~:@>"))))))


;;; start single-stepping
(!def-debug-command "START" ()
  (if (typep *debug-condition* 'step-condition)
      (format *debug-io* "~&Already single-stepping.~%")
      (let ((restart (find-restart 'continue *debug-condition*)))
        (cond (restart
               (sb-impl::enable-stepping)
               (invoke-restart restart))
              (t
               (format *debug-io* "~&Non-continuable error, cannot start stepping.~%"))))))

(defmacro !def-step-command (command-name restart-name)
  `(!def-debug-command ,command-name ()
     (if (typep *debug-condition* 'step-condition)
         (let ((restart (find-restart ',restart-name *debug-condition*)))
           (aver restart)
           (invoke-restart restart))
         (format *debug-io* "~&Not currently single-stepping. (Use START to activate the single-stepper)~%"))))

(!def-step-command "STEP" step-into)
(!def-step-command "NEXT" step-next)
(!def-step-command "STOP" step-continue)

(!def-debug-command-alias "S" "STEP")
(!def-debug-command-alias "N" "NEXT")

(!def-debug-command "OUT" ()
  (if (typep *debug-condition* 'step-condition)
      (if sb-impl::*step-out*
          (let ((restart (find-restart 'step-out *debug-condition*)))
            (aver restart)
            (invoke-restart restart))
          (format *debug-io* "~&OUT can only be used step out of frames that were originally stepped into with STEP.~%"))
      (format *debug-io* "~&Not currently single-stepping. (Use START to activate the single-stepper)~%")))

;;; miscellaneous commands

(!def-debug-command "DESCRIBE" ()
  (let* ((curloc (sb-di:frame-code-location *current-frame*))
         (debug-fun (sb-di:code-location-debug-fun curloc))
         (function (sb-di:debug-fun-fun debug-fun)))
    (if function
        (describe function)
        (format *debug-io* "can't figure out the function for this frame"))))

(!def-debug-command "SLURP" ()
  (loop while (read-char-no-hang *standard-input*)))

;;; RETURN-FROM-FRAME and RESTART-FRAME

(defun unwind-to-frame-and-call (frame thunk)
  #+unwind-to-frame-and-call-vop
  (flet ((sap-int/fixnum (sap)
           ;; On unithreaded X86 *BINDING-STACK-POINTER* and
           ;; *CURRENT-CATCH-BLOCK* are negative, so we need to jump through
           ;; some hoops to make these calculated values negative too.
           (ash (truly-the sb-vm:signed-word (sap-int sap))
                (- sb-vm:n-fixnum-tag-bits))))
    ;; To properly unwind the stack, we need three pieces of information:
    ;;   * The unwind block that should be active after the unwind
    ;;   * The catch block that should be active after the unwind
    ;;   * The values that the binding stack pointer should have after the
    ;;     unwind.
    (let ((catch-block (sap-int/fixnum (find-enclosing-catch-block frame)))
          (unbind-to (find-binding-stack-pointer frame)))
      ;; This VOP will run the necessary cleanup forms, reset the fp, and
      ;; then call the supplied function.
      (sb-vm::%primitive sb-vm::unwind-to-frame-and-call
                         (sb-di::frame-pointer frame)
                         (find-enclosing-uwp frame)
                         #-unbind-in-unwind
                         (lambda ()
                           ;; Before calling the user-specified
                           ;; function, we need to restore the binding
                           ;; stack and the catch block. The unwind block
                           ;; is taken care of by the VOP.
                           (sb-vm::%primitive sb-vm::unbind-to-here
                                              unbind-to)
                           (setf sb-vm:*current-catch-block* catch-block)
                           (funcall thunk))
                         #+unbind-in-unwind thunk
                         #+unbind-in-unwind unbind-to
                         #+(and unbind-in-unwind (not c-stack-is-control-stack))
                         (%primitive sb-c:current-nsp)
                         #+unbind-in-unwind catch-block)))
  #-unwind-to-frame-and-call-vop
  (let ((tag (gensym)))
    (sb-di:replace-frame-catch-tag frame
                                   'sb-c:debug-catch-tag
                                   tag)
    (throw tag thunk)))

#+unwind-to-frame-and-call-vop
(defun find-binding-stack-pointer (frame)
  (let ((debug-fun (sb-di:frame-debug-fun frame)))
    (if (eq (sb-di:debug-fun-kind debug-fun) :external)
        ;; XEPs do not bind anything, nothing to restore.
        ;; But they may call other code through SATISFIES
        ;; declaration, check that the interrupt is actually in the XEP.
        (and (sb-di::compiled-frame-escaped frame)
             sb-kernel::*interr-current-bsp*)
        (let* ((compiled-debug-fun (and
                                    (typep debug-fun 'sb-di::compiled-debug-fun)
                                    (sb-di::compiled-debug-fun-compiler-debug-fun debug-fun)))
               (bsp-save-offset (and compiled-debug-fun
                                     (sb-c::compiled-debug-fun-bsp-save compiled-debug-fun))))
          (when bsp-save-offset
            (sb-di::sub-access-debug-var-slot (sb-di::frame-pointer frame) bsp-save-offset))))))

(defun find-enclosing-catch-block (frame)
  ;; Walk the catch block chain looking for the first entry with an address
  ;; higher than the pointer for FRAME or a null pointer.
  (let* ((frame-pointer (sb-di::frame-pointer frame))
         (current-block (sb-di::current-catch-block-sap))
         (enclosing-block (loop for block = current-block
                                then (sap-ref-sap block
                                                  (* sb-vm:catch-block-previous-catch-slot
                                                     sb-vm:n-word-bytes))
                                when (or (zerop (sap-int block))
                                         #+stack-grows-downward-not-upward
                                         (sap> block frame-pointer)
                                         #-stack-grows-downward-not-upward
                                         (sap< block frame-pointer))
                                return block)))
    enclosing-block))

(defun find-enclosing-uwp (frame)
  ;; Walk the UWP chain looking for the first entry with an address
  ;; higher than the pointer for FRAME or a null pointer.
  (let* ((frame-pointer (sb-di::frame-pointer frame))
         (current-uwp (sb-di::current-uwp-block-sap))
         (enclosing-uwp (loop for uwp-block = current-uwp
                              then (sap-ref-sap uwp-block
                                                (* sb-vm:unwind-block-uwp-slot sb-vm:n-word-bytes))
                              when (or (zerop (sap-int uwp-block))
                                       #+stack-grows-downward-not-upward
                                       (sap> uwp-block frame-pointer)
                                       #-stack-grows-downward-not-upward
                                       (sap< uwp-block frame-pointer))
                              return uwp-block)))
    enclosing-uwp))

(!def-debug-command "RETURN" (&optional
                              (return (read-prompting-maybe
                                       "return: ")))
   (if (frame-has-debug-tag-p *current-frame*)
       (let* ((code-location (sb-di:frame-code-location *current-frame*))
              (values (multiple-value-list
                       (funcall (sb-di:preprocess-for-eval return code-location)
                                *current-frame*))))
         (unwind-to-frame-and-call *current-frame* (lambda ()
                                                     (values-list values))))
       (format *debug-io*
               "~@<can't find a tag for this frame ~
                 ~2I~_(hint: try increasing the DEBUG optimization quality ~
                 and recompiling)~:@>")))

(!def-debug-command "RESTART-FRAME" ()
  (if (frame-has-debug-tag-p *current-frame*)
      (multiple-value-bind (fname args) (frame-call *current-frame*)
        (multiple-value-bind (fun arglist ok)
            (if (and (legal-fun-name-p fname) (fboundp fname))
                (values (fdefinition fname) args t)
                (values (sb-di:debug-fun-fun (sb-di:frame-debug-fun *current-frame*))
                        (frame-args-as-list *current-frame* call-arguments-limit)
                        nil))
          (when (and fun
                     (or ok
                         (y-or-n-p "~@<No global function for the frame, but we ~
                                    do have access to a function object that we ~
                                    can try to call -- but if it is normally part ~
                                    of a closure, then this is NOT going to end well.~_~_~
                                    Try it anyways?~:@>")))
            (unwind-to-frame-and-call *current-frame*
                                      (lambda ()
                                        ;; Ensure TCO.
                                        (declare (optimize (debug 0)))
                                        (apply fun arglist))))
          (format *debug-io*
              "Can't restart ~S: no function for frame."
              *current-frame*)))
      (format *debug-io*
              "~@<Can't restart ~S: tag not found. ~
               ~2I~_(hint: try increasing the DEBUG optimization quality ~
               and recompiling)~:@>"
              *current-frame*)))

(defun frame-has-debug-tag-p (frame)
  #+unwind-to-frame-and-call-vop
  ;; XEPs do not bind anything, nothing to restore
  (find-binding-stack-pointer frame)
  #-unwind-to-frame-and-call-vop
  (find 'sb-c:debug-catch-tag (sb-di:frame-catches frame) :key #'car))

(defun frame-has-debug-vars-p (frame)
  (sb-di:debug-var-info-available
   (sb-di:code-location-debug-fun
    (sb-di:frame-code-location frame))))

;;;; debug loop command utilities

(defun read-prompting-maybe (prompt)
  (unless (listen-skip-whitespace *debug-io*)
    (princ prompt *debug-io*)
    (force-output *debug-io*))
  (read *debug-io*))

(defun read-if-available (default)
  (if (listen-skip-whitespace *debug-io*)
      (read *debug-io*)
      default))

#+(and sb-devel x86-64)
(defun show-catch-tags ()
  (declare (notinline format))
  (let ((sap (sb-di::current-catch-block-sap)))
    (loop
     (let ((tag (sap-ref-lispobj sap (ash sb-vm:catch-block-tag-slot sb-vm:word-shift)))
           (link (sap-ref-sap sap (ash sb-vm:catch-block-previous-catch-slot sb-vm:word-shift))))
       (format t "~S ~A~%" tag link)
       (setq sap link)
       (if (= (sap-int sap) 0) (return))))))

;;; Sometimes in cold-init it is not possible to call LIST-BACKTRACE
;;; because that depends on a zillion things being set up correctly.
;;; This simple version seems to always work.
#+(or x86 x86-64)
(defun ultralite-backtrace (&optional (decode-pcs t))
  ;; this misses the current frame but that's perfectly fine for its intended use
  (let ((fp (current-fp)) (list))
    (loop
     (let ((prev-fp (sap-ref-sap fp 0)))
       (cond ((and (sap> prev-fp fp)
                   (sap< prev-fp (descriptor-sap sb-vm:*control-stack-end*)))
              (push (sap-ref-sap fp sb-vm:n-word-bytes) list) ; pc
              (setq fp prev-fp))
             (t
              (return)))))
    (setq list (nreverse list))
    (if decode-pcs
        (mapcar (lambda (pc)
                  (let ((code (sb-di::code-header-from-pc pc)))
                    (if code
                        (cons code (sap- pc (code-instructions code)))
                        pc)))
                list)
        list)))

;; Yet another stack unwinder, this one via libunwind, if present.
;; Calls lose() if runtime was not built with -lunwind.
#+(and x86-64 sb-thread)
(progn
;; get_proc_name can slow down the unwind by 100x. Depending on whether you need
;; every stack trace with C symbols many times quickly, or not so many times but
;; more informatively, you'd set this off or on respectively.
(defglobal *use-libunwind-get-proc-name* nil)
(defun libunwind-backtrace (thread thread-sap context stream)
  (declare (ignorable thread thread-sap))
  (sb-alien:with-alien
      ((get-sizeof-unw-cursor (function sb-alien:int) :extern)
       (sb-unw-init (function sb-alien:int system-area-pointer system-area-pointer) :extern)
       (sb-unw-get-pc (function sb-alien:int system-area-pointer (* sb-alien:unsigned)) :extern)
       (sb-unw-get-proc-name (function sb-alien:int system-area-pointer system-area-pointer
                                       sb-alien:int
                                       (* sb-alien:unsigned))
                             :extern)
       (sb-unw-step (function sb-alien:int system-area-pointer) :extern)
       (word sb-alien:unsigned))
    (let* ((cursor-size (the (mod 2048) (sb-alien:alien-funcall get-sizeof-unw-cursor)))
           (cursor (make-array cursor-size :element-type '(unsigned-byte 8)))
           (string (make-array 127 :element-type 'base-char))
           (n 0)
           code)
      (declare (dynamic-extent cursor string))
      (aver (zerop
             (sb-alien:alien-funcall sb-unw-init
                                     (vector-sap cursor) (sb-alien:alien-sap context))))
      (loop
       (aver (zerop
              (sb-alien:alien-funcall sb-unw-get-pc (vector-sap cursor) (sb-alien:addr word))))
       (format stream "~D: #x~12,'0X " n word)
       (incf n)
       (cond ((setq code (sb-di::code-header-from-pc word))
              (let ((pc (sap- (int-sap word) (code-instructions code))))
                (format stream "~A"
                        (sb-di:debug-fun-name (sb-di::debug-fun-from-pc code pc nil)))))
             ((not *use-libunwind-get-proc-name*))
             ((= (sb-alien:alien-funcall sb-unw-get-proc-name (vector-sap cursor)
                                         (vector-sap string) (1+ (length string))
                                         (sb-alien:addr word)) 0)
              (format stream "~s + #x~x"
                      (sb-alien::%naturalize-c-string (vector-sap string)) word))
             (t
              (format stream "???")))
       (terpri stream)
       ;; nonzero = success, 0 = failure / end-of-walk
       (when (= (sb-alien:alien-funcall sb-unw-step (vector-sap cursor)) 0)
         (return))))))

(defun vmthread-state (vmthread)
  ;; Refer to the definition of thread_state_word in src/runtime/genesis/thread.h
  ;; and 'enum threadstate' in src/runtime/thread.h
  (ecase (sap-ref-8 vmthread (+ (ash sb-vm:thread-state-word-slot sb-vm:word-shift) 2))
    (1 :running)
    (2 :stopped)
    (3 :dead)))

(export 'backtrace-all-threads)
(defun backtrace-all-threads (&aux (stream (make-string-output-stream))
                                   results)
  (without-gcing
      (when (sb-kernel::try-acquire-gc-lock
             (sb-kernel::gc-stop-the-world))
        ;; The GC's thread list is exactly what we want to traverse here
        ;; since that is the set of threads responding to the stop signal.
        (do ((vmthread (sb-alien:extern-alien "all_threads" system-area-pointer)
                       (sap-ref-sap vmthread (ash sb-vm::thread-next-slot sb-vm:word-shift))))
            ((zerop (sap-int vmthread)))
          (let ((tls-size (sb-alien:extern-alien "dynamic_values_bytes" (sb-alien:unsigned 32)))
                (thread-instance
                 (sap-ref-lispobj vmthread
                                  (ash sb-vm::thread-lisp-thread-slot sb-vm:word-shift))))
            (cond ((eq thread-instance sb-thread:*current-thread*)
                   (print-backtrace :stream stream))
                  ((eq (vmthread-state vmthread) :stopped)
                   (format stream "Backtrace for: ~S~%" thread-instance)
                   (let* ((ici (sb-sys:sap-ref-lispobj
                                vmthread (symbol-tls-index '*free-interrupt-context-index*)))
                          (context-sap
                           (sap-ref-sap vmthread
                                        (+ tls-size (ash (1- ici) sb-vm:word-shift))))
                          (context (sb-alien:sap-alien context-sap (* os-context-t))))
                     (libunwind-backtrace thread-instance vmthread context stream)))))
          (push (get-output-stream-string stream) results))
        (sb-kernel::gc-start-the-world)))
  results)
) ; end PROGN
