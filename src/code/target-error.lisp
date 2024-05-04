;;;; that part of the condition system which can or should come early
;;;; (mostly macro-related)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(declaim (global *type-system-initialized*))

(defun decode-internal-error-args (sap trap-number &optional error-number)
  (let ((error-number (cond (error-number)
                            ((>= trap-number sb-vm:error-trap)
                             (prog1
                                 (- trap-number sb-vm:error-trap)
                               (setf trap-number sb-vm:error-trap)))
                            (t
                             (prog1 (sap-ref-8 sap 0)
                               (setf sap (sap+ sap 1)))))))
    (let ((length (error-length error-number)))
      (declare (type (unsigned-byte 8) length))
      (values error-number
              (loop with index = 0
                    repeat length
                    collect (sb-c:sap-read-var-integerf sap index))
              trap-number))))

(defun muffle-warning-p (warning)
  (declare (special *muffled-warnings*))
  (typep warning *muffled-warnings*))

;;; Each cluster is an alist of the form
;;;
;;;  ((TYPE-TEST1 . HANDLER1) (TYPE-TEST2 . HANDLER2) ...)
;;;
;;; where TYPE-TESTN are functions of one argument which test a given
;;; condition instance for the type required by the corresponding
;;; HANDLERN. HANDLERN are function designators.
;;;
;;; Newly established handlers are added at the beginning of the
;;; list. Elements to the left of the alist take precedence over
;;; elements to the right.
;;;
;;; Lists to which *HANDLER-CLUSTERS* is bound generally have dynamic
;;; extent.

(defmethod print-object ((restart restart) stream)
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t)
        (prin1 (restart-name restart) stream))
      (restart-report restart stream)))

(setf (documentation 'restart-name 'function)
      "Return the name of the given restart object.")

(defun restart-report (restart stream)
  (if (restart-report-function restart)
      (funcall (truly-the function (restart-report-function restart))
               stream)
      (prin1 (or (restart-name restart)
                 restart)
             stream)))

(defvar *restart-test-stack* nil)

;; Call FUNCTION with all restarts in the current dynamic environment,
;; 1) that are associated to CONDITION (when CONDITION is NIL, all
;;    restarts are processed)
;; 2) and for which the restart test returns non-NIL for CONDITION.
;; When CALL-TEST-P is non-NIL, all restarts are processed.
(defun map-restarts (function &optional condition (call-test-p t))
  (declare (function function))
  (let ((stack *restart-test-stack*))
    (dolist (restart-cluster *restart-clusters*)
      (dolist (restart restart-cluster)
        (when (and (or (not condition)
                       (null (restart-associated-conditions restart))
                       (memq condition (restart-associated-conditions restart)))
                   ;; A call to COMPUTE-RESTARTS -- from an error,
                   ;; from user code, whatever -- inside the test
                   ;; function would cause infinite recursion here, so
                   ;; we disable each restart using
                   ;; *restart-test-stack* for the duration of the
                   ;; test call.
                   (not (memq restart stack))
                   (or (not call-test-p)
                       (let ((*restart-test-stack* (cons restart stack)))
                         (declare (dynamic-extent *restart-test-stack*))
                         (funcall (restart-test-function restart) condition))))
          (funcall function restart))))))

(defun compute-restarts (&optional condition)
  "Return a list of all the currently active restarts ordered from most recently
established to less recently established. If CONDITION is specified, then only
restarts associated with CONDITION (or with no condition) will be returned."
  (collect ((result))
    (map-restarts (lambda (restart) (result restart)) condition)
    (result)))

(defun %find-restart (identifier condition &optional (call-test-p t))
  (flet ((eq-restart-p (restart)
           (when (eq identifier restart)
             (return-from %find-restart restart)))
         (named-restart-p (restart)
           (when (eq identifier (restart-name restart))
             (return-from %find-restart restart))))
    ;; KLUDGE: can the compiler infer this dx automatically?
    (declare (dynamic-extent #'eq-restart-p #'named-restart-p))
    (if (typep identifier 'restart)
        ;; The code under #+previous-... below breaks the abstraction
        ;; introduced by MAP-RESTARTS, but is about twice as
        ;; fast as #+equivalent-... . Also, it is a common case due to
        ;;
        ;;    (INVOKE-RESTART RESTART)
        ;; -> (FIND-RESTART-OR-CONTROL-ERROR RESTART)
        ;; -> (FIND-RESTART RESTART)
        ;;
        ;; However, both #+previous-... and #+equivalent-... may be
        ;; wrong altogether because of
        ;; https://bugs.launchpad.net/sbcl/+bug/774410:
        ;; The behavior expected in that report can be achieved by the
        ;; following line (which is, of course, the slowest of all
        ;; possibilities):
        (map-restarts #'eq-restart-p condition call-test-p)

        #+equivalent-to-previous-sbcl-behavior--faster-but-see-bug-774410
        (map-restarts #'eq-restart-p nil nil)

        #+previous-behavior--fastest-but-see-bug-774410
        (and (find-if (lambda (cluster) (find identifier cluster)) *restart-clusters*)
             identifier)

        (map-restarts #'named-restart-p condition call-test-p))))

(defun find-restart (identifier &optional condition)
  "Return the first restart identified by IDENTIFIER. If IDENTIFIER is a symbol,
then the innermost applicable restart with that name is returned. If IDENTIFIER
is a restart, it is returned if it is currently active. Otherwise NIL is
returned. If CONDITION is specified and not NIL, then only restarts associated
with that condition (or with no condition) will be returned."
  ;; Calls MAP-RESTARTS such that restart test functions are
  ;; respected.
  (%find-restart identifier condition))

;;; helper for the various functions which are ANSI-spec'ed to do
;;; something with a restart or signal CONTROL-ERROR if there is none
(define-error-wrapper find-restart-or-control-error (identifier &optional condition (call-test-p t))
  (or (%find-restart identifier condition call-test-p)
      (error 'simple-control-error
             :format-control "No restart ~S is active~@[ for ~S~]."
             :format-arguments (list identifier condition))))

(defun invoke-restart (restart &rest values)
  "Calls the function associated with the given restart, passing any given
   arguments. If the argument restart is not a restart or a currently active
   non-nil restart name, then a CONTROL-ERROR is signalled."
  (/show "entering INVOKE-RESTART" restart)
  ;; The following code calls MAP-RESTARTS (through
  ;; FIND-RESTART-OR-CONTROL-ERROR -> %FIND-RESTART) such that restart
  ;; test functions are respected when RESTART is a symbol, but not
  ;; when RESTART is a RESTART instance.
  ;;
  ;; Without disabling test functions for the RESTART instance case,
  ;; the following problem would arise:
  ;;
  ;;  (restart-case
  ;;      (handler-bind
  ;;          ((some-condition (lambda (c)
  ;;                             (invoke-restart (find-restart 'foo c)) ; a)
  ;;                             (invoke-restart 'foo)                  ; b)
  ;;                             )))
  ;;        (signal 'some-condition))
  ;;    (foo ()
  ;;     :test (lambda (c) (typep c 'some-condition))))
  ;;
  ;; In case a), INVOKE-RESTART receives the RESTART instance, but
  ;; cannot supply the condition instance needed by the test. In case
  ;; b) INVOKE-RESTART calls FIND-RESTART, but again cannot supply the
  ;; condition instance. As a result, the restart would be impossible
  ;; the invoke.
  (let ((real-restart (find-restart-or-control-error
                       restart nil (symbolp restart))))
    (apply (restart-function real-restart) values)))

(defun interactive-restart-arguments (real-restart)
  (let ((interactive-function (restart-interactive-function real-restart)))
    (if interactive-function
        (funcall interactive-function)
        '())))

(defun invoke-restart-interactively (restart)
  "Calls the function associated with the given restart, prompting for any
   necessary arguments. If the argument restart is not a restart or a
   currently active non-NIL restart name, then a CONTROL-ERROR is signalled."
  ;; For an explanation of the call to FIND-RESTART-OR-CONTROL-ERROR,
  ;; see comment in INVOKE-RESTART.
  (let* ((real-restart (find-restart-or-control-error
                        restart nil (symbolp restart)))
         (args (interactive-restart-arguments real-restart)))
    (apply (restart-function real-restart) args)))

;;; To reduce expansion size of RESTART-CASE
(defun with-simple-condition-restarts (function cerror-arg datum &rest arguments)
  (let ((sb-debug:*stack-top-hint* (or sb-debug:*stack-top-hint*
                                       'with-simple-condition-restarts))
        (condition (apply #'coerce-to-condition datum
                          (case function
                            (warn 'simple-warning)
                            (signal 'simple-condition)
                            (t 'simple-error))
                          function
                          arguments)))
    (with-condition-restarts condition (car *restart-clusters*)
      (if (eq function 'cerror)
          (cerror cerror-arg condition)
          (funcall function condition)))))


;;;; Conditions.

(!defstruct-with-alternate-metaclass condition
  :slot-names (assigned-slots)
  :constructor nil
  :superclass-name t
  :metaclass-name condition-classoid
  :metaclass-constructor make-condition-classoid
  :dd-type structure)

;;; Needed for !CALL-A-METHOD to pick out CONDITIONs
(defun !condition-p (x) (typep x 'condition))

(defstruct (condition-slot (:copier nil))
  (name (missing-arg) :type symbol)
  ;; list of all applicable initargs
  (initargs (missing-arg) :type list)
  ;; names of reader and writer functions
  (readers (missing-arg) :type list)
  (writers (missing-arg) :type list)
  ;; true if :INITFORM was specified
  (initform-p (missing-arg) :type (member t nil))
  ;; the initform if :INITFORM was specified, otherwise NIL
  (initform nil :type t)
  ;; if this is a function, call it with no args to get the initform value
  (initfunction (missing-arg) :type t)
  ;; allocation of this slot, or NIL until defaulted
  (allocation nil :type (member :instance :class nil))
  ;; If ALLOCATION is :CLASS, this is a cons whose car holds the value
  (cell nil :type (or cons null))
  ;; slot documentation
  (documentation nil :type (or string null)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((condition-class (find-classoid 'condition)))
    (setf (condition-classoid-cpl condition-class)
          (list condition-class))))

(setf (condition-classoid-report (find-classoid 'condition))
      (lambda (cond stream)
        (format stream "Condition ~/sb-impl:print-type-specifier/ was signalled."
                (type-of cond))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun find-condition-layout (name parent-types)
  (let* ((cpl (remove-duplicates
               (reverse
                (reduce #'append
                        (mapcar (lambda (x)
                                  (condition-classoid-cpl
                                   (find-classoid x)))
                                parent-types)))))
         (cond-layout (info :type :compiler-layout 'condition))
         (olayout (info :type :compiler-layout name))
         ;; FIXME: Does this do the right thing in case of multiple
         ;; inheritance? A quick look at DEFINE-CONDITION didn't make
         ;; it obvious what ANSI intends to be done in the case of
         ;; multiple inheritance, so it's not actually clear what the
         ;; right thing is..
         (new-inherits
          (order-layout-inherits (concatenate 'simple-vector
                                              (layout-inherits cond-layout)
                                              (mapcar #'classoid-layout cpl)))))
    (if (and olayout
             (not (mismatch (layout-inherits olayout) new-inherits)))
        olayout
        ;; All condition classoid layouts carry the same LAYOUT-INFO - the defstruct
        ;; description for CONDITION - which is a representation of the primitive object
        ;; and not the lisp-level object.
        (make-layout (hash-layout-name name)
                     (make-undefined-classoid name)
                     :info (layout-info cond-layout)
                     :flags (logior +condition-layout-flag+ +strictly-boxed-flag+)
                     :inherits new-inherits
                     :depthoid -1
                     :length (layout-length cond-layout)))))

) ; EVAL-WHEN


;;;; slots of CONDITION objects

(defun find-slot-default (condition classoid slot &optional boundp)
  (multiple-value-bind (value found) (find-slot-default-initarg classoid slot)
    ;; When CLASSOID or a superclass has a default initarg for SLOT, use
    ;; that.
    (cond (found
           value)
          ;; Otherwise use the initform of SLOT, if there is one.
          ((condition-slot-initform-p slot)
           (let ((initfun (condition-slot-initfunction slot)))
             (aver (functionp initfun))
             (funcall initfun)))
          ;; if we're computing SLOT-BOUNDP, return an unbound marker
          (boundp sb-pcl:+slot-unbound+)
          ;; if we're computing SLOT-VALUE, call SLOT-UNBOUND
          (t
           (let ((class (classoid-pcl-class classoid))
                 (name (condition-slot-name slot)))
             (values (slot-unbound class condition name)))))))

(defun find-slot-default-initarg (classoid slot)
  (let ((initargs (condition-slot-initargs slot))
        (cpl (condition-classoid-cpl classoid)))
    (dolist (classoid cpl)
      (let ((direct-default-initargs
              (condition-classoid-direct-default-initargs classoid)))
        (dolist (initarg initargs)
          (let ((initfunction (third (assoc initarg direct-default-initargs))))
            (when initfunction
              (return-from find-slot-default-initarg
                (values (funcall initfunction) t)))))))
    (values nil nil)))

(defun find-condition-class-slot (condition-class slot-name)
  (dolist (sclass
           (condition-classoid-cpl condition-class)
           (error "There is no slot named ~S in ~S."
                  slot-name condition-class))
    (dolist (slot (condition-classoid-slots sclass))
      (when (eq (condition-slot-name slot) slot-name)
        (return-from find-condition-class-slot slot)))))

;;;; MAKE-CONDITION

;;; Pre-scan INITARGS to see whether any are stack-allocated.
;;; If not, then life is easy. If any are, then depending on whether the
;;; condition is a TYPE-ERROR, call TYPE-OF on the bad datum, so that
;;; if the condition outlives the extent of the object, and someone tries
;;; to print the condition, we don't crash.
;;; Putting a placeholder in for the datum would work, but seems a bit evil,
;;; since the user might actually want to know what it was. And we shouldn't
;;; assume that the object would definitely escape its dynamic-extent.

(defun allocate-condition (designator &rest initargs)
  (when (oddp (length initargs))
    (error 'simple-error
           :format-control "odd-length initializer list: ~S."
           ;; Passing the initargs to LIST avoids consing them into
           ;; a list except when this error is signaled.
           :format-arguments (list (apply #'list initargs))))
  ;; I am going to assume that people are not somehow getting to here
  ;; with a CLASSOID, which is not strictly legal as a designator,
  ;; but which is accepted because it is actually the desired thing.
  ;; It doesn't seem worth sweating over that detail, and in any event
  ;; we could say that it's a supported extension.
  (let ((classoid (named-let lookup ((designator designator))
                    (typecase designator
                      (symbol (find-classoid designator nil))
                      (class (lookup (class-name designator)))
                      (t designator)))))
    (unless (condition-classoid-p classoid)
      (error 'simple-type-error
             :datum designator
             :expected-type 'sb-pcl::condition-class
             :format-control "~S does not designate a condition class."
             :format-arguments (list designator)))
    (flet ((stream-err-p (layout)
             (let ((stream-err-layout (load-time-value (find-layout 'stream-error))))
               (or (eq layout stream-err-layout)
                   (find stream-err-layout (layout-inherits layout)))))
           (type-err-p (layout)
             (let ((type-err-layout (load-time-value (find-layout 'type-error))))
               (or (eq layout type-err-layout)
                   (find type-err-layout (layout-inherits layout)))))
           ;; avoid full calls to STACK-ALLOCATED-P here
           (stackp (x)
             (let ((addr (get-lisp-obj-address x)))
               (and (sb-vm:is-lisp-pointer addr)
                    (<= (get-lisp-obj-address sb-vm:*control-stack-start*) addr)
                    (< addr (get-lisp-obj-address sb-vm:*control-stack-end*))))))
      (let* ((any-dx
               (loop for arg-index from 1 below (length initargs) by 2
                       thereis (stackp (fast-&rest-nth arg-index initargs))))
             (layout (classoid-layout classoid))
             (extra (if (and any-dx (type-err-p layout)) 2 0)) ; space for secret initarg
             (instance (%new-instance layout
                                      (+ sb-vm:instance-data-start
                                         1 ; ASSIGNED-SLOTS
                                         (length initargs)
                                         extra)))
             (data-index (1+ sb-vm:instance-data-start))
             (arg-index 0)
             (have-type-error-datum)
             (type-error-datum))
        (setf (condition-assigned-slots instance) nil)
        (macrolet ((store-pair (key val)
                     `(progn (%instance-set instance data-index ,key)
                             (%instance-set instance (1+ data-index) ,val))))
          (cond ((not any-dx)
                 ;; uncomplicated way
                 (loop (when (>= arg-index (length initargs)) (return))
                       (store-pair (fast-&rest-nth arg-index initargs)
                                   (fast-&rest-nth (1+ arg-index) initargs))
                       (incf data-index 2)
                       (incf arg-index 2)))
                (t
                 (loop (when (>= arg-index (length initargs)) (return))
                       (let ((key (fast-&rest-nth arg-index initargs))
                             (val (fast-&rest-nth (1+ arg-index) initargs)))
                         (when (and (eq key :datum)
                                    (not have-type-error-datum)
                                    (type-err-p layout))
                           (setq type-error-datum val
                                 have-type-error-datum t))
                         (if (and (eq key :stream) (stream-err-p layout) (stackp val))
                             (store-pair key (sb-impl::make-stub-stream val))
                             (store-pair key val)))
                       (incf data-index 2)
                       (incf arg-index 2))
                 (when (and have-type-error-datum (/= extra 0))
                   ;; We can get into serious trouble here if the
                   ;; datum is already stack garbage!
                   (let ((actual-type (type-of type-error-datum)))
                     (store-pair 'dx-object-type actual-type))))))
        (values instance classoid)))))

;;; Access the type of type-error-datum if the datum can't be accessed.
;;; Testing the stack pointer when rendering the condition is a heuristic
;;; that might work, but more likely, the erring frame has been exited
;;; and then the stack pointer changed again to make it seems like the
;;; object pointer is valid. I'm not sure what to do, but we can leave
;;; that decision for later.
(defun type-error-datum-stored-type (condition)
  (do ((i (- (%instance-length condition) 2) (- i 2)))
      ((<= i (1+ sb-vm:instance-data-start))
       (make-unbound-marker))
    (when (eq (%instance-ref condition i) 'dx-object-type)
      (return (%instance-ref condition (1+ i))))))

(defun make-condition (type &rest initargs)
  "Make an instance of a condition object using the specified initargs."
  ;; Note: While ANSI specifies no exceptional situations in this function,
  ;; ALLOCATE-CONDITION will signal a type error if TYPE does not designate
  ;; a condition class. This seems fair enough.
  (declare (explicit-check))
  ;; FIXME: the compiler should have a way to make GETF operate on a &MORE arg
  ;; so that the initargs are never listified.
  (declare (dynamic-extent initargs))
  (multiple-value-bind (condition classoid)
      (apply #'allocate-condition type initargs)

    ;; Set any class slots with initargs present in this call.
    (dolist (cslot (condition-classoid-class-slots classoid))
      (loop for (key value) on initargs by #'cddr
            when (memq key (condition-slot-initargs cslot))
            do (setf (car (condition-slot-cell cslot)) value)
               (return)
            finally
            (multiple-value-bind (value found)
                (find-slot-default-initarg classoid cslot)
              (when found
                (setf (car (condition-slot-cell cslot)) value)))))

    ;; Default any slots with non-constant defaults now.
    (dolist (hslot (condition-classoid-hairy-slots classoid))
      (when (dolist (initarg (condition-slot-initargs hslot) t)
              (unless (unbound-marker-p (getf initargs initarg sb-pcl:+slot-unbound+))
                (return nil)))
        (push (cons (condition-slot-name hslot)
                    (find-slot-default condition classoid hslot))
              (condition-assigned-slots condition))))

    condition))

;;;; DEFINE-CONDITION

(define-load-time-global *define-condition-hooks* nil)

(defun %set-condition-report (name report)
  (setf (condition-classoid-report (find-classoid name))
        report))

;;; Early definitions of slot accessor creators.
;;;
;;; Slot accessors must be generic functions, but ANSI does not seem
;;; to specify any of them, and we cannot support it before end of
;;; warm init. So we use ordinary functions inside SBCL, and switch to
;;; GFs only at the end of building.
(declaim (notinline install-condition-slot-reader
                    install-condition-slot-writer))
(defun install-condition-slot-reader (name condition slot-name)
  (declare (ignore condition))
  (setf (fdefinition name)
        (set-closure-name
         (lambda (condition) (condition-slot-value condition slot-name))
         t
         `(condition-slot-reader ,name))))
(defun install-condition-slot-writer (name condition slot-name)
  (declare (ignore condition))
  (setf (fdefinition name)
        (set-closure-name
         (lambda (new-value condition)
           (set-condition-slot-value condition new-value slot-name))
         t
         `(condition-slot-writer ,name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %%compiler-define-condition (name direct-supers layout readers writers)
  (declare (notinline find-classoid))
  (preinform-compiler-about-class-type name nil)
  (preinform-compiler-about-accessors 'condition readers writers)
  (multiple-value-bind (class old-layout)
      (insured-find-classoid name
                             #'condition-classoid-p
                             #'make-condition-classoid)
    (setf (layout-classoid layout) class)
    (setf (classoid-direct-superclasses class)
          (mapcar #'find-classoid direct-supers))
    (cond ((not old-layout)
           (register-layout layout))
          ((not *type-system-initialized*)
           (setf (layout-classoid old-layout) class)
           (setq layout old-layout)
           (unless (eq (classoid-layout class) layout)
             (register-layout layout)))
          ((warn-if-altered-layout  "current"
                                    old-layout
                                    "new"
                                    (layout-length layout)
                                    (layout-inherits layout)
                                    (layout-depthoid layout)
                                    (layout-bitmap layout))
           (register-layout layout :invalidate t))
          ((not (classoid-layout class))
           (register-layout layout)))

    (setf (find-classoid name) class)

    ;; Initialize CPL slot.
    (setf (condition-classoid-cpl class)
          (remove-if-not #'condition-classoid-p
                         (std-compute-class-precedence-list class)))))

(defun %compiler-define-condition (name direct-supers layout readers writers)
  (call-with-defining-class
   'condition name
   (lambda ()
     (%%compiler-define-condition name direct-supers layout readers writers))))
) ; EVAL-WHEN

(defun %define-condition (name parent-types layout slots
                          direct-default-initargs all-readers all-writers
                          source-location &optional documentation)
  (call-with-defining-class
   'condition name
   (lambda ()
     (%%compiler-define-condition name parent-types layout all-readers all-writers)
     (let ((classoid (find-classoid name)))
       (when source-location
         (setf (classoid-source-location classoid) source-location))
       (setf (condition-classoid-slots classoid) slots
             (condition-classoid-direct-default-initargs classoid) direct-default-initargs
             (documentation name 'type) documentation)

       (dolist (slot slots)
         ;; Set up reader and writer functions.
         (let ((slot-name (condition-slot-name slot)))
           (dolist (reader (condition-slot-readers slot))
             (install-condition-slot-reader reader name slot-name))
           (dolist (writer (condition-slot-writers slot))
             (install-condition-slot-writer writer name slot-name))))

       ;; Compute effective slots and set up the class and hairy slots
       ;; (subsets of the effective slots.)
       (setf (condition-classoid-class-slots classoid) '()
             (condition-classoid-hairy-slots classoid) '())
       (let ((eslots (compute-effective-slots classoid))
             (e-def-initargs
              (reduce #'append
                      (mapcar #'condition-classoid-direct-default-initargs
                              (condition-classoid-cpl classoid)))))
         (dolist (slot eslots)
           (ecase (condition-slot-allocation slot)
             (:class
              (unless (condition-slot-cell slot)
                (setf (condition-slot-cell slot)
                      (list (if (condition-slot-initform-p slot)
                                (let ((initfun (condition-slot-initfunction slot)))
                                  (aver (functionp initfun))
                                  (funcall initfun))
                                sb-pcl:+slot-unbound+))))
              (push slot (condition-classoid-class-slots classoid)))
             ((:instance nil)
              (setf (condition-slot-allocation slot) :instance)
              ;; FIXME: isn't this "always hairy"?
              (when (or (functionp (condition-slot-initfunction slot))
                        (dolist (initarg (condition-slot-initargs slot) nil)
                          (when (functionp (third (assoc initarg e-def-initargs)))
                            (return t))))
                (push slot (condition-classoid-hairy-slots classoid)))))))
       (when *type-system-initialized*
         (dolist (fun *define-condition-hooks*)
           (funcall fun classoid))))))
  name)

(defmacro define-condition (name (&rest parent-types) (&rest slot-specs)
                                 &body options)
  "DEFINE-CONDITION Name (Parent-Type*) (Slot-Spec*) Option*
   Define NAME as a condition type. This new type inherits slots and its
   report function from the specified PARENT-TYPEs. A slot spec is a list of:
     (slot-name :reader <rname> :initarg <iname> {Option Value}*

   The DEFINE-CLASS slot options :ALLOCATION, :INITFORM, [slot] :DOCUMENTATION
   and :TYPE and the overall options :DEFAULT-INITARGS and
   [type] :DOCUMENTATION are also allowed.

   The :REPORT option is peculiar to DEFINE-CONDITION. Its argument is either
   a string or a two-argument lambda or function name. If a function, the
   function is called with the condition and stream to report the condition.
   If a string, the string is printed.

   Condition types are classes, but (as allowed by ANSI and not as described in
   CLtL2) are neither STANDARD-OBJECTs nor STRUCTURE-OBJECTs. WITH-SLOTS and
   SLOT-VALUE may not be used on condition objects."
  (check-designator name 'define-condition)
  (let* ((parent-types (or parent-types '(condition)))
         (layout (find-condition-layout name parent-types))
         (documentation nil)
         (report nil)
         (direct-default-initargs ()))
    (collect ((slots)
              (all-readers nil append)
              (all-writers nil append))
      (dolist (spec slot-specs)
        (with-current-source-form (spec)
          (when (keywordp spec)
            (warn "Keyword slot name indicates probable syntax error:~%  ~S"
                  spec))
          (let* ((spec (if (consp spec) spec (list spec)))
                 (slot-name (first spec))
                 (allocation :instance)
                 (initform-p nil)
                 documentation
                 initform)
            (collect ((initargs)
                      (readers)
                      (writers))
              (do ((options (rest spec) (cddr options)))
                  ((null options))
                (unless (and (consp options) (consp (cdr options)))
                  (error "malformed condition slot spec:~%  ~S." spec))
                (let ((arg (second options)))
                  (case (first options)
                    (:reader (readers arg))
                    (:writer (writers arg))
                    (:accessor
                     (readers arg)
                     (writers `(setf ,arg)))
                    (:initform
                     (when initform-p
                       (error "more than one :INITFORM in ~S" spec))
                     (setq initform-p t)
                     (setq initform arg))
                    (:initarg (initargs arg))
                    (:allocation
                     (setq allocation arg))
                    (:documentation
                     (when documentation
                       (error "more than one :DOCUMENTATION in ~S" spec))
                     (unless (stringp arg)
                       (error "slot :DOCUMENTATION argument is not a string: ~S"
                              arg))
                     (setq documentation arg))
                    (:type
                     (check-slot-type-specifier
                      arg slot-name (cons 'define-condition name)))
                    (t
                     (error "unknown slot option:~%  ~S" (first options))))))

              (all-readers (readers))
              (all-writers (writers))
              (slots `(make-condition-slot
                       :name ',slot-name
                       :initargs ',(initargs)
                       :readers ',(readers)
                       :writers ',(writers)
                       :initform-p ',initform-p
                       :documentation ',documentation
                       :initform ,(when initform-p `',initform)
                       :initfunction ,(when initform-p
                                        `#'(lambda () ,initform))
                       :allocation ',allocation))))))

      (dolist (option options)
        (unless (consp option)
          (error "bad option:~%  ~S" option))
        (case (first option)
          (:documentation (setq documentation (second option)))
          (:report
           (let ((arg (second option)))
             (setq report
                   `#'(named-lambda (condition-report ,name) (condition stream)
                        (declare (type condition condition)
                                 (type stream stream))
                        ,@(if (stringp arg)
                              `((declare (ignore condition))
                                (write-string ,arg stream))
                              `((funcall #',arg condition stream)))))))
          (:default-initargs
           (doplist (initarg initform) (rest option)
             (push ``(,',initarg ,',initform ,#'(lambda () ,initform))
                   direct-default-initargs)))
          (t
           (error "unknown option: ~S" (first option)))))

      ;; Maybe kill docstring, but only under the cross-compiler.
      #+(and (not sb-doc) sb-xc-host) (setq documentation nil)
      `(progn
         ,@(when *top-level-form-p*
             ;; Avoid dumping uninitialized layouts, for sb-fasl::dump-layout
             `((eval-when (:compile-toplevel)
                 (%compiler-define-condition ',name ',parent-types ,layout
                                             ',(all-readers) ',(all-writers)))))
         (%define-condition ',name
                            ',parent-types
                            ,(if *top-level-form-p*
                                 layout
                                 `(find-condition-layout ',name ',parent-types))
                            (list ,@(slots))
                            (list ,@direct-default-initargs)
                            ',(all-readers)
                            ',(all-writers)
                            (sb-c:source-location)
                            ,@(and documentation
                                   `(,documentation)))
         ;; This needs to be after %DEFINE-CONDITION in case :REPORT
         ;; is a lambda referring to condition slot accessors:
         ;; they're not proclaimed as functions before it has run if
         ;; we're under EVAL or loaded as source.
         (%set-condition-report ',name ,report)
         ',name))))

;;; Compute the effective slots of CLASS, copying inherited slots and
;;; destructively modifying direct slots.
;;;
;;; FIXME: It'd be nice to explain why it's OK to destructively modify
;;; direct slots. Presumably it follows from the semantics of
;;; inheritance and redefinition of conditions, but finding the cite
;;; and documenting it here would be good. (Or, if this is not in fact
;;; ANSI-compliant, fixing it would also be good.:-)
(defun compute-effective-slots (class)
  (collect ((res (copy-list (condition-classoid-slots class))))
    (dolist (sclass (cdr (condition-classoid-cpl class)))
      (dolist (sslot (condition-classoid-slots sclass))
        (let ((found (find (condition-slot-name sslot) (res)
                           :key #'condition-slot-name)))
          (cond (found
                 (setf (condition-slot-initargs found)
                       (union (condition-slot-initargs found)
                              (condition-slot-initargs sslot)))
                 (unless (condition-slot-initform-p found)
                   (setf (condition-slot-initform-p found)
                         (condition-slot-initform-p sslot))
                   (setf (condition-slot-initform found)
                         (condition-slot-initform sslot))
                   (setf (condition-slot-initfunction found)
                         (condition-slot-initfunction sslot)))
                 (unless (condition-slot-allocation found)
                   (setf (condition-slot-allocation found)
                         (condition-slot-allocation sslot))))
                (t
                 (res (copy-structure sslot)))))))
    (res)))



;;;; various CONDITIONs specified by ANSI

(define-condition serious-condition (condition) ())

(define-condition error (serious-condition) ())

(define-condition warning (condition) ())
(define-condition style-warning (warning) ())

(defun simple-condition-printer (condition stream)
  (let ((control (simple-condition-format-control condition)))
    (if control
        (apply #'format stream
               control
               (simple-condition-format-arguments condition))
        (error "No format-control for ~S" condition))))

(define-condition simple-condition ()
  ((format-control :reader simple-condition-format-control
                   :initarg :format-control
                   :initform nil
                   :type format-control)
   (format-arguments :reader simple-condition-format-arguments
                     :initarg :format-arguments
                     :initform nil
                     :type list))
  (:report simple-condition-printer))

(define-condition simple-warning (simple-condition warning) ())

(define-condition simple-error (simple-condition error) ())

(define-condition storage-condition (serious-condition) ())

(defun decode-type-error-context (context type)
  (typecase context
    (cons
     (case (car context)
       (struct-context
        (format nil "when setting slot ~s of structure ~s"
                (cddr context) (cadr context)))
       (t context)))
    ((eql sb-c::aref-context)
     (let (*print-circle*)
       (format nil "when setting an element of (ARRAY ~s)"
               type)))
    ((eql sb-c::ftype-context)
     "from the function type declaration.")
    ((member map)
     (format nil "for the result type of ~a." context))
    ((and symbol
          (not null))
     (format nil "when binding ~s" context))
    (t
     context)))

(define-condition type-error (error)
  ((datum :reader type-error-datum :initarg :datum)
   (expected-type :reader type-error-expected-type :initarg :expected-type)
   (context :initform nil :reader type-error-context :initarg :context))
  (:report report-general-type-error))
(defun report-general-type-error (condition stream)
  (let ((type (type-error-expected-type condition))
        (context (type-error-context condition)))
    (if (eq context :multiple-values)
        (format stream  "~@<The values ~
                         ~@:_~2@T~S ~
                         ~@:_are not of type ~
                         ~@:_~2@T~/sb-impl:print-type-specifier/~:@>"
                   (type-error-datum condition)
                   type)
        (format stream  "~@<The value ~
                         ~@:_~2@T~S ~
                         ~@:_is not of type ~
                         ~@:_~2@T~/sb-impl:print-type-specifier/~@[ ~
                         ~@:_~a~]~:@>"
                (type-error-datum condition)
                type
                (decode-type-error-context (type-error-context condition)
                                           type)))))

;;; not specified by ANSI, but too useful not to have around.
(define-condition simple-style-warning (simple-condition style-warning) ())
(define-condition simple-type-error (simple-condition type-error) ())

(define-condition program-error (error) ())
(define-condition parse-error   (error) ())
(define-condition control-error (error) ())
(define-condition stream-error  (error)
  ((stream :reader stream-error-stream :initarg :stream)))

(define-condition end-of-file (stream-error) ()
  (:report
   (lambda (condition stream)
     (format stream
             "end of file on ~S"
             (stream-error-stream condition)))))

(define-condition closed-stream-error (stream-error) ()
  (:report
   (lambda (condition stream)
     (format stream "~S is closed" (stream-error-stream condition)))))

(define-condition closed-saved-stream-error (closed-stream-error) ()
  (:report
   (lambda (condition stream)
     (format stream "~S was closed by SB-EXT:SAVE-LISP-AND-DIE" (stream-error-stream condition)))))

(define-condition file-error (error)
  ((pathname :reader file-error-pathname :initarg :pathname))
  (:report
   (lambda (condition stream)
     (format stream "error on file ~S" (file-error-pathname condition)))))

(define-condition package-error (error)
  ((package :reader package-error-package :initarg :package)))

(define-condition cell-error (error)
  ((name :reader cell-error-name :initarg :name)))

(define-condition values-list-argument-error (type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Attempt to use ~S on a dotted list or non-list: ~
                     ~2I~_~S~:>"
             'values-list (type-error-datum condition)))))

(define-condition unbound-variable (cell-error)
  ((not-yet-loaded :initform nil :reader not-yet-loaded :initarg :not-yet-loaded))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<The variable ~S is unbound.~@?~@:>"
             (cell-error-name condition)
             (case (not-yet-loaded condition)
               (:local
                "~:@_It is a local variable ~
                       not available at compile-time.")
               (t
                ""))))))

(define-condition undefined-function (cell-error)
  ((not-yet-loaded :initform nil :reader not-yet-loaded :initarg :not-yet-loaded))
  (:report
   (lambda (condition stream)
     (let ((name (cell-error-name condition)))
       (format stream
               (if (and (symbolp name) (macro-function name))
                   (sb-format:tokens "~@<~/sb-ext:print-symbol-with-prefix/ is a macro, ~
                                      not a function.~@:>")
                   (sb-format:tokens "~@<The function ~/sb-ext:print-symbol-with-prefix/ ~
                                      is undefined.~@?~@:>"))
               name
               (case (not-yet-loaded condition)
                 (:local
                  (sb-format:tokens "~:@_It is a local function ~
                                     not available at compile-time."))
                 ((t) (sb-format:tokens "~:@_It is defined earlier in the ~
                                         file but is not available at compile-time."))
                 (t
                  "")))))))

(define-condition retry-undefined-function
    (simple-condition undefined-function) ())

(define-condition special-form-function (undefined-function) ()
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot FUNCALL the SYMBOL-FUNCTION of special operator ~S."
             (cell-error-name condition)))))

(define-condition arithmetic-error (error)
  ((operation :reader arithmetic-error-operation
              :initarg :operation
              :initform nil)
   (operands :reader arithmetic-error-operands
             :initarg :operands))
  (:report (lambda (condition stream)
             (format stream
                     "arithmetic error ~S signalled"
                     (type-of condition))
             (when (arithmetic-error-operation condition)
               (format stream
                       "~%Operation was (~S ~{~S~^ ~})."
                       (arithmetic-error-operation condition)
                       (arithmetic-error-operands condition))))))

(define-condition division-by-zero         (arithmetic-error) ())
(define-condition floating-point-overflow  (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())
(define-condition floating-point-inexact   (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())

(define-condition illegal-class-name-error (error)
  ((name :initarg :name :reader illegal-class-name-error-name))
  (:default-initargs :name (missing-arg))
  (:report (lambda (condition stream)
             (format stream "~@<~S is not a legal class name.~@:>"
                     (illegal-class-name-error-name condition)))))

(define-condition print-not-readable (error)
  ((object :reader print-not-readable-object :initarg :object))
  (:report
   (lambda (condition stream)
     (let ((obj (print-not-readable-object condition))
           (*print-array* nil))
       (format stream "~S cannot be printed readably." obj)))))

(define-condition reader-error (parse-error stream-error) ()
  (:report (lambda (condition stream)
             (%report-reader-error condition stream))))

;;; a READER-ERROR whose REPORTing is controlled by FORMAT-CONTROL and
;;; FORMAT-ARGS (the usual case for READER-ERRORs signalled from
;;; within SBCL itself)
;;;
;;; (Inheriting CL:SIMPLE-CONDITION here isn't quite consistent with
;;; the letter of the ANSI spec: this is not a condition signalled by
;;; SIGNAL when a format-control is supplied by the function's first
;;; argument. It seems to me (WHN) to be basically in the spirit of
;;; the spec, but if not, it'd be straightforward to do our own
;;; DEFINE-CONDITION SB-INT:SIMPLISTIC-CONDITION with
;;; FORMAT-CONTROL and FORMAT-ARGS slots, and use that condition in
;;; place of CL:SIMPLE-CONDITION here.)
(define-condition simple-reader-error (reader-error simple-condition)
  ()
  (:report (lambda (condition stream)
             (%report-reader-error condition stream :simple t))))

;;; base REPORTing of a READER-ERROR
;;;
;;; When SIMPLE, we expect and use SIMPLE-CONDITION-ish FORMAT-CONTROL
;;; and FORMAT-ARGS slots.
(defun %report-reader-error (condition stream &key simple position)
  (let ((error-stream (stream-error-stream condition)))
    (pprint-logical-block (stream nil)
      (if simple
          (apply #'format stream
                 (simple-condition-format-control condition)
                 (simple-condition-format-arguments condition))
          (prin1 (class-name (class-of condition)) stream))
      (format stream "~2I~@[~:@_ ~:@_~:{~:(~A~): ~S~:^, ~:_~}~]~:@_ ~:@_Stream: ~S"
              (stream-error-position-info error-stream position)
              error-stream))))

;;;; special SBCL extension conditions

;;; an error apparently caused by a bug in SBCL itself
;;;
;;; Note that we don't make any serious effort to use this condition
;;; for *all* errors in SBCL itself. E.g. type errors and array
;;; indexing errors can occur in functions called from SBCL code, and
;;; will just end up as ordinary TYPE-ERROR or invalid index error,
;;; because the signalling code has no good way to know that the
;;; underlying problem is a bug in SBCL. But in the fairly common case
;;; that the signalling code does know that it's found a bug in SBCL,
;;; this condition is appropriate, reusing boilerplate and helping
;;; users to recognize it as an SBCL bug.
(define-condition bug (simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "~@<  ~? ~:@_~?~:>"
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)
             "~@<This is probably a bug in SBCL itself. (Alternatively, ~
              SBCL might have been corrupted by bad user code, e.g. by an ~
              undefined Lisp operation like ~S, or by stray pointers from ~
              alien code or from unsafe Lisp code; or there might be a bug ~
              in the OS or hardware that SBCL is running on.) If it seems to ~
              be a bug in SBCL itself, the maintainers would like to know ~
              about it. Bug reports are welcome on the SBCL ~
              mailing lists, which you can find at ~
              <http://sbcl.sourceforge.net/>.~:@>"
             '((fmakunbound 'compile))))))

(define-condition simple-storage-condition (storage-condition simple-condition)
  ())

(define-condition sanitizer-error (simple-error)
  ((value :reader sanitizer-error-value :initarg :value)
   (address :reader sanitizer-error-address :initarg :address)
   (size :reader sanitizer-error-size :initarg :size)))

;;; a condition for use in stubs for operations which aren't supported
;;; on some platforms
;;;
;;; E.g. in sbcl-0.7.0.5, it might be appropriate to do something like
;;;   #-(or freebsd linux)
;;;   (defun load-foreign (&rest rest)
;;;     (error 'unsupported-operator :name 'load-foreign))
;;;   #+(or freebsd linux)
;;;   (defun load-foreign ... actual definition ...)
;;; By signalling a standard condition in this case, we make it
;;; possible for test code to distinguish between (1) intentionally
;;; unimplemented and (2) unintentionally just screwed up somehow.
;;; (Before this condition was defined, test code tried to deal with
;;; this by checking for FBOUNDP, but that didn't work reliably. In
;;; sbcl-0.7.0, a package screwup left the definition of
;;; LOAD-FOREIGN in the wrong package, so it was unFBOUNDP even on
;;; architectures where it was supposed to be supported, and the
;;; regression tests cheerfully passed because they assumed that
;;; unFBOUNDPness meant they were running on an system which didn't
;;; support the extension.)
(define-condition unsupported-operator (simple-error) ())

;;; (:ansi-cl :function remove)
;;; (:ansi-cl :section (a b c))
;;; (:ansi-cl :glossary "similar")
;;;
;;; (:sbcl :node "...")
;;; (:sbcl :variable *ed-functions*)

;;;
;;; FIXME: this is not the right place for this.
(defun print-reference (reference stream)
  (ecase (car reference)
    (:amop
     (format stream "AMOP")
     (format stream ", ")
     (destructuring-bind (type data) (cdr reference)
       (ecase type
         (:readers "Readers for ~:(~A~) Metaobjects"
                   (substitute #\  #\- (symbol-name data)))
         (:initialization
          (format stream "Initialization of ~:(~A~) Metaobjects"
                  (substitute #\  #\- (symbol-name data))))
         (:generic-function (format stream "Generic Function ~S" data))
         (:function (format stream "Function ~S" data))
         (:section (format stream "Section ~{~D~^.~}" data)))))
    (:ansi-cl
     (format stream "The ANSI Standard")
     (format stream ", ")
     (destructuring-bind (type data) (cdr reference)
       (ecase type
         (:function (format stream "Function ~S" data))
         (:special-operator (format stream "Special Operator ~S" data))
         (:macro (format stream "Macro ~S" data))
         (:section (format stream "Section ~{~D~^.~}" data))
         (:glossary (format stream "Glossary entry for ~S" data))
         (:type (format stream "Type ~S" data))
         (:system-class (format stream "System Class ~S" data))
         (:issue (format stream "writeup for Issue ~A" data)))))
    (:sbcl
     (format stream "The SBCL Manual")
     (format stream ", ")
     (destructuring-bind (type data) (cdr reference)
       (ecase type
         (:node (format stream "Node ~S" data))
         (:variable (format stream "Variable ~S" data))
         (:function (format stream "Function ~S" data)))))
    ;; FIXME: other documents (e.g. CLIM, Franz documentation :-)
    ))
(define-condition reference-condition ()
  ((references :initarg :references :reader reference-condition-references)))
(defvar *print-condition-references* t)

(define-condition simple-reference-error (reference-condition simple-error)
  ())

(define-condition simple-reference-warning (reference-condition simple-warning)
  ())

(define-condition arguments-out-of-domain-error
    (arithmetic-error reference-condition)
  ())

;; per CLHS: "The consequences are unspecified if functions are ...
;; multiply defined in the same file." so we are within reason to do any
;; unspecified behavior at compile-time and/or time, but the compiler was
;; annoyingly mum about genuinely inadvertent duplicate macro definitions.
;; Redefinition is henceforth a style-warning, and for compatibility it does
;; not cause the ERRORP value from COMPILE-TIME to be T.
;; Nor do we cite section 3.2.2.3 as the governing prohibition.
(defun report-duplicate-definition (condition stream)
  (format stream "~@<Duplicate definition for ~S found in one file.~@:>"
          (slot-value condition 'name)))

(define-condition duplicate-definition (reference-condition warning)
  ((name :initarg :name :reader duplicate-definition-name))
  (:report report-duplicate-definition)
  (:default-initargs :references '((:ansi-cl :section (3 2 2 3)))))
;; To my thinking, DUPLICATE-DEFINITION should be the ancestor condition,
;; and not fatal. But changing the meaning of that concept would be a bad idea,
;; so instead there is a new condition for the softer variant, which does not
;; inherit from the former.
(define-condition same-file-redefinition-warning (style-warning)
  ;; Slot readers aren't proper generic functions until CLOS is built,
  ;; so this doesn't get a reader because you can't pick the same name,
  ;; and it wouldn't do any good to pick a different name that nothing knows.
  ((name :initarg :name))
  (:report report-duplicate-definition))

(define-condition constant-modified (reference-condition warning)
  ((fun-name :initarg :fun-name :reader constant-modified-fun-name)
   (values :initform nil :initarg :values :reader constant-modified-values))
  (:report (lambda (c s)
             (format s "~@<Destructive function ~S called on ~
                        constant data: ~{~s~^, ~}~:>"
                     (constant-modified-fun-name c)
                     (constant-modified-values c))))
  (:default-initargs :references '((:ansi-cl :special-operator quote)
                                   (:ansi-cl :section (3 7 1)))))

(define-condition macro-arg-modified (constant-modified)
  ((variable :initform nil :initarg :variable :reader macro-arg-modified-variable))
  (:report (lambda (c s)
             (format s "~@<Destructive function ~S called on a macro argument: ~S.~:>"
                     (constant-modified-fun-name c)
                     (macro-arg-modified-variable c))))
  (:default-initargs :references nil))

(define-condition package-at-variance (reference-condition simple-warning)
  ()
  (:default-initargs :references '((:ansi-cl :macro defpackage)
                                   (:sbcl :variable *on-package-variance*))))

(define-condition package-at-variance-error (reference-condition simple-condition
                                             package-error)
  ()
  (:default-initargs :references '((:ansi-cl :macro defpackage))))

(define-condition defconstant-uneql (reference-condition error)
  ((name :initarg :name :reader defconstant-uneql-name)
   (old-value :initarg :old-value :reader defconstant-uneql-old-value)
   (new-value :initarg :new-value :reader defconstant-uneql-new-value))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<The constant ~S is being redefined (from ~S to ~S)~@:>"
             (defconstant-uneql-name condition)
             (defconstant-uneql-old-value condition)
             (defconstant-uneql-new-value condition))))
  (:default-initargs :references '((:ansi-cl :macro defconstant)
                                   (:sbcl :node "Idiosyncrasies"))))

(define-condition array-initial-element-mismatch
    (reference-condition simple-warning)
  ()
  (:default-initargs
      :references '((:ansi-cl :function make-array)
                    (:ansi-cl :function upgraded-array-element-type))))

(define-condition initial-element-mismatch-style-warning
    (array-initial-element-mismatch simple-style-warning)
  ())

(define-condition type-warning (reference-condition simple-warning)
  ()
  (:default-initargs :references '((:sbcl :node "Handling of Types"))))
(define-condition type-style-warning (reference-condition simple-style-warning)
  ()
  (:default-initargs :references '((:sbcl :node "Handling of Types"))))
(define-condition slot-initform-type-style-warning (type-style-warning) ())

(define-condition local-argument-mismatch (reference-condition simple-warning)
  ()
  (:default-initargs :references '((:ansi-cl :section (3 2 2 3)))))

(define-condition format-args-mismatch (reference-condition)
  ()
  (:default-initargs :references '((:ansi-cl :section (22 3 10 2)))))

(define-condition format-too-few-args-warning
    (format-args-mismatch simple-warning)
  ())
(define-condition format-too-many-args-warning
    (format-args-mismatch simple-style-warning)
  ())

(define-condition implicit-generic-function-warning (style-warning)
  ((name :initarg :name :reader implicit-generic-function-name))
  (:report
   (lambda (condition stream)
     (format stream "~@<Implicitly creating new generic function ~
                     ~/sb-ext:print-symbol-with-prefix/.~:@>"
             (implicit-generic-function-name condition)))))

(define-condition extension-failure (reference-condition simple-error)
  ())

(define-condition structure-initarg-not-keyword
    (reference-condition simple-style-warning)
  ()
  (:default-initargs :references '((:ansi-cl :section (2 4 8 13)))))

(define-condition package-lock-violation (package-error
                                          reference-condition
                                          simple-condition)
  ((current-package :initform *package*
                    :reader package-lock-violation-in-package))
  (:report
   (lambda (condition stream)
     (let ((control (simple-condition-format-control condition))
           (error-package (package-name
                           (package-error-package condition)))
           (current-package (package-name
                             (package-lock-violation-in-package condition))))
       (format stream "~@<Lock on package ~A violated~@[~{ when ~?~}~] ~
                       while in package ~A.~:@>"
               error-package
               (when control
                 (list control (simple-condition-format-arguments condition)))
               current-package))))
  ;; no :default-initargs -- reference-stuff provided by the
  ;; signalling form in target-package.lisp
  (:documentation
   "Subtype of CL:PACKAGE-ERROR. A subtype of this error is signalled
when a package-lock is violated."))

(define-condition package-locked-error (package-lock-violation) ()
  (:documentation
   "Subtype of SB-EXT:PACKAGE-LOCK-VIOLATION. An error of this type is
signalled when an operation on a package violates a package lock."))

(define-condition symbol-package-locked-error (package-lock-violation)
  ((symbol :initarg :symbol :reader package-locked-error-symbol))
  (:documentation
   "Subtype of SB-EXT:PACKAGE-LOCK-VIOLATION. An error of this type is
signalled when an operation on a symbol violates a package lock. The
symbol that caused the violation is accessed by the function
SB-EXT:PACKAGE-LOCKED-ERROR-SYMBOL."))

(define-condition undefined-alien-error (cell-error) ()
  (:report
   (lambda (condition stream)
     (if (slot-boundp condition 'name)
         (format stream "Undefined alien: ~S" (cell-error-name condition))
         (format stream "Undefined alien symbol.")))))

(define-condition undefined-alien-variable-error (undefined-alien-error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Attempt to access an undefined alien variable."))))

(define-condition undefined-alien-function-error (undefined-alien-error) ()
  (:report
   (lambda (condition stream)
     (if (and (slot-boundp condition 'name)
              (cell-error-name condition))
         (format stream "The alien function ~s is undefined."
                 (cell-error-name condition))
         (format stream "Attempt to call an undefined alien function.")))))

(define-condition unknown-keyword-argument (program-error)
  ((name :reader unknown-keyword-argument-name :initarg :name))
  (:report
   (lambda (condition stream)
     (format stream "Unknown &KEY argument: ~S"
             (unknown-keyword-argument-name condition)))))


;;;; various other (not specified by ANSI) CONDITIONs
;;;;
;;;; These might logically belong in other files; they're here, after
;;;; setup of CONDITION machinery, only because that makes it easier to
;;;; get cold init to work.

;;; OAOOM warning: see cross-condition.lisp
(define-condition encapsulated-condition (condition)
  ((condition :initarg :condition :reader encapsulated-condition)))

;;; KLUDGE: a condition for floating point errors when we can't or
;;; won't figure out what type they are. (In FreeBSD and OpenBSD we
;;; don't know how, at least as of sbcl-0.6.7; in Linux we probably
;;; know how but the old code was broken by the conversion to POSIX
;;; signal handling and hasn't been fixed as of sbcl-0.6.7.)
;;;
;;; FIXME: Perhaps this should also be a base class for all
;;; floating point exceptions?
(define-condition floating-point-exception (arithmetic-error)
  ((flags :initarg :traps
          :initform nil
          :reader floating-point-exception-traps))
  (:report (lambda (condition stream)
             (format stream
                     "An arithmetic error ~S was signalled.~%"
                     (type-of condition))
             (let ((traps (floating-point-exception-traps condition)))
               (if traps
                   (format stream
                           "Trapping conditions are: ~%~{ ~S~^~}~%"
                           traps)
                   (write-line
                    "No traps are enabled? How can this be?"
                    stream))))))

(define-condition invalid-array-index-error (type-error)
  ((array :initarg :array :reader invalid-array-index-error-array)
   (axis :initarg :axis :reader invalid-array-index-error-axis))
  (:report
   (lambda (condition stream)
     (let ((array (invalid-array-index-error-array condition))
           (index (type-error-datum condition)))
       (if (integerp index)
           (format stream "Invalid index ~D for ~@[axis ~D of ~]~
~S~@[, ~:@_should be a non-negative integer below ~D~]."
                   (type-error-datum condition)
                   (when (> (array-rank array) 1)
                     (invalid-array-index-error-axis condition))
                   (type-of array)
                   ;; Extract the bound from (INTEGER 0 (BOUND))
                   (let ((max (caaddr (type-error-expected-type condition))))
                     (if (> max 0) max)))
           (format stream "~s is not of type INTEGER." index))))))

(define-condition invalid-array-error (reference-condition type-error) ()
  (:report
   (lambda (condition stream)
     (let ((*print-array* nil))
       (format stream
               "~@<Displaced array originally of type ~
                 ~/sb-impl:print-type-specifier/ has been invalidated ~
                 due its displaced-to array ~S having become too small ~
                 to hold it: the displaced array's dimensions have all ~
                 been set to zero to trap accesses to it.~:@>"
               (type-error-expected-type condition)
               (array-displacement (type-error-datum condition))))))
  (:default-initargs
      :references
      (list '(:ansi-cl :function adjust-array))))

(define-condition uninitialized-element-error (cell-error) ()
  (:report
   (lambda (condition stream)
     ;; NAME is a cons of the array and index
     (destructuring-bind (array . index) (cell-error-name condition)
       (declare (ignorable index))
       #+ubsan
       (let* ((origin-pc
               (ash (sb-vm::vector-extra-data
                     (if (simple-vector-p array)
                         array
                         (sb-vm::vector-extra-data array)))
                    -3)) ; XXX: ubsan magic
              (origin-code (sb-di::code-header-from-pc (int-sap origin-pc))))
         (let ((*print-array* nil))
           (format stream "Element ~D of array ~_~S ~_was not assigned a value.~%Origin=~X"
                   index array (or origin-code origin-pc))))
       #-ubsan
       ;; FOLD-INDEX-ADDRESSING could render INDEX wrong. There's no way to know.
       (let ((*print-array* nil))
         (format stream "Uninitialized element accessed in array ~S"
                 array))))))


;;; We signal this one for SEQUENCE operations, but INVALID-ARRAY-INDEX-ERROR
;;; for arrays. Might it be better to use the above condition for operations
;;; on SEQUENCEs that happen to be arrays?
(define-condition index-too-large-error (type-error)
  ((sequence :initarg :sequence))
  (:report
   (lambda (condition stream)
     (let ((sequence (slot-value condition 'sequence))
           (index (type-error-datum condition)))
       (if (vectorp sequence)
           (format stream "Invalid index ~D for ~S~@[ with fill-pointer ~D~]~
~@[, ~:@_should be a non-negative integer below ~D~]."
                   index
                   (type-of sequence)
                   (and (array-has-fill-pointer-p sequence)
                        (fill-pointer sequence))
                   (let ((l (length sequence))) (if (> l 0) l)))
           (format stream
                   "The index ~D is too large for a ~a of length ~D."
                   index
                   (if (listp sequence)
                       "list"
                       "sequence")
                   (length sequence)))))))

(define-condition bounding-indices-bad-error (reference-condition type-error)
  ((object :reader bounding-indices-bad-object :initarg :object))
  (:report
   (lambda (condition stream)
     (let* ((datum (type-error-datum condition))
            (start (car datum))
            (end (cdr datum))
            (object (bounding-indices-bad-object condition)))
       (etypecase object
         (sequence
          (format stream
                  "The bounding indices ~S and ~S are bad ~
                   for a sequence of length ~S."
                  start end (length object)))
         (array
          ;; from WITH-ARRAY-DATA
          (format stream
                  "The START and END parameters ~S and ~S are ~
                   bad for an array of total size ~S."
                  start end (array-total-size object)))))))
  (:default-initargs
      :references
      (list '(:ansi-cl :glossary "bounding index designator")
            '(:ansi-cl :issue "SUBSEQ-OUT-OF-BOUNDS:IS-AN-ERROR"))))

(define-condition nil-array-accessed-error (reference-condition type-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "An attempt to access an array of element-type ~
                      NIL was made.  Congratulations!")))
  (:default-initargs
      :references '((:ansi-cl :function upgraded-array-element-type)
                    (:ansi-cl :section (15 1 2 1))
                    (:ansi-cl :section (15 1 2 2)))))

(define-condition namestring-parse-error (parse-error)
  ((complaint :reader namestring-parse-error-complaint :initarg :complaint)
   (args :reader namestring-parse-error-args :initarg :args :initform nil)
   (namestring :reader namestring-parse-error-namestring :initarg :namestring)
   (offset :reader namestring-parse-error-offset :initarg :offset))
  (:report
   (lambda (condition stream)
     (format stream
             "parse error in namestring: ~?~%  ~A~%  ~V@T^"
             (namestring-parse-error-complaint condition)
             (namestring-parse-error-args condition)
             (namestring-parse-error-namestring condition)
             (namestring-parse-error-offset condition)))))

(define-condition pathname-unparse-error (file-error
                                          simple-condition)
  ((problem :reader pathname-unparse-error-problem :initarg :problem))
  (:report (lambda (condition stream)
             (format stream "~@<The pathname ~S ~A~:[.~; because ~:*~?~]~@:>"
                     (file-error-pathname condition)
                     (pathname-unparse-error-problem condition)
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition))))
  (:default-initargs
   :problem (missing-arg)))

(define-condition no-namestring-error (pathname-unparse-error
                                       reference-condition)
  ()
  (:default-initargs
   :problem "does not have a namestring"
   :references '((:ansi-cl :section (19 1 2)))))
(defun no-namestring-error
    (pathname &optional format-control &rest format-arguments)
  (error 'no-namestring-error
         :pathname pathname
         :format-control format-control :format-arguments format-arguments))

(define-condition no-native-namestring-error (pathname-unparse-error)
  ()
  (:default-initargs
   :problem "does not have a native namestring"))
(defun no-native-namestring-error
    (pathname &optional format-control &rest format-arguments)
  (error 'no-native-namestring-error
         :pathname pathname
         :format-control format-control :format-arguments format-arguments))

(define-condition simple-package-error (simple-condition package-error) ())

(define-condition package-does-not-exist (simple-package-error) ())

(define-condition simple-reader-package-error (simple-reader-error package-error) ())
(define-condition reader-package-does-not-exist (simple-reader-package-error package-does-not-exist) ())

(define-condition reader-eof-error (end-of-file)
  ((context :reader reader-eof-error-context :initarg :context))
  (:report
   (lambda (condition stream)
     (format stream
             "unexpected end of file on ~S ~A"
             (stream-error-stream condition)
             (reader-eof-error-context condition)))))

(define-condition reader-impossible-number-error (simple-reader-error)
  ((error :reader reader-impossible-number-error-error :initarg :error))
  (:report
   (lambda (condition stream)
     (let ((error-stream (stream-error-stream condition)))
       (format stream
               "READER-ERROR ~@[at ~W ~]on ~S:~%~?~%Original error: ~A"
               (sb-impl::file-position-or-nil-for-error error-stream) error-stream
               (simple-condition-format-control condition)
               (simple-condition-format-arguments condition)
               (reader-impossible-number-error-error condition))))))

(define-condition standard-readtable-modified-error (reference-condition error)
  ((operation :initarg :operation :reader standard-readtable-modified-operation))
  (:report (lambda (condition stream)
             (format stream "~S would modify the standard readtable."
                     (standard-readtable-modified-operation condition))))
  (:default-initargs :references `((:ansi-cl :section (2 1 1 2))
                                   (:ansi-cl :glossary "standard readtable"))))

(define-condition standard-pprint-dispatch-table-modified-error
    (reference-condition error)
  ((operation :initarg :operation
              :reader standard-pprint-dispatch-table-modified-operation))
  (:report (lambda (condition stream)
             (format stream "~S would modify the standard pprint dispatch table."
                     (standard-pprint-dispatch-table-modified-operation
                      condition))))
  (:default-initargs
   :references `((:ansi-cl :glossary "standard pprint dispatch table"))))

(define-condition timeout (serious-condition)
  ((seconds :initarg :seconds :initform nil :reader timeout-seconds))
  (:report (lambda (condition stream)
             (format stream "Timeout occurred~@[ after ~A second~:P~]."
                     (timeout-seconds condition))))
  (:documentation
   "Signaled when an operation does not complete within an allotted time budget."))

(define-condition io-timeout (stream-error timeout)
  ((direction :reader io-timeout-direction :initarg :direction))
  (:report
   (lambda (condition stream)
     (declare (type stream stream))
     (format stream
             "I/O timeout while doing ~(~A~) on ~S."
             (io-timeout-direction condition)
             (stream-error-stream condition)))))

(define-condition deadline-timeout (timeout)
  ()
  (:report (lambda (condition stream)
             (format stream "A deadline was reached after ~A second~:P."
                     (timeout-seconds condition))))
  (:documentation
   "Signaled when an operation in the context of a deadline takes
longer than permitted by the deadline."))

(define-condition declaration-type-conflict-error (reference-condition
                                                   simple-error)
  ()
  (:default-initargs
   :format-control
   #.(macroexpand-1 ; stuff in a literal #<fmt-control>
      '(sb-format:tokens "Symbol ~/sb-ext:print-symbol-with-prefix/ cannot ~
     be both the name of a type and the name of a declaration"))
   :references '((:ansi-cl :section (3 8 21)))))

;;; Single stepping conditions

(define-condition step-condition ()
  ((form :initarg :form :reader step-condition-form))
  (:documentation "Common base class of single-stepping conditions.
STEP-CONDITION-FORM holds a string representation of the form being
stepped."))

(setf (documentation 'step-condition-form 'function)
      "Form associated with the STEP-CONDITION.")

(define-condition step-form-condition (step-condition)
  ((args :initarg :args :reader step-condition-args))
  (:report
   (lambda (condition stream)
     (let ((*print-circle* t)
           (*print-pretty* t)
           (*print-readably* nil))
       (format stream
                 "Evaluating call:~%~<  ~@;~A~:>~%~
                  ~:[With arguments:~%~{  ~S~%~}~;With unknown arguments~]~%"
               (list (step-condition-form condition))
               (eq (step-condition-args condition) :unknown)
               (step-condition-args condition)))))
  (:documentation "Condition signalled by code compiled with
single-stepping information when about to execute a form.
STEP-CONDITION-FORM holds the form, STEP-CONDITION-PATHNAME holds the
pathname of the original file or NIL, and STEP-CONDITION-SOURCE-PATH
holds the source-path to the original form within that file or NIL.
Associated with this condition are always the restarts STEP-INTO,
STEP-NEXT, and STEP-CONTINUE."))

(define-condition step-result-condition (step-condition)
  ((result :initarg :result :reader step-condition-result)))

(setf (documentation 'step-condition-result 'function)
      "Return values associated with STEP-VALUES-CONDITION as a list,
or the variable value associated with STEP-VARIABLE-CONDITION.")

(define-condition step-values-condition (step-result-condition)
  ()
  (:documentation "Condition signalled by code compiled with
single-stepping information after executing a form.
STEP-CONDITION-FORM holds the form, and STEP-CONDITION-RESULT holds
the values returned by the form as a list. No associated restarts."))

(define-condition step-finished-condition (step-condition)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Returning from STEP")))
  (:documentation "Condition signaled when STEP returns."))

;;; A knob for muffling warnings, mostly for use while loading files.
(defvar *muffled-warnings* 'uninteresting-redefinition
  "A type that ought to specify a subtype of WARNING.  Whenever a
warning is signaled, if the warning is of this type and is not
handled by any other handler, it will be muffled.")

;;; Various STYLE-WARNING signaled in the system.
;; For the moment, we're only getting into the details for function
;; redefinitions, but other redefinitions could be done later
;; (e.g. methods).
(define-condition redefinition-warning (style-warning)
  ((name
    :initarg :name
    :reader redefinition-warning-name)
   (new-location
    :initarg :new-location
    :reader redefinition-warning-new-location)))

(define-condition function-redefinition-warning (redefinition-warning)
  ((new-function
    :initarg :new-function
    :reader function-redefinition-warning-new-function)))

(define-condition redefinition-with-defun (function-redefinition-warning)
  ()
  (:report (lambda (warning stream)
             (format stream "redefining ~/sb-ext:print-symbol-with-prefix/ ~
                             in DEFUN"
                     (redefinition-warning-name warning)))))

(define-condition redefinition-with-defmacro (function-redefinition-warning)
  ()
  (:report (lambda (warning stream)
             (format stream "redefining ~/sb-ext:print-symbol-with-prefix/ ~
                             in DEFMACRO"
                     (redefinition-warning-name warning)))))

(define-condition redefinition-with-defgeneric (redefinition-warning)
  ()
  (:report (lambda (warning stream)
             (format stream "redefining ~/sb-ext:print-symbol-with-prefix/ ~
                             in DEFGENERIC"
                     (redefinition-warning-name warning)))))

(define-condition redefinition-with-defmethod (redefinition-warning)
  ((qualifiers :initarg :qualifiers
               :reader redefinition-with-defmethod-qualifiers)
   (specializers :initarg :specializers
                 :reader redefinition-with-defmethod-specializers)
   (new-location :initarg :new-location
                 :reader redefinition-with-defmethod-new-location)
   (old-method :initarg :old-method
               :reader redefinition-with-defmethod-old-method))
  (:report (lambda (warning stream)
             (format stream "redefining ~S~{ ~S~} ~S in DEFMETHOD"
                     (redefinition-warning-name warning)
                     (redefinition-with-defmethod-qualifiers warning)
                     (redefinition-with-defmethod-specializers warning)))))

;;;; Deciding which redefinitions are "interesting".

(defun function-file-namestring (function)
  (when (typep function 'interpreted-function)
    (return-from function-file-namestring
      #+sb-eval
      (sb-c:definition-source-location-namestring
          (sb-eval:interpreted-function-source-location function))
      #+sb-fasteval
      (awhen (sb-interpreter:fun-source-location function)
        (sb-c:definition-source-location-namestring it))))
  (let* ((fun (%fun-fun function))
         (code (fun-code-header fun))
         (debug-info (%code-debug-info code))
         (debug-source (when debug-info
                         (sb-c::debug-info-source debug-info)))
         (namestring (when debug-source
                       (debug-source-namestring debug-source))))
    namestring))

(defun interesting-function-redefinition-warning-p (warning old)
  (let ((new (function-redefinition-warning-new-function warning)))
    (or
     ;; compiled->interpreted is interesting.
     (and (typep old 'compiled-function)
          (typep new '(not compiled-function)))
     ;; fin->regular is interesting except for interpreted->compiled.
     (and (typep new '(not funcallable-instance))
          (typep old '(and funcallable-instance (not interpreted-function))))
     ;; different file or unknown location is interesting.
     (let* ((old-namestring (function-file-namestring old))
            (new-namestring (function-file-namestring new)))
       (and (or (not old-namestring)
                (not new-namestring)
                (not (string= old-namestring new-namestring))))))))

(setf (info :function :predicate-truth-constraint
            'uninteresting-ordinary-function-redefinition-p) 'warning)
(defun uninteresting-ordinary-function-redefinition-p (warning)
  (and
   (typep warning 'redefinition-with-defun)
   ;; Shared logic.
   (let ((name (redefinition-warning-name warning)))
     (not (interesting-function-redefinition-warning-p
           warning (or (fdefinition name) (macro-function name)))))))

(setf (info :function :predicate-truth-constraint
            'uninteresting-macro-redefinition-p) 'warning)
(defun uninteresting-macro-redefinition-p (warning)
  (and
   (typep warning 'redefinition-with-defmacro)
   ;; Shared logic.
   (let ((name (redefinition-warning-name warning)))
     (not (interesting-function-redefinition-warning-p
           warning (or (macro-function name) (fdefinition name)))))))

(setf (info :function :predicate-truth-constraint
            'uninteresting-generic-function-redefinition-p) 'warning)
(defun uninteresting-generic-function-redefinition-p (warning)
  (and
   (typep warning 'redefinition-with-defgeneric)
   ;; Can't use the shared logic above, since GF's don't get a "new"
   ;; definition -- rather the FIN-FUNCTION is set.
   (let* ((name (redefinition-warning-name warning))
          (old (fdefinition name))
          (old-location (when (typep old 'generic-function)
                          (sb-pcl::definition-source old)))
          (old-namestring (when old-location
                            (sb-c:definition-source-location-namestring old-location)))
          (new-location (redefinition-warning-new-location warning))
          (new-namestring (when new-location
                           (sb-c:definition-source-location-namestring new-location))))
     (and old-namestring
          new-namestring
          (string= old-namestring new-namestring)))))

(setf (info :function :predicate-truth-constraint
            'uninteresting-method-redefinition-p) 'warning)
(defun uninteresting-method-redefinition-p (warning)
  (and
   (typep warning 'redefinition-with-defmethod)
   ;; Can't use the shared logic above, since GF's don't get a "new"
   ;; definition -- rather the FIN-FUNCTION is set.
   (let* ((old-method (redefinition-with-defmethod-old-method warning))
          (old-location (sb-pcl::definition-source old-method))
          (old-namestring (when old-location
                            (sb-c:definition-source-location-namestring old-location)))
          (new-location (redefinition-warning-new-location warning))
          (new-namestring (when new-location
                            (sb-c:definition-source-location-namestring new-location))))
         (and new-namestring
              old-namestring
              (string= new-namestring old-namestring)))))

(deftype uninteresting-redefinition ()
  '(or (satisfies uninteresting-ordinary-function-redefinition-p)
       (satisfies uninteresting-macro-redefinition-p)
       (satisfies uninteresting-generic-function-redefinition-p)
       (satisfies uninteresting-method-redefinition-p)))

(define-condition redefinition-with-deftransform (redefinition-warning)
  ((transform :initarg :transform
              :reader redefinition-with-deftransform-transform))
  (:report (lambda (warning stream)
             (format stream "Overwriting ~S"
                     (redefinition-with-deftransform-transform warning)))))

;;; Various other STYLE-WARNINGS
(define-condition dubious-asterisks-around-variable-name
    (style-warning simple-condition)
  ()
  (:report (lambda (warning stream)
             (format stream "~@?, even though the name follows~@
the usual naming convention (names like *FOO*) for special variables"
                     (simple-condition-format-control warning)
                     (simple-condition-format-arguments warning)))))

(define-condition asterisks-around-lexical-variable-name
    (dubious-asterisks-around-variable-name)
  ())

(define-condition asterisks-around-constant-variable-name
    (dubious-asterisks-around-variable-name)
  ())

(define-condition &optional-and-&key-in-lambda-list (simple-style-warning) ())

;; We call this UNDEFINED-ALIEN-STYLE-WARNING because there are some
;; subclasses of ERROR above having to do with undefined aliens.
(define-condition undefined-alien-style-warning (style-warning)
  ((symbol :initarg :symbol :reader undefined-alien-symbol))
  (:report (lambda (warning stream)
             (format stream "Undefined alien: ~S"
                     (undefined-alien-symbol warning)))))

;;; Formerly this was guarded by "#+(or sb-eval sb-fasteval)", but
;;; why would someone build with no interpreter? And if they did,
;;; would they really care that one extra condition definition exists?
(define-condition lexical-environment-too-complex (style-warning)
  ((form :initarg :form :reader lexical-environment-too-complex-form)
   (lexenv :initarg :lexenv :reader lexical-environment-too-complex-lexenv))
  (:report (lambda (warning stream)
             (format stream
                     "~@<Native lexical environment too complex for ~
                         SB-EVAL to evaluate ~S, falling back to ~
                         SIMPLE-EVAL-IN-LEXENV.  Lexenv: ~S~:@>"
                     (lexical-environment-too-complex-form warning)
                     (lexical-environment-too-complex-lexenv warning)))))

;; If the interpreter is in use (and the REPL is interpreted),
;; it's easy to accidentally make the macroexpand-hook an interpreted
;; function. So MACROEXPAND-1 is a little more careful,
;; and might signal this, instead of only EVAL being able to signal it.
(define-condition macroexpand-hook-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream "The value of *MACROEXPAND-HOOK* is not a designator for a compiled function: ~S"
                     (type-error-datum condition)))))

;; Although this has -ERROR- in the name, it's just a STYLE-WARNING.
(define-condition character-decoding-error-in-comment (style-warning)
  ((stream :initarg :stream :reader decoding-error-in-comment-stream)
   (position :initarg :position :reader decoding-error-in-comment-position))
  (:report (lambda (warning stream)
             (format stream
                      "Character decoding error in a ~A-comment at ~
                      position ~A reading source stream ~A, ~
                      resyncing."
                      (decoding-error-in-comment-macro warning)
                      (decoding-error-in-comment-position warning)
                      (decoding-error-in-comment-stream warning)))))

(define-condition character-decoding-error-in-macro-char-comment
    (character-decoding-error-in-comment)
  ((char :initform #\; :initarg :char
         :reader character-decoding-error-in-macro-char-comment-char)))

(define-condition character-decoding-error-in-dispatch-macro-char-comment
    (character-decoding-error-in-comment)
  ;; ANSI doesn't give a way for a reader function invoked by a
  ;; dispatch macro character to determine which dispatch character
  ;; was used, so if a user wants to signal one of these from a custom
  ;; comment reader, he'll have to supply the :DISP-CHAR himself.
  ((disp-char :initform #\# :initarg :disp-char
              :reader character-decoding-error-in-macro-char-comment-disp-char)
   (sub-char :initarg :sub-char
             :reader character-decoding-error-in-macro-char-comment-sub-char)))

(defun decoding-error-in-comment-macro (warning)
  (etypecase warning
    (character-decoding-error-in-macro-char-comment
     (character-decoding-error-in-macro-char-comment-char warning))
    (character-decoding-error-in-dispatch-macro-char-comment
     (format
      nil "~C~C"
      (character-decoding-error-in-macro-char-comment-disp-char warning)
      (character-decoding-error-in-macro-char-comment-sub-char warning)))))

(define-condition deprecated-eval-when-situations (style-warning)
  ((situations :initarg :situations
               :reader deprecated-eval-when-situations-situations))
  (:report (lambda (warning stream)
             (format stream "using deprecated EVAL-WHEN situation names~{ ~S~}"
                     (deprecated-eval-when-situations-situations warning)))))

(define-condition proclamation-mismatch (condition)
  ((kind :initarg :kind :reader proclamation-mismatch-kind)
   (description :initarg :description :reader proclamation-mismatch-description :initform nil)
   (name :initarg :name :reader proclamation-mismatch-name)
   (old :initarg :old :reader proclamation-mismatch-old)
   (new :initarg :new :reader proclamation-mismatch-new)
   (value :initarg :value))
  (:report
   (lambda (condition stream)
     (if (slot-boundp condition 'value)
         (format stream
                 "~@<The new ~A proclamation for~@[ ~A~] ~
               ~/sb-ext:print-symbol-with-prefix/~
               ~@:_~2@T~/sb-impl:print-type-specifier/~@:_~
               does not match the current value ~S of type~
               ~@:_~2@T~/sb-impl:print-type-specifier/~@:>"
                 (proclamation-mismatch-kind condition)
                 (proclamation-mismatch-description condition)
                 (proclamation-mismatch-name condition)
                 (proclamation-mismatch-new condition)
                 (slot-value condition 'value)
                 (proclamation-mismatch-old condition))
         (format stream
                 "~@<The new ~A proclamation for~@[ ~A~] ~
               ~/sb-ext:print-symbol-with-prefix/~
               ~@:_~2@T~/sb-impl:print-type-specifier/~@:_~
               does not match the old ~4:*~A~3* proclamation~
               ~@:_~2@T~/sb-impl:print-type-specifier/~@:>"
                 (proclamation-mismatch-kind condition)
                 (proclamation-mismatch-description condition)
                 (proclamation-mismatch-name condition)
                 (proclamation-mismatch-new condition)
                 (proclamation-mismatch-old condition))))))

(define-condition type-proclamation-mismatch (proclamation-mismatch)
  ()
  (:default-initargs :kind 'type))

(define-condition type-proclamation-mismatch-warning (style-warning
                                                      type-proclamation-mismatch)
  ())

(define-condition ftype-proclamation-mismatch (proclamation-mismatch)
  ()
  (:default-initargs :kind 'ftype))

(define-condition ftype-proclamation-mismatch-warning (style-warning
                                                       ftype-proclamation-mismatch)
  ())

(define-condition ftype-proclamation-mismatch-error (error
                                                     ftype-proclamation-mismatch)
  ()
  (:default-initargs :kind 'ftype :description "known function"))

(define-condition ftype-proclamation-derived-mismatch-warning (ftype-proclamation-mismatch-warning)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "~@<The new ~A proclamation for~@[ ~A~] ~
               ~/sb-ext:print-symbol-with-prefix/~
               ~@:_~2@T~/sb-impl:print-type-specifier/~@:_~
               does not match the derived return type~
               ~@:_~2@T~/sb-impl:print-type-specifier/~@:>"
             (proclamation-mismatch-kind condition)
             (proclamation-mismatch-description condition)
             (proclamation-mismatch-name condition)
             (proclamation-mismatch-new condition)
             (proclamation-mismatch-old condition)))))

;;;; deprecation conditions

(define-condition deprecation-condition (reference-condition)
  ((namespace     :initarg :namespace
                  :reader deprecation-condition-namespace)
   (name          :initarg :name
                  :reader deprecation-condition-name)
   (replacements  :initarg :replacements
                  :reader deprecation-condition-replacements)
   (software      :initarg :software
                  :reader deprecation-condition-software)
   (version       :initarg :version
                  :reader deprecation-condition-version)
   (runtime-error :initarg :runtime-error
                  :reader deprecation-condition-runtime-error
                  :initform nil))
  (:default-initargs
   :namespace (missing-arg)
   :name (missing-arg)
   :replacements (missing-arg)
   :software (missing-arg)
   :version (missing-arg)
   :references '((:sbcl :node "Deprecation Conditions")))
  (:documentation
   "Superclass for deprecation-related error and warning
conditions."))

(defmethod print-object ((condition deprecation-condition) stream)
  (flet ((print-it (stream)
           (print-deprecation-message
            (deprecation-condition-namespace condition)
            (deprecation-condition-name condition)
            (deprecation-condition-software condition)
            (deprecation-condition-version condition)
            (deprecation-condition-replacements condition)
            stream)))
    (if *print-escape*
        (print-unreadable-object (condition stream :type t)
          (print-it stream))
        (print-it stream))))

(macrolet ((define-deprecation-warning
               (name superclass check-runtime-error format-string
                &optional documentation)
             `(progn
                (define-condition ,name (,superclass deprecation-condition)
                  ()
                  ,@(when documentation
                      `((:documentation ,documentation))))

                (defmethod print-object :after ((condition ,name) stream)
                  (when (and (not *print-escape*)
                             ,@(when check-runtime-error
                                `((deprecation-condition-runtime-error condition))))
                    (format stream ,format-string
                            (deprecation-condition-software condition)
                            (deprecation-condition-name condition)))))))

  ;; These conditions must not occur in self-build!
  (define-deprecation-warning early-deprecation-warning style-warning nil
     "~%~@<~:@_In future~@[ ~A~] versions ~
      ~/sb-ext:print-symbol-with-prefix/ will signal a full warning ~
      at compile-time.~:@>"
    "This warning is signaled when the use of a variable,
function, type, etc. in :EARLY deprecation is detected at
compile-time. The use will work at run-time with no warning or
error.")

  (define-deprecation-warning late-deprecation-warning warning t
     "~%~@<~:@_In future~@[ ~A~] versions ~
      ~/sb-ext:print-symbol-with-prefix/ will signal a runtime ~
      error.~:@>"
    "This warning is signaled when the use of a variable,
function, type, etc. in :LATE deprecation is detected at
compile-time. The use will work at run-time with no warning or
error.")

  (define-deprecation-warning final-deprecation-warning warning t
     "~%~@<~:@_~*An error will be signaled at runtime for ~
      ~/sb-ext:print-symbol-with-prefix/.~:@>"
    "This warning is signaled when the use of a variable,
function, type, etc. in :FINAL deprecation is detected at
compile-time. An error will be signaled at run-time."))

(define-condition deprecation-error (error deprecation-condition)
  ()
  (:documentation
   "This error is signaled at run-time when an attempt is made to use
a thing that is in :FINAL deprecation, i.e. call a function or access
a variable."))

;;;; restart definitions

(define-condition abort-failure (control-error) ()
  (:report
   "An ABORT restart was found that failed to transfer control dynamically."))

(defun abort (&optional condition)
  "Transfer control to a restart named ABORT, signalling a CONTROL-ERROR if
   none exists."
  (invoke-restart (find-restart-or-control-error 'abort condition))
  ;; ABORT signals an error in case there was a restart named ABORT
  ;; that did not transfer control dynamically. This could happen with
  ;; RESTART-BIND.
  (error 'abort-failure))

(defun muffle-warning (&optional condition)
  "Transfer control to a restart named MUFFLE-WARNING, signalling a
   CONTROL-ERROR if none exists."
  (invoke-restart (find-restart-or-control-error 'muffle-warning condition)))

(defun try-restart (name condition &rest arguments)
  (let ((restart (find-restart name condition)))
    (when restart
      (apply #'invoke-restart restart arguments))))

(macrolet ((define-nil-returning-restart (name args doc)
             `(defun ,name (,@args &optional condition)
                ,doc
                (try-restart ',name condition ,@args))))
  (define-nil-returning-restart continue ()
    "Transfer control to a restart named CONTINUE, or return NIL if none exists.")
  (define-nil-returning-restart store-value (value)
    "Transfer control and VALUE to a restart named STORE-VALUE, or
return NIL if none exists.")
  (define-nil-returning-restart use-value (value)
    "Transfer control and VALUE to a restart named USE-VALUE, or
return NIL if none exists.")
  (define-nil-returning-restart print-unreadably ()
    "Transfer control to a restart named SB-EXT:PRINT-UNREADABLY, or
return NIL if none exists."))

;;; single-stepping restarts

(macrolet ((def (name doc)
               `(defun ,name (condition)
                 ,doc
                 (invoke-restart (find-restart-or-control-error ',name condition)))))
  (def step-continue
      "Transfers control to the STEP-CONTINUE restart associated with
the condition, continuing execution without stepping. Signals a
CONTROL-ERROR if the restart does not exist.")
  (def step-next
      "Transfers control to the STEP-NEXT restart associated with the
condition, executing the current form without stepping and continuing
stepping with the next form. Signals CONTROL-ERROR if the restart does
not exist.")
  (def step-into
      "Transfers control to the STEP-INTO restart associated with the
condition, stepping into the current form. Signals a CONTROL-ERROR if
the restart does not exist."))

;;; Compiler macro magic

(define-condition compiler-macro-keyword-problem ()
  ((argument :initarg :argument :reader compiler-macro-keyword-argument))
  (:report (lambda (condition stream)
             (format stream "~@<Argument ~S in keyword position is not ~
                             a self-evaluating symbol, preventing compiler-macro ~
                             expansion.~@:>"
                     (compiler-macro-keyword-argument condition)))))

;; After (or if) we deem this the optimal name for this condition,
;; it should be exported from SB-EXT so that people can muffle it.
(define-condition sb-c:inlining-dependency-failure (simple-style-warning) ())


(define-condition layout-invalid (type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "~@<invalid structure layout: ~
              ~2I~_A test for class ~4I~_~S ~
              ~2I~_was passed the obsolete instance ~4I~_~S~:>"
             (classoid-proper-name (type-error-expected-type condition))
             (type-error-datum condition)))))

(define-condition case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   ;; This is an internal symbol of SB-KERNEL, so I can't imagine that anyone
   ;; expects an invariant that it be a list.
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:report
   (lambda (condition stream)
     (let ((possibilities (case-failure-possibilities condition)))
       (if (symbolp possibilities)
           (report-general-type-error condition stream)
           (let ((*print-escape* t))
             (format stream "~@<~S fell through ~S expression.~@[ ~
                      ~:_Wanted one of (~/pprint-fill/).~]~:>"
                     (type-error-datum condition)
                     (case-failure-name condition)
                     (case-failure-possibilities condition))))))))

(define-condition compiled-program-error (program-error)
  ((message :initarg :message :reader program-error-message)
   (source :initarg :source :reader program-error-source))
  (:report (lambda (condition stream)
             (format stream "Execution of a form compiled with errors.~%~
                             Form:~%  ~A~%~
                             Compile-time error:~%  ~A"
                       (program-error-source condition)
                       (program-error-message condition)))))

(define-condition simple-control-error (simple-condition control-error) ())

(define-condition simple-file-error (simple-condition file-error)
  ((message :initarg :message :reader simple-file-error-message :initform nil))
  (:report
   (lambda (condition stream)
     (format stream "~@<~?~@[: ~2I~_~A~]~@:>"
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)
             (simple-file-error-message condition)))))

(defun %file-error (pathname &optional datum &rest arguments)
  (typecase datum
    (format-control (error 'simple-file-error :pathname pathname
                                              :format-control datum
                                              :format-arguments arguments))
    (t (apply #'error datum :pathname pathname arguments))))

(define-condition file-exists (simple-file-error) ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The file ~S already exists~@[: ~2I~_~A~]~@:>"
             (file-error-pathname condition)
             (simple-file-error-message condition)))))

(define-condition file-does-not-exist (simple-file-error) ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The file ~S does not exist~@[: ~2I~_~A~]~@:>"
             (file-error-pathname condition)
             (simple-file-error-message condition)))))

(define-condition delete-file-error (simple-file-error) ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not delete the file ~S~@[: ~2I~_~A~]~@:>"
             (file-error-pathname condition)
             (simple-file-error-message condition)))))

(define-condition simple-stream-error (simple-condition stream-error) ())
(define-condition simple-parse-error  (simple-condition parse-error)  ())

(define-condition broken-pipe (simple-stream-error) ())

(define-condition character-coding-error (error)
  ((external-format :initarg :external-format :reader character-coding-error-external-format)))
(define-condition character-encoding-error (character-coding-error)
  ((code :initarg :code :reader character-encoding-error-code)))
(define-condition character-decoding-error (character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets)))
(define-condition stream-encoding-error (stream-error character-encoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (code (character-encoding-error-code c)))
       (format s "~@<~S stream encoding error on ~S: ~2I~_~
                  the character with code ~D cannot be encoded.~@:>"
               (character-coding-error-external-format c)
               stream
               code)))))
(define-condition stream-decoding-error (stream-error character-decoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (octets (character-decoding-error-octets c)))
       (format s "~@<~S stream decoding error on ~S: ~2I~_~
                  the octet sequence ~S cannot be decoded.~@:>"
               (character-coding-error-external-format c)
               stream
               octets)))))

(define-condition c-string-encoding-error (character-encoding-error)
  ()
  (:report
   (lambda (c s)
     (format s "~@<~S c-string encoding error: ~2I~_~
                  the character with code ~D cannot be encoded.~@:>"
               (character-coding-error-external-format c)
               (character-encoding-error-code c)))))

(define-condition c-string-decoding-error (character-decoding-error)
  ()
  (:report
   (lambda (c s)
     (format s "~@<~S c-string decoding error: ~2I~_~
                  the octet sequence ~S cannot be decoded.~@:>"
             (character-coding-error-external-format c)
             (character-decoding-error-octets c)))))


(define-condition stack-allocated-object-overflows-stack (storage-condition)
  ((size :initarg :size :reader stack-allocated-object-overflows-stack-size))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<Stack allocating object of size ~D bytes exceeds the ~
remaining space left on the control stack.~@:>"
             (stack-allocated-object-overflows-stack-size condition)))))

(define-condition control-stack-exhausted (storage-condition)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream
              ;; no pretty-printing, because that would use a lot of stack.
              "Control stack exhausted (no more space for function call frames).
This is probably due to heavily nested or infinitely recursive function
calls, or a tail call that SBCL cannot or has not optimized away.

PROCEED WITH CAUTION."))))

(define-condition binding-stack-exhausted (storage-condition)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream
              ;; no pretty-printing, because that would use a lot of stack.
              "Binding stack exhausted.

PROCEED WITH CAUTION."))))

(define-condition alien-stack-exhausted (storage-condition)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream
              ;; no pretty-printing, because that would use a lot of stack.
              "Alien stack exhausted.

PROCEED WITH CAUTION."))))

(define-condition heap-exhausted-error (storage-condition)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (declare (special *heap-exhausted-error-available-bytes*
                       *heap-exhausted-error-requested-bytes*))
     ;; See comments in interr.lisp -- there is a method to this madness.
     (if (and (boundp '*heap-exhausted-error-available-bytes*)
              (boundp '*heap-exhausted-error-requested-bytes*))
         (format stream
                 ;; no pretty-printing, because that will use a lot of heap.
                 "Heap exhausted (no more space for allocation).
~D bytes available, ~D requested.

PROCEED WITH CAUTION."
                 *heap-exhausted-error-available-bytes*
                 *heap-exhausted-error-requested-bytes*)
         (format stream
                 "A ~S condition without bindings for heap statistics.  (If
you did not expect to see this message, please report it."
                 'heap-exhausted-error)))))

(define-condition system-condition (condition)
  ((address :initarg :address :reader system-condition-address :initform nil)
   (context :initarg :context :reader system-condition-context :initform nil)))

(define-condition breakpoint-error (system-condition error) ()
  (:report
   (lambda (condition stream)
     (format stream "Unhandled breakpoint/trap at #x~X."
             (system-condition-address condition)))))

(define-condition interactive-interrupt (system-condition serious-condition) ()
  (:report
   (lambda (condition stream)
     (format stream "Interactive interrupt at #x~X."
             (system-condition-address condition)))))


;;;; Condition reporting:

;;; FIXME: ANSI's definition of DEFINE-CONDITION says
;;;   Condition reporting is mediated through the PRINT-OBJECT method
;;;   for the condition type in question, with *PRINT-ESCAPE* always
;;;   being nil. Specifying (:REPORT REPORT-NAME) in the definition of
;;;   a condition type C is equivalent to:
;;;     (defmethod print-object ((x c) stream)
;;;       (if *print-escape* (call-next-method) (report-name x stream)))
;;; The current code doesn't seem to quite match that.
(defmethod print-object ((object condition) stream)
  (declare (notinline classoid-of)) ; to avoid can't inline warning. speed irrelevant here
  (cond
    ((not *print-escape*)
     ;; KLUDGE: A comment from CMU CL here said
     ;;   7/13/98 BUG? CPL is not sorted and results here depend on order of
     ;;   superclasses in define-condition call!
     (funcall (or (some #'condition-classoid-report
                        (condition-classoid-cpl (classoid-of object)))
                  (error "no REPORT? shouldn't happen!"))
              object stream))
    ((and (typep object 'simple-condition)
          (condition-slot-value object 'format-control))
     (print-unreadable-object (object stream :type t :identity t)
       (write (simple-condition-format-control object)
              :stream stream :lines 1)))
    (t
     (print-unreadable-object (object stream :type t :identity t)))))


(defun assert-error (assertion &rest rest)
  (let* ((rest rest)
         (n-args-and-values (if (fixnump (car rest))
                                (* (pop rest) 2)
                                0))
         (args-and-values (subseq rest 0 n-args-and-values)))
    (destructuring-bind (&optional places datum &rest arguments)
        (subseq rest n-args-and-values)
      (let ((cond (if datum
                      (apply #'coerce-to-condition
                             datum 'simple-error 'error arguments)
                      (make-condition
                       'simple-error
                       :format-control "~@<The assertion ~S failed~:[.~:; ~
                                           with ~:*~{~S = ~S~^, ~}.~]~:@>"
                       :format-arguments (list assertion args-and-values)))))
        (restart-case
            (error cond)
          (continue ()
            :report (lambda (stream)
                      (format stream "Retry assertion")
                      (if places
                          (format stream " with new value~P for ~{~S~^, ~}."
                                  (length places) places)
                          (format stream ".")))
            nil))))))

;;; READ-EVALUATED-FORM is used as the interactive method for restart cases
;;; setup by the Common Lisp "casing" (e.g., CCASE and CTYPECASE) macros
;;; and by CHECK-TYPE.
(defun read-evaluated-form (&optional (prompt-control nil promptp)
                            &rest prompt-args)
  (apply #'format *query-io*
         (if promptp prompt-control "~&Enter a form to be evaluated: ")
         prompt-args)
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun read-evaluated-form-of-type (type &optional (prompt-control nil promptp)
                                    &rest prompt-args)
  (loop (apply #'format *query-io*
               (if promptp prompt-control "~&Enter a form evaluating to a value of type ~a: ")
               (if promptp prompt-args (list type)))
        (finish-output *query-io*)
        (let ((result (eval (read *query-io*))))
          (when (typep result type)
            (return (list result)))
          (format *query-io* "~s is not of type ~s" result type))))

;;; Same as above but returns multiple values
(defun mv-read-evaluated-form (&optional (prompt-control nil promptp)
                               &rest prompt-args)
  (apply #'format *query-io*
         (if promptp prompt-control "~&Enter a form to be evaluated: ")
         prompt-args)
  (finish-output *query-io*)
  (multiple-value-list (eval (read *query-io*))))

(defun check-type-error (place place-value type &optional type-string)
  (let ((condition
         (make-condition
          'simple-type-error
          :datum place-value
          :expected-type type
          :format-control
          "The value of ~S is ~S, which is not ~:[of type ~S~;~:*~A~]."
          :format-arguments (list place place-value type-string type))))
    (restart-case (error condition)
      (store-value (value)
        :report (lambda (stream)
                  (format stream "Supply a new value for ~S." place))
        :interactive read-evaluated-form
        value))))

(define-error-wrapper etypecase-failure (value keys)
  (error 'case-failure
         :name 'etypecase
         :datum value
         :expected-type (if (symbolp keys) keys `(or ,@keys))
         :possibilities keys))

(define-error-wrapper ecase-failure (value keys)
  ;; inline definition not seen yet. Can't move this file later
  ;; in build because **<foo>-clusters** are needed early.
  (declare (notinline coerce))
  (when (vectorp keys) (setq keys (coerce keys 'list)))
  (error 'case-failure
         :name 'ecase
         :datum value
         :expected-type `(member ,@keys)
         :possibilities keys))

(defun case-body-error (name keyform keyform-value expected-type keys)
  (restart-case
      (error 'case-failure
             :name name
             :datum keyform-value
             :expected-type expected-type
             :possibilities keys)
    (store-value (value)
      :report (lambda (stream)
                (format stream "Supply a new value for ~S." keyform))
      :interactive read-evaluated-form
      value)))
