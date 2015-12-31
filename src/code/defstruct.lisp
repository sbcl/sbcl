;;;; that part of DEFSTRUCT implementation which is needed not just
;;;; in the target Lisp but also in the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(/show0 "code/defstruct.lisp 15")

;;;; getting LAYOUTs

;;; Return the compiler layout for NAME. (The class referred to by
;;; NAME must be a structure-like class.)
(defun compiler-layout-or-lose (name)
  (let ((res (info :type :compiler-layout name)))
    (cond ((not res)
           (error "Class is not yet defined or was undefined: ~S" name))
          ((not (typep (layout-info res) 'defstruct-description))
           (error "Class is not a structure class: ~S" name))
          (t
           (sb!int:check-deprecated-type name)
           res))))

(defun compiler-layout-ready-p (name)
  (let ((layout (info :type :compiler-layout name)))
    (and layout (typep (layout-info layout) 'defstruct-description))))

(sb!xc:defmacro %make-structure-instance-macro (dd slot-specs &rest slot-vars)
  (if (compiler-layout-ready-p (dd-name dd))
      `(truly-the ,(dd-name dd)
                  (%make-structure-instance ,dd ,slot-specs ,@slot-vars))
      ;; Non-toplevel defstructs don't have a layout at compile time,
      ;; so we need to construct the actual function at runtime -- but
      ;; we cache it at the call site, so that we don't perform quite
      ;; so horribly.
      `(let* ((cell (load-time-value (list nil)))
              (fun (car cell)))
         (if (functionp fun)
             (funcall fun ,@slot-vars)
             (funcall (setf (car cell)
                            (%make-structure-instance-allocator ,dd ,slot-specs))
                      ,@slot-vars)))))

(declaim (ftype (sfunction (defstruct-description list) function)
                %make-structure-instance-allocator))
(defun %make-structure-instance-allocator (dd slot-specs)
  (let ((vars (make-gensym-list (length slot-specs))))
    (values (compile nil
                     `(lambda (,@vars)
                        (%make-structure-instance-macro ,dd ',slot-specs ,@vars))))))

(defun %make-funcallable-structure-instance-allocator (dd slot-specs)
  (when slot-specs
    (bug "funcallable-structure-instance allocation with slots unimplemented"))
  (let ((name (dd-name dd))
        (length (dd-length dd))
        (nobject (gensym "OBJECT")))
    (values
     (compile nil `(lambda ()
                     (let ((,nobject (%make-funcallable-instance ,length)))
                       (setf (%funcallable-instance-layout ,nobject)
                             (%delayed-get-compiler-layout ,name))
                       ,nobject))))))

;;; Delay looking for compiler-layout until the constructor is being
;;; compiled, since it doesn't exist until after the EVAL-WHEN
;;; (COMPILE) stuff is compiled. (Or, in the oddball case when
;;; DEFSTRUCT is executing in a non-toplevel context, the
;;; compiler-layout still doesn't exist at compilation time, and we
;;; delay still further.)
(sb!xc:defmacro %delayed-get-compiler-layout (name)
  (let ((layout (info :type :compiler-layout name)))
    (cond (layout
           ;; ordinary case: When the DEFSTRUCT is at top level,
           ;; then EVAL-WHEN (COMPILE) stuff will have set up the
           ;; layout for us to use.
           (unless (typep (layout-info layout) 'defstruct-description)
             (error "Class is not a structure class: ~S" name))
           `,layout)
          (t
           ;; KLUDGE: In the case that DEFSTRUCT is not at top-level
           ;; the layout doesn't exist at compile time. In that case
           ;; we laboriously look it up at run time. This code will
           ;; run on every constructor call and will likely be quite
           ;; slow, so if anyone cares about performance of
           ;; non-toplevel DEFSTRUCTs, it should be rewritten to be
           ;; cleverer. -- WHN 2002-10-23
           (sb!c:compiler-notify
            "implementation limitation: ~
             Non-toplevel DEFSTRUCT constructors are slow.")
           (with-unique-names (layout)
             `(let ((,layout (info :type :compiler-layout ',name)))
                (unless (typep (layout-info ,layout) 'defstruct-description)
                  (error "Class is not a structure class: ~S" ',name))
                ,layout))))))

;;; re. %DELAYED-GET-COMPILER-LAYOUT and COMPILE-TIME-FIND-LAYOUT, above..
;;;
;;; FIXME: Perhaps both should be defined with DEFMACRO-MUNDANELY?
;;; FIXME: Do we really need both? If so, their names and implementations
;;; should probably be tweaked to be more parallel.

;;;; DEFSTRUCT-DESCRIPTION

;;; The DEFSTRUCT-DESCRIPTION structure holds compile-time information
;;; about a structure type.
;;; Its definition occurs in 'early-classoid.lisp'
(def!method print-object ((x defstruct-description) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (prin1 (dd-name x) stream)))

;;; Does DD describe a structure with a class?
(defun dd-class-p (dd)
  (if (member (dd-type dd) '(structure funcallable-structure)) t nil))

(defun dd-layout-or-lose (dd)
  (compiler-layout-or-lose (dd-name dd)))

;;;; DEFSTRUCT-SLOT-DESCRIPTION

;;; A DEFSTRUCT-SLOT-DESCRIPTION holds compile-time information about
;;; a structure slot.
(def!struct (defstruct-slot-description
             (:make-load-form-fun just-dump-it-normally)
             (:conc-name dsd-)
             (:copier nil)
             #-sb-xc-host (:pure t))
  ;; name of slot
  name
  ;; its position in the implementation sequence
  (index (missing-arg) :type fixnum)
  ;; the name of the accessor function
  ;;
  ;; (CMU CL had extra complexity here ("..or NIL if this accessor has
  ;; the same name as an inherited accessor (which we don't want to
  ;; shadow)") but that behavior doesn't seem to be specified by (or
  ;; even particularly consistent with) ANSI, so it's gone in SBCL.)
  (accessor-name nil :type symbol)
  default                       ; default value expression
  (type t)                      ; declared type specifier
  (safe-p t :type boolean)      ; whether the slot is known to be
                                ; always of the specified type
  ;; Index into *RAW-SLOT-DATA* vector of the RAW-SLOT-DATA for this slot.
  ;; The index is -1 if this slot is not raw.
  (%raw-type -1 :type (integer -1 (#.(length *raw-slot-data*))))
  (read-only nil :type (member t nil)))
#!-sb-fluid (declaim (freeze-type defstruct-slot-description))
(def!method print-object ((x defstruct-slot-description) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (dsd-name x) stream)))
(defun dsd-raw-slot-data (dsd)
  (let ((type-index (dsd-%raw-type dsd)))
    (and (>= type-index 0)
         (svref *raw-slot-data* type-index))))
(defun dsd-raw-type (dsd)
  (acond ((dsd-raw-slot-data dsd) (raw-slot-data-raw-type it))
         (t)))

;;;; typed (non-class) structures

;;; Return a type specifier we can use for testing :TYPE'd structures.
(defun dd-lisp-type (defstruct)
  (ecase (dd-type defstruct)
    (list 'list)
    (vector `(simple-array ,(dd-element-type defstruct) (*)))))

;;;; shared machinery for inline and out-of-line slot accessor functions

;;; Classic comment preserved for entertainment value:
;;;
;;; "A lie can travel halfway round the world while the truth is
;;; putting on its shoes." -- Mark Twain

;;;; the legendary DEFSTRUCT macro itself (both CL:DEFSTRUCT and its
;;;; close personal friend SB!XC:DEFSTRUCT)

(sb!xc:defmacro delay-defstruct-functions (name &rest forms)
  ;; KLUDGE: If DEFSTRUCT is not at the top-level,
  ;; (typep x 'name) and similar forms can't get optimized
  ;; and produce style-warnings for unknown types.
  (let ((forms (cons 'progn forms)))
    (if (compiler-layout-ready-p name)
        forms
        `(eval ',forms))))

(defun %defstruct-package-locks (dd)
  (let ((name (dd-name dd)))
    #+sb-xc-host (declare (ignore name))
    (with-single-package-locked-error
        (:symbol name "defining ~S as a structure"))
    (awhen (dd-predicate-name dd)
      (with-single-package-locked-error
          (:symbol it "defining ~s as a predicate for ~s structure" name)))
    (awhen (dd-copier-name dd)
      (with-single-package-locked-error
          (:symbol it "defining ~s as a copier for ~s structure" name)))
    #-sb-xc-host ; does nothing anyway except warn about non-use of CTOR
    (dolist (ctor (dd-constructors dd))
      (with-single-package-locked-error
          (:symbol (car ctor) "defining ~s as a constructor for ~s structure" name)))
    (dolist (dsd (dd-slots dd))
      (awhen (dsd-accessor-name dsd)
        (with-single-package-locked-error
            (:symbol it "defining ~s as an accessor for ~s structure" name))))))

;;; shared logic for host macroexpansion for SB!XC:DEFSTRUCT and
;;; cross-compiler macroexpansion for CL:DEFSTRUCT
;;; This monster has exactly one inline use in the final image,
;;; and we can drop the definition.
(declaim (inline !expander-for-defstruct))
(defun !expander-for-defstruct (null-env-p name-and-options slot-descriptions
                                expanding-into-code-for)
  (let* ((dd (parse-defstruct null-env-p name-and-options slot-descriptions))
         (name (dd-name dd))
         (inherits
          (if (dd-class-p dd)
              #+sb-xc-host (!inherits-for-structure dd)
              #-sb-xc-host
              (let ((super (compiler-layout-or-lose (or (first (dd-include dd))
                                                        'structure-object))))
                (concatenate 'simple-vector
                             (layout-inherits super) (vector super)))))
         (boa-constructors (member-if #'listp (dd-constructors dd) :key #'cdr))
         (keyword-constructors (ldiff (dd-constructors dd) boa-constructors))
         (constructor-definitions
          (nconc
           (when keyword-constructors
             (let ((primary (caar keyword-constructors)))
               (cons
                `(defun ,primary ,@(structure-ctor-lambda-parts dd :default))
                ;; Quasi-bogus: not all the right effects on globaldb
                ;; happen by defining functions this cheating way.
                (mapcar (lambda (other)
                          `(setf (fdefinition ',(car other))
                                 (fdefinition ',primary)))
                        (rest keyword-constructors)))))
           (mapcar (lambda (ctor)
                     `(defun ,(car ctor)
                             ,@(structure-ctor-lambda-parts dd (cdr ctor))))
                   boa-constructors)))
         (print-method
          (when (dd-print-option dd)
            (let* ((x (sb!xc:gensym "OBJECT"))
                   (s (sb!xc:gensym "STREAM"))
                   (fname (dd-printer-fname dd))
                   (depthp (eq (dd-print-option dd) :print-function)))
              ;; Giving empty :PRINT-OBJECT or :PRINT-FUNCTION options
              ;; leaves FNAME eq to NIL. The user-level effect is
              ;; to generate a PRINT-OBJECT method specialized for the type,
              ;; implementing the default #S structure-printing behavior.
              (cond ((not fname)
                     (setf fname 'default-structure-print depthp t))
                    ((not (symbolp fname))
                     ;; Don't dump the source form into the DD constant;
                     ;; just indicate that there was an expression there.
                     (setf (dd-printer-fname dd) t)))
              `((sb!xc:defmethod print-object ((,x ,name) ,s)
                  (funcall #',fname ,x ,s
                           ,@(if depthp `(*current-level-in-print*)))))))))
    ;; Return a list of forms and the DD-NAME.
    (values
     (if (dd-class-p dd)
         `(,@(when (eq expanding-into-code-for :target)
               ;; Note we intentionally enforce package locks, calling
               ;; %DEFSTRUCT first. %DEFSTRUCT has the tests (and resulting
               ;; CERROR) for collisions with LAYOUTs which already exist in
               ;; the runtime. If there are collisions, we want the user's
               ;; response to CERROR to control what happens. If the ABORT
               ;; restart is chosen, %COMPILER-DEFSTRUCT should not modify
               ;; the definition the class.
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (%defstruct-package-locks ',dd))))
           (%defstruct ',dd ',inherits (sb!c:source-location))
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (%compiler-defstruct ',dd ',inherits))
           ,@(when (eq expanding-into-code-for :target)
               `((delay-defstruct-functions
                  ,name
                  ,@(awhen (dd-copier-name dd)
                      `((defun ,(dd-copier-name dd) (instance)
                          (copy-structure (the ,(dd-name dd) instance)))))
                  ,@(awhen (dd-predicate-name dd)
                      `((defun ,(dd-predicate-name dd) (object)
                          (typep object ',(dd-name dd)))))
                  ,@(accessor-definitions dd))
                 ;; This must be in the same lexical environment
                 ,@constructor-definitions
                 ,@print-method
                 ;; Various other operations only make sense on the target SBCL.
                 (%target-defstruct ',dd))))
         ;; Not DD-CLASS-P
         ;; FIXME: missing package lock checks
         `((eval-when (:compile-toplevel :load-toplevel :execute)
             (%proclaim-defstruct-ctors ',dd)
             (setf (info :typed-structure :info ',name) ',dd))
           (setf (info :source-location :typed-structure ',name)
                 (sb!c:source-location))
           ,@(when (eq expanding-into-code-for :target)
               `(,@(typed-accessor-definitions dd)
                 ,@(typed-predicate-definitions dd)
                 ,@(typed-copier-definitions dd)
                 ,@constructor-definitions
                 ,@(when (dd-doc dd)
                     `((setf (fdocumentation ',(dd-name dd) 'structure)
                             ',(dd-doc dd))))))))
     name)))

#+sb-xc-host
(sb!xc:defmacro defstruct (name-and-options &rest slot-descriptions)
  ;; All defstructs are toplevel in SBCL's own source code,
  ;; so pass T for null-lexenv.
  `(progn ,@(!expander-for-defstruct
             t name-and-options slot-descriptions :target)))

#+sb-xc
(sb!xc:defmacro defstruct (name-and-options &rest slot-descriptions
                           &environment env)
  #!+sb-doc
  "DEFSTRUCT {Name | (Name Option*)} [Documentation] {Slot | (Slot [Default] {Key Value}*)}
   Define the structure type Name. Instances are created by MAKE-<name>,
   which takes &KEY arguments allowing initial slot values to the specified.
   A SETF'able function <name>-<slot> is defined for each slot to read and
   write slot values. <name>-p is a type predicate.

   Popular DEFSTRUCT options (see manual for others):

   (:CONSTRUCTOR Name)
   (:PREDICATE Name)
       Specify the name for the constructor or predicate.

   (:CONSTRUCTOR Name Lambda-List)
       Specify the name and arguments for a BOA constructor
       (which is more efficient when keyword syntax isn't necessary.)

   (:INCLUDE Supertype Slot-Spec*)
       Make this type a subtype of the structure type Supertype. The optional
       Slot-Specs override inherited slot options.

   Slot options:

   :TYPE Type-Spec
       Asserts that the value of this slot is always of the specified type.

   :READ-ONLY {T | NIL}
       If true, no setter function is defined for this slot."
  (multiple-value-bind (forms name)
      (!expander-for-defstruct
       (etypecase env
         (sb!kernel:lexenv (sb!c::null-lexenv-p env))
         ;; a LOCALLY environment would be fine,
         ;; but is not an important case to handle.
         #!+sb-fasteval (sb!interpreter:basic-env nil)
         (null t))
       name-and-options slot-descriptions :target)
    `(progn ,@forms ',name)))

#+sb-xc-host
(defmacro sb!xc:defstruct (name-and-options &rest slot-descriptions)
  #!+sb-doc
  "Cause information about a target structure to be built into the
  cross-compiler."
  `(progn ,@(!expander-for-defstruct
             t name-and-options slot-descriptions :host)))

;;;; functions to generate code for various parts of DEFSTRUCT definitions

;;; First, a helper to determine whether a name names an inherited
;;; accessor.
(defun accessor-inherited-data (name defstruct)
  (assoc name (dd-inherited-accessor-alist defstruct) :test #'eq))

;;; Return a list of forms which create a predicate function for a
;;; typed DEFSTRUCT.
(defun typed-predicate-definitions (defstruct)
  (let ((name (dd-name defstruct))
        (predicate-name (dd-predicate-name defstruct))
        (argname 'x)) ; KISS: no user code appears in the DEFUN
    (when predicate-name
      (aver (dd-named defstruct))
      (let ((ltype (dd-lisp-type defstruct))
            (name-index (cdr (car (last (find-name-indices defstruct))))))
        `((defun ,predicate-name (,argname)
            (and (typep ,argname ',ltype)
                 ,(cond
                   ((subtypep ltype 'list)
                     `(do ((head (the ,ltype ,argname) (cdr head))
                           (i 0 (1+ i)))
                          ((or (not (consp head)) (= i ,name-index))
                           (and (consp head) (eq ',name (car head))))))
                   ((subtypep ltype 'vector)
                    `(and (>= (length (the ,ltype ,argname))
                           ,(dd-length defstruct))
                          (eq ',name (aref (the ,ltype ,argname) ,name-index))))
                   (t (bug "Uncatered-for lisp type in typed DEFSTRUCT: ~S."
                           ltype))))))))))

;;; Return a list of forms to create a copier function of a typed DEFSTRUCT.
(defun typed-copier-definitions (defstruct)
  (when (dd-copier-name defstruct)
    `((setf (fdefinition ',(dd-copier-name defstruct)) #'copy-seq)
      (declaim (ftype function ,(dd-copier-name defstruct))))))

;;; Return a list of function definitions for accessing and setting
;;; the slots of a typed DEFSTRUCT. The functions are proclaimed to be
;;; inline, and the types of their arguments and results are declared
;;; as well. We count on the compiler to do clever things with ELT.
(defun typed-accessor-definitions (defstruct)
  (collect ((stuff))
    (let ((ltype (dd-lisp-type defstruct)))
      (dolist (slot (dd-slots defstruct))
        (let ((name (dsd-accessor-name slot))
              (index (dsd-index slot))
              (new-value '(value))
              (slot-type `(and ,(dsd-type slot)
                               ,(dd-element-type defstruct))))
          (let ((inherited (accessor-inherited-data name defstruct)))
            (cond
              ((not inherited)
               (stuff `(declaim (inline ,name ,@(unless (dsd-read-only slot)
                                                        `((setf ,name))))))
               (stuff `(defun ,name (structure)
                        (declare (type ,ltype structure))
                        (the ,slot-type (elt structure ,index))))
               (unless (dsd-read-only slot)
                 (stuff
                  `(defun (setf ,name) (,(car new-value) structure)
                    (declare (type ,ltype structure) (type ,slot-type . ,new-value))
                    (setf (elt structure ,index) . ,new-value)))))
              ((not (= (cdr inherited) index))
               (style-warn "~@<Non-overwritten accessor ~S does not access ~
                            slot with name ~S (accessing an inherited slot ~
                            instead).~:@>" name (dsd-name slot))))))))
    (stuff)))

;;;; parsing

;;; CLHS says that
;;;   A defstruct option can be either a keyword or a list of a keyword
;;;   and arguments for that keyword; specifying the keyword by itself is
;;;   equivalent to specifying a list consisting of the keyword
;;;   and no arguments.
;;; It is unclear whether that is meant to imply that any of the keywords
;;; may be present in their atom form, or only if the grammar at the top
;;; shows the atom form does <atom> have the meaning of (<atom>).
;;; At least one other implementation accepts :NAMED as a singleton list.
;;  We take a more rigid view that the depicted grammar is exhaustive.
;;;
(defconstant-eqx +dd-option-names+
    ;; Each keyword, except :CONSTRUCTOR which may appear more than once,
    ;; and :NAMED which is trivial, and unambiguous if present more than
    ;; once, though possibly worth a style-warning.
    #(:include        ; at least 1 argument
      :initial-offset ; exactly 1 argument
      :pure           ; exactly 1 argument [nonstandard]
      :type           ; exactly 1 argument
      :conc-name      ; 0 or 1 arg
      :copier         ; "
      :predicate      ; "
      :print-function ; "
      :print-object)  ; "
  #'equalp)

;;; Parse a single DEFSTRUCT option and store the results in DD.
(defun parse-1-dd-option (option dd seen-options)
  (let* ((keyword (first option))
         (bit (position keyword +dd-option-names+))
         (args (rest option))
         (arg-p (consp args))
         (arg (if arg-p (car args)))
         (name (dd-name dd)))
    (declare (type (unsigned-byte 9) seen-options)) ; mask over DD-OPTION-NAMES
    (when bit
      (if (logbitp bit seen-options)
          (error "More than one ~S option is not allowed" keyword)
          (setf seen-options (logior seen-options (ash 1 bit))))
      (multiple-value-bind (syntax-group winp)
          (cond ; Perform checking per comment at +DD-OPTION-NAMES+.
            ((= bit 0) (values 0 (and arg-p (proper-list-p args)))) ; >1 arg
            ((< bit 4) (values 1 (and arg-p (not (cdr args))))) ; exactly 1
            (t         (values 2 (or (not args) (singleton-p args))))) ; 0 or 1
        (unless winp
          (if (proper-list-p option)
              (error "DEFSTRUCT option ~S ~[requires at least~;~
requires exactly~;accepts at most~] one argument" keyword syntax-group)
              (error "Invalid syntax in DEFSTRUCT option ~S" option)))))
    (case keyword
      (:conc-name
       ;; unlike (:predicate) and (:copier) which mean "yes" if supplied
       ;; without their argument, (:conc-name) and :conc-name mean no conc-name.
       ;; Also note a subtle difference in :conc-name "" vs :conc-name NIL.
       ;; The former re-interns each slot name into *PACKAGE* which might
       ;; not be the same as using the given name directly as an accessor.
       (setf (dd-conc-name dd) (if arg (string arg))))
      (:constructor ; takes 0 to 2 arguments.
       (destructuring-bind (&optional (cname (symbolicate "MAKE-" name))
                                      (lambda-list nil ll-supplied-p)) args
         (setf lambda-list
               (cond ((not cname)
                      ;; Implementations disagree on the meaning of
                      ;;   (:CONSTRUCTOR NIL (A B C)).
                      ;; The choices seem to be: don't define a constructor,
                      ;; define a constructor named NIL, signal a user error,
                      ;; or crash the system itself. The spec implies
                      ;; the behavior that we have, but at least a
                      ;; style-warning seems appropriate.
                      (when ll-supplied-p
                        (style-warn "~S does not define a constructor" option)))
                     ((not ll-supplied-p) :default)
                     (t
                      (multiple-value-call
                         (lambda (&rest x)
                          (declare (dynamic-extent x))
                          (subseq x 0 ; remove trailing NILs
                                  (1+ (position-if #'identity x :from-end t))))
                       (parse-lambda-list
                        lambda-list
                        :accept (lambda-list-keyword-mask
                                 '(&optional &rest &key &allow-other-keys &aux))
                        :silent t))))
               (dd-constructors dd) ; preserve order, just because
               (nconc (dd-constructors dd) (list (cons cname lambda-list))))))
      (:copier
       (setf (dd-copier-name dd) (if arg-p arg (symbolicate "COPY-" name))))
      (:predicate
       (setf (dd-predicate-name dd) (if arg-p arg (symbolicate name "-P"))))
      (:include
       (setf (dd-include dd) args))
      ((:print-function :print-object)
       (when (dd-print-option dd)
         (error "~S and ~S may not both be specified"
                (dd-print-option dd) keyword))
       (setf (dd-print-option dd) keyword (dd-printer-fname dd) arg))
      (:type
       (cond ((member arg '(list vector))
              (setf (dd-type dd) arg (dd-element-type dd) t))
             ((and (listp arg) (eq (first arg) 'vector))
              (destructuring-bind (elt-type) (cdr arg)
                (setf (dd-type dd) 'vector (dd-element-type dd) elt-type)))
             (t
              (error "~S is a bad :TYPE for DEFSTRUCT." arg))))
      (:named
       (error "The DEFSTRUCT option :NAMED takes no arguments."))
      (:initial-offset
       (setf (dd-offset dd) arg)) ; FIXME: disallow (:INITIAL-OFFSET NIL)
      (:pure
       (setf (dd-pure dd) arg))
      (t
       (error "unknown DEFSTRUCT option:~%  ~S" option)))
    seen-options))

;;; Parse OPTIONS into the given DD.
(defun parse-defstruct-options (options dd)
  (let ((seen-options 0)
        (named-p nil))
    (dolist (option options)
      (if (eq option :named)
          (setf named-p t (dd-named dd) t)
          (setq seen-options
                (parse-1-dd-option
                 (cond ((consp option) option)
                       ((member option
                                '(:conc-name :constructor :copier :predicate))
                        (list option))
                       (t
                        ;; FIXME: ugly message (defstruct (s :include) a)
                        ;; saying "unrecognized" when it means "bad syntax"
                        (error "unrecognized DEFSTRUCT option: ~S" option)))
                 dd seen-options))))
    (case (dd-type dd)
      (structure
       (when (dd-offset dd)
         (error ":OFFSET can't be specified unless :TYPE is specified."))
       (unless (dd-include dd)
         ;; FIXME: It'd be cleaner to treat no-:INCLUDE as defaulting
         ;; to :INCLUDE STRUCTURE-OBJECT, and then let the general-case
         ;; (INCF (DD-LENGTH DD) (DD-LENGTH included-DD)) logic take
         ;; care of this. (Except that the :TYPE VECTOR and :TYPE
         ;; LIST cases, with their :NAMED and un-:NAMED flavors,
         ;; make that messy, alas.)
         (incf (dd-length dd))))
      (t
       ;; In case we are here, :TYPE is specified.
       (if named-p
           ;; CLHS - "The structure can be :named only if the type SYMBOL
           ;; is a subtype of the supplied element-type."
           (multiple-value-bind (winp certainp)
               (subtypep 'symbol (dd-element-type dd))
             (when (and (not winp) certainp)
               (error ":NAMED option is incompatible with element type ~S"
                      (dd-element-type dd))))
           (when (dd-predicate-name dd)
             (error ":PREDICATE cannot be used with :TYPE ~
unless :NAMED is also specified.")))
       (awhen (dd-print-option dd)
         (error ":TYPE option precludes specification of ~S option" it))
       (when named-p
         (incf (dd-length dd)))
       (let ((offset (dd-offset dd)))
         (when offset (incf (dd-length dd) offset)))))

    (let ((name (dd-name dd)))
      (collect ((keyword-ctors) (boa-ctors))
        (let (no-constructors)
          (dolist (constructor (dd-constructors dd))
            (destructuring-bind (ctor-name . ll) constructor
              (cond ((not ctor-name) (setq no-constructors t))
                    ((eq ll :default) (keyword-ctors constructor))
                    (t (boa-ctors constructor)))))
          ;; Remove (NIL) and sort so that BOA constructors are last.
          (setf (dd-constructors dd)
                (if no-constructors
                    (progn
                      (when (or (keyword-ctors) (boa-ctors))
                        (error "(:CONSTRUCTOR NIL) combined with other :CONSTRUCTORs"))
                      nil)
                    (append (or (keyword-ctors)
                                (unless (boa-ctors)
                                  `((,(symbolicate "MAKE-" name) . :default))))
                            (boa-ctors))))))

      (flet ((option-present-p (bit-name)
               (logbitp (position bit-name +dd-option-names+) seen-options)))
        (declare (inline option-present-p))
        (when (and (not (option-present-p :predicate))
                   (or (dd-class-p dd) named-p))
          (setf (dd-predicate-name dd) (symbolicate name "-P")))
        (unless (option-present-p :conc-name)
          (setf (dd-conc-name dd) (concatenate 'string (string name) "-")))
        (unless (option-present-p :copier)
          (setf (dd-copier-name dd) (symbolicate "COPY-" name))))))
  dd)

;;; BOA constructors is (&aux x), i.e. without the default value, the
;;; value of the slot is unspecified, but it should signal a type
;;; error only when it's accessed. safe-p slot in dsd determines
;;; whether to check the type after accessing the slot.
;;;
;;; This was performed during boa constructor creating, but the
;;; constructors are created after this information is used to inform
;;; the compiler how to treat such slots.
(defun determine-unsafe-slots (dd)
  (dolist (ctor (dd-constructors dd))
    (let ((ll-parts (cdr ctor))) ; = (llks req opt rest key aux)
      (dolist (var (if (listp ll-parts) (sixth ll-parts)))
        (let* ((name (if (listp var) (car var) var))
               (dsd (find name (dd-slots dd)
                          :key #'dsd-name
                          ;; FIXME: Not sure why this is #'EQ.
                          ;; Everywhere else we use STRING=.
                          :test #'eq)))
          (when dsd
            (setf (dsd-safe-p dsd) nil)))))))

;;; Given name and options and slot descriptions (and possibly doc
;;; string at the head of slot descriptions) return a DD holding that
;;; info.
(defun parse-defstruct (null-env-p name-and-options slot-descriptions)
  (binding* (((name options)
              (if (listp name-and-options)
                  (values (car name-and-options) (cdr name-and-options))
                  (values name-and-options nil)))
             (result (make-defstruct-description null-env-p name)))
    (parse-defstruct-options options result)
    (when (dd-include result)
      (frob-dd-inclusion-stuff result))
    (when (stringp (car slot-descriptions))
      (setf (dd-doc result) (pop slot-descriptions)))
    (dolist (slot-description slot-descriptions)
      (allocate-1-slot result (parse-1-dsd result slot-description)))
    (determine-unsafe-slots result)
    result))

;;;; stuff to parse slot descriptions

;;; Parse a slot description for DEFSTRUCT, add it to the description
;;; and return it. If supplied, SLOT is a pre-initialized DSD
;;; that we modify to get the new slot. This is supplied when handling
;;; included slots.
(defun parse-1-dsd (defstruct spec &optional
                    (slot (make-defstruct-slot-description :name ""
                                                           :index 0
                                                           :type t)))
  #-sb-xc-host (declare (muffle-conditions style-warning))
  (multiple-value-bind (name default default-p type type-p read-only ro-p)
      (typecase spec
        (symbol
         (typecase spec
           ((or null (member :conc-name :constructor :copier :predicate :named))
            (warn "slot name of ~S indicates probable syntax error in DEFSTRUCT" spec))
           (keyword
            (style-warn "slot name of ~S indicates possible syntax error in DEFSTRUCT" spec)))
         spec)
        (cons
         (destructuring-bind
               (name &optional (default nil default-p)
                     &key (type nil type-p) (read-only nil ro-p))
             spec
           (when (dd-conc-name defstruct)
             ;; the warning here is useful, but in principle we cannot
             ;; distinguish between legitimate and erroneous use of
             ;; these names when :CONC-NAME is NIL.  In the common
             ;; case (CONC-NAME non-NIL), there are alternative ways
             ;; of writing code with the same effect, so a full
             ;; warning is justified.
             (typecase name
               ((member :conc-name :constructor :copier :predicate :include
                        :print-function :print-object :type :initial-offset :pure)
                (warn "slot name of ~S indicates probable syntax error in DEFSTRUCT" name))))
           (values name default default-p
                   (uncross type) type-p
                   read-only ro-p)))
        (t (error 'simple-program-error
                  :format-control "in DEFSTRUCT, ~S is not a legal slot ~
                                   description."
                  :format-arguments (list spec))))

    (when (find name (dd-slots defstruct)
                :test #'string=
                :key (lambda (x) (symbol-name (dsd-name x))))
      (error 'simple-program-error
             ;; Todo: indicate whether name is a duplicate in the directly
             ;; specified slots vs. exists in the ancestor and so should
             ;; be in the (:include ...) clause instead of where it is.
             :format-control "duplicate slot name ~S"
             :format-arguments (list name)))
    (setf (dsd-name slot) name)
    (setf (dd-slots defstruct) (nconc (dd-slots defstruct) (list slot)))

    (let ((accessor-name (if (dd-conc-name defstruct)
                             (symbolicate (dd-conc-name defstruct) name)
                             name))
          (predicate-name (dd-predicate-name defstruct)))
      (setf (dsd-accessor-name slot) accessor-name)
      (when (eql accessor-name predicate-name)
        ;; Some adventurous soul has named a slot so that its accessor
        ;; collides with the structure type predicate. ANSI doesn't
        ;; specify what to do in this case. As of 2001-09-04, Martin
        ;; Atzmueller reports that CLISP and Lispworks both give
        ;; priority to the slot accessor, so that the predicate is
        ;; overwritten. We might as well do the same (as well as
        ;; signalling a warning).
        (style-warn
         "~@<The structure accessor name ~S is the same as the name of the ~
          structure type predicate. ANSI doesn't specify what to do in ~
          this case. We'll overwrite the type predicate with the slot ~
          accessor, but you can't rely on this behavior, so it'd be wise to ~
          remove the ambiguity in your code.~@:>"
         accessor-name)
        (setf (dd-predicate-name defstruct) nil))
      ;; FIXME: It would be good to check for name collisions here, but
      ;; the easy check,
      ;;x#-sb-xc-host
      ;;x(when (and (fboundp accessor-name)
      ;;x           (not (accessor-inherited-data accessor-name defstruct)))
      ;;x  (style-warn "redefining ~/sb-impl::print-symbol-with-prefix/ ~
      ;;                in DEFSTRUCT" accessor-name)))
      ;; which was done until sbcl-0.8.11.18 or so, is wrong: it causes
      ;; a warning at MACROEXPAND time, when instead the warning should
      ;; occur not just because the code was constructed, but because it
      ;; is actually compiled or loaded.
      )

    (when default-p
      (setf (dsd-default slot) default))
    (when type-p
      (setf (dsd-type slot)
            (if (eq (dsd-type slot) t)
                type
                `(and ,(dsd-type slot) ,type))))
    (when ro-p
      (if read-only
          (setf (dsd-read-only slot) t)
          (when (dsd-read-only slot)
            (error "~@<The slot ~S is :READ-ONLY in superclass, and so must ~
                       be :READ-ONLY in subclass.~:@>"
                   (dsd-name slot)))))
    slot))

;;; When a value of type TYPE is stored in a structure, should it be
;;; stored in a raw slot?  Return the index of the matching RAW-SLOT-DATA
;;; if TYPE should be stored in a raw slot, or NIL if not.
(defun structure-raw-slot-data-index (type)
  (multiple-value-bind (fixnum? fixnum-certain?)
      (sb!xc:subtypep type 'fixnum)
    ;; (The extra test for FIXNUM-CERTAIN? here is intended for
    ;; bootstrapping the system. In particular, in sbcl-0.6.2, we set up
    ;; LAYOUT before FIXNUM is defined, and so could bogusly end up
    ;; putting INDEX-typed values into raw slots if we didn't test
    ;; FIXNUM-CERTAIN?.)
    (if (or fixnum? (not fixnum-certain?))
        nil
        (dotimes (i (length *raw-slot-data*))
          (let ((data (svref *raw-slot-data* i)))
            (when (sb!xc:subtypep type (raw-slot-data-raw-type data))
              (return i)))))))

;;; Allocate storage for a DSD in DD. This is where we decide whether
;;; a slot is raw or not. Raw objects are aligned on the unit of their size.
(defun allocate-1-slot (dd dsd)
  (let ((rsd-index (if (eq (dd-type dd) 'structure)
                       (structure-raw-slot-data-index (dsd-type dsd))
                       nil)))
    (cond ((null rsd-index)
           (setf (dsd-index dsd) (dd-length dd))
           (incf (dd-length dd)))
          (t
           (setf (dsd-%raw-type dsd) rsd-index)
           (let* ((rsd (svref *raw-slot-data* rsd-index))
                  (words (raw-slot-data-n-words rsd))
                  (alignment (raw-slot-data-alignment rsd)))
             #!-interleaved-raw-slots
             (let ((off (rem (dd-raw-length dd) alignment)))
               (unless (zerop off)
                 (incf (dd-raw-length dd) (- alignment off)))
               (setf (dsd-index dsd) (dd-raw-length dd))
               (incf (dd-raw-length dd) words))
             #!+interleaved-raw-slots
             (let ((len (dd-length dd)))
               (setf (dd-length dd)
                     ;; this formula works but can it be made less unclear?
                     (- len (nth-value 1 (ceiling (1- len) alignment))))
               (setf (dsd-index dsd) (dd-length dd))
               (incf (dd-length dd) words))))))
  (values))

(defun typed-structure-info-or-lose (name)
  (or (info :typed-structure :info name)
      (error ":TYPE'd DEFSTRUCT ~S not found for inclusion." name)))

;;; Process any included slots pretty much like they were specified.
;;; Also inherit various other attributes.
(defun frob-dd-inclusion-stuff (dd)
  (destructuring-bind (included-name &rest modified-slots) (dd-include dd)
    (let* ((type (dd-type dd))
           (included-structure
            (if (dd-class-p dd)
                (layout-info (compiler-layout-or-lose included-name))
                (typed-structure-info-or-lose included-name))))

      ;; checks on legality
      (unless (and (eq type (dd-type included-structure))
                   (type= (specifier-type (dd-element-type included-structure))
                          (specifier-type (dd-element-type dd))))
        (error ":TYPE option mismatch between structures ~S and ~S"
               (dd-name dd) included-name))
      (let ((included-classoid (find-classoid included-name nil)))
        (when included-classoid
          ;; It's not particularly well-defined to :INCLUDE any of the
          ;; CMU CL INSTANCE weirdosities like CONDITION or
          ;; GENERIC-FUNCTION, and it's certainly not ANSI-compliant.
          (let* ((included-layout (classoid-layout included-classoid))
                 (included-dd (layout-info included-layout)))
            (when (dd-alternate-metaclass included-dd)
              (error "can't :INCLUDE class ~S (has alternate metaclass)"
                     included-name)))))

      ;; A few more sanity checks: every allegedly modified slot exists
      ;; and no name appears more than once.
      (flet ((included-slot-name (slot-desc)
               (if (atom slot-desc) slot-desc (car slot-desc))))
        (mapl (lambda (slots &aux (name (included-slot-name (car slots))))
                (unless (find name (dd-slots included-structure)
                              :test #'string= :key #'dsd-name)
                  (error 'simple-program-error
                         :format-control "slot name ~S not present in included structure"
                         :format-arguments (list name)))
                (when (find name (cdr slots)
                            :test #'string= :key #'included-slot-name)
                  (error 'simple-program-error
                         :format-control "included slot name ~S specified more than once"
                         :format-arguments (list name))))
              modified-slots))

      (incf (dd-length dd) (dd-length included-structure))
      (when (dd-class-p dd)
        (when (eq (dd-pure dd) :unspecified)
          (setf (dd-pure dd) (dd-pure included-structure)))
        #!-interleaved-raw-slots
        (setf (dd-raw-length dd) (dd-raw-length included-structure)))

      (setf (dd-inherited-accessor-alist dd)
            (dd-inherited-accessor-alist included-structure))
      (dolist (included-slot (dd-slots included-structure))
        (let* ((included-name (dsd-name included-slot))
               (modified (or (find included-name modified-slots
                                   :key (lambda (x) (if (atom x) x (car x)))
                                   :test #'string=)
                             `(,included-name))))
          ;; We stash away an alist of accessors to parents' slots
          ;; that have already been created to avoid conflicts later
          ;; so that structures with :INCLUDE and :CONC-NAME (and
          ;; other edge cases) can work as specified.
          (when (dsd-accessor-name included-slot)
            ;; the "oldest" (i.e. highest up the tree of inheritance)
            ;; will prevail, so don't push new ones on if they
            ;; conflict.
            (pushnew (cons (dsd-accessor-name included-slot)
                           (dsd-index included-slot))
                     (dd-inherited-accessor-alist dd)
                     :test #'eq :key #'car))
          (let ((new-slot (parse-1-dsd dd
                                       modified
                                       (copy-structure included-slot))))
            (when (and (neq (dsd-type new-slot) (dsd-type included-slot))
                       (not (sb!xc:subtypep (dsd-type included-slot)
                                            (dsd-type new-slot)))
                       (dsd-safe-p included-slot))
              (setf (dsd-safe-p new-slot) nil)
              ;; XXX: notify?
              )))))))

;;;; various helper functions for setting up DEFSTRUCTs

;;; This function is called at macroexpand time to compute the INHERITS
;;; vector for a structure type definition.
;;; The cross-compiler is allowed to magically compute LAYOUT-INHERITS.
(defun !inherits-for-structure (info)
  (declare (type defstruct-description info))
  (let* ((include (dd-include info))
         (superclass-opt (dd-alternate-metaclass info))
         (super
          (if include
              (compiler-layout-or-lose (first include))
              (classoid-layout (find-classoid
                                (or (first superclass-opt)
                                    'structure-object))))))
    (case (dd-name info)
      ((ansi-stream)
       ;; STREAM is an abstract class and you can't :include it,
       ;; so the inheritance has to be hardcoded.
       (concatenate 'simple-vector
                    (layout-inherits super)
                    (vector super (classoid-layout (find-classoid 'stream)))))
      ((fd-stream) ; Similarly, FILE-STREAM is abstract
       (concatenate 'simple-vector
                    (layout-inherits super)
                    (vector super
                            (classoid-layout (find-classoid 'file-stream)))))
      ((sb!impl::string-input-stream ; etc
        sb!impl::string-output-stream
        sb!impl::fill-pointer-output-stream)
       (concatenate 'simple-vector
                    (layout-inherits super)
                    (vector super
                            (classoid-layout (find-classoid 'string-stream)))))
      (t (concatenate 'simple-vector
                      (layout-inherits super)
                      (vector super))))))

;;; Do miscellaneous (LOAD EVAL) time actions for the structure
;;; described by DD. Create the class and LAYOUT, checking for
;;; incompatible redefinition.
(defun %defstruct (dd inherits source-location)
  (declare (type defstruct-description dd))

  ;; We set up LAYOUTs even in the cross-compilation host.
  (multiple-value-bind (classoid layout old-layout)
      (ensure-structure-class dd inherits "current" "new")
    (cond ((not old-layout)
           (unless (eq (classoid-layout classoid) layout)
             (register-layout layout)))
          (t
           (%redefine-defstruct classoid old-layout layout)
           (let ((old-dd (layout-info old-layout)))
             (when (defstruct-description-p old-dd)
               (dolist (slot (dd-slots old-dd))
                 (fmakunbound (dsd-accessor-name slot))
                 (unless (dsd-read-only slot)
                   (fmakunbound `(setf ,(dsd-accessor-name slot)))))))
           (setq layout (classoid-layout classoid))))
    (setf (find-classoid (dd-name dd)) classoid)

    (when source-location
      (setf (layout-source-location layout) source-location))))


;;; Return a form accessing the writable place used for the slot
;;; described by DD and DSD in the INSTANCE (a form).
(defun %accessor-place-form (dd dsd instance)
  (let (;; Compute REF even if not using it, as a sanity-check of DD-TYPE.
        (ref (ecase (dd-type dd)
               (structure '%instance-ref)
               (list 'nth)
               (vector 'aref)))
        (index (dsd-index dsd))
        (rsd (dsd-raw-slot-data dsd)))
    (if (not rsd) ; if not raw slot
        (multiple-value-call 'list ref
         (if (eq ref 'nth) (values index instance) (values instance index)))
        `(,(raw-slot-data-accessor-name rsd) ,instance ,index))))

;;; Return the transform of conceptual FUNCTION one of {:READ,:WRITE,:SETF}
;;; as applied to ARGS, given SLOT-KEY which is a cons of a DD and a DSD.
;;; Return NIL on failure.
(defun slot-access-transform (function args slot-key)
  (when (consp args) ; need at least one arg
    (let* ((dd (car slot-key))
           (dsd (cdr slot-key))
           ;; optimistically compute PLACE before checking length of ARGS
           ;; because we expect success, and this unifies the three cases.
           ;; :SETF is like an invocation of the SETF macro - newval is
           ;; the second arg, but :WRITER is #'(SETF fn) - newval is first.
           (place
            (%accessor-place-form
             dd dsd `(the ,(dd-name dd)
                       ,(car (if (eq function :write) (cdr args) args)))))
           (type-spec (dsd-type dsd)))
      (if (eq function :read)
          (when (singleton-p args)
            (if (eq type-spec t)
                place
                `(,(if (dsd-safe-p dsd) 'truly-the 'the) ,type-spec ,place)))
          (when (singleton-p (cdr args))
            (let ((inverse (info :setf :inverse (car place))))
              (flet ((check (newval)
                       (if (eq type-spec t) newval `(the ,type-spec ,newval))))
                (ecase function
                  (:setf
                   ;; Instance setters take newval last, which matches
                   ;; the order in which a use of SETF has them.
                   `(,inverse ,@(cdr place) ,(check (second args))))
                  (:write
                   ;; The call to #'(SETF fn) had newval first.
                   ;; We need to preserve L-to-R evaluation.
                   (once-only ((new (first args)))
                     `(,inverse ,@(cdr place) ,(check new))))))))))))

;;; Return a LAMBDA form which can be used to set a slot
(defun slot-setter-lambda-form (dd dsd)
  `(lambda (newval instance)
     ,(slot-access-transform :setf '(instance newval) (cons dd dsd))))

;;; Blow away all the compiler info for the structure CLASS. Iterate
;;; over this type, clearing the compiler structure type info, and
;;; undefining all the associated functions.  If SUBCLASSES-P, also do
;;; the same for subclasses.  FIXME: maybe rename UNDEFINE-FUN-NAME to
;;; UNDECLARE-FUNCTION-NAME?
(defun undeclare-structure (classoid subclasses-p)
  (let ((info (layout-info (classoid-layout classoid))))
    (when (defstruct-description-p info)
      (let ((type (dd-name info)))
        (clear-info :type :compiler-layout type)
        ;; FIXME: shouldn't this undeclare any constructors too?
        (undefine-fun-name (dd-copier-name info))
        (undefine-fun-name (dd-predicate-name info))
        (dolist (slot (dd-slots info))
          (let ((fun (dsd-accessor-name slot)))
            (unless (accessor-inherited-data fun info)
              (undefine-fun-name fun)
              (unless (dsd-read-only slot)
                (undefine-fun-name `(setf ,fun)))))))
      ;; Clear out the SPECIFIER-TYPE cache so that subsequent
      ;; references are unknown types.
      (values-specifier-type-cache-clear)))
  (when subclasses-p
    (let ((subclasses (classoid-subclasses classoid)))
      (when subclasses
        (collect ((subs))
          (dohash ((classoid layout)
                   subclasses
                   :locked t)
            (declare (ignore layout))
            (undeclare-structure classoid nil)
            (subs (classoid-proper-name classoid)))
          ;; Is it really necessary to warn about
          ;; undeclaring functions for subclasses?
          (when (subs)
            (warn "undeclaring functions for old subclasses ~
                               of ~S:~%  ~S"
                  (classoid-name classoid)
                  (subs))))))))

;;; core compile-time setup of any class with a LAYOUT, used even by
;;; !DEFSTRUCT-WITH-ALTERNATE-METACLASS weirdosities
(defun %compiler-set-up-layout (dd
                                &optional
                                ;; Several special cases
                                ;; (STRUCTURE-OBJECT itself, and
                                ;; structures with alternate
                                ;; metaclasses) call this function
                                ;; directly, and they're all at the
                                ;; base of the instance class
                                ;; structure, so this is a handy
                                ;; default.  (But note
                                ;; FUNCALLABLE-STRUCTUREs need
                                ;; assistance here)
                                (inherits (vector (find-layout t))))

  (multiple-value-bind (classoid layout old-layout)
      (multiple-value-bind (clayout clayout-p)
          (info :type :compiler-layout (dd-name dd))
        (ensure-structure-class dd
                                inherits
                                (if clayout-p
                                    "The most recently compiled"
                                    "The current")
                                "the most recently loaded"
                                :compiler-layout clayout))
    (cond (old-layout
           (undeclare-structure (layout-classoid old-layout)
                                (and (classoid-subclasses classoid)
                                     (not (eq layout old-layout))))
           (setf (layout-invalid layout) nil)
           ;; FIXME: it might be polite to hold onto old-layout and
           ;; restore it at the end of the file.  -- RMK 2008-09-19
           ;; (International Talk Like a Pirate Day).
           (warn "~@<Clobbering the compiler's idea of the layout of ~A.~:@>"
                 classoid))
          (t
           (unless (eq (classoid-layout classoid) layout)
             (register-layout layout :invalidate nil))
           (setf (find-classoid (dd-name dd)) classoid)))

    ;; At this point the class should be set up in the INFO database.
    ;; But the logic that enforces this is a little tangled and
    ;; scattered, so it's not obvious, so let's check.
    (aver (find-classoid (dd-name dd) nil))

    (setf (info :type :compiler-layout (dd-name dd)) layout))
  (values))

(defun %proclaim-defstruct-ctors (dd)
  (dolist (ctor (dd-constructors dd))
    (let ((ftype (%struct-ctor-ftype dd (cdr ctor) (dd-element-type dd))))
      (sb!c:proclaim-ftype (car ctor) dd ftype :declared))))

;;; Do (COMPILE LOAD EVAL)-time actions for the normal (not
;;; ALTERNATE-LAYOUT) DEFSTRUCT described by DD.
;;; This includes generation of a style-warning about previously compiled
;;; calls to the accessors and/or predicate that weren't inlined.
(defun %compiler-defstruct (dd inherits)
  (declare (type defstruct-description dd))

  (aver (dd-class-p dd)) ; LIST and VECTOR representation are not allowed
  (let ((check-inlining
         ;; Why use the secondary result of INFO, not the primary?
         ;; Because when DEFSTRUCT is evaluated, not via the file-compiler,
         ;; the first thing to happen is %DEFSTRUCT, which sets up FIND-CLASS.
         ;; Due to :COMPILER-LAYOUT's defaulting expression in globaldb,
         ;; it has a value - the layout of the classoid - that we don't want.
         ;; Also, since structures are technically not redefineable,
         ;; I don't worry about failure to inline a function that was
         ;; formerly not known as an accessor but now is.
         (null (nth-value 1 (info :type :compiler-layout (dd-name dd)))))
        (fnames))
    (%compiler-set-up-layout dd inherits)
    (%proclaim-defstruct-ctors dd)

    (awhen (dd-copier-name dd)
      (let ((dtype (dd-name dd)))
        (sb!xc:proclaim `(ftype (sfunction (,dtype) ,dtype) ,it))))

    (let ((predicate-name (dd-predicate-name dd)))
      (when predicate-name
        (when check-inlining
          (push predicate-name fnames))
        (setf (info :function :source-transform predicate-name)
              (cons dd :predicate))))

    (dolist (dsd (dd-slots dd))
      (let ((accessor-name (dsd-accessor-name dsd)))
        ;; Why this WHEN guard here, if there is neither a standards-specified
        ;; nor implementation-specific way to skip defining an accessor? Dunno.
        ;; And furthermore, by ignoring a package lock, it's possible to name
        ;; an accessor NIL: (defstruct (x (:conc-name "N")) IL)
        ;; making this test kinda bogus in two different ways.
        (when accessor-name
          (let ((inherited (accessor-inherited-data accessor-name dd)))
            (cond
              ((not inherited)
               (let ((writer `(setf ,accessor-name))
                     (slot-key (cons dd dsd)))
                 (when check-inlining
                   (push accessor-name fnames))
                 (setf (info :function :source-transform accessor-name)
                       slot-key)
                 (unless (dsd-read-only dsd)
                   (when check-inlining
                     (push writer fnames))
                   (setf (info :function :source-transform writer) slot-key))))
              ((not (= (cdr inherited) (dsd-index dsd)))
               (style-warn "~@<Non-overwritten accessor ~S does not access ~
                            slot with name ~S (accessing an inherited slot ~
                            instead).~:@>"
                           accessor-name
                           (dsd-name dsd))))))))

    (awhen (remove-if-not #'sb!c::emitted-full-call-count fnames)
      (sb!c:compiler-style-warn
       'sb!c:inlining-dependency-failure
       ;; This message omits the http://en.wikipedia.org/wiki/Serial_comma
       :format-control
       (!uncross-format-control
        "~@<Previously compiled call~P to ~
~{~/sb!impl:print-symbol-with-prefix/~^~#[~; and~:;,~] ~} ~
could not be inlined because the structure definition for ~
~/sb!impl:print-symbol-with-prefix/ was not yet seen. To avoid this warning, ~
DEFSTRUCT should precede references to the affected functions, ~
or they must be declared locally notinline at each call site.~@:>")
       :format-arguments (list (length it) (nreverse it) (dd-name dd))))))

;;;; redefinition stuff

;;; Compare the slots of OLD and NEW, returning 3 lists of slot names:
;;;   1. Slots which have moved,
;;;   2. Slots whose type has changed,
;;;   3. Deleted slots.
(defun compare-slots (old new)
  (let* ((oslots (dd-slots old))
         (nslots (dd-slots new))
         (onames (mapcar #'dsd-name oslots))
         (nnames (mapcar #'dsd-name nslots)))
    (collect ((moved)
              (retyped))
      (dolist (name (intersection onames nnames))
        (let ((os (find name oslots :key #'dsd-name :test #'string=))
              (ns (find name nslots :key #'dsd-name :test #'string=)))
          (unless (sb!xc:subtypep (dsd-type ns) (dsd-type os))
            (retyped name))
          (unless (and (= (dsd-index os) (dsd-index ns))
                       (eq (dsd-raw-type os) (dsd-raw-type ns)))
            (moved name))))
      (values (moved)
              (retyped)
              (set-difference onames nnames :test #'string=)))))

;;; If we are redefining a structure with different slots than in the
;;; currently loaded version, give a warning and return true.
(defun redefine-structure-warning (classoid old new)
  (declare (type defstruct-description old new)
           (type classoid classoid)
           (ignore classoid))
  (let ((name (dd-name new)))
    (multiple-value-bind (moved retyped deleted) (compare-slots old new)
      (when (or moved retyped deleted)
        (warn
         "incompatibly redefining slots of structure class ~S~@
          Make sure any uses of affected accessors are recompiled:~@
          ~@[  These slots were moved to new positions:~%    ~S~%~]~
          ~@[  These slots have new incompatible types:~%    ~S~%~]~
          ~@[  These slots were deleted:~%    ~S~%~]"
         name moved retyped deleted)
        t))))

;;; This function is called when we are incompatibly redefining a
;;; structure CLASS to have the specified NEW-LAYOUT. We signal an
;;; error with some proceed options and return the layout that should
;;; be used.
(defun %redefine-defstruct (classoid old-layout new-layout)
  (declare (type classoid classoid)
           (type layout old-layout new-layout))
  (let ((name (classoid-proper-name classoid)))
    (restart-case
        (error "~@<attempt to redefine the ~S class ~S incompatibly with the current definition~:@>"
               'structure-object
               name)
      (continue ()
       :report (lambda (s)
                 (format s
                         "~@<Use the new definition of ~S, invalidating ~
                          already-loaded code and instances.~@:>"
                         name))
       (register-layout new-layout))
      (recklessly-continue ()
       :report (lambda (s)
                 (format s
                         "~@<Use the new definition of ~S as if it were ~
                          compatible, allowing old accessors to use new ~
                          instances and allowing new accessors to use old ~
                          instances.~@:>"
                         name))
       ;; classic CMU CL warning: "Any old ~S instances will be in a bad way.
       ;; I hope you know what you're doing..."
       (register-layout new-layout
                        :invalidate nil
                        :destruct-layout old-layout))
      (clobber-it ()
       ;; FIXME: deprecated 2002-10-16, and since it's only interactive
       ;; hackery instead of a supported feature, can probably be deleted
       ;; in early 2003
       :report "(deprecated synonym for RECKLESSLY-CONTINUE)"
       (register-layout new-layout
                        :invalidate nil
                        :destruct-layout old-layout))))
  (values))

(declaim (inline dd-layout-length))
(defun dd-layout-length (dd)
  (+ (dd-length dd) #!-interleaved-raw-slots (dd-raw-length dd)))

(declaim (ftype (sfunction (defstruct-description) index) dd-instance-length))
(defun dd-instance-length (dd)
  ;; Make sure the object ends at a two-word boundary.  Note that this does
  ;; not affect the amount of memory used, since the allocator would add the
  ;; same padding anyway.  However, raw slots are indexed from the length of
  ;; the object as indicated in the header, so the pad word needs to be
  ;; included in that length to guarantee proper alignment of raw double float
  ;; slots, necessary for (at least) the SPARC backend.
  ;; On backends with interleaved raw slots, the convention of having the
  ;; header possibly "lie" about an extra word is more of a bug than a feature.
  ;; Because the structure base is aligned, double-word raw slots are properly
  ;; aligned, and won't change alignment in descendant object types. It would
  ;; be correct to store the true instance length even though GC preserves
  ;; the extra data word (as it does for odd-length SIMPLE-VECTOR), treating
  ;; the total physical length as rounded-to-even. But having two different
  ;; conventions would be even more unnecessarily confusing, so we use
  ;; the not-sensible convention even when it does not make sense.
  (logior (dd-layout-length dd) 1))

(defun dd-bitmap (dd)
  ;; The bitmap stores a 1 for each untagged word,
  ;; including any internal padding words for alignment.
  ;; The 0th bit is initialized to 0 because the LAYOUT is a tagged
  ;; slot that is not present in DD-SLOTS.
  ;; All other bits start as 1 and are cleared if the word is tagged.
  ;; A final padding word, if any, is regarded as tagged.
  (let ((bitmap (ldb (byte (dd-length dd) 0)
                     (ash -1 sb!vm:instance-data-start))))
    (dolist (slot (dd-slots dd) bitmap)
      (when (eql t (dsd-raw-type slot))
        (setf (ldb (byte 1 (dsd-index slot)) bitmap) 0)))))

;;; This is called when we are about to define a structure class. It
;;; returns a (possibly new) class object and the layout which should
;;; be used for the new definition (may be the current layout, and
;;; also might be an uninstalled forward referenced layout.) The third
;;; value is true if this is an incompatible redefinition, in which
;;; case it is the old layout.
(defun ensure-structure-class (info inherits old-context new-context
                                    &key compiler-layout)
  (declare (type defstruct-description info))
  (multiple-value-bind (class old-layout)
      (multiple-value-bind (class constructor)
          (acond ((cdr (dd-alternate-metaclass info))
                  (values (first it) (second it)))
                 (t
                  (values 'structure-classoid 'make-structure-classoid)))
        (insured-find-classoid (dd-name info)
                               (if (eq class 'structure-classoid)
                                   (lambda (x)
                                     (sb!xc:typep x 'structure-classoid))
                                   (lambda (x)
                                     (sb!xc:typep x (classoid-name (find-classoid class)))))
                               (fdefinition constructor)))
    (setf (classoid-direct-superclasses class)
          (case (dd-name info)
            ((ansi-stream
              fd-stream
              sb!impl::string-input-stream sb!impl::string-output-stream
              sb!impl::fill-pointer-output-stream)
             (list (layout-classoid (svref inherits (1- (length inherits))))
                   (layout-classoid (svref inherits (- (length inherits) 2)))))
            (t
             (list (layout-classoid
                    (svref inherits (1- (length inherits))))))))
    (let* ((old-layout (or compiler-layout old-layout))
           (new-layout
            (when (or (not old-layout) *type-system-initialized*)
              (make-layout :classoid class
                           :inherits inherits
                           :depthoid (length inherits)
                           :length (dd-layout-length info)
                           :info info . #!-interleaved-raw-slots
                                        (:n-untagged-slots (dd-raw-length info))
                                        #!+interleaved-raw-slots
                                        (:untagged-bitmap (dd-bitmap info))))))
      (cond
       ((not old-layout)
        (values class new-layout nil))
       ((not new-layout)
        ;; The assignment of INFO here can almost be deleted,
        ;; except for a few magical types that don't d.t.r.t. in cold-init:
        ;;  STRUCTURE-OBJECT, CONDITION, ALIEN-VALUE, INTERPRETED-FUNCTION
        (setf (layout-info old-layout) info)
        (values class old-layout nil))
       (;; This clause corresponds to an assertion in REDEFINE-LAYOUT-WARNING
        ;; of classic CMU CL. I moved it out to here because it was only
        ;; exercised in this code path anyway. -- WHN 19990510
        (not (eq (layout-classoid new-layout) (layout-classoid old-layout)))
        (error "shouldn't happen: weird state of OLD-LAYOUT?"))
       ((redefine-layout-warning old-context
                                 old-layout
                                 new-context
                                 (layout-length new-layout)
                                 (layout-inherits new-layout)
                                 (layout-depthoid new-layout)
                                 (layout-raw-slot-metadata new-layout))
        (values class new-layout old-layout))
       (t
        (let ((old-info (layout-info old-layout)))
          (if old-info
             (cond ((redefine-structure-warning class old-info info)
                    (values class new-layout old-layout))
                   (t
                    (setf (layout-info old-layout) info)
                    (values class old-layout nil)))
             (progn
               (setf (layout-info old-layout) info)
               (values class old-layout nil)))))))))

;;; Return a list of pairs (name . index). Used for :TYPE'd
;;; constructors to find all the names that we have to splice in &
;;; where. Note that these types don't have a layout, so we can't look
;;; at LAYOUT-INHERITS.
(defun find-name-indices (defstruct)
  (collect ((res))
    (let ((infos ()))
      (do ((info defstruct
                 (typed-structure-info-or-lose (first (dd-include info)))))
          ((not (dd-include info))
           (push info infos))
        (push info infos))

      (let ((i 0))
        (dolist (info infos)
          (incf i (or (dd-offset info) 0))
          (when (dd-named info)
            (res (cons (dd-name info) i)))
          (setq i (dd-length info)))))

    (res)))

;;; These functions are called to actually make a constructor after we
;;; have processed the arglist. The correct variant (according to the
;;; DD-TYPE) should be called. The function is defined with the
;;; specified name and arglist. VARS and TYPES are used for argument
;;; type declarations. VALUES are the values for the slots (in order.)
;;;
;;; This is split into two functions:
;;;   * INSTANCE-CONSTRUCTOR-FORM has to deal with raw slots
;;;     (there are two variations on this)
;;;   * TYPED-CONSTRUCTOR-FORM deal with LIST & VECTOR
;;;     which might have "name" symbols stuck in at various weird places.
(defun instance-constructor-form (dd values)
   ;; The difference between the two implementations here is that on all
   ;; platforms we don't have the appropriate RAW-INSTANCE-INIT VOPS, which
   ;; must be able to deal with immediate values as well -- unlike
   ;; RAW-INSTANCE-SET VOPs, which never end up seeing immediate values. With
   ;; some additional cleverness we might manage without them and just a single
   ;; implementation here, though -- figure out a way to ensure that on those
   ;; platforms we always still get a non-immediate TN in every case...
   ;;
   ;; Until someone does that, this means that instances with raw slots can be
   ;; DX allocated only on platforms with those additional VOPs.
  (let ((dd-slots (dd-slots dd)))
    (aver (= (length dd-slots) (length values)))
    #!+raw-instance-init-vops
    (collect ((slot-specs) (slot-values))
      (mapc (lambda (dsd value)
              (unless (eq value '.do-not-initialize-slot.)
                (slot-specs (list* :slot (dsd-raw-type dsd) (dsd-index dsd)))
                (slot-values value)))
            dd-slots values)
      `(%make-structure-instance-macro ,dd ',(slot-specs) ,@(slot-values)))
    #!-raw-instance-init-vops
    (collect ((slot-specs) (slot-values) (raw-slots) (raw-values))
      ;; Partition into non-raw and raw
      (mapc (lambda (dsd value)
              (unless (eq value '.do-not-initialize-slot.)
                (let ((raw-type (dsd-raw-type dsd)))
                  (cond ((eq t raw-type)
                         (slot-specs (list* :slot raw-type (dsd-index dsd)))
                         (slot-values value))
                        (t
                         (raw-slots dsd)
                         (raw-values value))))))
            dd-slots values)
      (let ((instance-form
             `(%make-structure-instance-macro ,dd
                                              ',(slot-specs) ,@(slot-values))))
        (if (raw-slots)
            (let ((temp (make-symbol "INSTANCE")))
              `(let ((,temp ,instance-form))
                 ;; Transform to %RAW-INSTANCE-SET/foo, not SETF,
                 ;; in case any slots are readonly.
                 ,@(mapcar (lambda (dsd value)
                             (slot-access-transform
                              :setf (list temp value) (cons dd dsd)))
                           (raw-slots) (raw-values))
                 ,temp))
            instance-form)))))

;;; A "typed" constructors prefers to use a single call to LIST or VECTOR
;;; if possible, but can't always do that for VECTOR because it might not
;;; be a (VECTOR T). If not, we fallback to MAKE-ARRAY and (SETF AREF).
(defun typed-constructor-form (dd values)
  (multiple-value-bind (operator initial-element)
      (cond ((and (eq (dd-type dd) 'vector) (eq (dd-element-type dd) t))
             (values 'vector 0))
            ((eq (dd-type dd) 'list)
             (values 'list nil)))
    (let* ((length (dd-length dd))
           (slots (dd-slots dd))
           ;; Possibly the most useless feature ever: more than one name slot.
           (names (find-name-indices dd)))
      (aver (= (length slots) (length values)))
      (if operator
          ;; The initial-element provides values for slots that are skipped
          ;; due to :initial-offset, not slots that are skipped due to
          ;; &AUX variables with no initial value.
          (let ((vals (make-list length :initial-element initial-element)))
            (dolist (x names)
              (setf (elt vals (cdr x)) `',(car x)))
            (mapc (lambda (dsd val)
                    ;; For both vectors and lists, .DO-NOT-INITIALIZE-SLOT.
                    ;; becomes 0 even though lists otherwise use NIL for slots
                    ;; that are skipped to due :initial-offset.
                    (setf (elt vals (dsd-index dsd))
                          ;; All VALs have been wrapped in THE if necessary.
                          (if (eq val '.do-not-initialize-slot.) 0 val)))
                  slots values)
            (cons operator vals))
          (let ((temp (make-symbol "OBJ")))
            `(let ((,temp (make-array ,length
                                      :element-type ',(dd-element-type dd))))
               ,@(mapcar (lambda (x) `(setf (aref ,temp ,(cdr x))  ',(car x)))
                         names)
               ,@(mapcan (lambda (dsd val)
                           (unless (eq val '.do-not-initialize-slot.)
                             `((setf (aref ,temp ,(dsd-index dsd)) ,val))))
                         slots values)
               ,temp))))))

;;; Return the FTYPE for a DD constructor.
;;; This is tricky in uses such as the following:
;;;  (DEFSTRUCT (S (:CONSTRUCTOR MS (A &AUX (A (ABS A))))) (A 0 :TYPE (MOD 4)))
;;; The constructor accepts integers betweeen -3 and 3 because the &AUX binding
;;; hides the positional argument A, and we can't actually put any constraint
;;; on A unless we figure out what the action of ABS is.
;;;
;;; The FTYPE is actually not a strong enough constraint anyway, so when IR1
;;; tests for the call compatibility it will test for correctness *after*
;;; argument defaulting.
(defun %struct-ctor-ftype (dd args elt-type)
  (flet ((elt-type-intersect (dsd &aux (slot-type (dsd-type dsd)))
           (cond ((eq slot-type t) elt-type)
                 ((eq elt-type t) slot-type)
                 (t `(and ,elt-type ,slot-type)))))
    `(function
      ,(if (eq args :default)
           `(&key ,@(mapcar (lambda (dsd)
                              `(,(keywordicate (dsd-name dsd))
                                ,(elt-type-intersect dsd)))
                            (dd-slots dd)))
           (destructuring-bind (llks &optional req opt rest keys aux) args
             (let ((aux (mapcar (lambda (var) (if (listp var) (car var) var))
                                aux)))
               (flet ((get-arg-type (name)
                        (let ((slot (unless (member name aux :test #'string=)
                                      (find name (dd-slots dd) :key #'dsd-name
                                            :test #'string=))))
                          ;; If no slot, the arg restriction is T,
                          ;; because we don't know where it goes.
                          (if slot (elt-type-intersect slot) t))))
                 (make-lambda-list
                  llks nil (mapcar #'get-arg-type req)
                  (mapcar (lambda (arg)
                            (get-arg-type (parse-optional-arg-spec arg)))
                          opt)
                  (if rest (list t))
                  (mapcar (lambda (arg)
                            (multiple-value-bind (key var) (parse-key-arg-spec arg)
                              `(,key ,(get-arg-type var))))
                          keys))))))
      (values ,(cond ((dd-class-p dd) (dd-name dd))
                     ((eq (dd-type dd) 'list) 'list)
                     (t `(vector ,(dd-element-type dd) ,(dd-length dd))))
              &optional))))

(defun struct-ctor-ftype (dd name)
  (let ((ctor (assq name (dd-constructors dd))))
    (aver ctor)
    (%struct-ctor-ftype dd (cdr ctor) (dd-element-type dd))))

(defun proclaimed-ftype (name)
  (multiple-value-bind (info foundp) (info :function :type name)
    (values (cond ((defstruct-description-p info)
                   (specifier-type (struct-ctor-ftype info name)))
                  #-sb-xc-host ; PCL doesn't exist
                  ((eq info :generic-function) (sb!pcl::compute-gf-ftype name))
                  (t info))
            foundp)))

;;; Given a DD and a constructor spec (a cons of name and pre-parsed
;;; BOA lambda list, or the symbol :DEFAULT), return the effective
;;; lambda list and the body of the lambda.
(defun structure-ctor-lambda-parts
    (dd args &aux (creator (ecase (dd-type dd)
                             (structure #'instance-constructor-form)
                             ((list vector) #'typed-constructor-form))))
  (when (eq args :default)
    (let ((lambda-list (mapcar (lambda (dsd)
                                 (let* ((temp (copy-symbol (dsd-name dsd)))
                                        (keyword (keywordicate temp)))
                                   `((,keyword ,temp) ,(dsd-default dsd))))
                               (dd-slots dd))))
      (return-from structure-ctor-lambda-parts
        `((&key ,@lambda-list)
          (declare (explicit-check))
          ,(funcall creator dd
                    (mapcar (lambda (dsd arg)
                              (let ((type (dsd-type dsd))
                                    (var (cadar arg)))
                                (if (eq type t) var `(the ,type ,var))))
                            (dd-slots dd) lambda-list))))))
  (destructuring-bind (llks &optional req opt rest keys aux) args
    (collect ((vars (copy-list req)) ; list of bound vars
              (aux-vars)
              (skipped-vars))
      (dolist (binding aux)
        (let ((name (if (listp binding) (car binding) binding)))
          (aux-vars name)
          (unless (typep binding '(cons t cons))
            (skipped-vars name))))
      (macrolet ((rewrite (input key parse)
                   `(mapcar
                     (lambda (arg)
                       (multiple-value-bind (,@key var def sup-p) (,parse arg)
                         (declare (ignore ,@key def))
                         (rewrite-1 arg var sup-p)))
                     ,input)))
        (flet ((rewrite-1 (arg var sup-p-var)
                 (vars var)
                 (when sup-p-var (vars (car sup-p-var)))
                 (let* ((slot (unless (member var (aux-vars) :test #'string=)
                                (find var (dd-slots dd)
                                      :key #'dsd-name :test #'string=)))
                        (default (and slot (dsd-default slot))))
                   ;; If VAR initializes a slot and did not have a default in
                   ;; the lambda list, and DSD-DEFAULT is not NIL,
                   ;; then change the lambda-list's default for the variable.
                   ;; Always prefer to insert (CAR ARG) if ARG was a list
                   ;; so that (:KEY var) syntax is preserved.
                   (if (and slot (not (typep arg '(cons t cons))) default)
                       `(,(if (consp arg) (car arg) var) ,default ,@sup-p-var)
                       arg)))) ; keep it as it was
          ;; Can we substitute symbols that are not EQ to symbols
          ;; naming slots, so we don't have to compare by STRING= later?
          ;; Probably not because other symbols could reference them.
          (setq opt (rewrite opt () parse-optional-arg-spec))
          (when rest (vars (car rest)))
          (setq keys (rewrite keys (key) parse-key-arg-spec))
          (dolist (arg (aux-vars)) (vars arg))))
      `(,(sb!c::make-lambda-list
          llks nil req opt rest keys
          ;; &AUX vars which do not initialize a slot are not mentioned
          ;; in the lambda list, though it's not clear what to do if
          ;; subsequent bindings refer to the deleted ones.
          ;; And worse, what if it's SETQd - is that even legal?
          (remove-if (lambda (x) (not (typep x '(cons t cons)))) aux))
        (declare (explicit-check))
        ,(funcall
          creator dd
          (mapcar
           (lambda (slot &aux (name (dsd-name slot)))
             (if (find name (skipped-vars) :test #'string=)
                 ;; CLHS 3.4.6 Boa Lambda Lists
                 '.do-not-initialize-slot.
                 (let* ((type (dsd-type slot))
                        (found (member (dsd-name slot) (vars) :test #'string=))
                        (initform (if found (car found) (dsd-default slot))))
                   ;; We can ignore the DD-ELEMENT-TYPE
                   ;; because the container itself will check.
                   (if (eq type t) initform `(the ,type ,initform)))))
           (dd-slots dd)))))))

(defun accessor-definitions (dd)
  (loop for dsd in (dd-slots dd)
        for accessor-name = (dsd-accessor-name dsd)
        unless (accessor-inherited-data accessor-name dd)
        nconc (dx-let ((key (cons dd dsd)))
                `(,@(unless (dsd-read-only dsd)
                     `((defun (setf ,accessor-name) (value instance)
                         ,(slot-access-transform :setf '(instance value) key))))
                  (defun ,accessor-name (instance)
                    ,(slot-access-transform :read '(instance) key))))))

;;;; instances with ALTERNATE-METACLASS
;;;;
;;;; The CMU CL support for structures with ALTERNATE-METACLASS was a
;;;; fairly general extension embedded in the main DEFSTRUCT code, and
;;;; the result was an fairly impressive mess as ALTERNATE-METACLASS
;;;; extension mixed with ANSI CL generality (e.g. :TYPE and :INCLUDE)
;;;; and CMU CL implementation hairiness (esp. raw slots). This SBCL
;;;; version is much less ambitious, noticing that ALTERNATE-METACLASS
;;;; is only used to implement CONDITION, STANDARD-INSTANCE, and
;;;; GENERIC-FUNCTION, and defining a simple specialized
;;;; separate-from-DEFSTRUCT macro to provide only enough
;;;; functionality to support those.
;;;;
;;;; KLUDGE: The defining macro here is so specialized that it's ugly
;;;; in its own way. It also violates once-and-only-once by knowing
;;;; much about structures and layouts that is already known by the
;;;; main DEFSTRUCT macro. Hopefully it will go away presently
;;;; (perhaps when CL:CLASS and SB-PCL:CLASS meet) as per FIXME below.
;;;; -- WHN 2001-10-28
;;;;
;;;; FIXME: There seems to be no good reason to shoehorn CONDITION,
;;;; STANDARD-INSTANCE, and GENERIC-FUNCTION into mutated structures
;;;; instead of just implementing them as primitive objects. (This
;;;; reduced-functionality macro seems pretty close to the
;;;; functionality of !DEFINE-PRIMITIVE-OBJECT..)

;;; The complete list of alternate-metaclass DEFSTRUCTs:
;;;   CONDITION SB-EVAL:INTERPRETED-FUNCTION
;;;   SB-PCL::STANDARD-INSTANCE SB-PCL::STANDARD-FUNCALLABLE-INSTANCE
;;;   SB-PCL::CTOR SB-PCL::%METHOD-FUNCTION
;;;
(defun make-dd-with-alternate-metaclass (&key (class-name (missing-arg))
                                              (superclass-name (missing-arg))
                                              (metaclass-name (missing-arg))
                                              (dd-type (missing-arg))
                                              metaclass-constructor
                                              slot-names)
  (let* ((dd (make-defstruct-description t class-name))
         (conc-name (concatenate 'string (symbol-name class-name) "-"))
         (dd-slots (let ((reversed-result nil)
                         ;; The index starts at 1 for ordinary named
                         ;; slots because slot 0 is magical, used for
                         ;; the LAYOUT in CONDITIONs and
                         ;; FUNCALLABLE-INSTANCEs.  (This is the same
                         ;; in ordinary structures too: see (INCF
                         ;; DD-LENGTH) in
                         ;; PARSE-DEFSTRUCT-NAME-AND-OPTIONS).
                         (index 1))
                     (dolist (slot-name slot-names)
                       (push (make-defstruct-slot-description
                              :name slot-name
                              :index index
                              :accessor-name (symbolicate conc-name slot-name))
                             reversed-result)
                       (incf index))
                     (nreverse reversed-result))))
    ;; We do *not* fill in the COPIER-NAME and PREDICATE-NAME
    ;; because none of the magical alternate-metaclass structures
    ;; have copiers and predicates that "Just work"
    (case dd-type
      ;; We don't support inheritance of alternate metaclass stuff,
      ;; and it's not a general-purpose facility, so sanity check our
      ;; own code.
      (structure
       (aver (eq superclass-name 't)))
      (funcallable-structure
       (aver (eq superclass-name 'function)))
      (t (bug "Unknown DD-TYPE in ALTERNATE-METACLASS: ~S" dd-type)))
    (setf (dd-alternate-metaclass dd) (list superclass-name
                                            metaclass-name
                                            metaclass-constructor)
          (dd-slots dd) dd-slots
          (dd-length dd) (1+ (length slot-names))
          (dd-type dd) dd-type)
    dd))

;;; make !DEFSTRUCT-WITH-ALTERNATE-METACLASS compilable by the host
;;; lisp, installing the information we need to reason about the
;;; structures (layouts and classoids).
;;;
;;; FIXME: we should share the parsing and the DD construction between
;;; this and the cross-compiler version, but my brain was too small to
;;; get that right.  -- CSR, 2006-09-14
#+sb-xc-host
(defmacro !defstruct-with-alternate-metaclass
    (class-name &key
                (slot-names (missing-arg))
                (boa-constructor (missing-arg))
                (superclass-name (missing-arg))
                (metaclass-name (missing-arg))
                (metaclass-constructor (missing-arg))
                (dd-type (missing-arg))
                predicate
                (runtime-type-checks-p t))

  (declare (type (and list (not null)) slot-names))
  (declare (type (and symbol (not null))
                 boa-constructor
                 superclass-name
                 metaclass-name
                 metaclass-constructor))
  (declare (type symbol predicate))
  (declare (type (member structure funcallable-structure) dd-type))
  (declare (ignore boa-constructor predicate runtime-type-checks-p))

  (let* ((dd (make-dd-with-alternate-metaclass
              :class-name class-name
              :slot-names slot-names
              :superclass-name superclass-name
              :metaclass-name metaclass-name
              :metaclass-constructor metaclass-constructor
              :dd-type dd-type)))
    `(progn

      (eval-when (:compile-toplevel :load-toplevel :execute)
        (%compiler-set-up-layout ',dd ',(!inherits-for-structure dd))))))

(sb!xc:defmacro !defstruct-with-alternate-metaclass
    (class-name &key
                (slot-names (missing-arg))
                (boa-constructor (missing-arg))
                (superclass-name (missing-arg))
                (metaclass-name (missing-arg))
                (metaclass-constructor (missing-arg))
                (dd-type (missing-arg))
                predicate
                (runtime-type-checks-p t))

  (declare (type (and list (not null)) slot-names))
  (declare (type (and symbol (not null))
                 boa-constructor
                 superclass-name
                 metaclass-name
                 metaclass-constructor))
  (declare (type symbol predicate))
  (declare (type (member structure funcallable-structure) dd-type))

  (let* ((dd (make-dd-with-alternate-metaclass
              :class-name class-name
              :slot-names slot-names
              :superclass-name superclass-name
              :metaclass-name metaclass-name
              :metaclass-constructor metaclass-constructor
              :dd-type dd-type))
         (dd-slots (dd-slots dd))
         (dd-length (1+ (length slot-names)))
         (object-gensym (make-symbol "OBJECT"))
         (new-value-gensym (make-symbol "NEW-VALUE"))
         (delayed-layout-form `(%delayed-get-compiler-layout ,class-name)))
    (multiple-value-bind (raw-maker-form raw-reffer-operator)
        (ecase dd-type
          (structure
           (values `(%make-structure-instance-macro ,dd nil)
                   '%instance-ref))
          (funcallable-structure
           (values `(let ((,object-gensym
                           (%make-funcallable-instance ,dd-length)))
                      (setf (%funcallable-instance-layout ,object-gensym)
                            ,delayed-layout-form)
                      ,object-gensym)
                   '%funcallable-instance-info)))
      `(progn

         (eval-when (:compile-toplevel :load-toplevel :execute)
           (%compiler-set-up-layout ',dd ',(!inherits-for-structure dd)))

         ;; slot readers and writers
         (declaim (inline ,@(mapcar #'dsd-accessor-name dd-slots)))
         ,@(mapcar (lambda (dsd)
                     `(defun ,(dsd-accessor-name dsd) (,object-gensym)
                        ,@(when runtime-type-checks-p
                            `((declare (type ,class-name ,object-gensym))))
                        (,raw-reffer-operator ,object-gensym
                                              ,(dsd-index dsd))))
                   dd-slots)
         (declaim (inline ,@(mapcar (lambda (dsd)
                                      `(setf ,(dsd-accessor-name dsd)))
                                    dd-slots)))
         ,@(mapcar (lambda (dsd)
                     `(defun (setf ,(dsd-accessor-name dsd)) (,new-value-gensym
                                                              ,object-gensym)
                        ,@(when runtime-type-checks-p
                            `((declare (type ,class-name ,object-gensym))))
                        (setf (,raw-reffer-operator ,object-gensym
                                                    ,(dsd-index dsd))
                              ,new-value-gensym)))
                   dd-slots)

         ;; constructor
         (defun ,boa-constructor ,slot-names
           (let ((,object-gensym ,raw-maker-form))
             ,@(mapcar (lambda (slot-name)
                         (let ((dsd (or (find slot-name dd-slots
                                              :key #'dsd-name :test #'string=)
                                        (bug "Bogus alt-metaclass boa ctor"))))
                           `(setf (,(dsd-accessor-name dsd) ,object-gensym)
                                  ,slot-name)))
                       slot-names)
             ,object-gensym))

         ;; predicate
         ,@(when predicate
             ;; Just delegate to the compiler's type optimization
             ;; code, which knows how to generate inline type tests
             ;; for the whole CMU CL INSTANCE menagerie.
             `(defun ,predicate (,object-gensym)
                (typep ,object-gensym ',class-name)))

         ;; Usually we AVER instead of ASSERT, but one alternate-metaclass
         ;; structure definition is cross-compiled before AVER is a known macro.
         ;; It could be a def!macro perhaps, but ASSERT works just fine here
         ;; without adding to image size, since these toplevel forms
         ;; belong to code that is discarded after cold-init.
         (assert (null (symbol-value '*defstruct-hooks*)))))))

;;;; finalizing bootstrapping

;;; Set up DD and LAYOUT for STRUCTURE-OBJECT class itself.
;;;
;;; Ordinary structure classes effectively :INCLUDE STRUCTURE-OBJECT
;;; when they have no explicit :INCLUDEs, so (1) it needs to be set up
;;; before we can define ordinary structure classes, and (2) it's
;;; special enough (and simple enough) that we just build it by hand
;;; instead of trying to generalize the ordinary DEFSTRUCT code.
(defun !set-up-structure-object-class ()
  (let ((dd (make-defstruct-description t 'structure-object)))
    (setf
     (dd-slots dd) nil
     (dd-length dd) 1
     (dd-type dd) 'structure)
    (%compiler-set-up-layout dd)))
#+sb-xc-host(!set-up-structure-object-class)

;;; early structure predeclarations: Set up DD and LAYOUT for ordinary
;;; (non-ALTERNATE-METACLASS) structures which are needed early.
(dolist (args
         '#.(sb-cold:read-from-file
             "src/code/early-defstruct-args.lisp-expr"))
  (let* ((dd (parse-defstruct t (first args) (rest args)))
         (inherits (!inherits-for-structure dd)))
    (%compiler-defstruct dd inherits)))

(defun find-defstruct-description (name &optional (errorp t))
  (let* ((classoid (find-classoid name errorp))
         (info (and classoid
                    (layout-info (classoid-layout classoid)))))
    (cond ((defstruct-description-p info)
           info)
          (errorp
           (error "No DEFSTRUCT-DESCRIPTION for ~S." name)))))

(defun structure-instance-accessor-p (name)
  (let ((info (info :function :source-transform name)))
    (and (listp info)
         (defstruct-slot-description-p (cdr info))
         info)))

(defun dd-default-constructor (dd)
  (let ((ctor (first (dd-constructors dd))))
    (when (typep ctor '(cons t (eql :default)))
      (car ctor))))

;;; These functions are required to emulate SBCL kernel functions
;;; in a vanilla ANSI Common Lisp cross-compilation host.
;;; The emulation doesn't need to be efficient, since it's needed
;;; only for object dumping.
#+sb-xc-host
(progn
  (defun xc-dumpable-structure-instance-p (x)
    (and (typep x 'cl:structure-object)
         (let ((name (type-of x)))
           ;; Don't allow totally random structures, only ones that the
           ;; cross-compiler has been advised will work.
           (and (get name :sb-xc-allow-dumping-instances)
                ;; but we must also have cross-compiled it for real.
                (sb!kernel::compiler-layout-ready-p name)
                ;; and I don't know anything about raw slots
                ;; Coincidentally, in either representation of
                ;; raw-slot-metadata, 0 represents no untagged slots.
                (zerop (layout-raw-slot-metadata
                        (info :type :compiler-layout name)))))))
  (defun %instance-layout (instance)
    (aver (or (typep instance 'structure!object)
              (xc-dumpable-structure-instance-p instance)))
    (classoid-layout (find-classoid (type-of instance))))
  (defun %instance-length (instance)
    ;; INSTANCE-LENGTH tells you how many data words the backend is able to
    ;; physically access in this structure. Since every structure occupies
    ;; an even number of words, the storage slots comprise an odd number
    ;; of words after subtracting 1 for the header.
    ;; And in fact the fasl dumper / loader do write and read potentially
    ;; one cell beyond the instance's LAYOUT-LENGTH if it was not odd.
    ;; I'm not sure whether that is a good or bad thing.
    ;; But be that as it may, in the cross-compiler you must not access
    ;; more cells than there are in the declared structure because there
    ;; is no lower level storage that you can peek at.
    ;; So INSTANCE-LENGTH is exactly the same as LAYOUT-LENGTH on the host.
    (layout-length (%instance-layout instance)))
  (defun %instance-ref (instance index)
    (let ((layout (%instance-layout instance)))
      ;; with compact headers, 0 is an ordinary slot index.
      ;; without, it's the layout.
      (if (eql index (1- sb!vm:instance-data-start))
          (error "XC Host should use %INSTANCE-LAYOUT, not %INSTANCE-REF 0")
          (let* ((dd (layout-info layout))
                 ;; If data starts at 1, then subtract 1 from index.
                 ;; otherwise use the index as-is.
                 (dsd (elt (dd-slots dd)
                           (- index sb!vm:instance-data-start)))
                 (accessor-name (dsd-accessor-name dsd)))
            ;; Why AVER these: because it is slightly abstraction-breaking
            ;; to assume that the slot-index N is the NTH item in the DSDs.
            ;; The target Lisp never assumes that.
            (aver (and (eql (dsd-index dsd) index) (eq (dsd-raw-type dsd) t)))
            (funcall accessor-name instance)))))

  (defun %raw-instance-ref/word (instance index)
    (declare (ignore instance index))
    (error "No such thing as raw structure access on the host"))

  ;; Setting with (FUNCALL `(SETF ,accessor) ...) is unportable because
  ;;  "The mechanism by which defstruct arranges for slot accessors to be
  ;;   usable with setf is implementation-dependent; for example, it may
  ;;   use setf functions, setf expanders, or some other
  ;;   implementation-dependent mechanism ..."
  ;; But such capability seems not to be needed.
  (defun %instance-set (instance index new-value)
    (declare (ignore instance index new-value))
    (error "Can not use %INSTANCE-SET on cross-compilation host.")))

;;; If LAMBDA-LIST and BODY constitute an auto-generated structure function
;;; (accessor or predicate) for NAME, return the kind of thing it is.
(defun defstruct-generated-defn-p (name lambda-list body)
  (unless (singleton-p body)
    (return-from defstruct-generated-defn-p nil))
  (let ((info (info :function :source-transform name))
        (form (car body)))
    (when (consp info)
      (when (and (eq (cdr info) :predicate)
                 (equal lambda-list '(object))
                 (typep form
                        '(cons (eql typep)
                               (cons (eql object)
                                     (cons (cons (eql quote) (cons t null))
                                           null))))
                 ;; extract dd-name from `(TYPEP OBJECT ',THING)
                 (eq (second (third form)) (dd-name (car info))))
        (return-from defstruct-generated-defn-p :predicate))
      (when (defstruct-slot-description-p (cdr info))
        (multiple-value-bind (mode expected-lambda-list xform-args)
            (if (consp name)
                (values :setf '(value instance) '(instance value))
                (values :read '(instance) '(instance)))
          (when (and (equal expected-lambda-list lambda-list)
                     (equal (slot-access-transform mode xform-args info) form))
            (return-from defstruct-generated-defn-p :accessor)))))))

(/show0 "code/defstruct.lisp end of file")
