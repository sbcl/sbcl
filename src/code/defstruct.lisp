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
          (t res))))

(defun compiler-layout-ready-p (name)
  (let ((layout (info :type :compiler-layout name)))
    (and layout (typep (layout-info layout) 'defstruct-description))))

(sb!xc:defmacro %make-structure-instance-macro (dd slot-specs &rest slot-vars)
  `(truly-the ,(dd-name dd)
              ,(if (compiler-layout-ready-p (dd-name dd))
                   `(%make-structure-instance ,dd ,slot-specs ,@slot-vars)
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
                                   ,@slot-vars))))))

(declaim (ftype (sfunction (defstruct-description list) function)
                %make-structure-instance-allocator))
(defun %make-structure-instance-allocator (dd slot-specs)
  (let ((vars (make-gensym-list (length slot-specs))))
    (values (compile nil `(lambda (,@vars)
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
(def!struct (defstruct-description
             (:conc-name dd-)
             (:make-load-form-fun just-dump-it-normally)
             #-sb-xc-host (:pure t)
             (:constructor make-defstruct-description
                           (name &aux
                                 (conc-name (symbolicate name "-"))
                                 (copier-name (symbolicate "COPY-" name))
                                 (predicate-name (symbolicate name "-P")))))
  ;; name of the structure
  (name (missing-arg) :type symbol :read-only t)
  ;; documentation on the structure
  (doc nil :type (or string null))
  ;; prefix for slot names. If NIL, none.
  (conc-name nil :type (or symbol null))
  ;; the name of the primary standard keyword constructor, or NIL if none
  (default-constructor nil :type (or symbol null))
  ;; all the explicit :CONSTRUCTOR specs, with name defaulted
  (constructors () :type list)
  ;; name of copying function
  (copier-name nil :type (or symbol null))
  ;; name of type predicate
  (predicate-name nil :type (or symbol null))
  ;; the arguments to the :INCLUDE option, or NIL if no included
  ;; structure
  (include nil :type list)
  ;; properties used to define structure-like classes with an
  ;; arbitrary superclass and that may not have STRUCTURE-CLASS as the
  ;; metaclass. Syntax is:
  ;;    (superclass-name metaclass-name metaclass-constructor)
  (alternate-metaclass nil :type list)
  ;; a list of DEFSTRUCT-SLOT-DESCRIPTION objects for all slots
  ;; (including included ones)
  (slots () :type list)
  ;; a list of (NAME . INDEX) pairs for accessors of included structures
  (inherited-accessor-alist () :type list)
  ;; number of elements we've allocated (See also RAW-LENGTH, which is not
  ;; included in LENGTH.)
  (length 0 :type index)
  ;; General kind of implementation.
  (type 'structure :type (member structure vector list
                                 funcallable-structure))

  ;; The next three slots are for :TYPE'd structures (which aren't
  ;; classes, DD-CLASS-P = NIL)
  ;;
  ;; vector element type
  (element-type t)
  ;; T if :NAMED was explicitly specified, NIL otherwise
  (named nil :type boolean)
  ;; any INITIAL-OFFSET option on this direct type
  (offset nil :type (or index null))

  ;; the argument to the PRINT-FUNCTION option, or NIL if a
  ;; PRINT-FUNCTION option was given with no argument, or 0 if no
  ;; PRINT-FUNCTION option was given
  (print-function 0 :type (or cons symbol (member 0)))
  ;; the argument to the PRINT-OBJECT option, or NIL if a PRINT-OBJECT
  ;; option was given with no argument, or 0 if no PRINT-OBJECT option
  ;; was given
  (print-object 0 :type (or cons symbol (member 0)))
  ;; The number of untagged slots at the end.
  (raw-length 0 :type index)
  ;; the value of the :PURE option, or :UNSPECIFIED. This is only
  ;; meaningful if DD-CLASS-P = T.
  (pure :unspecified :type (member t nil :substructure :unspecified)))
(def!method print-object ((x defstruct-description) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (dd-name x) stream)))

;;; Does DD describe a structure with a class?
(defun dd-class-p (dd)
  (member (dd-type dd)
          '(structure funcallable-structure)))

;;; a type name which can be used when declaring things which operate
;;; on structure instances
(defun dd-declarable-type (dd)
  (if (dd-class-p dd)
      ;; Native classes are known to the type system, and we can
      ;; declare them as types.
      (dd-name dd)
      ;; Structures layered on :TYPE LIST or :TYPE VECTOR aren't part
      ;; of the type system, so all we can declare is the underlying
      ;; LIST or VECTOR type.
      (dd-type dd)))

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
  (accessor-name nil)
  default                       ; default value expression
  (type t)                      ; declared type specifier
  (safe-p t :type boolean)      ; whether the slot is known to be
                                ; always of the specified type
  ;; If this object does not describe a raw slot, this value is T.
  ;;
  ;; If this object describes a raw slot, this value is the type of the
  ;; value that the raw slot holds.
  (raw-type t :type (member t single-float double-float
                            #!+long-float long-float
                            complex-single-float complex-double-float
                            #!+long-float complex-long-float
                            sb!vm:word))
  (read-only nil :type (member t nil)))
(def!method print-object ((x defstruct-slot-description) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (dsd-name x) stream)))

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

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

  ;; information about how a slot of a given DSD-RAW-TYPE is to be accessed
  (defstruct raw-slot-data
    ;; the raw slot type, or T for a non-raw slot
    ;;
    ;; (Non-raw slots are in the ordinary place you'd expect, directly
    ;; indexed off the instance pointer.  Raw slots are indexed from the end
    ;; of the instance and skipped by GC.)
    (raw-type (missing-arg) :type (or symbol cons) :read-only t)
    ;; What operator is used to access a slot of this type?
    (accessor-name (missing-arg) :type symbol :read-only t)
    (init-vop (missing-arg) :type symbol :read-only t)
    ;; How many words are each value of this type?
    (n-words (missing-arg) :type (and index (integer 1)) :read-only t)
    ;; Necessary alignment in units of words.  Note that instances
    ;; themselves are aligned by exactly two words, so specifying more
    ;; than two words here would not work.
    (alignment 1 :type (integer 1 2) :read-only t))

  (defvar *raw-slot-data-list*
    (let ((double-float-alignment
           ;; white list of architectures that can load unaligned doubles:
           #!+(or x86 x86-64 ppc) 1
           ;; at least sparc, mips and alpha can't:
           #!-(or x86 x86-64 ppc) 2))
      (list
       (make-raw-slot-data :raw-type 'sb!vm:word
                           :accessor-name '%raw-instance-ref/word
                           :init-vop 'sb!vm::raw-instance-init/word
                           :n-words 1)
       (make-raw-slot-data :raw-type 'single-float
                           :accessor-name '%raw-instance-ref/single
                           :init-vop 'sb!vm::raw-instance-init/single
                           ;; KLUDGE: On 64 bit architectures, we
                           ;; could pack two SINGLE-FLOATs into the
                           ;; same word if raw slots were indexed
                           ;; using bytes instead of words.  However,
                           ;; I don't personally find optimizing
                           ;; SINGLE-FLOAT memory usage worthwile
                           ;; enough.  And the other datatype that
                           ;; would really benefit is (UNSIGNED-BYTE
                           ;; 32), but that is a subtype of FIXNUM, so
                           ;; we store it unraw anyway.  :-( -- DFL
                           :n-words 1)
       (make-raw-slot-data :raw-type 'double-float
                           :accessor-name '%raw-instance-ref/double
                           :init-vop 'sb!vm::raw-instance-init/double
                           :alignment double-float-alignment
                           :n-words (/ 8 sb!vm:n-word-bytes))
       (make-raw-slot-data :raw-type 'complex-single-float
                           :accessor-name '%raw-instance-ref/complex-single
                           :init-vop 'sb!vm::raw-instance-init/complex-single
                           :n-words (/ 8 sb!vm:n-word-bytes))
       (make-raw-slot-data :raw-type 'complex-double-float
                           :accessor-name '%raw-instance-ref/complex-double
                           :init-vop 'sb!vm::raw-instance-init/complex-double
                           :alignment double-float-alignment
                           :n-words (/ 16 sb!vm:n-word-bytes))
       #!+long-float
       (make-raw-slot-data :raw-type long-float
                           :accessor-name '%raw-instance-ref/long
                           :init-vop 'sb!vm::raw-instance-init/long
                           :n-words #!+x86 3 #!+sparc 4)
       #!+long-float
       (make-raw-slot-data :raw-type complex-long-float
                           :accessor-name '%raw-instance-ref/complex-long
                           :init-vop 'sb!vm::raw-instance-init/complex-long
                           :n-words #!+x86 6 #!+sparc 8)))))
(defun raw-slot-words (type)
  (let ((rsd (find type *raw-slot-data-list* :key #'raw-slot-data-raw-type)))
    (if rsd
        (raw-slot-data-n-words rsd)
        (error "Invalid raw slot type: ~S" type))))

;;;; the legendary DEFSTRUCT macro itself (both CL:DEFSTRUCT and its
;;;; close personal friend SB!XC:DEFSTRUCT)

;;; Return a list of forms to install PRINT and MAKE-LOAD-FORM funs,
;;; mentioning them in the expansion so that they can be compiled.
(defun class-method-definitions (defstruct)
  (let ((name (dd-name defstruct)))
    `((locally
        ;; KLUDGE: There's a FIND-CLASS DEFTRANSFORM for constant
        ;; class names which creates fast but non-cold-loadable,
        ;; non-compact code. In this context, we'd rather have
        ;; compact, cold-loadable code. -- WHN 19990928
        (declare (notinline find-classoid))
        ,@(let ((pf (dd-print-function defstruct))
                (po (dd-print-object defstruct))
                (x (sb!xc:gensym "OBJECT"))
                (s (sb!xc:gensym "STREAM")))
            ;; Giving empty :PRINT-OBJECT or :PRINT-FUNCTION options
            ;; leaves PO or PF equal to NIL. The user-level effect is
            ;; to generate a PRINT-OBJECT method specialized for the type,
            ;; implementing the default #S structure-printing behavior.
            (when (or (eq pf nil) (eq po nil))
              (setf pf '(default-structure-print)
                    po 0))
            (flet (;; Given an arg from a :PRINT-OBJECT or :PRINT-FUNCTION
                   ;; option, return the value to pass as an arg to FUNCTION.
                   (farg (oarg)
                     (destructuring-bind (fun-name) oarg
                       fun-name)))
              (cond ((not (eql pf 0))
                     `((def!method print-object ((,x ,name) ,s)
                         (funcall #',(farg pf)
                                  ,x
                                  ,s
                                  *current-level-in-print*))))
                    ((not (eql po 0))
                     `((def!method print-object ((,x ,name) ,s)
                         (funcall #',(farg po) ,x ,s))))
                    (t nil))))
        ,@(let ((pure (dd-pure defstruct)))
            (cond ((eq pure t)
                   `((setf (layout-pure (classoid-layout
                                         (find-classoid ',name)))
                           t)))
                  ((eq pure :substructure)
                   `((setf (layout-pure (classoid-layout
                                         (find-classoid ',name)))
                           0)))))
        ,@(let ((def-con (dd-default-constructor defstruct)))
            (when (and def-con (not (dd-alternate-metaclass defstruct)))
              `((setf (structure-classoid-constructor (find-classoid ',name))
                      #',def-con))))))))

;;; shared logic for host macroexpansion for SB!XC:DEFSTRUCT and
;;; cross-compiler macroexpansion for CL:DEFSTRUCT
(defmacro !expander-for-defstruct (name-and-options
                                   slot-descriptions
                                   expanding-into-code-for-xc-host-p)
  `(let ((name-and-options ,name-and-options)
         (slot-descriptions ,slot-descriptions)
         (expanding-into-code-for-xc-host-p
          ,expanding-into-code-for-xc-host-p))
     (let* ((dd (parse-defstruct-name-and-options-and-slot-descriptions
                 name-and-options
                 slot-descriptions))
            (name (dd-name dd)))
       (if (dd-class-p dd)
           (let ((inherits (inherits-for-structure dd)))
             `(progn
                ;; Note we intentionally enforce package locks and
                ;; call %DEFSTRUCT first, and especially before
                ;; %COMPILER-DEFSTRUCT. %DEFSTRUCT has the tests (and
                ;; resulting CERROR) for collisions with LAYOUTs which
                ;; already exist in the runtime. If there are any
                ;; collisions, we want the user's response to CERROR
                ;; to control what happens. Especially, if the user
                ;; responds to the collision with ABORT, we don't want
                ;; %COMPILER-DEFSTRUCT to modify the definition of the
                ;; class.
                (with-single-package-locked-error
                    (:symbol ',name "defining ~A as a structure"))
                (%defstruct ',dd ',inherits (sb!c:source-location))
                (eval-when (:compile-toplevel :load-toplevel :execute)
                  (%compiler-defstruct ',dd ',inherits))
                ,@(unless expanding-into-code-for-xc-host-p
                    (append ;; FIXME: We've inherited from CMU CL nonparallel
                            ;; code for creating copiers for typed and untyped
                            ;; structures. This should be fixed.
                            ;(copier-definition dd)
                            (constructor-definitions dd)
                            (class-method-definitions dd)))
                ',name))
           `(progn
              (with-single-package-locked-error
                  (:symbol ',name "defining ~A as a structure"))
              (eval-when (:compile-toplevel :load-toplevel :execute)
                (setf (info :typed-structure :info ',name) ',dd))
              (eval-when (:load-toplevel :execute)
                (setf (info :source-location :typed-structure ',name)
                      (sb!c:source-location)))
              ,@(unless expanding-into-code-for-xc-host-p
                  (append (typed-accessor-definitions dd)
                          (typed-predicate-definitions dd)
                          (typed-copier-definitions dd)
                          (constructor-definitions dd)
                          (when (dd-doc dd)
                            `((setf (fdocumentation ',(dd-name dd) 'structure)
                               ',(dd-doc dd))))))
              ',name)))))

(sb!xc:defmacro defstruct (name-and-options &rest slot-descriptions)
  #!+sb-doc
  "DEFSTRUCT {Name | (Name Option*)} {Slot | (Slot [Default] {Key Value}*)}
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
    (!expander-for-defstruct name-and-options slot-descriptions nil))
#+sb-xc-host
(defmacro sb!xc:defstruct (name-and-options &rest slot-descriptions)
  #!+sb-doc
  "Cause information about a target structure to be built into the
  cross-compiler."
  (!expander-for-defstruct name-and-options slot-descriptions t))

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
        (argname (gensym)))
    (when (and predicate-name (dd-named defstruct))
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
                    `(and (= (length (the ,ltype ,argname))
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
              (slot-type `(and ,(dsd-type slot)
                               ,(dd-element-type defstruct))))
          (let ((inherited (accessor-inherited-data name defstruct)))
            (cond
              ((not inherited)
               (stuff `(declaim (inline ,name ,@(unless (dsd-read-only slot)
                                                        `((setf ,name))))))
               ;; FIXME: The arguments in the next two DEFUNs should
               ;; be gensyms. (Otherwise e.g. if NEW-VALUE happened to
               ;; be the name of a special variable, things could get
               ;; weird.)
               (stuff `(defun ,name (structure)
                        (declare (type ,ltype structure))
                        (the ,slot-type (elt structure ,index))))
               (unless (dsd-read-only slot)
                 (stuff
                  `(defun (setf ,name) (new-value structure)
                    (declare (type ,ltype structure) (type ,slot-type new-value))
                    (setf (elt structure ,index) new-value)))))
              ((not (= (cdr inherited) index))
               (style-warn "~@<Non-overwritten accessor ~S does not access ~
                            slot with name ~S (accessing an inherited slot ~
                            instead).~:@>" name (dsd-name slot))))))))
    (stuff)))

;;;; parsing

(defun require-no-print-options-so-far (defstruct)
  (unless (and (eql (dd-print-function defstruct) 0)
               (eql (dd-print-object defstruct) 0))
    (error "No more than one of the following options may be specified:
  :PRINT-FUNCTION, :PRINT-OBJECT, :TYPE")))

;;; Parse a single DEFSTRUCT option and store the results in DD.
(defun parse-1-dd-option (option dd)
  (let ((args (rest option))
        (name (dd-name dd)))
    (case (first option)
      (:conc-name
       (destructuring-bind (&optional conc-name) args
         (setf (dd-conc-name dd)
               (if (symbolp conc-name)
                   conc-name
                   (make-symbol (string conc-name))))))
      (:constructor
       (destructuring-bind (&optional (cname (symbolicate "MAKE-" name))
                                      &rest stuff)
           args
         (push (cons cname stuff) (dd-constructors dd))))
      (:copier
       (destructuring-bind (&optional (copier (symbolicate "COPY-" name)))
           args
         (setf (dd-copier-name dd) copier)))
      (:predicate
       (destructuring-bind (&optional (predicate-name (symbolicate name "-P")))
           args
         (setf (dd-predicate-name dd) predicate-name)))
      (:include
       (when (dd-include dd)
         (error "more than one :INCLUDE option"))
       (setf (dd-include dd) args))
      (:print-function
       (require-no-print-options-so-far dd)
       (setf (dd-print-function dd)
             (the (or symbol cons) args)))
      (:print-object
       (require-no-print-options-so-far dd)
       (setf (dd-print-object dd)
             (the (or symbol cons) args)))
      (:type
       (destructuring-bind (type) args
         (cond ((member type '(list vector))
                (setf (dd-element-type dd) t)
                (setf (dd-type dd) type))
               ((and (consp type) (eq (first type) 'vector))
                (destructuring-bind (vector vtype) type
                  (declare (ignore vector))
                  (setf (dd-element-type dd) vtype)
                  (setf (dd-type dd) 'vector)))
               (t
                (error "~S is a bad :TYPE for DEFSTRUCT." type)))))
      (:named
       (error "The DEFSTRUCT option :NAMED takes no arguments."))
      (:initial-offset
       (destructuring-bind (offset) args
         (setf (dd-offset dd) offset)))
      (:pure
       (destructuring-bind (fun) args
         (setf (dd-pure dd) fun)))
      (t (error "unknown DEFSTRUCT option:~%  ~S" option)))))

;;; Given name and options, return a DD holding that info.
(defun parse-defstruct-name-and-options (name-and-options)
  (destructuring-bind (name &rest options) name-and-options
    (aver name) ; A null name doesn't seem to make sense here.
    (let ((dd (make-defstruct-description name)))
      (dolist (option options)
        (cond ((eq option :named)
               (setf (dd-named dd) t))
              ((consp option)
               (parse-1-dd-option option dd))
              ((member option '(:conc-name :constructor :copier :predicate))
               (parse-1-dd-option (list option) dd))
              (t
               (error "unrecognized DEFSTRUCT option: ~S" option))))

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
         (require-no-print-options-so-far dd)
         (when (dd-named dd)
           (incf (dd-length dd)))
         (let ((offset (dd-offset dd)))
           (when offset (incf (dd-length dd) offset)))))

      (when (dd-include dd)
        (frob-dd-inclusion-stuff dd))

      dd)))

;;; Given name and options and slot descriptions (and possibly doc
;;; string at the head of slot descriptions) return a DD holding that
;;; info.
(defun parse-defstruct-name-and-options-and-slot-descriptions
    (name-and-options slot-descriptions)
  (let ((result (parse-defstruct-name-and-options (if (atom name-and-options)
                                                      (list name-and-options)
                                                      name-and-options))))
    (when (stringp (car slot-descriptions))
      (setf (dd-doc result) (pop slot-descriptions)))
    (dolist (slot-description slot-descriptions)
      (allocate-1-slot result (parse-1-dsd result slot-description)))
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
  (multiple-value-bind (name default default-p type type-p read-only ro-p)
      (typecase spec
        (symbol
         (when (keywordp spec)
           (style-warn "Keyword slot name indicates probable syntax ~
                        error in DEFSTRUCT: ~S."
                       spec))
         spec)
        (cons
         (destructuring-bind
               (name
                &optional (default nil default-p)
                &key (type nil type-p) (read-only nil ro-p))
             spec
           (values name
                   default default-p
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
;;; stored in a raw slot?  Return the matching RAW-SLOT-DATA structure
;; if TYPE should be stored in a raw slot, or NIL if not.
(defun structure-raw-slot-data (type)
  (multiple-value-bind (fixnum? fixnum-certain?)
      (sb!xc:subtypep type 'fixnum)
    ;; (The extra test for FIXNUM-CERTAIN? here is intended for
    ;; bootstrapping the system. In particular, in sbcl-0.6.2, we set up
    ;; LAYOUT before FIXNUM is defined, and so could bogusly end up
    ;; putting INDEX-typed values into raw slots if we didn't test
    ;; FIXNUM-CERTAIN?.)
    (if (or fixnum? (not fixnum-certain?))
        nil
        (dolist (data *raw-slot-data-list*)
          (when (sb!xc:subtypep type (raw-slot-data-raw-type data))
            (return data))))))

;;; Allocate storage for a DSD in DD. This is where we decide whether
;;; a slot is raw or not. Raw objects are aligned on the unit of their size.
(defun allocate-1-slot (dd dsd)
  (let ((rsd
         (if (eq (dd-type dd) 'structure)
             (structure-raw-slot-data (dsd-type dsd))
             nil)))
    (cond
      ((null rsd)
        (setf (dsd-index dsd) (dd-length dd))
        (incf (dd-length dd)))
      (t
        (let* ((words (raw-slot-data-n-words rsd))
               (alignment (raw-slot-data-alignment rsd))
               (off (rem (dd-raw-length dd) alignment)))
          (unless (zerop off)
            (incf (dd-raw-length dd) (- alignment off)))
          (setf (dsd-raw-type dsd) (raw-slot-data-raw-type rsd))
          (setf (dsd-index dsd) (dd-raw-length dd))
          (incf (dd-raw-length dd) words)))))
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
            (when (and (dd-alternate-metaclass included-dd)
                       ;; As of sbcl-0.pre7.73, anyway, STRUCTURE-OBJECT
                       ;; is represented with an ALTERNATE-METACLASS. But
                       ;; it's specifically OK to :INCLUDE (and PCL does)
                       ;; so in this one case, it's OK to include
                       ;; something with :ALTERNATE-METACLASS after all.
                       (not (eql included-name 'structure-object)))
              (error "can't :INCLUDE class ~S (has alternate metaclass)"
                     included-name)))))

      (incf (dd-length dd) (dd-length included-structure))
      (when (dd-class-p dd)
        (let ((mc (rest (dd-alternate-metaclass included-structure))))
          (when (and mc (not (dd-alternate-metaclass dd)))
            (setf (dd-alternate-metaclass dd)
                  (cons included-name mc))))
        (when (eq (dd-pure dd) :unspecified)
          (setf (dd-pure dd) (dd-pure included-structure)))
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
(defun inherits-for-structure (info)
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
       (concatenate 'simple-vector
                    (layout-inherits super)
                    (vector super (classoid-layout (find-classoid 'stream)))))
      ((fd-stream)
       (concatenate 'simple-vector
                    (layout-inherits super)
                    (vector super
                            (classoid-layout (find-classoid 'file-stream)))))
      ((sb!impl::string-input-stream
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
;;; incompatible redefinition. Define those functions which are
;;; sufficiently stereotyped that we can implement them as standard
;;; closures.
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

    (sb!c:with-source-location (source-location)
      (setf (layout-source-location layout) source-location))

    ;; Various other operations only make sense on the target SBCL.
    #-sb-xc-host
    (%target-defstruct dd layout))

  (values))

;;; Return a form describing the writable place used for this slot
;;; in the instance named INSTANCE-NAME.
(defun %accessor-place-form (dd dsd instance-name)
  (let (;; the operator that we'll use to access a typed slot
        (ref (ecase (dd-type dd)
               (structure '%instance-ref)
               (list 'nth-but-with-sane-arg-order)
               (vector 'aref)))
        (raw-type (dsd-raw-type dsd)))
    (if (eq raw-type t) ; if not raw slot
        `(,ref ,instance-name ,(dsd-index dsd))
        (let* ((raw-slot-data (find raw-type *raw-slot-data-list*
                                    :key #'raw-slot-data-raw-type
                                    :test #'equal))
               (raw-slot-accessor (raw-slot-data-accessor-name raw-slot-data)))
          `(,raw-slot-accessor ,instance-name ,(dsd-index dsd))))))

;;; Return source transforms for the reader and writer functions of
;;; the slot described by DSD. They should be inline expanded, but
;;; source transforms work faster.
(defun slot-accessor-transforms (dd dsd)
  (let ((accessor-place-form (%accessor-place-form dd dsd
                                                   `(the ,(dd-name dd) instance)))
        (dsd-type (dsd-type dsd))
        (value-the (if (dsd-safe-p dsd) 'truly-the 'the)))
    (values (sb!c:source-transform-lambda (instance)
              `(,value-the ,dsd-type ,(subst instance 'instance
                                             accessor-place-form)))
            (sb!c:source-transform-lambda (new-value instance)
              (destructuring-bind (accessor-name &rest accessor-args)
                  accessor-place-form
                (once-only ((new-value new-value)
                            (instance instance))
                  `(,(info :setf :inverse accessor-name)
                     ,@(subst instance 'instance accessor-args)
                     (the ,dsd-type ,new-value))))))))

;;; Return a LAMBDA form which can be used to set a slot.
(defun slot-setter-lambda-form (dd dsd)
  ;; KLUDGE: Evaluating the results of SLOT-ACCESSOR-TRANSFORMS needs
  ;; a lexenv.
  (let ((sb!c:*lexenv* (if (boundp 'sb!c:*lexenv*)
                           sb!c:*lexenv*
                           (sb!c::make-null-lexenv))))
    `(lambda (new-value instance)
       ,(funcall (nth-value 1 (slot-accessor-transforms dd dsd))
                 '(dummy new-value instance)))))

;;; Blow away all the compiler info for the structure CLASS. Iterate
;;; over this type, clearing the compiler structure type info, and
;;; undefining all the associated functions.  If SUBCLASSES-P, also do
;;; the same for subclasses.  FIXME: maybe rename UNDEFINE-FUN-NAME to
;;; UNDECLARE-FUNCTION-NAME?
(defun undeclare-structure (classoid subclasses-p)
  (let ((info (layout-info (classoid-layout classoid))))
    (when (defstruct-description-p info)
      (let ((type (dd-name info)))
        (remhash type *typecheckfuns*)
        (setf (info :type :compiler-layout type) nil)
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

;;; Do (COMPILE LOAD EVAL)-time actions for the normal (not
;;; ALTERNATE-LAYOUT) DEFSTRUCT described by DD.
(defun %compiler-defstruct (dd inherits)
  (declare (type defstruct-description dd))

  (%compiler-set-up-layout dd inherits)

  (let* ((dtype (dd-declarable-type dd)))

    (let ((copier-name (dd-copier-name dd)))
      (when copier-name
        (sb!xc:proclaim `(ftype (sfunction (,dtype) ,dtype) ,copier-name))))

    (let ((predicate-name (dd-predicate-name dd)))
      (when predicate-name
        (sb!xc:proclaim `(ftype (sfunction (t) boolean) ,predicate-name))
        ;; Provide inline expansion (or not).
        (ecase (dd-type dd)
          ((structure funcallable-structure)
           ;; Let the predicate be inlined.
           (setf (info :function :inline-expansion-designator predicate-name)
                 (lambda ()
                   `(lambda (x)
                      ;; This dead simple definition works because the
                      ;; type system knows how to generate inline type
                      ;; tests for instances.
                      (typep x ',(dd-name dd))))
                 (info :function :inlinep predicate-name)
                 :inline))
          ((list vector)
           ;; Just punt. We could provide inline expansions for :TYPE
           ;; LIST and :TYPE VECTOR predicates too, but it'd be a
           ;; little messier and we don't bother. (Does anyway use
           ;; typed DEFSTRUCTs at all, let alone for high
           ;; performance?)
           ))))

    (dolist (dsd (dd-slots dd))
      (let* ((accessor-name (dsd-accessor-name dsd))
             (dsd-type (dsd-type dsd)))
        (when accessor-name
          (setf (info :function :structure-accessor accessor-name) dd)
          (let ((inherited (accessor-inherited-data accessor-name dd)))
            (cond
              ((not inherited)
               (multiple-value-bind (reader-designator writer-designator)
                   (slot-accessor-transforms dd dsd)
                 (sb!xc:proclaim `(ftype (sfunction (,dtype) ,dsd-type)
                                   ,accessor-name))
                 (setf (info :function :source-transform accessor-name)
                       reader-designator)
                 (unless (dsd-read-only dsd)
                   (let ((setf-accessor-name `(setf ,accessor-name)))
                     (sb!xc:proclaim
                      `(ftype (sfunction (,dsd-type ,dtype) ,dsd-type)
                        ,setf-accessor-name))
                     (setf (info :function :source-transform setf-accessor-name)
                           writer-designator)))))
              ((not (= (cdr inherited) (dsd-index dsd)))
               (style-warn "~@<Non-overwritten accessor ~S does not access ~
                            slot with name ~S (accessing an inherited slot ~
                            instead).~:@>"
                           accessor-name
                           (dsd-name dsd)))))))))
  (values))

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
  (+ (dd-length dd) (dd-raw-length dd)))

(declaim (ftype (sfunction (defstruct-description) index) dd-instance-length))
(defun dd-instance-length (dd)
  ;; Make sure the object ends at a two-word boundary.  Note that this does
  ;; not affect the amount of memory used, since the allocator would add the
  ;; same padding anyway.  However, raw slots are indexed from the length of
  ;; the object as indicated in the header, so the pad word needs to be
  ;; included in that length to guarantee proper alignment of raw double float
  ;; slots, necessary for (at least) the SPARC backend.
  (let ((layout-length (dd-layout-length dd)))
    (declare (type index layout-length))
    (+ layout-length (mod (1+ layout-length) 2))))

;;; This is called when we are about to define a structure class. It
;;; returns a (possibly new) class object and the layout which should
;;; be used for the new definition (may be the current layout, and
;;; also might be an uninstalled forward referenced layout.) The third
;;; value is true if this is an incompatible redefinition, in which
;;; case it is the old layout.
(defun ensure-structure-class (info inherits old-context new-context
                                    &key compiler-layout)
  (multiple-value-bind (class old-layout)
      (destructuring-bind
          (&optional
           name
           (class 'structure-classoid)
           (constructor 'make-structure-classoid))
          (dd-alternate-metaclass info)
        (declare (ignore name))
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
    (let ((new-layout (make-layout :classoid class
                                   :inherits inherits
                                   :depthoid (length inherits)
                                   :length (dd-layout-length info)
                                   :n-untagged-slots (dd-raw-length info)
                                   :info info))
          (old-layout (or compiler-layout old-layout)))
      (cond
       ((not old-layout)
        (values class new-layout nil))
       (;; This clause corresponds to an assertion in REDEFINE-LAYOUT-WARNING
        ;; of classic CMU CL. I moved it out to here because it was only
        ;; exercised in this code path anyway. -- WHN 19990510
        (not (eq (layout-classoid new-layout) (layout-classoid old-layout)))
        (error "shouldn't happen: weird state of OLD-LAYOUT?"))
       ((not *type-system-initialized*)
        (setf (layout-info old-layout) info)
        (values class old-layout nil))
       ((redefine-layout-warning old-context
                                 old-layout
                                 new-context
                                 (layout-length new-layout)
                                 (layout-inherits new-layout)
                                 (layout-depthoid new-layout)
                                 (layout-n-untagged-slots new-layout))
        (values class new-layout old-layout))
       (t
        (let ((old-info (layout-info old-layout)))
          (typecase old-info
            ((or defstruct-description)
             (cond ((redefine-structure-warning class old-info info)
                    (values class new-layout old-layout))
                   (t
                    (setf (layout-info old-layout) info)
                    (values class old-layout nil))))
            (null
             (setf (layout-info old-layout) info)
             (values class old-layout nil))
            (t
             (error "shouldn't happen! strange thing in LAYOUT-INFO:~%  ~S"
                    old-layout)
             (values class new-layout old-layout)))))))))

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
;;; This is split three ways because:
;;;   * LIST & VECTOR structures need "name" symbols stuck in at
;;;     various weird places, whereas STRUCTURE structures have
;;;     a LAYOUT slot.
;;;   * We really want to use LIST to make list structures, instead of
;;;     MAKE-LIST/(SETF ELT). (We can't in general use VECTOR in an
;;;     analogous way, since VECTOR makes a SIMPLE-VECTOR and vector-typed
;;;     structures can have arbitrary subtypes of VECTOR, not necessarily
;;;     SIMPLE-VECTOR.)
;;;   * STRUCTURE structures can have raw slots that must also be
;;;     allocated and indirectly referenced.
(defun create-vector-constructor (dd cons-name arglist vars types values)
  (let ((temp (gensym))
        (etype (dd-element-type dd)))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar (lambda (var type) `(type (and ,type ,etype) ,var))
                          vars types))
       (let ((,temp (make-array ,(dd-length dd)
                                :element-type ',(dd-element-type dd))))
         ,@(mapcar (lambda (x)
                     `(setf (aref ,temp ,(cdr x))  ',(car x)))
                   (find-name-indices dd))
         ,@(mapcar (lambda (dsd value)
                     (unless (eq value '.do-not-initialize-slot.)
                         `(setf (aref ,temp ,(dsd-index dsd)) ,value)))
                   (dd-slots dd) values)
         ,temp))))
(defun create-list-constructor (dd cons-name arglist vars types values)
  (let ((vals (make-list (dd-length dd) :initial-element nil)))
    (dolist (x (find-name-indices dd))
      (setf (elt vals (cdr x)) `',(car x)))
    (loop for dsd in (dd-slots dd) and val in values do
      (setf (elt vals (dsd-index dsd))
            (if (eq val '.do-not-initialize-slot.) 0 val)))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar (lambda (var type) `(type ,type ,var)) vars types))
       (list ,@vals))))
(defun create-structure-constructor (dd cons-name arglist vars types values)
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
  #!+raw-instance-init-vops
  (let* ((slot-values nil)
         (slot-specs
          (mapcan (lambda (dsd value)
                    (unless (eq value '.do-not-initialize-slot.)
                      (push value slot-values)
                      (list (list* :slot (dsd-raw-type dsd) (dsd-index dsd)))))
                  (dd-slots dd)
                  values)))
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar (lambda (var type) `(type ,type ,var)) vars types))
       (%make-structure-instance-macro ,dd ',slot-specs ,@(reverse slot-values))))
  #!-raw-instance-init-vops
  (let ((instance (gensym "INSTANCE")) slot-values slot-specs raw-slots raw-values)
    (mapc (lambda (dsd value)
            (unless (eq value '.do-not-initialize-slot.)
              (let ((raw-type (dsd-raw-type dsd)))
                (cond ((eq t raw-type)
                       (push value slot-values)
                       (push (list* :slot raw-type (dsd-index dsd)) slot-specs))
                      (t
                       (push value raw-values)
                       (push dsd raw-slots))))))
          (dd-slots dd)
          values)
    `(defun ,cons-name ,arglist
       (declare ,@(mapcar (lambda (var type) `(type ,type ,var)) vars types))
       ,(if raw-slots
            `(let ((,instance (%make-structure-instance-macro ,dd ',slot-specs ,@slot-values)))
              ,@(mapcar (lambda (dsd value)
                          ;; (Note that we can't in general use the
                          ;; ordinary named slot setter function here
                          ;; because the slot might be :READ-ONLY, so we
                          ;; whip up new LAMBDA representations of slot
                          ;; setters for the occasion.)
                          `(,(slot-setter-lambda-form dd dsd) ,value ,instance))
                        raw-slots
                        raw-values)
              ,instance)
            `(%make-structure-instance-macro ,dd ',slot-specs ,@slot-values)))))

;;; Create a default (non-BOA) keyword constructor.
(defun create-keyword-constructor (defstruct creator)
  (declare (type function creator))
  (collect ((arglist (list '&key))
            (types)
            (vals))
    (dolist (slot (dd-slots defstruct))
      (let ((dum (sb!xc:gensym "DUM"))
            (name (dsd-name slot)))
        (arglist `((,(keywordicate name) ,dum) ,(dsd-default slot)))
        (types (dsd-type slot))
        (vals dum)))
    (funcall creator
             defstruct (dd-default-constructor defstruct)
             (arglist) (vals) (types) (vals))))

;;; Given a structure and a BOA constructor spec, call CREATOR with
;;; the appropriate args to make a constructor.
(defun create-boa-constructor (defstruct boa creator)
  (declare (type function creator))
  (multiple-value-bind (req opt restp rest keyp keys allowp auxp aux)
      (parse-lambda-list (second boa))
    (collect ((arglist)
              (vars)
              (types)
              (skipped-vars))
      (labels ((get-slot (name)
                 (let ((res (find name (dd-slots defstruct)
                                  :test #'string=
                                  :key #'dsd-name)))
                   (if res
                       (values (dsd-type res) (dsd-default res))
                       (values t nil))))
               (do-default (arg)
                 (multiple-value-bind (type default) (get-slot arg)
                   (arglist `(,arg ,default))
                   (vars arg)
                   (types type))))
        (dolist (arg req)
          (arglist arg)
          (vars arg)
          (types (get-slot arg)))

        (when opt
          (arglist '&optional)
          (dolist (arg opt)
            (cond ((consp arg)
                   (destructuring-bind
                         ;; FIXME: this shares some logic (though not
                         ;; code) with the &key case below (and it
                         ;; looks confusing) -- factor out the logic
                         ;; if possible. - CSR, 2002-04-19
                         (name
                          &optional
                          (def (nth-value 1 (get-slot name)))
                          (supplied-test nil supplied-test-p))
                       arg
                     (arglist `(,name ,def ,@(if supplied-test-p `(,supplied-test) nil)))
                     (vars name)
                     (types (get-slot name))))
                  (t
                   (do-default arg)))))

        (when restp
          (arglist '&rest rest)
          (vars rest)
          (types 'list))

        (when keyp
          (arglist '&key)
          (dolist (key keys)
            (if (consp key)
                (destructuring-bind (wot
                                     &optional
                                     (def nil def-p)
                                     (supplied-test nil supplied-test-p))
                    key
                  (let ((name (if (consp wot)
                                  (destructuring-bind (key var) wot
                                    (declare (ignore key))
                                    var)
                                  wot)))
                    (multiple-value-bind (type slot-def)
                        (get-slot name)
                      (arglist `(,wot ,(if def-p def slot-def)
                                 ,@(if supplied-test-p `(,supplied-test) nil)))
                      (vars name)
                      (types type))))
                (do-default key))))

        (when allowp (arglist '&allow-other-keys))

        (when auxp
          (arglist '&aux)
          (dolist (arg aux)
            (if (proper-list-of-length-p arg 2)
                (let ((var (first arg)))
                  (arglist arg)
                  (vars var)
                  (types (get-slot var)))
                (skipped-vars (if (consp arg) (first arg) arg))))))

      (funcall creator defstruct (first boa)
               (arglist) (vars) (types)
               (loop for slot in (dd-slots defstruct)
                     for name = (dsd-name slot)
                     collect (cond ((find name (skipped-vars) :test #'string=)
                                    ;; CLHS 3.4.6 Boa Lambda Lists
                                    (setf (dsd-safe-p slot) nil)
                                    '.do-not-initialize-slot.)
                                   ((or (find (dsd-name slot) (vars) :test #'string=)
                                        (let ((type (dsd-type slot)))
                                          (if (eq t type)
                                              (dsd-default slot)
                                              `(the ,type ,(dsd-default slot))))))))))))

;;; Grovel the constructor options, and decide what constructors (if
;;; any) to create.
(defun constructor-definitions (defstruct)
  (let ((no-constructors nil)
        (boas ())
        (defaults ())
        (creator (ecase (dd-type defstruct)
                   (structure #'create-structure-constructor)
                   (vector #'create-vector-constructor)
                   (list #'create-list-constructor))))
    (dolist (constructor (dd-constructors defstruct))
      (destructuring-bind (name &optional (boa-ll nil boa-p)) constructor
        (declare (ignore boa-ll))
        (cond ((not name) (setq no-constructors t))
              (boa-p (push constructor boas))
              (t (push name defaults)))))

    (when no-constructors
      (when (or defaults boas)
        (error "(:CONSTRUCTOR NIL) combined with other :CONSTRUCTORs"))
      (return-from constructor-definitions ()))

    (unless (or defaults boas)
      (push (symbolicate "MAKE-" (dd-name defstruct)) defaults))

    (collect ((res) (names))
      (when defaults
        (let ((cname (first defaults)))
          (setf (dd-default-constructor defstruct) cname)
          (res (create-keyword-constructor defstruct creator))
          (names cname)
          (dolist (other-name (rest defaults))
            (res `(setf (fdefinition ',other-name) (fdefinition ',cname)))
            (names other-name))))

      (dolist (boa boas)
        (res (create-boa-constructor defstruct boa creator))
        (names (first boa)))

      (res `(declaim (ftype
                      (sfunction *
                                 ,(if (eq (dd-type defstruct) 'structure)
                                      (dd-name defstruct)
                                      '*))
                      ,@(names))))

      (res))))

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
;;;; functionality of DEFINE-PRIMITIVE-OBJECT..)

(defun make-dd-with-alternate-metaclass (&key (class-name (missing-arg))
                                              (superclass-name (missing-arg))
                                              (metaclass-name (missing-arg))
                                              (dd-type (missing-arg))
                                              metaclass-constructor
                                              slot-names)
  (let* ((dd (make-defstruct-description class-name))
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
        (%compiler-set-up-layout ',dd ',(inherits-for-structure dd))))))

(sb!xc:proclaim '(special *defstruct-hooks*))

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
         (object-gensym (sb!xc:gensym "OBJECT"))
         (new-value-gensym (sb!xc:gensym "NEW-VALUE-"))
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
           (%compiler-set-up-layout ',dd ',(inherits-for-structure dd)))

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
                         (let ((dsd (find (symbol-name slot-name) dd-slots
                                          :key (lambda (x)
                                                 (symbol-name (dsd-name x)))
                                          :test #'string=)))
                           ;; KLUDGE: bug 117 bogowarning.  Neither
                           ;; DECLAREing the type nor TRULY-THE cut
                           ;; the mustard -- it still gives warnings.
                           (enforce-type dsd defstruct-slot-description)
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

         (when (boundp '*defstruct-hooks*)
           (dolist (fun *defstruct-hooks*)
             (funcall fun (find-classoid ',(dd-name dd)))))))))

;;;; finalizing bootstrapping

;;; Set up DD and LAYOUT for STRUCTURE-OBJECT class itself.
;;;
;;; Ordinary structure classes effectively :INCLUDE STRUCTURE-OBJECT
;;; when they have no explicit :INCLUDEs, so (1) it needs to be set up
;;; before we can define ordinary structure classes, and (2) it's
;;; special enough (and simple enough) that we just build it by hand
;;; instead of trying to generalize the ordinary DEFSTRUCT code.
(defun !set-up-structure-object-class ()
  (let ((dd (make-defstruct-description 'structure-object)))
    (setf
     ;; Note: This has an ALTERNATE-METACLASS only because of blind
     ;; clueless imitation of the CMU CL code -- dunno if or why it's
     ;; needed. -- WHN
     (dd-alternate-metaclass dd) '(t)
     (dd-slots dd) nil
     (dd-length dd) 1
     (dd-type dd) 'structure)
    (%compiler-set-up-layout dd)))
(!set-up-structure-object-class)

;;; early structure predeclarations: Set up DD and LAYOUT for ordinary
;;; (non-ALTERNATE-METACLASS) structures which are needed early.
(dolist (args
         '#.(sb-cold:read-from-file
             "src/code/early-defstruct-args.lisp-expr"))
  (let* ((dd (parse-defstruct-name-and-options-and-slot-descriptions
              (first args)
              (rest args)))
         (inherits (inherits-for-structure dd)))
    (%compiler-defstruct dd inherits)))

;;; finding these beasts
(defun find-defstruct-description (name &optional (errorp t))
  (let ((info (layout-info (classoid-layout (find-classoid name errorp)))))
    (if (defstruct-description-p info)
        info
        (when errorp
          (error "No DEFSTRUCT-DESCRIPTION for ~S." name)))))

(/show0 "code/defstruct.lisp end of file")
