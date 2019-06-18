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

(in-package "SB-KERNEL")

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
           (check-deprecated-type name)
           res))))

(defun compiler-layout-ready-p (name)
  (let ((layout (info :type :compiler-layout name)))
    (and layout (typep (layout-info layout) 'defstruct-description))))

(sb-xc:defmacro %make-structure-instance-macro (dd slot-specs &rest slot-vars)
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
(sb-xc:defmacro %delayed-get-compiler-layout (name)
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
           (sb-c:compiler-notify
            "implementation limitation: ~
             Non-toplevel DEFSTRUCT constructors are slow.")
           (with-unique-names (layout)
             `(let ((,layout (info :type :compiler-layout ',name)))
                (unless (typep (layout-info ,layout) 'defstruct-description)
                  (error "Class is not a structure class: ~S" ',name))
                ,layout))))))

;;; re. %DELAYED-GET-COMPILER-LAYOUT and COMPILE-TIME-FIND-LAYOUT, above..
;;;
;;; FIXME: Perhaps both should be defined with SB-XC:DEFMACRO?
;;; FIXME: Do we really need both? If so, their names and implementations
;;; should probably be tweaked to be more parallel.

;;;; DEFSTRUCT-DESCRIPTION

;;; The DEFSTRUCT-DESCRIPTION structure holds compile-time information
;;; about a structure type.
;;; Its definition occurs in 'early-classoid.lisp'
(defmethod print-object ((x defstruct-description) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (prin1 (dd-name x) stream)))

;;; Does DD describe a structure with a class?
(defun dd-class-p (dd)
  (if (member (dd-type dd) '(structure funcallable-structure)) t nil))

(defun dd-layout-or-lose (dd)
  (compiler-layout-or-lose (dd-name dd)))

;;;; DEFSTRUCT-SLOT-DESCRIPTION

;;; A DEFSTRUCT-SLOT-DESCRIPTION holds compile-time information about
;;; a structure slot. These objects are immutable.
(def!struct (defstruct-slot-description
             (:constructor make-dsd (name type accessor-name bits default))
             (:conc-name dsd-)
             (:copier nil)
             #-sb-xc-host (:pure t))
  (name nil :read-only t)                       ; name of slot
  (type t :read-only t)                         ; declared type specifier
  (accessor-name nil :type symbol :read-only t) ; name of the accessor function
  ;; Packed integer with 4 subfields.
  ;; FIXNUM is ok for the host - it's guaranteed to be at least 16 signed bits
  ;; and we don't have structures whose slot indices run into the thousands.
  (bits 0 :type fixnum :read-only t)
  (default nil :read-only t))                    ; default value expression
#-sb-fluid (declaim (freeze-type defstruct-slot-description))

(eval-when (:compile-toplevel)
  ;; Ensure that rsd-index is representable in 3 bits. (Can easily be changed)
  (assert (<= (1+ (length *raw-slot-data*)) 8)))

;; genesis needs to know how many bits are to the right of the 'index' field
;; in the packed BITS slot of a DSD.
(defconstant +dsd-index-shift+ 6)
(defun pack-dsd-bits (index read-only safe-p always-boundp rsd-index)
  (logior (ash index +dsd-index-shift+)
          (if read-only (ash 1 5) 0)
          (if safe-p (ash 1 4) 0)
          (if always-boundp (ash 1 3) 0)
          (the (unsigned-byte 3) (if rsd-index (1+ rsd-index) 0))))

(declaim (inline dsd-always-boundp
                 dsd-safe-p
                 ; dsd-read-only ; compilation order problem
                 ; dsd-index
                 ))

;;; In general we type-check a slot when it is written, not when read.
;;; There are cases where we must check each read though:

;;; (1) a structure subtype can constrain a slot type more highly than the
;;; parent type constrains it. This requires that each read via the subtype's
;;; accessor be type-checked, because a write via the parent writer may
;;; store a value that does not satisfy the more restrictive constraint.
;;; These slots have SAFE-P = 0 in the dsd.
;;; (2) If a BOA constructor leaves an ordinary (non-raw) slot uninitialized,
;;; then the slot contains the unbound-marker which can be tested with just
;;; an EQ comparison. Such slots have ALWAYS-BOUNDP = 0 in the dsd.
;;; This does not apply to raw slots, which can not hold an unbound marker.

;;; Note that inheritance in the presence of a BOA constructor can cause
;;; the parent structure's notion of ALWAYS-BOUNDP to be wrong.
;;; We don't try to deal with that.
;;; FIXME: We could emit a style-warning if this happens, and/or if any code
;;; was compiled under the assumption that the slot was safe.

;;; Further note that MAKE-LOAD-FORM methods can do damage to type invariants
;;; without any efficient means of detection, if MAKE-LOAD-FORM-SAVING-SLOTS
;;; is used without specifying all slots.

;; Index into *RAW-SLOT-DATA* vector of the RAW-SLOT-DATA for this slot.
;; The index is -1 if this slot is not raw.
(defun dsd-rsd-index (dsd)
  (let ((val (ldb (byte 3 0) (dsd-bits dsd))))
    (if (plusp val) (1- val))))
;; Whether the slot is always bound. Slots are almost always bound,
;; the exception being those which appear as an &AUX var with no value
;; in a BOA constructor.
(defun dsd-always-boundp (dsd) (logbitp 3 (dsd-bits dsd)))
;; Whether the slot is known to be always of the specified type
;; A slot may be SAFE-P even if not always-boundp.
(defun dsd-safe-p (dsd) (logbitp 4 (dsd-bits dsd)))
(defun dsd-read-only (dsd) (logbitp 5 (dsd-bits dsd)))
;; its position in the implementation sequence
(defun dsd-index (dsd) (ash (dsd-bits dsd) (- +dsd-index-shift+)))

(!set-load-form-method defstruct-slot-description (:host :xc :target))
(defmethod print-object ((x defstruct-slot-description) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (dsd-name x) stream)))
(defun dsd-raw-slot-data (dsd)
  (let ((rsd-index (dsd-rsd-index dsd)))
    (and rsd-index
         (svref *raw-slot-data* rsd-index))))
(defun dsd-raw-type (dsd)
  (acond ((dsd-raw-slot-data dsd) (raw-slot-data-raw-type it))
         (t)))
(defun dsd-primitive-accessor (dsd &aux (rsd (dsd-raw-slot-data dsd)))
  (if rsd (raw-slot-data-accessor-name rsd) '%instance-ref))

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
;;;; close personal friend SB-XC:DEFSTRUCT)

(defun %defstruct-package-locks (dd)
  (declare (ignorable dd))
  #-sb-xc-host
  (let ((name (dd-name dd)))
    (with-single-package-locked-error
        (:symbol name "defining ~S as a structure"))
    (awhen (dd-predicate-name dd)
      (with-single-package-locked-error
          (:symbol it "defining ~s as a predicate for ~s structure" name)))
    (awhen (dd-copier-name dd)
      (with-single-package-locked-error
          (:symbol it "defining ~s as a copier for ~s structure" name)))
    (dolist (ctor (dd-constructors dd))
      (with-single-package-locked-error
          (:symbol (car ctor) "defining ~s as a constructor for ~s structure" name)))
    (dolist (dsd (dd-slots dd))
      (awhen (dsd-accessor-name dsd)
        (with-single-package-locked-error
            (:symbol it "defining ~s as an accessor for ~s structure" name))))))

;;; shared logic for host macroexpansion for SB-XC:DEFSTRUCT and
;;; cross-compiler macroexpansion for CL:DEFSTRUCT
;;; This monster has exactly one inline use in the final image,
;;; and we can drop the definition.
;;;
;;; The DELAYP argument deserves some explanation - Because :COMPILE-TOPLEVEL effects
;;; of non-toplevel DEFSTRUCT forms don't happen, the compiler is unable to produce
;;; good accessor code. By delaying compilation until the DEFSTRUCT happens, whenever
;;; or if ever that may be, we can produce better code. DELAY, if true, says to defer
;;; compilation, ignoring the original lexical environment. This approach produces a
;;; nicer expansion in general, versus macroexpanding to yet another macro whose sole
;;; purpose is to sense that earlier compile-time effects have happened.
;;;
;;; Some caveats: (1) a non-toplevel defstruct compiled after already seeing
;;; the same, due to repeated compilation of a file perhaps, will use the known
;;; definition, since technically structures must not be incompatibly redefined.
;;; (2) delayed DEFUNS don't get the right TLF index in their debug info.
;;; We could expand into the internal expansion of DEFUN with an extra argument
;;; for the source location, which would get whatever "here" is instead of random.
;;; In other words: `(progn (sb-impl::%defun struct-slot (...) ... ,(source-location))
;;; and then use that to stuff in the correct info at delayed-compile time.
;;;
;;; Note also that sb-fasteval has some hairy logic to JIT-compile slot accessors.
;;; It might be a lot nicer to pull out that junk, and have this expander know that
;;; it is producing code for fasteval, and explicitly compile much the same way
;;; that delayed accessor compilation happens. Here's a REPL session:
;;; * (defstruct foo val) => FOO
;;; * #'foo-val => #<INTERPRETED-FUNCTION FOO-VAL>
;;; * (defun foovalx (afoo) (* (foo-val afoo) 2)) => FOOVALX
;;; * (foovalx (make-foo :val 9)) => 18
;;; * #'foo-val => #<FUNCTION FOO-VAL>
;;; So FOO-VAL got compiled on demand.
;;;
(declaim (inline !expander-for-defstruct))
(defun !expander-for-defstruct (null-env-p delayp name-and-options
                                slot-descriptions expanding-into-code-for)
  (binding*
        (((name options)
          (if (listp name-and-options)
              (values (car name-and-options) (cdr name-and-options))
              (values name-and-options nil)))
         ((nil) (unless (symbolp name)
                  ;; Rather than hit MAKE-DEFSTRUCT-DESCRIPTION's type-check
                  ;; on the NAME slot, we can be a little more clear.
                  (error "DEFSTRUCT: ~S is not a symbol." name)))
         (dd (make-defstruct-description null-env-p name))
         ((classoid inherits) (parse-defstruct dd options slot-descriptions))
         (constructor-definitions
          (mapcar (lambda (ctor)
                    `(sb-c:xdefun ,(car ctor)
                       :constructor
                       ,@(structure-ctor-lambda-parts dd (cdr ctor))))
                  (dd-constructors dd)))
         (print-method
          (when (dd-print-option dd)
            (let* ((x (make-symbol "OBJECT"))
                   (s (make-symbol "STREAM"))
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
              `((defmethod print-object ((,x ,name) ,s)
                  (funcall #',fname ,x ,s
                           ,@(if depthp `(*current-level-in-print*)))))))))
    ;; Return a list of forms
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
           (%defstruct ',dd ',inherits (sb-c:source-location))
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (%compiler-defstruct ',dd ',inherits))
           ,@(when (eq expanding-into-code-for :target)
               `(,@(let ((defuns
                          `(,@(awhen (dd-copier-name dd)
                                `((sb-c:xdefun ,(dd-copier-name dd) :copier (instance)
                                    (copy-structure (the ,(dd-name dd) instance)))))
                            ,@(awhen (dd-predicate-name dd)
                                `((sb-c:xdefun ,(dd-predicate-name dd) :predicate (object)
                                    (typep object ',(dd-name dd)))))
                            ,@(accessor-definitions dd classoid))))
                     (if (and delayp (not (compiler-layout-ready-p name)))
                         `((sb-impl::%simple-eval ',(cons 'progn defuns)
                                                  (make-null-lexenv)))
                         defuns))
                 ;; This must be in the same lexical environment
                 ,@constructor-definitions
                 ,@print-method
                 ;; Various other operations only make sense on the target SBCL.
                 (%target-defstruct ',dd)))) ; returns NAME
         ;; Not DD-CLASS-P
         ;; FIXME: missing package lock checks
         `((eval-when (:compile-toplevel :load-toplevel :execute)
             (%proclaim-defstruct-ctors ',dd)
             (setf (info :typed-structure :info ',name) ',dd))
           (setf (info :source-location :typed-structure ',name)
                 (sb-c:source-location))
           ,@(when (eq expanding-into-code-for :target)
               `(,@(typed-accessor-definitions dd)
                 ,@(typed-predicate-definitions dd)
                 ,@(typed-copier-definitions dd)
                 ,@constructor-definitions
                 ,@(when (dd-doc dd)
                     `((setf (documentation ',(dd-name dd) 'structure)
                             ',(dd-doc dd))))))
           ',name))))

#+sb-xc-host
(defmacro sb-xc:defstruct (name-and-options &rest slot-descriptions)
  "Cause information about a target structure to be built into the
  cross-compiler."
  `(progn ,@(!expander-for-defstruct
             t nil name-and-options slot-descriptions :host)))

(sb-xc:defmacro defstruct (name-and-options &rest slot-descriptions
                           &environment env)
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
  (let* ((null-env-p
          (etypecase env
           (sb-kernel:lexenv (sb-c::null-lexenv-p env))
           ;; a LOCALLY environment would be fine,
           ;; but is not an important case to handle.
           #+sb-fasteval (sb-interpreter:basic-env nil)
           (null t)))
         ;; Decide whether the expansion should delay reference
         ;; to this structure type. (See explanation up above).
         ;; This is about performance, not semantics. Non-toplevel
         ;; effects happen when they should even if this were to
         ;; produce the preferred expansion for toplevel.
         (delayp (not (or *top-level-form-p* null-env-p))))
    `(progn
       ,@(!expander-for-defstruct
          null-env-p delayp name-and-options slot-descriptions
          :target))))

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
                   (t (bug "Unhandled representation type in typed DEFSTRUCT: ~
                            ~/sb-impl:print-type-specifier/."
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
       #-compact-instance-header
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
               (error ":NAMED option is incompatible with element ~
                        type ~/sb-impl:print-type-specifier/"
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
          (setf (dd-conc-name dd) (string (gensymify* name "-"))))
        (unless (option-present-p :copier)
          (setf (dd-copier-name dd) (symbolicate "COPY-" name)))))
    seen-options))

;;; Given name and options and slot descriptions (and possibly doc
;;; string at the head of slot descriptions) return a DD holding that
;;; info.
(defun parse-defstruct (dd options slot-descriptions)
  (declare (type defstruct-description dd))
  (let ((option-bits (parse-defstruct-options options dd)))
    (when (dd-include dd)
      (frob-dd-inclusion-stuff dd option-bits))
    (when (stringp (car slot-descriptions))
      (setf (dd-doc dd) (pop slot-descriptions)))
    (let* ((inherits
            (if (dd-class-p dd)
                #+sb-xc-host (!inherits-for-structure dd)
                #-sb-xc-host
                (let ((super (compiler-layout-or-lose (or (first (dd-include dd))
                                                          'structure-object))))
                  (concatenate 'simple-vector
                               (layout-inherits super) (vector super)))))
           (proto-classoid
            (if (dd-class-p dd)
                (let* ((classoid (make-structure-classoid :name (dd-name dd)))
                       (layout (make-layout classoid :inherits inherits)))
                  (setf (layout-invalid layout) nil
                        (classoid-layout classoid) layout)
                  classoid))))
      ;; Bind *pending-defstruct-type* to this classoid, which fixes a problem
      ;; when redefining a DEFTYPE which appeared to be a raw slot. e.g.
      ;;   (DEFTYPE X () 'SINGLE-FLOAT) and later (DEFSTRUCT X (A 0 :TYPE X)).
      ;; This is probably undefined behavior, but at least we'll not crash.
      ;; Also make self-referential definitions not signal PARSE-UNKNOWN-TYPE
      ;; on slots whose :TYPE option allows an instance of itself
      (flet ((parse-slots ()
               (dolist (slot-description slot-descriptions)
                 (parse-1-dsd dd slot-description))))
        (if (dd-class-p dd)
            (progn
              (when (info :type :kind (dd-name dd))
                ;; It could be buried anywhere in a complicated type expression.
                ;; There's no way to clear selectively, so just flush the cache.
                (values-specifier-type-cache-clear))
              (let ((*pending-defstruct-type* proto-classoid))
                (parse-slots)))
            (parse-slots)))
      (values proto-classoid inherits))))

;;;; stuff to parse slot descriptions

;;; Parse a slot description for DEFSTRUCT, add it to the description
;;; and return it. If supplied, INCLUDED-SLOT is used to get the default,
;;; type, and read-only flag for the new slot.
(defun parse-1-dsd (defstruct spec &optional included-slot
                    &aux accessor-name (always-boundp t) (safe-p t)
                         rsd-index index)
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
        (t (%program-error "in DEFSTRUCT, ~S is not a legal slot description."
                           spec)))

    (when (find name (dd-slots defstruct) :test #'string= :key #'dsd-name)
      (let* ((parent (find-defstruct-description (first (dd-include defstruct))))
             (included? (find name
                              (dd-slots parent)
                              :key #'dsd-name
                              :test #'string=)))
        (if included?
          (%program-error "slot name ~s duplicated via included ~a"
                          name
                          (dd-name parent))
          (%program-error "duplicate slot name ~S" name))))

    (setf accessor-name (if (dd-conc-name defstruct)
                            (symbolicate (dd-conc-name defstruct) name)
                            name))
    (let ((predicate-name (dd-predicate-name defstruct)))
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
      ;;x  (style-warn "redefining ~/sb-ext:print-symbol-with-prefix/ ~
      ;;                in DEFSTRUCT" accessor-name))
      ;; which was done until sbcl-0.8.11.18 or so, is wrong: it causes
      ;; a warning at MACROEXPAND time, when instead the warning should
      ;; occur not just because the code was constructed, but because it
      ;; is actually compiled or loaded.
      )

    (when (and (not default-p) included-slot)
      (setf default (dsd-default included-slot)))

    (let ((inherited-type (if included-slot (dsd-type included-slot) t)))
      (setf type (cond ((not type-p) inherited-type)
                       ((eq inherited-type t) type)
                       (t `(and ,inherited-type ,type)))))
    (cond (included-slot
           (cond ((not ro-p)
                  (setq read-only (dsd-read-only included-slot)))
                 ((and ro-p (not read-only) (dsd-read-only included-slot))
                  (error "~@<The slot ~S is :READ-ONLY in superclass, and so must ~
                          be :READ-ONLY in subclass.~:@>"
                         name)))
           (setf rsd-index (dsd-rsd-index included-slot)
                 safe-p (dsd-safe-p included-slot)
                 always-boundp (dsd-always-boundp included-slot)
                 index (dsd-index included-slot))
           (when (and safe-p
                      (not (equal type (dsd-type included-slot)))
                      (not (sb-xc:subtypep (dsd-type included-slot) type)))
             (setf safe-p nil)))
          (t
           ;; Compute the index of this DSD. First decide whether the slot is raw.
           (setf rsd-index (and (eq (dd-type defstruct) 'structure)
                                (structure-raw-slot-data-index type)))
           (let ((n-words
                  (if rsd-index
                      (let ((rsd (svref *raw-slot-data* rsd-index)))
                        ;; If slot requires alignment of 2, then ensure that
                        ;; it has an odd (i.e. doubleword aligned) index.
                        (when (and (eql (raw-slot-data-alignment rsd) 2)
                                   (evenp (dd-length defstruct)))
                          (incf (dd-length defstruct)))
                        (raw-slot-data-n-words rsd))
                      1)))
             (setf index (dd-length defstruct))
             (incf (dd-length defstruct) n-words))))

    ;; Check for existence of any BOA constructor that leaves the
    ;; slot with an unspecified value, as when it's initialized
    ;; by an &AUX binding with no value (CLHS 3.4.6)
    (when (and always-boundp
               (some (lambda (ctor &aux (ll-parts (cdr ctor)))
                       ;; Keyword constructors store :DEFAULT in the cdr of the cell.
                       ;; BOA constructors store the parsed lambda list.
                       (and (listp ll-parts) ; = (llks req opt rest key aux)
                            (some (lambda (binding)
                                    (and (or (atom binding) (not (cdr binding)))
                                         (string= (if (atom binding) binding (car binding))
                                                  name)))
                                  (sixth ll-parts))))
                     (dd-constructors defstruct)))
      (setf always-boundp nil))
    (unless always-boundp
      ;; FIXME: the :TYPE option should not preclude storing #<unbound>
      ;; unless the storage is a specialized numeric vector.
      (when (or rsd-index (neq (dd-type defstruct) 'structure))
        (setf always-boundp t safe-p nil))) ; "demote" to unsafe.

    (let ((dsd (make-dsd name type accessor-name
                         (pack-dsd-bits index read-only safe-p
                                        always-boundp rsd-index)
                         default)))
      (setf (dd-slots defstruct) (nconc (dd-slots defstruct) (list dsd)))
      dsd)))

;;; When a value of type TYPE is stored in a structure, should it be
;;; stored in a raw slot?  Return the index of the matching RAW-SLOT-DATA
;;; if TYPE should be stored in a raw slot, or NIL if not.
(defun structure-raw-slot-data-index (type)
  ;; If TYPE isn't a subtype of NUMBER, it can't go in a raw slot.
  ;; In the negative case (which is most often), doing 1 SUBTYPEP test
  ;; beats doing 5 or 6.
  ;; During self-build, first test whether the type can hold NIL,
  ;; as it avoid a bootstrapping problem. But before testing that,
  ;; check that the type is fully defined, as otherwise it requires
  ;; extra complexity in CROSS-TYPEP.
  ;; Skip it normally, since SUBTYPEP NUMBER is sufficient.
  (when (and #+sb-xc-host (not (sb-xc:typep nil type))
             (sb-xc:subtypep type 'number)
             ;; FIXNUMs and smaller go in tagged slots, not raw slots
             (not (sb-xc:subtypep type 'fixnum)))
    (dotimes (i (length *raw-slot-data*))
      (let ((data (svref *raw-slot-data* i)))
        (when (sb-xc:subtypep type (raw-slot-data-raw-type data))
          (return i))))))

(defun typed-structure-info-or-lose (name)
  (or (info :typed-structure :info name)
      (error ":TYPE'd DEFSTRUCT ~S not found for inclusion." name)))

;;; Process any included slots pretty much like they were specified.
;;; Also inherit various other attributes.
(defun frob-dd-inclusion-stuff (dd option-bits)
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
                  (%program-error "slot name ~S not present in included structure"
                                 name))
                (when (find name (cdr slots)
                            :test #'string= :key #'included-slot-name)
                  (%program-error "included slot name ~S specified more than once"
                                 name)))
              modified-slots))

      (incf (dd-length dd) (dd-length included-structure))
      (when (dd-class-p dd)
        ;; FIXME: This POSITION call should be foldable without read-time eval
        ;; since literals are immutable, and +DD-OPTION-NAMES+ was initialized
        ;; from a literal.
        (unless (logbitp #.(position :pure +dd-option-names+) option-bits)
          (setf (dd-pure dd) (dd-pure included-structure))))

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
          (let ((new-slot (parse-1-dsd dd modified included-slot)))
            (when (and (dsd-safe-p included-slot) (not (dsd-safe-p new-slot)))
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
      ((sb-impl::string-input-stream ; etc
        sb-impl::string-output-stream
        sb-impl::fill-pointer-output-stream)
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
      (setf (classoid-source-location classoid) source-location))))


;;; Return a form accessing the writable place used for the slot
;;; described by DD and DSD in the INSTANCE (a form).
(defun %accessor-place-form (dd dsd instance)
  (let (;; Compute REF even if not using it, as a sanity-check of DD-TYPE.
        (ref (ecase (dd-type dd)
               (structure '%instance-ref)
               (funcallable-structure '%funcallable-instance-info)
               (list 'nth)
               (vector 'aref)))
        (index (dsd-index dsd))
        (rsd (dsd-raw-slot-data dsd)))
    (cond (rsd
           (list (raw-slot-data-accessor-name rsd) instance index))
          ((eq ref 'nth)
           (list ref index instance))
          (t
           (list ref instance index)))))

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
            ;; There are 4 cases of {safe,unsafe} x {always-boundp,possibly-unbound}
            ;; If unsafe - which implies TYPE-SPEC other than type T - then we must
            ;; check the type on each read. Assuming that type-checks reject
            ;; the unbound-marker, then we needn't separately check for it.
            (cond ((not (dsd-safe-p dsd))
                   `(the ,type-spec ,place))
                  (t
                   (unless (dsd-always-boundp dsd)
                     (setf place `(the* ((not (satisfies sb-vm::unbound-marker-p))
                                         :context (:struct-read ,(dd-name dd) . ,(dsd-name dsd)))
                                         ,place)))
                   (if (eq type-spec t) place `(the* (,type-spec :derive-type-only t) ,place)))))
          (when (singleton-p (cdr args))
            (let ((inverse (info :setf :expander (car place))))
              (flet ((check (newval)
                       (if (eq type-spec t)
                           newval
                           `(the* (,type-spec :context
                                              (:struct ,(dd-name dd) . ,(dsd-name dsd)))
                                  ,newval))))
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

;;; Apply TRANSFORM - a special indicator stored in :SOURCE-TRANSFORM
;;; for a DEFSTRUCT copier, accessor, or predicate - to SEXPR.
;;; NAME is needed to select between the reader and writer transform.
(defun sb-c::struct-fun-transform (transform sexpr name)
  (let* ((snippet (cdr transform))
         (result
          (cond ((eq snippet :constructor)
                 ;; All defstruct-defined things use the :source-transform as
                 ;; an indicator of magic-ness, but actually doing the transform
                 ;; for constructors could cause inadvertent variable capture.
                 nil)
                ((symbolp snippet) ; predicate or copier
                 (when (singleton-p (cdr sexpr)) ; exactly 1 arg
                   (let ((type (dd-name (car transform)))
                         (arg (cadr sexpr)))
                     (ecase snippet
                      (:predicate `(sb-c::%instance-typep ,arg ',type))
                      (:copier `(copy-structure (the ,type ,arg)))))))
                (t
                 (slot-access-transform (if (consp name) :write :read)
                                        (cdr sexpr) transform)))))
    (values result (not result))))

;;; Return a LAMBDA form which can be used to set a slot
(defun slot-setter-lambda-form (dd dsd)
  `(lambda (newval instance)
     (declare (optimize (debug 0)))
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
  (aver (not (dd-class-p dd)))
  (let ((info `(,dd . :constructor)))
    (dolist (ctor (dd-constructors dd))
      (setf (info :function :source-transform (car ctor)) info))))

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
    (let ((xform `(,dd . :constructor)))
      (dolist (ctor (dd-constructors dd))
        ;; Don't check-inlining because ctors aren't always inlined
        (setf (info :function :source-transform (car ctor)) xform)))
    (awhen (dd-copier-name dd)
      (when check-inlining (push it fnames))
      (setf (info :function :source-transform it) (cons dd :copier)))
    (awhen (dd-predicate-name dd)
      (when check-inlining (push it fnames))
      (setf (info :function :source-transform it) (cons dd :predicate)))

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

    (awhen (remove-if-not #'sb-c::emitted-full-call-count fnames)
      (sb-c:compiler-style-warn
       'sb-c:inlining-dependency-failure
       ;; This message omits the http://en.wikipedia.org/wiki/Serial_comma
       :format-control "~@<Previously compiled call~P to ~
~{~/sb-ext:print-symbol-with-prefix/~^~#[~; and~:;,~] ~} ~
could not be inlined because the structure definition for ~
~/sb-ext:print-symbol-with-prefix/ was not yet seen. To avoid this warning, ~
DEFSTRUCT should precede references to the affected functions, ~
or they must be declared locally notinline at each call site.~@:>"
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
          (unless (sb-xc:subtypep (dsd-type ns) (dsd-type os))
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

;;; Return true if destructively modifying OLD-LAYOUT into NEW-LAYOUT
;;; would be possible in as much as it won't harm the garbage collector.
;;; Harm potentially results from turning a raw word into a tagged word.
(defun mutable-layout-p (old-layout new-layout)
  (if (layout-info old-layout)
      (let ((old-bitmap (layout-bitmap old-layout))
            (new-bitmap (layout-bitmap new-layout)))
        (aver (= old-bitmap (dd-bitmap (layout-info old-layout))))
        (aver (= new-bitmap (dd-bitmap (layout-info new-layout))))
        (dotimes (i (dd-length (layout-info old-layout)) t)
          (when (and (logbitp i new-bitmap) ; a tagged (i.e. scavenged) slot
                     (not (logbitp i old-bitmap))) ; that was opaque bits
            (return nil))))
      t))

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
       :test (lambda (c)
               (declare (ignore c))
               (mutable-layout-p old-layout new-layout))
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
                        :destruct-layout old-layout))))
  (values))

;;; Compute DD's bitmap, storing 1 for each tagged word.
;;; The bitmap should be stored as a negative fixnum in two cases:
;;;  (1) if the positive value is a bignum but the negative is a fixnum.
;;;  (2) if there are no raw slots at all.
;;; Example: given (DEFSTRUCT S A B C), the computed bitmap is #b11111 -
;;; one bit for the layout; one each for A, B, C; and one for padding.
;;; Whether this is stored as 31 or -1 is mostly immaterial,
;;; but -1 is preferable because GC has a special case for it.
;;; Suppose instead we have 1 untagged word followed by N tagged words
;;; for N > n-fixnum-bits. The computed bitmap is #b111...11101
;;; but the sign-extended value is -3, which is a fixnum.
;;; If both the + and - values are fixnums, and raw slots are present,
;;; we'll choose the positive value.
(defun dd-bitmap (dd)
  ;; With compact instances, LAYOUT is not reflected in the bitmap.
  ;; Without compact instances, the 0th bitmap bit (for the LAYOUT) is always 1.
  ;; In neither case is the place for the layout represented in in DD-SLOTS.
  (let ((bitmap sb-vm:instance-data-start))
    (dolist (slot (dd-slots dd))
      (when (eql t (dsd-raw-type slot))
        (setf bitmap (logior bitmap (ash 1 (dsd-index slot))))))
    (let* ((length (dd-length dd))
           (n-bits (logior length 1)))
      (when (evenp length) ; Add padding word if necessary.
        (setq bitmap (logior bitmap (ash 1 length))))
      (when (and (logbitp (1- n-bits) bitmap)
                 ;; Bitmap of -1 implies that all slots are tagged,
                 ;; and no extraordinary GC treatment is needed.
                 ;; If all are tagged but any special treatment is required,
                 ;; then the bitmap can't be -1.
                 (named-let admits-bitmap-optimization ((dd dd))
                   (cond ((eq (dd-name dd) 'list-node) nil)
                         ((not (dd-include dd)) t)
                         ((admits-bitmap-optimization
                           (find-defstruct-description (car (dd-include dd))))))))
        (let ((sign-ext (logior (ash -1 n-bits) bitmap)))
          (when (or (and (fixnump sign-ext) (sb-xc:typep bitmap 'bignum))
                    (eql sign-ext -1))
            (return-from dd-bitmap sign-ext)))))
    bitmap))

;;; This is called when we are about to define a structure class. It
;;; returns a (possibly new) class object and the layout which should
;;; be used for the new definition (may be the current layout, and
;;; also might be an uninstalled forward referenced layout.) The third
;;; value is true if this is an incompatible redefinition, in which
;;; case it is the old layout.
(defun ensure-structure-class (info inherits old-context new-context
                                    &key compiler-layout)
  (declare (type defstruct-description info))
  (multiple-value-bind (classoid old-layout)
      (multiple-value-bind (class constructor)
          (acond ((cdr (dd-alternate-metaclass info))
                  (values (first it) (second it)))
                 (t
                  (values 'structure-classoid 'make-structure-classoid)))
        (insured-find-classoid (dd-name info)
                               (if (eq class 'structure-classoid)
                                   (lambda (x)
                                     (sb-xc:typep x 'structure-classoid))
                                   (lambda (x)
                                     (sb-xc:typep x (classoid-name (find-classoid class)))))
                               (fdefinition constructor)))
    (setf (classoid-direct-superclasses classoid)
          (case (dd-name info)
            ((ansi-stream
              fd-stream
              sb-impl::string-input-stream sb-impl::string-output-stream
              sb-impl::fill-pointer-output-stream)
             (list (layout-classoid (svref inherits (1- (length inherits))))
                   (layout-classoid (svref inherits (- (length inherits) 2)))))
            (t
             (list (layout-classoid
                    (svref inherits (1- (length inherits))))))))
    (let* ((old-layout (or compiler-layout old-layout))
           (flags (if (dd-alternate-metaclass info) 0 +structure-layout-flag+))
           (new-layout
            (when (or (not old-layout) *type-system-initialized*)
              (macrolet ((inherit (n) `(if (> depthoid ,n) (svref inherits ,n) 0)))
                (let ((depthoid (length inherits)))
                  (make-layout classoid
                               :flags flags
                               :inherits inherits
                               :depthoid depthoid
                               :depth2-ancestor (inherit 2)
                               :depth3-ancestor (inherit 3)
                               :depth4-ancestor (inherit 4)
                               :length (dd-length info)
                               :info info
                               :bitmap (dd-bitmap info)))))))
      (cond
       ((not old-layout)
        (values classoid new-layout nil))
       ((not new-layout)
        ;; The assignment of INFO here can almost be deleted,
        ;; except for a few magical types that don't d.t.r.t. in cold-init:
        ;;  STRUCTURE-OBJECT, CONDITION, ALIEN-VALUE, INTERPRETED-FUNCTION
        (setf (layout-info old-layout) info)
        (values classoid old-layout nil))
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
                                 (layout-bitmap new-layout))
        (values classoid new-layout old-layout))
       (t
        (let ((old-info (layout-info old-layout)))
          (if old-info
             (cond ((redefine-structure-warning classoid old-info info)
                    (values classoid new-layout old-layout))
                   (t
                    (setf (layout-info old-layout) info)
                    (values classoid old-layout nil)))
             (progn
               (setf (layout-info old-layout) info)
               (values classoid old-layout nil)))))))))

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
(defun instance-constructor-form (dd values &aux (dd-slots (dd-slots dd)))
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
  (aver (= (length dd-slots) (length values)))
  (if (or #+(or ppc ppc64 x86 x86-64) t)
    ;; Have raw-instance-init vops
    (collect ((slot-specs) (slot-values))
      (mapc (lambda (dsd value &aux (raw-type (dsd-raw-type dsd))
                                    (spec (list* :slot raw-type (dsd-index dsd))))
              (cond ((eq value '.do-not-initialize-slot.)
                     (when (eq raw-type t)
                       (rplaca spec :unbound)
                       (slot-specs spec)))
                    (t
                     (slot-specs spec)
                     (slot-values value))))
            dd-slots values)
      `(%make-structure-instance-macro ,dd ',(slot-specs) ,@(slot-values)))
    ;; Don't have raw-instance-init vops
    (collect ((slot-specs) (slot-values) (raw-slots) (raw-values))
      ;; Partition into non-raw and raw
      (mapc (lambda (dsd value &aux (raw-type (dsd-raw-type dsd))
                                    (spec (list* :slot raw-type (dsd-index dsd))))
              (cond ((eq value '.do-not-initialize-slot.)
                     (when (eq raw-type t)
                       (rplaca spec :unbound)
                       (slot-specs spec)))
                    ((eq raw-type t)
                     (slot-specs spec)
                     (slot-values value))
                    (t
                     (raw-slots dsd)
                     (raw-values value))))
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

;;; A "typed" constructor prefers to use a single call to LIST or VECTOR
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

;;; Return the ftype of global function NAME.
(defun global-ftype (name &aux xform)
  (multiple-value-bind (type foundp) (info :function :type name)
    (cond
     #-sb-xc-host ; PCL "doesn't exist" yet
     ((eq type :generic-function) (sb-pcl::compute-gf-ftype name))
     ((consp type) ; not parsed type
      ;; This case is used only for DEFKNOWN. It allows some out-of-order
      ;; definitions during bootstrap while avoiding the "uncertainty in typep"
      ;; error. It would work for user code as well, but users shouldn't write
      ;; out-of-order type definitions. In any case, it's not wrong to leave
      ;; this case in.
      (let ((ctype (specifier-type type)))
        (unless (contains-unknown-type-p ctype)
          (setf (info :function :type name) ctype))
        ctype))
     ;; In the absence of global info for a defstruct snippet, get the compiler's
     ;; opinion based on the defstruct definition, rather than reflecting on the
     ;; current function (as defined "now") which is what globaldb would get in
     ;; effect by calling FTYPE-FROM-FDEFN, that being less precise.
     ((and (not foundp)
           (typep (setq xform (info :function :source-transform name))
                  '(cons defstruct-description)))
      (let* ((dd (car xform))
             (snippet (cdr xform))
             (dd-name (dd-name dd)))
        (specifier-type
         (case snippet
          (:constructor
           (let ((ctor (assq name (dd-constructors dd))))
             (aver ctor)
             (%struct-ctor-ftype dd (cdr ctor) (dd-element-type dd))))
          (:predicate `(function (t) (values boolean &optional)))
          (:copier    `(function (,dd-name) (values ,dd-name &optional)))
          (t
           (let ((type (dsd-type snippet)))
             (if (consp name)
                 `(function (,type ,dd-name) (values ,type &optional))  ; writer
                 `(function (,dd-name) (values ,type &optional))))))))) ; reader
     (t
      type))))

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
                                (if (eq type t)
                                    var
                                    `(the* (,type :context
                                                  (:struct ,(dd-name dd) . ,(dsd-name dsd)))
                                           ,var))))
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
      `(,(sb-c::make-lambda-list
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

(defun accessor-definitions (dd *pending-defstruct-type*)
  (loop for dsd in (dd-slots dd)
        for accessor-name = (dsd-accessor-name dsd)
        unless (accessor-inherited-data accessor-name dd)
        nconc (dx-let ((key (cons dd dsd)))
                `(,@(unless (dsd-read-only dsd)
                     `((sb-c:xdefun (setf ,accessor-name) :accessor (value instance)
                         ,(slot-access-transform :setf '(instance value) key))))
                  (sb-c:xdefun ,accessor-name :accessor (instance)
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
;;;   CONDITION SB-KERNEL:INTERPRETED-FUNCTION
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
         (conc-name (string (gensymify* class-name "-")))
         ;; Without compact instance headers, the index starts at 1 for
         ;; named slots, because slot 0 is the LAYOUT.
         ;; This is the same in ordinary structures too: see (INCF DD-LENGTH)
         ;; in PARSE-DEFSTRUCT-NAME-AND-OPTIONS.
         ;; With compact instance headers, slot 0 is a data slot.
         (slot-index sb-vm:instance-data-start))
    ;; We do *not* fill in the COPIER-NAME and PREDICATE-NAME
    ;; because alternate-metaclass structures can not have either.
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
          (dd-slots dd)
          (mapcar (lambda (slot-name)
                    (make-dsd slot-name t (symbolicate conc-name slot-name)
                              (pack-dsd-bits (prog1 slot-index (incf slot-index))
                                             nil t t nil)
                              nil))
                  slot-names)
          (dd-length dd) slot-index
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
                (constructor (missing-arg))
                (superclass-name (missing-arg))
                (metaclass-name (missing-arg))
                (metaclass-constructor (missing-arg))
                (dd-type (missing-arg)))

  (declare (type (and list (not null)) slot-names))
  (declare (type (and symbol (not null))
                 superclass-name
                 metaclass-name
                 metaclass-constructor))
  (declare (symbol constructor)) ; NIL for none
  (declare (type (member structure funcallable-structure) dd-type))
  (declare (ignore constructor))

  (let* ((dd (make-dd-with-alternate-metaclass
              :class-name class-name
              :slot-names slot-names
              :superclass-name superclass-name
              :metaclass-name metaclass-name
              :metaclass-constructor metaclass-constructor
              :dd-type dd-type)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; COMPILER-DEFSTRUCT informs the compiler of all the
       ;; source-transforms for slot access. They can exist
       ;; despite lack of ftype information.
       (%compiler-defstruct ',dd ',(!inherits-for-structure dd)))))

(sb-xc:defmacro !defstruct-with-alternate-metaclass
    (class-name &key
                (slot-names (missing-arg))
                (constructor (missing-arg))
                (superclass-name (missing-arg))
                (metaclass-name (missing-arg))
                (metaclass-constructor (missing-arg))
                (dd-type (missing-arg)))

  (declare (type (and list (not null)) slot-names))
  (declare (type (and symbol (not null))
                 superclass-name
                 metaclass-name
                 metaclass-constructor))
  (declare (symbol constructor)) ; NIL for none
  (declare (type (member structure funcallable-structure) dd-type))

  (let* ((dd (make-dd-with-alternate-metaclass
              :class-name class-name
              :slot-names slot-names
              :superclass-name superclass-name
              :metaclass-name metaclass-name
              :metaclass-constructor metaclass-constructor
              :dd-type dd-type))
         (delayed-layout-form `(%delayed-get-compiler-layout ,class-name))
         (raw-maker-form
          (ecase dd-type
           (structure `(%make-structure-instance-macro ,dd nil))
          (funcallable-structure
           `(let ((object
                           ;; TRULY-THE should not be needed. But it is, to avoid
                           ;; a type check on the next SETF. Why???
                           (truly-the funcallable-instance
                            (%make-funcallable-instance ,(dd-length dd)))))
                      (setf (%funcallable-instance-layout object)
                            ,delayed-layout-form)
                      object)))))
    `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (%compiler-defstruct ',dd ',(!inherits-for-structure dd)))
         ;; None of the alternate-metaclass structures allow specifying a type
         ;; for any of their slots, so one of the reasons for passing a classoid
         ;; to ACCESSOR-DEFINITIONS is irrelevant. The other reason, redefinition,
         ;; is not even possible. But we have to pass something.
         ,@(accessor-definitions dd (make-undefined-classoid class-name))
         ,@(when constructor
             `((defun ,constructor (,@slot-names &aux (object ,raw-maker-form))
                 ,@(mapcar (lambda (dsd)
                             `(setf (,(dsd-accessor-name dsd) object) ,(dsd-name dsd)))
                           (dd-slots dd))
                 object)))

         ;; Usually we AVER instead of ASSERT, but AVER isn't defined yet.
         ;; A naive reading of 'build-order' suggests it is,
         ;; but due to def!struct delay voodoo, it isn't.
         (assert (null (symbol-value '*defstruct-hooks*))))))

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
     (dd-length dd) sb-vm:instance-data-start
     (dd-type dd) 'structure)
    (%compiler-set-up-layout dd)))
#+sb-xc-host(!set-up-structure-object-class)

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
  (defun %instance-layout (instance)
    (classoid-layout (find-classoid (type-of instance))))
  (defun %instance-length (instance)
    ;; In the target, it is theoretically possible to have %INSTANCE-LENGTH
    ;; exceeed layout length, but in the cross-compiler they're the same.
    (layout-length (%instance-layout instance)))
  (defun %instance-ref (instance index)
    (let ((layout (%instance-layout instance)))
      ;; with compact headers, 0 is an ordinary slot index.
      ;; without, it's the layout.
      (if (eql index (1- sb-vm:instance-data-start))
          (error "XC Host should use %INSTANCE-LAYOUT, not %INSTANCE-REF 0")
          (let* ((dd (layout-info layout))
                 ;; If data starts at 1, then subtract 1 from index.
                 ;; otherwise use the index as-is.
                 (dsd (elt (dd-slots dd)
                           (- index sb-vm:instance-data-start)))
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

;;; It's easier for the compiler to recognize the output of M-L-F-S-S
;;; without extraneous QUOTE forms, so we define some trivial wrapper macros.
(defmacro new-instance (type) `(allocate-instance (find-class ',type)))
(defmacro sb-pcl::set-slots (instance name-list &rest values)
  `(sb-pcl::%set-slots ,instance ',name-list ,@values))

;;; We require that MAKE-LOAD-FORM-SAVING-SLOTS produce deterministic output
;;; and that its output take a particular recognizable form so that it can
;;; be optimized into a sequence of fasl ops.
;;; The cross-compiler depends critically on optimizing the resulting sexprs
;;; so that the host can load cold objects, which it could not do
;;; if constructed by machine code for the target.
;;; This ends up being a performance win for the target system as well.
;;; It is possible to produce instances of structure-object which violate
;;; the assumption throughout the compiler that slot readers are safe
;;; unless dictated otherwise by the SAFE-P flag in the DSD.
;;;  * (defstruct S a (b (error "Must supply me") :type symbol))
;;;  * (defmethod make-load-form ((x S) &optional e) (m-l-f-s-s x :slot-names '(a)))
;;; After these definitions, a dumped S will have 0 in slot B.
;;;
(defun sb-xc:make-load-form-saving-slots (object &key (slot-names nil slot-names-p)
                                                        environment)
  (declare (ignore environment))
  (flet ((quote-p (thing) (not (self-evaluating-p thing))))
    (declare (inline quote-p))
    ;; If TYPE-OF isn't a symbol, the creation form probably can't be compiled
    ;; unless there is a MAKE-LOAD-FORM on the class without a proper-name.
    ;; This is better than returning a creation form that produces
    ;; something completely different.
    (values (let ((type (type-of object)))
              `(,(if (symbolp type) 'new-instance 'allocate-instance) ,type))
            (if (typep object 'structure-object)
                `(setf ,@(mapcan
                          (lambda (dsd)
                            (declare (type defstruct-slot-description dsd))
                            (when (or (not slot-names-p)
                                      (memq (dsd-name dsd) slot-names))
                              (let* ((acc (dsd-primitive-accessor dsd))
                                     (ind (dsd-index dsd))
                                     (val (funcall acc object ind)))
                                (list `(,acc ,object ,ind)
                                      (if (quote-p val) `',val val)))))
                          (dd-slots (layout-info (%instance-layout object)))))
                #-sb-xc-host
                (loop for slot in (sb-mop:class-slots (class-of object))
                      for name = (sb-mop:slot-definition-name slot)
                      when (if slot-names-p
                               (memq name slot-names)
                               (eq (sb-mop:slot-definition-allocation slot) :instance))
                      collect name into names
                      and
                      collect (if (slot-boundp object name)
                                  (let ((val (slot-value object name)))
                                    (if (quote-p val) `',val val))
                                  'sb-pcl:+slot-unbound+) into vals
                      finally (return `(sb-pcl::set-slots ,object ,names ,@vals)))))))

;;; Call MAKE-LOAD-FORM inside a condition handler in case the method fails,
;;; returning its two values on success.
;;; If the resulting CREATION-FORM and INIT-FORM are equivalent to those
;;; returned from MAKE-LOAD-FORM-SAVING-SLOTS, return NIL and 'SB-FASL::FOP-STRUCT.
(defun sb-c::%make-load-form (constant)
  (flet ((canonical-p (inits dsds object &aux reader)
           ;; Return T if (but not only-if) INITS came from M-L-F-S-S.
           (dolist (dsd dsds (null inits))
             (declare (type defstruct-slot-description dsd))
             (if (and (listp inits)
                      (let ((place (pop inits)))
                        (and (listp place)
                             (eq (setq reader (dsd-primitive-accessor dsd))
                                 (pop place))
                             (listp place) (eq object (pop place))
                             (singleton-p place)
                             (eql (dsd-index dsd) (car place))))
                      (let ((init (and (listp inits) (car inits)))
                            (val (funcall reader object (dsd-index dsd))))
                        (if (self-evaluating-p val)
                            (and inits (eql val init))
                            (and (typep init '(cons (eql quote)))
                                 (singleton-p (cdr init))
                                 (eq val (cadr init))))))
                 (pop inits)
                 (return nil)))))
    (multiple-value-bind (creation-form init-form)
        (handler-case (sb-xc:make-load-form constant (make-null-lexenv))
          (error (condition) (sb-c:compiler-error condition)))
      (cond ((and (listp creation-form)
                  (typep constant 'structure-object)
                  (typep creation-form
                         '(cons (eql new-instance) (cons symbol null)))
                  (eq (second creation-form) (type-of constant))
                  (typep init-form '(cons (eql setf)))
                  (canonical-p (cdr init-form)
                               (dd-slots (layout-info (%instance-layout constant)))
                               constant))
             (values nil 'sb-fasl::fop-struct))
            (t
             (values creation-form init-form))))))

(/show0 "code/defstruct.lisp end of file")
