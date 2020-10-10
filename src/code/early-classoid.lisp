;;;; This file contains structures and functions for the maintenance of
;;;; basic information about defined types. Different object systems
;;;; can be supported simultaneously.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;;; DEFSTRUCT-DESCRIPTION

;;; The DEFSTRUCT-DESCRIPTION structure holds compile-time information
;;; about a structure type.
;;; It is defined prior to LAYOUT because a LAYOUT-INFO slot
;;; is declared to hold a DEFSTRUCT-DESCRIPTION.
(def!struct (defstruct-description
             (:conc-name dd-)
             (:copier nil)
             (:pure t)
             (:constructor make-defstruct-description (null-lexenv-p name)))
  ;; name of the structure
  (name (missing-arg) :type symbol :read-only t)
  ;; documentation on the structure
  (doc nil :type (or string null))
  ;; prefix for slot names. If NIL, none.
  (conc-name nil :type (or string null))
  ;; All the :CONSTRUCTOR specs and posssibly an implied constructor,
  ;; keyword constructors first, then BOA constructors. NIL if none.
  (constructors () :type list)
  ;; True if the DEFSTRUCT appeared in a null lexical environment.
  (null-lexenv-p nil :type boolean :read-only t) ; the safe default is NIL
  ;; name of copying function
  (copier-name nil :type symbol)
  ;; name of type predicate
  (predicate-name nil :type symbol)
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
  ;; number of data words, including the layout itself if the layout
  ;; requires an entire word (when no immobile-space)
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

  ;; which :PRINT-mumble option was given, if either was.
  (print-option nil :type (member nil :print-function :print-object))
  ;; the argument to the PRINT-FUNCTION or PRINT-OBJECT option.
  ;; NIL if the option was given with no argument.
  (printer-fname nil :type (or cons symbol))

  ;; the value of the :PURE option, used by cheneygc when purifying.
  ;; This is true if objects of this class are never modified to
  ;; contain dynamic pointers in their slots or constant-like
  ;; substructure (and hence can be copied into read-only space by
  ;; PURIFY).
  ;; This is only meaningful if DD-CLASS-P = T.
  (pure nil :type (member t nil)))
(declaim (freeze-type defstruct-description))
(!set-load-form-method defstruct-description (:host :xc :target))

;;;; basic LAYOUT stuff

;;; Careful here: if you add more bits, then adjust the bit packing for
;;; 64-bit layouts which also store LENGTH + DEPTHOID in the same word.
(defconstant +structure-layout-flag+         #b00000001)
(defconstant +pathname-layout-flag+          #b00000010)
(defconstant +condition-layout-flag+         #b00001000)
(defconstant +pcl-object-layout-flag+        #b00010000)
(defconstant sb-vm:lockfree-list-node-flag   #b01000000) ; exported for use in gc-private.h

;;; The LAYOUT structure is pointed to by the first cell of instance
;;; (or structure) objects. It represents what we need to know for
;;; type checking and garbage collection. Whenever a class is
;;; incompatibly redefined, a new layout is allocated. If two object's
;;; layouts are EQ, then they are exactly the same type.
;;;
;;; *** IMPORTANT ***
;;;
;;; If you change the slots of LAYOUT, you need to alter genesis as
;;; well, since the initialization of layout slots is hardcoded there.
;;;
;;; FIXME: ...it would be better to automate this, of course...

;;; 64-bit layout FLAGS slot:
;;;
;;; | 4 bytes  | 16 bits | 16 bits |
;;; +----------+---------+---------+
;;;  depthoid    length     flags
;;;
;;; depthoid is stored as a tagged fixnum in its 4 byte field.
;;; I suspect that by further limiting the max depthoid and length
;;; we could shove the random CLOS-HASH into some unused bits while
;;; utilizing the entire 64-bit word as the random bit string for hashing.
;;; Checking for an invalid layout would need to mask out the
;;; length, depthoid, and flags since they have to stay correct at all times.
;;;
;;; 32-bit layout %BITS slot:
;;;
;;; | 2 bytes | 2 bytes |
;;; +---------+---------+
;;;  depthoid    length
;;; (FLAGS will remain as a separate slot)

;;; 32-bit is not done yet. Three slots are still used, instead of two.

;;; Maximum value of N in ANCESTOR_N. Couldn't come up with a better name.
(defconstant sb-c::layout-inherits-max-optimized-depth 5)
(sb-xc:defstruct (layout
             ;; Accept a specific subset of keywords
             #+64-bit (:constructor %make-layout
                          (clos-hash classoid flags info bitmap))
             #-64-bit (:constructor %make-layout
                          (clos-hash classoid depthoid length flags info bitmap))
             (:copier nil))

  ;; A packed field containing the DEPTHOID, LENGTH, and FLAGS
  #+64-bit (flags 0 :type (signed-byte #.sb-vm:n-word-bits))

  ;; a union of +something-LAYOUT-FLAG+ bits
  #-64-bit (flags 0 :type word :read-only nil)

  ;; a quasi-random hash value for use by CLOS. Determine by class-name
  ;; for classes named by a symbol, otherwise a pseudo-random value.
  ;; Must be acceptable as an argument to SB-INT:MIX
  (clos-hash (missing-arg) :type (and fixnum unsigned-byte))
  ;; the class that this is a layout for
  (classoid (missing-arg) :type classoid)
  ;; The value of this slot can be:
  ;;   * :UNINITIALIZED if not initialized yet;
  ;;   * NIL if this is the up-to-date layout for a class; or
  ;;   * T if this layout has been invalidated (by being replaced by
  ;;     a new, more-up-to-date LAYOUT).
  ;;   * something else (probably a list) if the class is a PCL wrapper
  ;;     and PCL has made it invalid and made a note to itself about it
  (invalid :uninitialized :type (or cons (member nil t :uninitialized)))
  ;; the layouts for all classes we inherit. If hierarchical, i.e. if
  ;; DEPTHOID >= 0, then these are ordered by ORDER-LAYOUT-INHERITS
  ;; (least to most specific), so that each inherited layout appears
  ;; at its expected depth, i.e. at its LAYOUT-DEPTHOID value.
  ;;
  ;; Remaining elements are filled by the non-hierarchical layouts or,
  ;; if they would otherwise be empty, by copies of succeeding layouts.
  (inherits #() :type simple-vector)
  ;; If inheritance is not hierarchical, this is -1. If inheritance is
  ;; hierarchical, this is the inheritance depth, i.e. (LENGTH INHERITS).
  ;; Note:
  ;;  (1) This turns out to be a handy encoding for arithmetically
  ;;      comparing deepness; it is generally useful to do a bare numeric
  ;;      comparison of these depthoid values, and we hardly ever need to
  ;;      test whether the values are negative or not.
  ;;  (2) This was called INHERITANCE-DEPTH in classic CMU CL. It was
  ;;      renamed because some of us find it confusing to call something
  ;;      a depth when it isn't quite.
  #-64-bit (depthoid -1 :type layout-depthoid)
  ;; the number of top level descriptor cells in each instance
  ;; For [FUNCALLABLE-]STANDARD-OBJECT instances, this is the slot vector
  ;; length, not the primitive object length.
  ;; I tried making a structure of this many slots, and the compiler blew up;
  ;; so it's fair to say this limit is sufficient for practical purposes,
  ;; Let's be consistent here between the two choices of word size.
  #-64-bit (length 0 :type layout-length) ; smaller than SB-INT:INDEX
  ;; If this layout has some kind of compiler meta-info, then this is
  ;; it. If a structure, then we store the DEFSTRUCT-DESCRIPTION here.
  (info nil :type (or null defstruct-description))
  ;; Map of raw slot indices.
  ;; These will eventually be moved to the end of the structure
  ;; as a variable-length raw slot.
  (bitmap +layout-all-tagged+ :type layout-bitmap :read-only t)
  ;; EQUALP comparator for two instances with this layout
  ;; Could be the generalized function, or a type-specific one
  ;; if the defstruct was compiled in a policy of SPEED 3.
  (equalp-impl #'equalp-err :type (sfunction (t t) boolean) :read-only t)
  ;; If this layout is for an object of metatype STANDARD-CLASS,
  ;; these are the EFFECTIVE-SLOT-DEFINITION metaobjects.
  (slot-list nil :type list)
  ;; Information about slots in the class to PCL: this provides fast
  ;; access to slot-definitions and locations by name, etc.
  ;; See MAKE-SLOT-TABLE in pcl/slots-boot.lisp for further details.
  (slot-table #(1 nil) :type simple-vector)
  ;; inherited layouts or 0, only pertinent to structure classoids.
  ;; There is no need to store the layout at depths 0 or 1
  ;; since they're predetermined to be T and STRUCTURE-OBJECT.
  (ancestor_2 0)
  (ancestor_3 0)
  (ancestor_4 0)
  (ancestor_5 0))
(declaim (freeze-type layout))

(defun equalp-err (a b)
  (bug "EQUALP ~S ~S" a b))

;;; Applicable only if bit-packed (for 64-bit architectures)
(defmacro pack-layout-flags (depthoid length flags)
  `(logior (ash ,(or depthoid -1) (+ 32 sb-vm:n-fixnum-tag-bits))
           (ash ,(or length 0) 16)
           ,(or flags 0)))

(defmacro get-dsd-index (type-name slot-name)
  ;; It seems to be an error in CCL to declare something NOTINLINE
  ;; if it is an unknown function.
  #+sb-xc (declare (notinline dsd-index)) ; forward reference
  (dsd-index (find slot-name
                   (dd-slots (find-defstruct-description type-name))
                   :key #'dsd-name)))

(defmacro set-layout-bitmap (layout bitmap)
  #+sb-xc-host (declare (ignore layout bitmap))
  #-sb-xc-host
  `(setf (%instance-ref (the layout ,layout) (get-dsd-index layout bitmap))
         ,bitmap))

(defmacro set-bitmap-from-layout (to-layout from-layout)
  #+sb-xc-host (declare (ignore to-layout from-layout))
  ;; While this obviously has a straightforward implementation for now,
  ;; that will change once the bits are stored as trailing slots.
  #-sb-xc-host
  `(setf (%instance-ref (the layout ,to-layout) (get-dsd-index layout bitmap))
        (layout-bitmap ,from-layout)))

;;; It is purely coincidental that these are the negatives of one another.
;;; See the pictures above DD-BITMAP in src/code/defstruct for the details.
(defconstant standard-gf-primitive-obj-layout-bitmap
  #+immobile-code  6
  #-immobile-code -6)

#+sb-xc-host
(defmacro set-layout-inherits (layout inherits)
  `(setf (layout-inherits ,layout) ,inherits))
#-sb-xc-host
(defmacro set-layout-inherits (layout inherits &optional recompute-bitmap)
  `(let* ((l ,layout) (i ,inherits) (d (length i)))
     (setf (layout-inherits l) i)
     (setf (layout-ancestor_2 l) (if (> d 2) (svref i 2) 0)
           (layout-ancestor_3 l) (if (> d 3) (svref i 3) 0)
           (layout-ancestor_4 l) (if (> d 4) (svref i 4) 0)
           (layout-ancestor_5 l) (if (> d 5) (svref i 5) 0))
     ;; This part is for PCL where a class can forward-reference its superclasses
     ;; and we only decide at class finalization time whether it is funcallable.
     ;; Picking the right bitmap could probably be done sooner given the metaclass,
     ;; but this approach avoids changing how PCL uses MAKE-LAYOUT.
     ;; The big comment above MAKE-IMMOBILE-FUNINSTANCE in src/code/x86-64-vm
     ;; explains why we differentiate between SGF and everything else.
     ,(when recompute-bitmap
        `(when (find ,(find-layout 'function) i)
           (set-layout-bitmap
            l
            #+immobile-code ; there are two possible bitmap
            ;; *SGF-WRAPPER* isn't defined as yet, but this is just an s-expression.
            (if (find sb-pcl::*sgf-wrapper* i)
                standard-gf-primitive-obj-layout-bitmap
                +layout-all-tagged+)
            ;; there is only one possible bitmap otherwise
            #-immobile-code standard-gf-primitive-obj-layout-bitmap)))
     l))
(push '("SB-KERNEL" set-layout-inherits) *!removable-symbols*)

;;; For lack of any better to place to write up some detail surrounding
;;; layout creation for structure types, I'm putting here.
;;; When you issue a DEFSTRUCT at the REPL, there are *three* instances
;;; of LAYOUT makde for the new structure.
;;; 1) The first is one associated with a temporary instance of
;;; structure-classoid used in parsing the DEFSTRUCT form so that
;;; we don't signal an UNKNOWN-TYPE condition for something like:
;;;   (defstruct chain (next nil :type (or null chain)).
;;; The temporary classoid is garbage immediately after parsing
;;; and is never installed.
;;; 2) The next is the actual LAYOUT that ends up being registered.
;;; 3) The third is a layout created when setting the "compiler layout"
;;; which contains copies of the length/depthoid/inherits etc
;;; that we compare against the isntalled one to make sure they match.
;;; The third one also gets thrown away.
#-sb-xc-host
(defun make-layout (clos-hash classoid
                    &key (depthoid -1) (length 0) (flags 0)
                         (inherits #())
                         (info nil)
                         (bitmap (if info (dd-bitmap info) 0))
                         (invalid :uninitialized))
  (let ((layout (%make-layout clos-hash classoid
                              #+64-bit (pack-layout-flags depthoid length flags)
                              #-64-bit depthoid #-64-bit length #-64-bit flags
                              info bitmap)))
    (set-layout-inherits layout inherits)
    (setf (layout-invalid layout) invalid)
    layout))

;;; The cross-compiler representation of a LAYOUT omits several things:
;;;   * BITMAP - obtainable via (DD-MAPMAP (LAYOUT-INFO layout)).
;;;     GC wants it in the layout to avoid double indirection.
;;;   * EQUALP-TESTS - needed only for the target's implementation of EQUALP.
;;;   * SLOT-TABLE, and SLOT-LIST - used only by the CLOS implementation.
;;;   * ANCESTOR_N are optimizations for TYPEP.
;;; So none of those really make sense on the host.
;;; Also, we eschew the packed representation of length+depthoid+flags.
;;; FLAGS are not even strictly necessary, since they are for optimizing
;;; various type checks.
#+sb-xc-host
(progn
  (defstruct (layout (:include structure!object)
                     (:constructor host-make-layout
                                   (clos-hash classoid &key info depthoid inherits
                                                       length flags invalid)))
    ;; CLOS-HASH is needed to convert some TYPECASE forms to jump tables.
    ;; Theoretically we don't need this in the cross-compiler, because the
    ;; layout has a classoid which has a name which has a known hash.
    ;; But there's no harm in storing it.
    (clos-hash nil :type (and sb-xc:fixnum unsigned-byte))
    (classoid nil :type classoid)
    (flags 0 :type word)
    (invalid :uninitialized :type (or cons (member nil t :uninitialized)))
    (inherits #() :type simple-vector)
    (depthoid -1 :type layout-depthoid)
    (length 0 :type layout-length)
    (info nil :type (or null defstruct-description)))
  (defun make-layout (&rest args)
    (let ((args (copy-list args)))
      (remf args :bitmap)
      (apply #'host-make-layout args)))
  (defun layout-bitmap (layout)
    (if (layout-info layout) (dd-bitmap (layout-info layout)) +layout-all-tagged+)))

(defmacro sb-c::layout-nth-ancestor-slot (n)
  `(case ,n
     (2 'layout-ancestor_2)
     (3 'layout-ancestor_3)
     (4 'layout-ancestor_4)
     (5 'layout-ancestor_5)))

#+(and (not sb-xc-host) 64-bit)
;;; LAYOUT-DEPTHOID gets a vop and a stub
(defmacro layout-length (layout) ; SETFable
  `(ldb (byte 16 16) (layout-flags ,layout)))

(defconstant layout-flags-mask #xffff) ; "strictly flags" bits from the packed field

;;; Abstract out the differences between {32-bit,64-bit} target and XC layouts.
;;; FLAGS can't change once assigned.
(defmacro assign-layout-slots (layout &key depthoid length
                                           (flags `(layout-flags ,layout)) flagsp)
  (let ((invalidate-p (and (eql depthoid -1) (not length) (not flagsp))))
    #+(and 64-bit (not xc-host)) ; packed slot
    `(setf (layout-flags ,layout)
           ,(if invalidate-p
                `(logior (ash -1 (+ 32 sb-vm:n-fixnum-tag-bits))
                         (ldb (byte 32 0) (layout-flags ,layout)))
                `(pack-layout-flags ,depthoid ,length
                                    (logand ,flags layout-flags-mask))))
    #+(or (not 64-bit) sb-xc-host) ; ordinary slot
    (if invalidate-p
        `(setf (layout-depthoid ,layout) -1)
        `(setf (layout-depthoid ,layout) ,depthoid
               (layout-length ,layout) ,length
               (layout-flags ,layout) ,flags))))

;;; True of STANDARD-OBJECT, which include generic functions.
;;; This one includes any class that mixes in STANDARD-OBJECT.
(declaim (inline layout-for-pcl-obj-p))
(defun layout-for-pcl-obj-p (x)
  (logtest (layout-flags x) +pcl-object-layout-flag+))

(declaim (inline sb-fasl:dumpable-layout-p))
(defun sb-fasl:dumpable-layout-p (x)
  (and (typep x 'layout) (not (layout-for-pcl-obj-p x))))

;;; The CLASSOID structure is a supertype of all classoid types.  A
;;; CLASSOID is also a CTYPE structure as recognized by the type
;;; system.  (FIXME: It's also a type specifier, though this might go
;;; away as with the merger of SB-PCL:CLASS and CL:CLASS it's no
;;; longer necessary)
(def!struct (classoid
             (:include ctype)
             (:constructor nil)
             (:copier nil)
             (:print-object
              (lambda (class stream)
                (let ((name (classoid-name class)))
                  (print-unreadable-object (class stream
                                                  :type t
                                                  :identity (not name))
                    (format stream
                            ;; FIXME: Make sure that this prints
                            ;; reasonably for anonymous classes.
                            "~:[anonymous~;~:*~S~]~@[ (~(~A~))~]"
                            name
                            (classoid-state class))))))
             #-sb-xc-host (:pure nil))
  ;; the value to be returned by CLASSOID-NAME.
  (name nil :type symbol)
  ;; the current layout for this class, or NIL if none assigned yet
  (layout nil :type (or layout null))
  ;; How sure are we that this class won't be redefined?
  ;;   :READ-ONLY = We are committed to not changing the effective
  ;;                slots or superclasses.
  ;;   :SEALED    = We can't even add subclasses.
  ;;   NIL        = Anything could happen.
  (state nil :type (member nil :read-only :sealed))
  ;; direct superclasses of this class. Always NIL for CLOS classes.
  (direct-superclasses () :type list)
  ;; Definition location
  ;; Not used for standard-classoid, because pcl has its own mechanism.
  (source-location nil)
  ;; representation of all of the subclasses (direct or indirect) of
  ;; this class. This is NIL if no subclasses or not initalized yet;
  ;; otherwise, it's an EQ hash-table mapping CLASSOID objects to the
  ;; subclass layout that was in effect at the time the subclass was
  ;; created.
  (subclasses nil :type (or null hash-table))
  ;; the PCL class (= CL:CLASS, but with a view to future flexibility
  ;; we don't just call it the CLASS slot) object for this class, or
  ;; NIL if none assigned yet
  (pcl-class nil))

(defun layout-classoid-name (x)
  (classoid-name (layout-classoid x)))

;;;; object types to represent classes

;;; An UNDEFINED-CLASSOID is a cookie we make up to stick in forward
;;; referenced layouts. Users should never see them.
(def!struct (undefined-classoid
             (:include classoid)
             (:copier nil)
             (:constructor make-undefined-classoid
                 (name &aux (%bits (pack-ctype-bits classoid name))))))

;;; BUILT-IN-CLASS is used to represent the standard classes that
;;; aren't defined with DEFSTRUCT and other specially implemented
;;; primitive types whose only attribute is their name.
;;;
;;; Some BUILT-IN-CLASSes have a TRANSLATION, which means that they
;;; are effectively DEFTYPE'd to some other type (usually a union of
;;; other classes or a "primitive" type such as NUMBER, ARRAY, etc.)
;;; This translation is done when type specifiers are parsed. Type
;;; system operations (union, subtypep, etc.) should never encounter
;;; translated classes, only their translation.
(def!struct (built-in-classoid (:include classoid)
                               (:copier nil)
                               (:constructor !make-built-in-classoid))
  ;; the type we translate to on parsing. If NIL, then this class
  ;; stands on its own; or it can be set to :INITIALIZING for a period
  ;; during cold-load.
  (translation nil :type (or ctype (member nil :initializing))))

(def!struct (condition-classoid (:include classoid)
                                (:copier nil)
                                (:constructor make-condition-classoid
                                    (&key name &aux (%bits (pack-ctype-bits classoid name)))))
  ;; list of CONDITION-SLOT structures for the direct slots of this
  ;; class
  (slots nil :type list)
  ;; list of CONDITION-SLOT structures for all of the effective class
  ;; slots of this class
  (class-slots nil :type list)
  ;; report function or NIL
  (report nil :type (or function null))
  ;; list of specifications of the form
  ;;
  ;;   (INITARG INITFORM THUNK)
  ;;
  ;; where THUNK, when called without arguments, returns the value for
  ;; INITARG.
  (direct-default-initargs () :type list)
  ;; class precedence list as a list of CLASS objects, with all
  ;; non-CONDITION classes removed
  (cpl () :type list)
  ;; a list of all the effective instance allocation slots of this
  ;; class that have a non-constant initform or default-initarg.
  ;; Values for these slots must be computed in the dynamic
  ;; environment of MAKE-CONDITION.
  (hairy-slots nil :type list))

;;;; classoid namespace

;;; We use an indirection to allow forward referencing of class
;;; definitions with load-time resolution.
(def!struct (classoid-cell
             (:copier nil)
             (:constructor make-classoid-cell (name &optional classoid))
             (:print-object (lambda (s stream)
                              (print-unreadable-object (s stream :type t)
                                (prin1 (classoid-cell-name s) stream)))))
  ;; Name of class we expect to find.
  (name nil :type symbol :read-only t)
  ;; Classoid or NIL if not yet defined.
  (classoid nil :type (or classoid null))
  ;; PCL class, if any
  (pcl-class nil))
(declaim (freeze-type classoid-cell))
(!set-load-form-method classoid-cell (:xc :target)
  (lambda (self env)
    (declare (ignore env))
    `(find-classoid-cell ',(classoid-cell-name self) :create t)))

(defun find-classoid-cell (name &key create)
  (let ((real-name (uncross name)))
    (cond ((info :type :classoid-cell real-name))
          (create
           (get-info-value-initializing :type :classoid-cell real-name
                                        (make-classoid-cell real-name))))))

;;; Return the classoid with the specified NAME. If ERRORP is false,
;;; then NIL is returned when no such class exists.
(defun find-classoid (name &optional (errorp t))
  (declare (type symbol name))
  (let ((cell (find-classoid-cell name)))
    (cond ((and cell (classoid-cell-classoid cell)))
          (errorp
           (error 'simple-type-error
                  :datum nil
                  :expected-type 'class
                  :format-control "Class not yet defined: ~S"
                  :format-arguments (list name))))))

;;;; PCL stuff

;;; the CLASSOID that we use to represent type information for
;;; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.  The type system
;;; side does not need to distinguish between STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.
(def!struct (standard-classoid (:include classoid)
                               (:copier nil)
                               (:constructor make-standard-classoid
                                   (&key name pcl-class
                                    &aux (%bits (pack-ctype-bits classoid name)))))
  old-layouts)
;;; a metaclass for classes which aren't standardlike but will never
;;; change either.
(def!struct (static-classoid (:include classoid)
                             (:copier nil)
                             (:constructor make-static-classoid
                                 (&key name &aux (%bits (pack-ctype-bits classoid name))))))

(declaim (freeze-type built-in-classoid condition-classoid
                      standard-classoid static-classoid))

(in-package "SB-C")

;;; layout for this type being used by the compiler
(define-info-type (:type :compiler-layout)
  :type-spec (or layout null)
  :default (lambda (name)
             (awhen (find-classoid name nil) (classoid-layout it))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun ftype-from-fdefn (name)
  (declare (ignorable name))
  ;; Again [as in (DEFINE-INFO-TYPE (:FUNCTION :TYPE) ...)] it's
  ;; not clear how to generalize the FBOUNDP expression to the
  ;; cross-compiler. -- WHN 19990330
  #+sb-xc-host
  (specifier-type 'function)
  #-sb-xc-host
  (let* ((fdefn (sb-kernel::find-fdefn name))
         (fun (and fdefn (fdefn-fun fdefn))))
    (if fun
        (handler-bind ((style-warning #'muffle-warning))
          (specifier-type (sb-impl::%fun-type fun)))
        (specifier-type 'function)))))

;;; The parsed or unparsed type for this function, or the symbol :GENERIC-FUNCTION.
;;; Ordinarily a parsed type is stored. Only if the parsed type contains
;;; an unknown type will the original specifier be stored; we attempt to reparse
;;; on each lookup, in the hope that the type becomes known at some point.
;;; If :GENERIC-FUNCTION, the info is recomputed from methods at the time of lookup
;;; and stored back. Method redefinition resets the value to :GENERIC-FUNCTION.
(define-info-type (:function :type)
  :type-spec (or ctype (cons (eql function)) (member :generic-function))
  :default #'ftype-from-fdefn)

(defun summarize-layouts ()
  (let ((prev -1))
    (dolist (layout (sort (loop for v being each hash-value
                                of (classoid-subclasses (find-classoid 't))
                                collect v)
                          #'< :key #'sb-kernel:layout-flags))
      (let ((flags (sb-kernel:layout-flags layout)))
        (unless (= flags prev)
          (format t "Layout flags = ~d~%" flags)
          (setq prev flags)))
      (format t "  ~a~%" layout))))
