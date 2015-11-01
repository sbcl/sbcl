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

(in-package "SB!KERNEL")

;;;; DEFSTRUCT-DESCRIPTION

;;; The DEFSTRUCT-DESCRIPTION structure holds compile-time information
;;; about a structure type.
;;; It is defined prior to LAYOUT because a LAYOUT-INFO slot
;;; is declared to hold a DEFSTRUCT-DESCRIPTION.
(def!struct (defstruct-description
             (:conc-name dd-)
             (:make-load-form-fun just-dump-it-normally)
             #-sb-xc-host (:pure t)
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

  ;; which :PRINT-mumble option was given, if either was.
  (print-option nil :type (member nil :print-function :print-object))
  ;; the argument to the PRINT-FUNCTION or PRINT-OBJECT option.
  ;; NIL if the option was given with no argument.
  (printer-fname nil :type (or cons symbol))

  ;; The number of untagged slots at the end.
  #!-interleaved-raw-slots (raw-length 0 :type index)
  ;; the value of the :PURE option, or :UNSPECIFIED. This is only
  ;; meaningful if DD-CLASS-P = T.
  (pure :unspecified :type (member t nil :unspecified)))
#!-sb-fluid (declaim (freeze-type defstruct-description))

;;;; basic LAYOUT stuff

;;; Note: This bound is set somewhat less than MOST-POSITIVE-FIXNUM
;;; in order to guarantee that several hash values can be added without
;;; overflowing into a bignum.
(defconstant layout-clos-hash-limit (1+ (ash sb!xc:most-positive-fixnum -3))
  #!+sb-doc
  "the exclusive upper bound on LAYOUT-CLOS-HASH values")
;; This must be DEF!TYPE and not just DEFTYPE because access to slots
;; of a layout occur "before" the structure definition is made in the
;; run-the-xc pass, and the source-transform of a slot accessor
;; wraps (TRULY-THE <type> ...) around %INSTANCE-REF,
;; so <type> had best be defined at that point.
(def!type layout-clos-hash () `(integer 0 ,layout-clos-hash-limit))
(declaim (ftype (sfunction () layout-clos-hash) random-layout-clos-hash))

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
(def!struct (layout
             ;; KLUDGE: A special hack keeps this from being
             ;; called when building code for the
             ;; cross-compiler. See comments at the DEFUN for
             ;; this. -- WHN 19990914
             (:make-load-form-fun #-sb-xc-host ignore-it
                                  ;; KLUDGE: DEF!STRUCT at #+SB-XC-HOST
                                  ;; time controls both the
                                  ;; build-the-cross-compiler behavior
                                  ;; and the run-the-cross-compiler
                                  ;; behavior. The value below only
                                  ;; works for build-the-cross-compiler.
                                  ;; There's a special hack in
                                  ;; EMIT-MAKE-LOAD-FORM which gives
                                  ;; effectively IGNORE-IT behavior for
                                  ;; LAYOUT at run-the-cross-compiler
                                  ;; time. It would be cleaner to
                                  ;; actually have an IGNORE-IT value
                                  ;; stored, but it's hard to see how to
                                  ;; do that concisely with the current
                                  ;; DEF!STRUCT setup. -- WHN 19990930
                                  #+sb-xc-host
                                  make-load-form-for-layout))
  ;; a pseudo-random hash value for use by CLOS.
  (clos-hash (random-layout-clos-hash) :type layout-clos-hash)
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
  (depthoid -1 :type layout-depthoid)
  ;; the number of top level descriptor cells in each instance
  (length 0 :type index)
  ;; If this layout has some kind of compiler meta-info, then this is
  ;; it. If a structure, then we store the DEFSTRUCT-DESCRIPTION here.
  (info nil :type (or null defstruct-description))
  ;; This is true if objects of this class are never modified to
  ;; contain dynamic pointers in their slots or constant-like
  ;; substructure (and hence can be copied into read-only space by
  ;; PURIFY).
  ;;
  ;; This slot is known to the C runtime support code.
  (pure nil :type (member t nil 0))
  ;; Number of raw words at the end.
  ;; This slot is known to the C runtime support code.
  ;; It counts the number of untagged cells, not user-visible slots.
  ;; e.g. on 32-bit machines, each (COMPLEX DOUBLE-FLOAT) counts as 4.
  #!-interleaved-raw-slots (n-untagged-slots 0 :type index)
  ;; Metadata
  #!+interleaved-raw-slots (untagged-bitmap 0 :type unsigned-byte)
  #!+interleaved-raw-slots (equalp-tests #() :type simple-vector)
  ;; Definition location
  (source-location nil)
  ;; If this layout is for an object of metatype STANDARD-CLASS,
  ;; these are the EFFECTIVE-SLOT-DEFINITION metaobjects.
  (slot-list nil :type list)
  ;; Information about slots in the class to PCL: this provides fast
  ;; access to slot-definitions and locations by name, etc.
  ;; See MAKE-SLOT-TABLE in pcl/slots-boot.lisp for further details.
  (slot-table #(1 nil) :type simple-vector)
  ;; True IFF the layout belongs to a standand-instance or a
  ;; standard-funcallable-instance.
  ;; Old comment was:
  ;;   FIXME: If we unify wrappers and layouts this can go away, since
  ;;   it is only used in SB-PCL::EMIT-FETCH-WRAPPERS, which can then
  ;;   use INSTANCE-SLOTS-LAYOUT instead (if there is are no slot
  ;;   layouts, there are no slots for it to pull.)
  ;; But while that's conceivable, it still seems advantageous to have
  ;; a single bit that decides whether something is STANDARD-OBJECT.
  (%for-std-class-b 0 :type bit :read-only t))
(declaim (freeze-type layout))

;;; The CLASSOID structure is a supertype of all classoid types.  A
;;; CLASSOID is also a CTYPE structure as recognized by the type
;;; system.  (FIXME: It's also a type specifier, though this might go
;;; away as with the merger of SB-PCL:CLASS and CL:CLASS it's no
;;; longer necessary)
(def!struct (classoid
             (:make-load-form-fun classoid-make-load-form-fun)
             (:include ctype
                       (class-info (type-class-or-lose 'classoid)))
             (:constructor nil)
             #-no-ansi-print-object
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

;;;; object types to represent classes

;;; An UNDEFINED-CLASSOID is a cookie we make up to stick in forward
;;; referenced layouts. Users should never see them.
(def!struct (undefined-classoid
             (:include classoid)
             (:constructor make-undefined-classoid (name))))

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
                               (:constructor make-built-in-classoid))
  ;; the type we translate to on parsing. If NIL, then this class
  ;; stands on its own; or it can be set to :INITIALIZING for a period
  ;; during cold-load.
  (translation nil :type (or ctype (member nil :initializing))))

(def!struct (condition-classoid (:include classoid)
                                (:constructor make-condition-classoid))
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
             (:constructor make-classoid-cell (name &optional classoid))
             (:make-load-form-fun (lambda (c)
                                    `(find-classoid-cell
                                      ',(classoid-cell-name c)
                                      :create t)))
             #-no-ansi-print-object
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

;;; This would be a logical place to define FIND-CLASSOID-CELL,
;;; but since 'globaldb' occurs later in the build order,
;;; you'd have to go out of your way to declare INFO notinline.

;;;; PCL stuff

;;; the CLASSOID that we use to represent type information for
;;; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.  The type system
;;; side does not need to distinguish between STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.
(def!struct (standard-classoid (:include classoid)
                               (:constructor make-standard-classoid)))
;;; a metaclass for classes which aren't standardlike but will never
;;; change either.
(def!struct (static-classoid (:include classoid)
                             (:constructor make-static-classoid)))

(declaim (freeze-type built-in-classoid condition-classoid
                      standard-classoid static-classoid))
